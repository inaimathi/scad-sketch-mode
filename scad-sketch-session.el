;;; scad-sketch-session.el --- Session construction for scad-sketch -*- lexical-binding: t; -*-

;;; Commentary:

;; Session and source-buffer discovery layer for scad-sketch.
;;
;; This layer owns the parser dependency.  The editor mode should stay mostly
;; parser-agnostic: it receives a `scad-sketch-session' and mutates the current
;; editable points/marks/selection.  This file is responsible for resolving the
;; buffer position into source regions and constructing that session.
;;
;; Current parser-backed milestone:
;;   - Parse the whole source buffer with `scad-sketch-parse'.
;;   - Find the array assignment node at point.
;;   - Build the same point-list session shape the editor already expects.
;;
;; The `scad-sketch-target' struct plus the AST/path/root fields on the session
;; are intentionally early scaffolding for the later tree/multi-object model
;; needed by boolean compositions and transformed 2D subtrees.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'scad-sketch-parse)

(defgroup scad-sketch nil
  "Keyboard sketch editor for OpenSCAD array literals."
  :group 'tools
  :prefix "scad-sketch-")

(defcustom scad-sketch-default-grid 1.0
  "Default grid step in sketch units."
  :type 'number :group 'scad-sketch)

(defcustom scad-sketch-default-fine-step 0.1
  "Default fine movement step in sketch units."
  :type 'number :group 'scad-sketch)

(defcustom scad-sketch-default-coarse-step 5.0
  "Default coarse movement step in sketch units."
  :type 'number :group 'scad-sketch)

(cl-defstruct scad-sketch-target
  id
  kind          ; currently only `array'; later `inline-polygon', `subtree', ...
  node          ; parser node this target came from
  source-node   ; array node if target resolves through a variable ref
  beg-marker
  end-marker
  name
  points
  metadata)

(cl-defstruct scad-sketch-session
  name units grid fine-step coarse-step closed
  points point
  marks          ; list of [x y], newest first; (car marks) is the current mark
  named-marks selected-index
  source-buffer content-beg content-end
  ast path root-node targets
  dirty undo-stack)

(defvar-local scad-sketch--session nil)

(defun scad-sketch--assert-session ()
  "Return the current sketch session or signal an error."
  (unless (and (boundp 'scad-sketch--session) scad-sketch--session)
    (user-error "No active scad-sketch session"))
  scad-sketch--session)

;;; Parser-backed source resolution

(defun scad-sketch-session--buffer-source ()
  "Return the current buffer contents without text properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun scad-sketch-session--buffer-offset (&optional pos)
  "Return 0-based source offset for buffer POS.
When POS is nil, use current point.  Parser nodes use 0-based string offsets;
Emacs buffer positions are 1-based."
  (1- (or pos (point))))

(defun scad-sketch-session--offset-marker (offset &optional insertion-type)
  "Return a marker in the current buffer for 0-based parser OFFSET."
  (copy-marker (1+ offset) insertion-type))

(defun scad-sketch-session--array-node-at-point (source pos)
  "Return parser information for an array node at POS in SOURCE.
POS is a 0-based source offset.  The return value is a plist:

  (:ast AST :path PATH :node NODE)

Signal a user error if point is not inside an array assignment."
  (let* ((ast  (scad-sketch-parse source))
         (path (scad-sketch-parse--path-to ast pos))
         (node (car (last path))))
    (unless (and node (eq (plist-get node :type) 'array))
      (user-error "Point is not inside a supported scad-sketch array assignment"))
    (list :ast ast :path path :node node)))

;;; Session construction

(defun scad-sketch-session--initial-point (points)
  "Return the initial cursor point for POINTS."
  (if points
      (list (float (nth 0 (car points)))
            (float (nth 1 (car points))))
    (list 0.0 0.0)))

(defun scad-sketch-session--make-array-target (node beg-marker end-marker)
  "Build a `scad-sketch-target' for array NODE."
  (make-scad-sketch-target
   :id 'array-0
   :kind 'array
   :node node
   :source-node node
   :beg-marker beg-marker
   :end-marker end-marker
   :name (plist-get node :name)
   :points (copy-tree (plist-get node :points))
   :metadata nil))

(defun scad-sketch--make-session (name points beg-marker end-marker
                                       &optional ast path root-node targets)
  "Create a sketch session for NAME with POINTS.
BEG-MARKER and END-MARKER delimit the current source replacement region.
AST, PATH, ROOT-NODE, and TARGETS are parser-backed scaffolding for later
multi-object/tree editing."
  (make-scad-sketch-session
   :name name
   :units "mm"
   :grid (float scad-sketch-default-grid)
   :fine-step (float scad-sketch-default-fine-step)
   :coarse-step (float scad-sketch-default-coarse-step)
   :closed t
   :points (copy-tree points)
   :point (scad-sketch-session--initial-point points)
   :marks nil
   :named-marks nil
   :selected-index (if points 0 nil)
   :source-buffer (current-buffer)
   :content-beg beg-marker
   :content-end end-marker
   :ast ast
   :path path
   :root-node root-node
   :targets targets
   :dirty nil
   :undo-stack nil))

(defun scad-sketch-session-at-point ()
  "Build a parser-backed sketch session for the array assignment at point.

This milestone deliberately supports only direct array nodes.  Polygon refs,
inline polygons, primitive shapes, transforms, and boolean subtrees should be
resolved here later, with the editor continuing to operate on the session data
it receives."
  (let* ((source (scad-sketch-session--buffer-source))
         (pos    (scad-sketch-session--buffer-offset))
         (info   (scad-sketch-session--array-node-at-point source pos))
         (ast    (plist-get info :ast))
         (path   (plist-get info :path))
         (node   (plist-get info :node))
         (beg    (scad-sketch-session--offset-marker (plist-get node :beg)))
         (end    (scad-sketch-session--offset-marker (plist-get node :end) t))
         (name   (plist-get node :name))
         (points (copy-tree (plist-get node :points)))
         (target (scad-sketch-session--make-array-target node beg end)))
    (scad-sketch--make-session name points beg end ast path node (list target))))

(defun scad-sketch-session-insert-array-at-point (name)
  "Insert a new empty array named NAME at point and return its session."
  (let (beg end node target)
    (setq beg (point-marker))
    (insert (format "%s = [\n];\n" name))
    (setq end (copy-marker (point) t))
    ;; Construct a minimal parser-shaped node so inserted sessions also expose
    ;; `root-node' and `targets' to later code.
    (setq node (list :type 'array
                     :name name
                     :points nil
                     :beg (1- (marker-position beg))
                     :end (1- (marker-position end))))
    (setq target (scad-sketch-session--make-array-target node beg end))
    (scad-sketch--make-session name nil beg end nil (list node) node (list target))))

(provide 'scad-sketch-session)
;;; scad-sketch-session.el ends here
