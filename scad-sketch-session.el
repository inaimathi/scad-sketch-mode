;;; scad-sketch-session.el --- Session struct and entry points for scad-sketch -*- lexical-binding: t; -*-

;;; Commentary:

;; Defines the `scad-sketch-session' struct that carries all editor state,
;; the functions that construct a session from an OpenSCAD source buffer,
;; the `scad-sketch-mode' minor mode that adds key bindings to .scad buffers,
;; and the top-level interactive entry points (`scad-sketch-at-point',
;; `scad-sketch-or-insert-at-point').
;;
;; DEPENDENCY ORDER
;;   scad-sketch-parse  ←  scad-sketch-geometry  ←  scad-sketch-session
;;                                                 ↑
;;                                        scad-sketch-editor-mode  (requires this file)
;;
;; SESSION STRUCT SLOTS
;; --------------------
;;   tree             list of top-level AST nodes (scad-sketch-parse output)
;;   source-text      original source string (for variable resolution)
;;   focused-path     nil or (i j …) path from root to the focused node
;;   hover-stack      list of nodes under cursor, deepest first (recomputed each render)
;;   selection        list of selection-ref plists (:kind :path :index)
;;   point            [x y] cursor position in model space
;;   marks            list of [x y], newest first
;;   named-marks      hash-table: polygon-node → (selected-index . closed)
;;                    (repurposed from the original per-polygon edit-state table)
;;   grid             current grid step (float)
;;   fine-step        fine movement step (float)
;;   coarse-step      coarse movement step (float)
;;   units            display unit string, e.g. "mm"
;;   source-buffer    the Emacs buffer being edited
;;   content-beg      marker: start of the editable region in source-buffer
;;   content-end      marker: end of the editable region (moves with inserts)
;;   dirty            non-nil when the tree differs from source-buffer content
;;   undo-stack       list of saved state plists

;;; Code:

(require 'cl-lib)
(require 'scad-sketch-parse)
(require 'scad-sketch-geometry)

;;;; ── Customization ───────────────────────────────────────────────────────────

(defgroup scad-sketch nil
  "Keyboard sketch editor for OpenSCAD 2D forms."
  :group 'tools
  :prefix "scad-sketch-")

(defcustom scad-sketch-default-grid 1.0
  "Default grid step in sketch units."
  :type 'number :group 'scad-sketch)

(defcustom scad-sketch-default-fine-step 0.1
  "Default fine movement step."
  :type 'number :group 'scad-sketch)

(defcustom scad-sketch-default-coarse-step 5.0
  "Default coarse movement step."
  :type 'number :group 'scad-sketch)

(defcustom scad-sketch-canvas-width 900
  "Canvas width in pixels."
  :type 'integer :group 'scad-sketch)

(defcustom scad-sketch-canvas-height 650
  "Canvas height in pixels."
  :type 'integer :group 'scad-sketch)

(defcustom scad-sketch-margin 48
  "Canvas margin in pixels."
  :type 'integer :group 'scad-sketch)

;;;; ── Session struct ─────────────────────────────────────────────────────────

(cl-defstruct scad-sketch-session
  ;; Tree and source
  tree
  source-text
  focused-path
  hover-stack
  selection
  ;; Cursor and marks
  point
  marks
  named-marks                           ; hash-table: node → (sel-idx . closed)
  grid
  fine-step
  coarse-step
  units
  ;; Source buffer linkage
  source-buffer
  content-beg
  content-end
  dirty
  undo-stack)

;;;; ── Minor mode (source buffer side) ────────────────────────────────────────

(defvar scad-sketch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'scad-sketch-at-point)
    (define-key map (kbd "C-c C-.") #'scad-sketch-or-insert-at-point)
    map)
  "Keymap for `scad-sketch-mode'.
C-c C-a  – open sketch editor on the form at point.
C-c C-.  – open editor, or insert a new named array if none found.")

;;;###autoload
(define-minor-mode scad-sketch-mode
  "Minor mode that adds sketch-editor key bindings to OpenSCAD source buffers.

\\{scad-sketch-mode-map}"
  :lighter " Sketch"
  :keymap scad-sketch-mode-map)

;;;; ── Internal: editor buffer constants ─────────────────────────────────────

(defconst scad-sketch--editor-buffer-prefix "*scad-sketch: "
  "Prefix for editor buffer names.")

;;;; ── Session construction helpers ──────────────────────────────────────────

(defun scad-sketch--path-in-list (nodes target)
  "Return the path (list of child indices from root) to TARGET in NODES.
NODES is the flat top-level list from `scad-sketch-parse'.
Returns nil if TARGET is not found."
  (cl-labels ((search (node-list prefix)
                (cl-loop for n in node-list
                         for i from 0
                         when (eq n target)
                           return (append prefix (list i))
                         thereis
                           (when (scad-sketch-parse--node-children n)
                             (search (scad-sketch-parse--node-children n)
                                     (append prefix (list i)))))))
    (search nodes nil)))

(defun scad-sketch--bbox-of (nodes node source-text)
  "Return bounding box (min-x max-x min-y max-y) for NODE in NODES context.
Constructs a minimal stub session so `scad-sketch-geo--node-bbox' can
resolve polygon variable references."
  ;; We build a minimal stub rather than importing the full editor-mode
  ;; machinery.  The polygon-points resolver only needs :tree and :source-text.
  (let ((stub (make-scad-sketch-session
               :tree nodes :source-text source-text
               :focused-path nil :hover-stack nil :selection nil
               :point '(0 0) :marks nil :named-marks nil
               :grid 1 :fine-step 0.1 :coarse-step 5 :units "mm"
               :source-buffer nil :content-beg nil :content-end nil
               :dirty nil :undo-stack nil)))
    (scad-sketch-geo--node-bbox
     node
     (lambda (n)
       (let ((pts (plist-get n :points))
             (src (plist-get n :source)))
         (if (and (null pts) src)
             (or (scad-sketch-parse--lookup-variable
                  src (scad-sketch-session-source-text stub) (plist-get n :beg))
                 pts)
           pts))))))

;;;; ── Public: session constructor ────────────────────────────────────────────

(defun scad-sketch-make-session (source-text origin-offset source-buffer beg end)
  "Parse SOURCE-TEXT and create a `scad-sketch-session' targeting ORIGIN-OFFSET.

SOURCE-BUFFER is the Emacs buffer being edited.  BEG and END are markers
bracketing the region that will be overwritten on write-back.

The session's initial focus is set to the deepest primitive (polygon, array,
circle, square, or text) that contains ORIGIN-OFFSET, if any.  Compositions
and transforms are left unfocused so the root view is shown first."
  (let* ((nodes        (scad-sketch-parse source-text))
         (target-node  (scad-sketch-parse-node-at nodes origin-offset))
         (target-path  (when target-node
                         (scad-sketch--path-in-list nodes target-node)))
         (focused-path (when (and target-node
                                  (memq (plist-get target-node :type)
                                        '(polygon array circle square text)))
                         target-path))
         (init-pt      (if target-node
                           (let ((bbox (scad-sketch--bbox-of
                                        nodes target-node source-text)))
                             (list (/ (+ (nth 0 bbox) (nth 1 bbox)) 2.0)
                                   (/ (+ (nth 2 bbox) (nth 3 bbox)) 2.0)))
                         (list 0.0 0.0))))
    (make-scad-sketch-session
     :tree          nodes
     :source-text   source-text
     :focused-path  focused-path
     :hover-stack   nil
     :selection     nil
     :point         init-pt
     :marks         nil
     :named-marks   nil
     :grid          (float scad-sketch-default-grid)
     :fine-step     (float scad-sketch-default-fine-step)
     :coarse-step   (float scad-sketch-default-coarse-step)
     :units         "mm"
     :source-buffer source-buffer
     :content-beg   beg
     :content-end   end
     :dirty         nil
     :undo-stack    nil)))

;;;; ── Public: open an editor buffer for a session ────────────────────────────

(defun scad-sketch-open-session (session)
  "Open (or reuse) an editor buffer for SESSION and switch to it.
The editor buffer is put into `scad-sketch-editor-mode', which is defined
in `scad-sketch-editor-mode.el' and required at load time."
  (require 'scad-sketch-editor-mode)
  (let* ((wconf (current-window-configuration))
         ;; Derive a human-readable buffer name from the focused node, if any.
         (name  (let ((path (scad-sketch-session-focused-path session)))
                  (if path
                      (let* ((tree (scad-sketch-session-tree session))
                             (node (nth (car path) tree)))
                        (or (plist-get node :name)
                            (symbol-name (plist-get node :type))))
                    "sketch")))
         (buf   (get-buffer-create
                 (format "%s%s*" scad-sketch--editor-buffer-prefix name))))
    (with-current-buffer buf
      (scad-sketch-editor-mode)
      (setq-local scad-sketch--session session)
      (setq-local scad-sketch--window-config wconf)
      (scad-sketch--render))
    (pop-to-buffer buf)))

;;;; ── Interactive entry points ───────────────────────────────────────────────

;;;###autoload
(defun scad-sketch-at-point ()
  "Open the sketch editor for the 2D OpenSCAD form at point.

The entire buffer is parsed (unknown constructs are silently skipped), so
variable references such as `polygon(pts)' can be resolved.  The editor
focuses on the most specific recognised primitive at point, if any.

Signals `user-error' if SVG support is absent or no 2D forms are found."
  (interactive)
  (unless (image-type-available-p 'svg)
    (user-error "This Emacs was not built with SVG image support"))
  (let* ((source-text (buffer-substring-no-properties (point-min) (point-max)))
         (origin-off  (1- (point)))
         (beg         (copy-marker (point-min)))
         (end         (copy-marker (point-max) t))
         (session     (condition-case err
                          (scad-sketch-make-session
                           source-text origin-off (current-buffer) beg end)
                        (user-error
                         (user-error "scad-sketch: %s" (cadr err))))))
    (unless (scad-sketch-session-tree session)
      (user-error "scad-sketch: no recognizable 2D forms found at or near point"))
    (scad-sketch-open-session session)))

;;;###autoload
(defun scad-sketch-insert-array-at-point (name)
  "Insert a new empty named array at point and open the sketch editor.
Prompts for a variable NAME, inserts `NAME = [];' at point, then opens
the editor with that array as the initial focus."
  (interactive "sArray name: ")
  (unless (image-type-available-p 'svg)
    (user-error "This Emacs was not built with SVG image support"))
  (let ((insert-pos (point)))
    (insert (format "%s = [];\n" name))
    (let* ((source-text (buffer-substring-no-properties (point-min) (point-max)))
           (origin-off  (1- insert-pos))
           (beg         (copy-marker (point-min)))
           (end         (copy-marker (point-max) t))
           (session     (scad-sketch-make-session
                         source-text origin-off (current-buffer) beg end)))
      (scad-sketch-open-session session))))

;;;###autoload
(defun scad-sketch-or-insert-at-point ()
  "Edit the SCAD form at point, or insert a new array if none is found.

Tries `scad-sketch-at-point' first.  If that signals a `user-error'
(typically because no 2D form was found at point), falls back to
`scad-sketch-insert-array-at-point', prompting for a variable name."
  (interactive)
  (unless (image-type-available-p 'svg)
    (user-error "This Emacs was not built with SVG image support"))
  (condition-case nil
      (scad-sketch-at-point)
    (user-error
     (call-interactively #'scad-sketch-insert-array-at-point))))

(provide 'scad-sketch-session)
;;; scad-sketch-session.el ends here
