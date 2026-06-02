;;; scad-sketch.el --- Keyboard sketch editor for OpenSCAD 2D forms -*- lexical-binding: t; -*-

;; Author: inaimathi, Claude Sonnet
;; Version: 0.5.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: cad, openscad, svg, tools

;;; Commentary:

;; Keyboard-driven SVG sketch editor for OpenSCAD 2D source forms.
;; Handles polygon arrays, circle/square/text primitives, boolean
;; compositions (difference/union/intersection), and transforms
;; (translate/rotate/scale/mirror).
;;
;; QUICK START
;; -----------
;; In any .scad buffer, position point inside a 2D form and run:
;;
;;   M-x scad-sketch-or-insert-at-point   (C-c C-. in scad-sketch-mode)
;;
;; The editor opens on the most specific 2D node at point:
;;   - Inside a primitive → edit that primitive
;;   - Inside a composition's braces/keyword → edit the composition
;;   - Inside a bare array assignment → edit that array
;;
;; To add scad-sketch-mode to all scad-mode buffers:
;;
;;   (add-hook 'scad-mode-hook #'scad-sketch-mode)
;;
;; SUPPORTED FORMS
;; ---------------
;;   name = [[x,y], ...];                plain array
;;   name = [[x,y,r], ...];             polyRound array
;;   polygon(pts) / polygon([[x,y]...]) polygon from array or variable
;;   polygon(polyRound(pts, fn))        polyRound polygon
;;   circle(r=N) / circle(d=N)
;;   square([W,H]) / square([W,H], center=true)
;;   text("str", size=N)
;;   difference() { ... } / union() { ... } / intersection() { ... }
;;   translate([x,y]) / rotate(a) / scale([x,y]) / mirror([x,y])
;;
;; EDITOR
;; ------
;; The editor buffer shows an SVG canvas with the current tree and
;; a live source preview below it.  Use C-h m or ? for key help.
;;
;; THREE INTERACTION STATES
;; ------------------------
;; Hovered  — the shape/point nearest to the cursor (computed each render).
;;            Use cycle-hover (bound to TAB at composition level) to cycle
;;            through overlapping candidates.
;; Selected — an explicit set of shapes/points toggled with SPC.
;;            Multi-select; no duplicates; cleared with C.
;; Focused  — one subtree being edited exclusively (others dimmed).
;;            Enter with RET; exit with ESC (pops one level).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'svg)
(require 'scad-sketch-parse)

;;;; Customization

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

;;;; Session struct

;; Selection refs: (:kind shape :path (0 1) :index nil)
;;                 (:kind point :path (0 1) :index 3)
;;
;; focused-path: nil = no focus (all shapes visible and interactive)
;;               (0) = focused on first top-level node
;;               (0 2) = focused on third child of first top-level node
;;
;; hover-stack: list of nodes at cursor position, deepest first.
;;              The car is the "active" hovered node.

(cl-defstruct scad-sketch-session
  ;; Tree and source
  tree             ; list of top-level AST nodes (from scad-sketch-parse)
  source-text      ; original source string (for variable lookup)
  focused-path     ; nil or list of child indices to the focused subtree
  hover-stack      ; list of nodes at cursor, deepest first (recomputed each render)
  selection        ; list of selection-ref plists
  ;; Cursor, marks, grid (unchanged from v0.4)
  point            ; [x y] cursor position
  marks            ; list of [x y], newest first
  named-marks
  grid
  fine-step
  coarse-step
  units
  ;; Source buffer linkage
  source-buffer
  content-beg      ; marker: start of editable region in source buffer
  content-end      ; marker: end of editable region
  dirty
  undo-stack)

(defvar-local scad-sketch--session nil)
(defvar-local scad-sketch--window-config nil)
(defvar scad-sketch--editor-buffer-prefix "*scad-sketch: ")

;;;; Minor mode

(defvar scad-sketch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'scad-sketch-at-point)
    (define-key map (kbd "C-c C-.") #'scad-sketch-or-insert-at-point)
    map)
  "Keymap for `scad-sketch-mode'.
C-c C-s and C-c C-o are intentionally left free for `scad-mode'.")

;;;###autoload
(define-minor-mode scad-sketch-mode
  "Minor mode for opening scad-sketch editors from OpenSCAD buffers."
  :lighter " Sketch"
  :keymap scad-sketch-mode-map)

;;;; Editor mode keymap

(defvar scad-sketch-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    ;; Cursor movement
    (define-key map (kbd "<left>")      #'scad-sketch-move-point-left)
    (define-key map (kbd "<right>")     #'scad-sketch-move-point-right)
    (define-key map (kbd "<up>")        #'scad-sketch-move-point-up)
    (define-key map (kbd "<down>")      #'scad-sketch-move-point-down)
    (define-key map (kbd "M-<left>")    #'scad-sketch-move-point-fine-left)
    (define-key map (kbd "M-<right>")   #'scad-sketch-move-point-fine-right)
    (define-key map (kbd "M-<up>")      #'scad-sketch-move-point-fine-up)
    (define-key map (kbd "M-<down>")    #'scad-sketch-move-point-fine-down)
    (define-key map (kbd "C-<left>")    #'scad-sketch-move-point-coarse-left)
    (define-key map (kbd "C-<right>")   #'scad-sketch-move-point-coarse-right)
    (define-key map (kbd "C-<up>")      #'scad-sketch-move-point-coarse-up)
    (define-key map (kbd "C-<down>")    #'scad-sketch-move-point-coarse-down)
    ;; Selected vertex/shape movement
    (define-key map (kbd "S-<left>")    #'scad-sketch-move-selected-left)
    (define-key map (kbd "S-<right>")   #'scad-sketch-move-selected-right)
    (define-key map (kbd "S-<up>")      #'scad-sketch-move-selected-up)
    (define-key map (kbd "S-<down>")    #'scad-sketch-move-selected-down)
    (define-key map (kbd "M-S-<left>")  #'scad-sketch-move-selected-fine-left)
    (define-key map (kbd "M-S-<right>") #'scad-sketch-move-selected-fine-right)
    (define-key map (kbd "M-S-<up>")    #'scad-sketch-move-selected-fine-up)
    (define-key map (kbd "M-S-<down>")  #'scad-sketch-move-selected-fine-down)
    (define-key map (kbd "C-S-<left>")  #'scad-sketch-move-selected-coarse-left)
    (define-key map (kbd "C-S-<right>") #'scad-sketch-move-selected-coarse-right)
    (define-key map (kbd "C-S-<up>")    #'scad-sketch-move-selected-coarse-up)
    (define-key map (kbd "C-S-<down>")  #'scad-sketch-move-selected-coarse-down)
    ;; Marks
    (define-key map (kbd "m")           #'scad-sketch-set-mark)
    (define-key map (kbd "M")           #'scad-sketch-push-mark)
    (define-key map (kbd "`")           #'scad-sketch-pop-mark)
    (define-key map (kbd "'")           #'scad-sketch-jump-to-mark)
    (define-key map (kbd "C")           #'scad-sketch-clear-marks)
    ;; Navigation / focus
    (define-key map (kbd "TAB")         #'scad-sketch-tab-next)
    (define-key map (kbd "<backtab>")   #'scad-sketch-tab-prev)
    (define-key map (kbd "RET")         #'scad-sketch-focus-hovered)
    (define-key map (kbd "ESC")         #'scad-sketch-unfocus)
    (define-key map (kbd "SPC")         #'scad-sketch-toggle-hovered-selection)
    ;; Vertex editing (within focused polygon)
    (define-key map (kbd "p")           #'scad-sketch-append-point)
    (define-key map (kbd "i")           #'scad-sketch-insert-point-after-selected)
    (define-key map (kbd "k")           #'scad-sketch-delete-selected)
    (define-key map (kbd "l")           #'scad-sketch-line-from-mark)
    (define-key map (kbd "r")           #'scad-sketch-rectangle-from-mark)
    (define-key map (kbd "c")           #'scad-sketch-toggle-closed)
    (define-key map (kbd "R")           #'scad-sketch-set-radius)
    (define-key map (kbd "x")           #'scad-sketch-set-x)
    (define-key map (kbd "y")           #'scad-sketch-set-y)
    (define-key map (kbd "X")           #'scad-sketch-set-delta-x)
    (define-key map (kbd "Y")           #'scad-sketch-set-delta-y)
    (define-key map (kbd "d")           #'scad-sketch-set-distance-from-mark)
    (define-key map (kbd "a")           #'scad-sketch-set-angle-from-mark)
    (define-key map (kbd "g")           #'scad-sketch-set-grid)
    ;; Session
    (define-key map (kbd "u")           #'scad-sketch-undo)
    (define-key map (kbd "w")           #'scad-sketch-write-back)
    (define-key map (kbd "q")           #'scad-sketch-quit)
    (define-key map (kbd "?")           #'scad-sketch-help)
    map)
  "Keymap for `scad-sketch-editor-mode'.")

(define-derived-mode scad-sketch-editor-mode special-mode "SCAD-Sketch"
  "Major mode for the scad-sketch visual editor.

The buffer shows an SVG canvas and a live source preview.

THREE INTERACTION STATES
  Hovered  — shape/point nearest the cursor crosshair (blue).
             TAB/S-TAB cycle hover candidates at the composition level.
  Selected — explicit set, toggled with SPC.  Shown in amber.
             C clears all selections.
  Focused  — one subtree being edited exclusively (others dimmed).
             RET focuses the hovered shape.  ESC pops one focus level.

CURSOR MOVEMENT
  <arrow>           grid step, snaps to grid
  C-<arrow>         coarse step, snaps to grid
  M-<arrow>         fine step, intentionally off-grid
  S-<arrow>         move selected vertex/shape (grid)
  M-S-<arrow>       move selected vertex/shape (fine, off-grid)
  C-S-<arrow>       move selected vertex/shape (coarse)

NAVIGATION
  TAB / S-TAB       cycle hover through shapes (composition level)
                    or through points (when a polygon is focused)
  RET               focus the hovered shape
  ESC               pop one focus level (or no-op at root)
  SPC               toggle hovered thing in/out of selection

MARKS
  m    replace all marks with cursor     M    push cursor onto mark stack
  `    pop most recent mark              '    jump to most recent mark
  C    clear all marks and selections

EDITING (within a focused polygon)
  p    append cursor as new vertex       i    insert after selected vertex
  k    delete selected vertex/shape      R    set polyRound radius
  c    toggle closed/open               l    line from marks
  r    rectangle from mark

COORDINATES
  x/y    set cursor X or Y              X/Y   set relative to mark
  d      distance from mark             a     angle from mark

SESSION
  g    set grid step     u    undo      w    write back     q    quit
  ?    key summary in echo area

\\{scad-sketch-editor-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t))

;;;; Session helpers

(defun scad-sketch--assert-session ()
  "Return the current sketch session or signal an error."
  (unless (and (boundp 'scad-sketch--session) scad-sketch--session)
    (user-error "No active scad-sketch session"))
  scad-sketch--session)

(defun scad-sketch--grid   (s) (float (scad-sketch-session-grid s)))
(defun scad-sketch--fine   (s) (float (scad-sketch-session-fine-step s)))
(defun scad-sketch--coarse (s) (float (scad-sketch-session-coarse-step s)))

;;;; Tree navigation helpers

(defun scad-sketch--node-at-path (session path)
  "Return the AST node reached by following PATH (list of child indices) from root."
  (let ((tree (scad-sketch-session-tree session)))
    (if (null path)
        tree  ; nil path = the whole tree (list of top-level nodes)
      (let ((node (nth (car path) tree)))
        (dolist (idx (cdr path))
          (let ((children (scad-sketch-parse--node-children node)))
            (setq node (nth idx children))))
        node))))

(defun scad-sketch--focused-node (session)
  "Return the currently focused AST node, or nil if nothing is focused."
  (let ((path (scad-sketch-session-focused-path session)))
    (when path
      (scad-sketch--node-at-path session path))))

(defun scad-sketch--focused-tree (session)
  "Return the list of nodes active for interaction.
If something is focused, returns its children (for a composition) or
a singleton list (for a primitive).  Otherwise returns all top-level nodes."
  (let ((focused (scad-sketch--focused-node session)))
    (if focused
        (let ((children (scad-sketch-parse--node-children focused)))
          (if children children (list focused)))
      (scad-sketch-session-tree session))))

(defun scad-sketch--path-of-node (session node)
  "Return the path (list of child indices) from root to NODE, or nil."
  (let ((tree (scad-sketch-session-tree session)))
    (cl-labels ((search (nodes prefix)
                  (cl-loop for n in nodes for i from 0
                           when (eq n node) return (append prefix (list i))
                           when (scad-sketch-parse--node-children n)
                           thereis (search (scad-sketch-parse--node-children n)
                                           (append prefix (list i))))))
      (search tree nil))))

(defun scad-sketch--polygon-points (node session)
  "Return the [x y r] points list for a polygon NODE.
Resolves variable references via scope-aware lookup."
  (let ((pts (plist-get node :points))
        (src (plist-get node :source)))
    (if (and (null pts) src)
        (or (scad-sketch-parse--lookup-variable
             src
             (scad-sketch-session-source-text session)
             (plist-get node :beg))
            pts)
      pts)))

(defun scad-sketch--set-polygon-points (session node new-points)
  "Update NODE's points in the session tree.
If the node had a variable reference (:source), resolve and inline the
updated points into that variable's array node in the tree instead."
  (let ((src (plist-get node :source)))
    (if src
        ;; Find the array assignment node named SRC and update its points.
        (let ((arr-node (cl-find-if
                         (lambda (n)
                           (and (eq (plist-get n :type) 'array)
                                (equal (plist-get n :name) src)))
                         (scad-sketch-session-tree session))))
          (when arr-node
            (plist-put arr-node :points new-points)))
      ;; Inline polygon — update directly.
      (plist-put node :points new-points))))

;;;; Selection helpers

(defun scad-sketch--sel-ref (kind path &optional index)
  "Build a selection reference plist."
  (list :kind kind :path path :index index))

(defun scad-sketch--sel-equal (a b)
  "Return non-nil if selection refs A and B refer to the same thing."
  (and (eq (plist-get a :kind) (plist-get b :kind))
       (equal (plist-get a :path) (plist-get b :path))
       (equal (plist-get a :index) (plist-get b :index))))

(defun scad-sketch--sel-member (ref selection)
  "Return non-nil if REF is in SELECTION."
  (cl-some (lambda (r) (scad-sketch--sel-equal r ref)) selection))

(defun scad-sketch--sel-toggle (ref selection)
  "Return SELECTION with REF added (if absent) or removed (if present)."
  (if (scad-sketch--sel-member ref selection)
      (cl-remove-if (lambda (r) (scad-sketch--sel-equal r ref)) selection)
    (cons ref selection)))

;;;; Focused-polygon editing state
;; When a polygon is focused, we maintain a selected-point-index (like the
;; old selected-index) and a closed flag. These live in the session under
;; separate slots mapped via a small alist keyed by node identity.

(defun scad-sketch--poly-state (session node)
  "Return the (selected-index . closed) cons for polygon NODE in SESSION.
Creates a default entry if none exists."
  (let* ((undo-entry (assq node (scad-sketch-session-undo-stack session))))
    ;; We store per-polygon edit state in a separate alist on the session.
    ;; Use a property on the session struct for this.
    (let ((tbl (scad-sketch-session-named-marks session))) ; repurposed as poly-state table
      (unless (and tbl (hash-table-p tbl))
        (setf (scad-sketch-session-named-marks session)
              (make-hash-table :test 'eq))
        (setq tbl (scad-sketch-session-named-marks session)))
      (or (gethash node tbl)
          (let ((default (cons 0 t)))
            (puthash node default tbl)
            default)))))

(defun scad-sketch--poly-selected-index (session node)
  (car (scad-sketch--poly-state session node)))

(defun scad-sketch--poly-closed (session node)
  (cdr (scad-sketch--poly-state session node)))

(defun scad-sketch--poly-set-selected (session node idx)
  (setcar (scad-sketch--poly-state session node) idx))

(defun scad-sketch--poly-set-closed (session node val)
  (setcdr (scad-sketch--poly-state session node) val))

;;;; Undo

(defun scad-sketch--push-undo (session)
  "Save session state onto the undo stack."
  (push (list :tree         (copy-tree (scad-sketch-session-tree session))
              :point        (copy-tree (scad-sketch-session-point session))
              :marks        (copy-tree (scad-sketch-session-marks session))
              :focused-path (copy-tree (scad-sketch-session-focused-path session))
              :selection    (copy-tree (scad-sketch-session-selection session))
              :poly-states  (when (hash-table-p (scad-sketch-session-named-marks session))
                              (let (st)
                                (maphash (lambda (k v) (push (cons k (copy-tree v)) st))
                                         (scad-sketch-session-named-marks session))
                                st)))
        (scad-sketch-session-undo-stack session)))

(defun scad-sketch--mark-dirty (session)
  (setf (scad-sketch-session-dirty session) t))

(defun scad-sketch--mutate (fn)
  "Push undo, call FN with session, mark dirty, re-render."
  (let ((session (scad-sketch--assert-session)))
    (scad-sketch--push-undo session)
    (funcall fn session)
    (scad-sketch--mark-dirty session)
    (scad-sketch--render)))

;;;; Cursor movement

(defun scad-sketch--move-xy (xy dx dy)
  (list (+ (float (nth 0 xy)) dx) (+ (float (nth 1 xy)) dy)))

(defun scad-sketch--snap-to-grid (v grid)
  (* grid (round (/ v grid))))

(defun scad-sketch--snap-xy (xy grid)
  (list (scad-sketch--snap-to-grid (nth 0 xy) grid)
        (scad-sketch--snap-to-grid (nth 1 xy) grid)))

(defun scad-sketch--move-point (dx dy &optional snap)
  (scad-sketch--mutate
   (lambda (s)
     (let ((new (scad-sketch--move-xy (scad-sketch-session-point s) dx dy)))
       (setf (scad-sketch-session-point s)
             (if snap (scad-sketch--snap-xy new (scad-sketch--grid s)) new))))))

(defun scad-sketch-move-point-left ()         (interactive) (scad-sketch--move-point (- (scad-sketch--grid   (scad-sketch--assert-session))) 0 t))
(defun scad-sketch-move-point-right ()        (interactive) (scad-sketch--move-point    (scad-sketch--grid   (scad-sketch--assert-session))  0 t))
(defun scad-sketch-move-point-up ()           (interactive) (scad-sketch--move-point 0  (scad-sketch--grid   (scad-sketch--assert-session))    t))
(defun scad-sketch-move-point-down ()         (interactive) (scad-sketch--move-point 0  (- (scad-sketch--grid   (scad-sketch--assert-session))) t))
(defun scad-sketch-move-point-fine-left ()    (interactive) (scad-sketch--move-point (- (scad-sketch--fine   (scad-sketch--assert-session))) 0))
(defun scad-sketch-move-point-fine-right ()   (interactive) (scad-sketch--move-point    (scad-sketch--fine   (scad-sketch--assert-session))  0))
(defun scad-sketch-move-point-fine-up ()      (interactive) (scad-sketch--move-point 0  (scad-sketch--fine   (scad-sketch--assert-session))))
(defun scad-sketch-move-point-fine-down ()    (interactive) (scad-sketch--move-point 0  (- (scad-sketch--fine   (scad-sketch--assert-session)))))
(defun scad-sketch-move-point-coarse-left ()  (interactive) (scad-sketch--move-point (- (scad-sketch--coarse (scad-sketch--assert-session))) 0 t))
(defun scad-sketch-move-point-coarse-right () (interactive) (scad-sketch--move-point    (scad-sketch--coarse (scad-sketch--assert-session))  0 t))
(defun scad-sketch-move-point-coarse-up ()    (interactive) (scad-sketch--move-point 0  (scad-sketch--coarse (scad-sketch--assert-session))    t))
(defun scad-sketch-move-point-coarse-down ()  (interactive) (scad-sketch--move-point 0  (- (scad-sketch--coarse (scad-sketch--assert-session))) t))

;;;; Selected vertex movement
;; When a polygon is focused, S-arrows move the selected vertex.
;; Otherwise S-arrows are a no-op (future: move selected shape via translate).

(defun scad-sketch--move-selected (dx dy &optional snap)
  "Move the selected vertex of the focused polygon by DX DY."
  (let ((session (scad-sketch--assert-session)))
    (let ((focused (scad-sketch--focused-node session)))
      (unless (and focused (eq (plist-get focused :type) 'polygon))
        (user-error "No focused polygon to move a vertex in"))
      (scad-sketch--mutate
       (lambda (s)
         (let* ((node   (scad-sketch--focused-node s))
                (pts    (scad-sketch--polygon-points node s))
                (idx    (scad-sketch--poly-selected-index s node))
                (old    (or (nth idx pts) (user-error "No selected vertex")))
                (new-xy (scad-sketch--move-xy
                         (list (float (nth 0 old)) (float (nth 1 old)))
                         dx dy))
                (snapped (if snap (scad-sketch--snap-xy new-xy (scad-sketch--grid s)) new-xy))
                (new-pt  (list (nth 0 snapped) (nth 1 snapped) (float (or (nth 2 old) 0)))))
           (scad-sketch--set-polygon-points
            s node (scad-sketch--replace-nth idx new-pt pts))
           (setf (scad-sketch-session-point s) snapped)))))))

(defun scad-sketch-move-selected-left ()         (interactive) (scad-sketch--move-selected (- (scad-sketch--grid   (scad-sketch--assert-session))) 0 t))
(defun scad-sketch-move-selected-right ()        (interactive) (scad-sketch--move-selected    (scad-sketch--grid   (scad-sketch--assert-session))  0 t))
(defun scad-sketch-move-selected-up ()           (interactive) (scad-sketch--move-selected 0  (scad-sketch--grid   (scad-sketch--assert-session))    t))
(defun scad-sketch-move-selected-down ()         (interactive) (scad-sketch--move-selected 0  (- (scad-sketch--grid   (scad-sketch--assert-session))) t))
(defun scad-sketch-move-selected-fine-left ()    (interactive) (scad-sketch--move-selected (- (scad-sketch--fine   (scad-sketch--assert-session))) 0))
(defun scad-sketch-move-selected-fine-right ()   (interactive) (scad-sketch--move-selected    (scad-sketch--fine   (scad-sketch--assert-session))  0))
(defun scad-sketch-move-selected-fine-up ()      (interactive) (scad-sketch--move-selected 0  (scad-sketch--fine   (scad-sketch--assert-session))))
(defun scad-sketch-move-selected-fine-down ()    (interactive) (scad-sketch--move-selected 0  (- (scad-sketch--fine   (scad-sketch--assert-session)))))
(defun scad-sketch-move-selected-coarse-left ()  (interactive) (scad-sketch--move-selected (- (scad-sketch--coarse (scad-sketch--assert-session))) 0 t))
(defun scad-sketch-move-selected-coarse-right () (interactive) (scad-sketch--move-selected    (scad-sketch--coarse (scad-sketch--assert-session))  0 t))
(defun scad-sketch-move-selected-coarse-up ()    (interactive) (scad-sketch--move-selected 0  (scad-sketch--coarse (scad-sketch--assert-session))    t))
(defun scad-sketch-move-selected-coarse-down ()  (interactive) (scad-sketch--move-selected 0  (- (scad-sketch--coarse (scad-sketch--assert-session))) t))

;;;; Mark commands (unchanged from v0.4)

(defun scad-sketch-set-mark ()
  "Replace all marks with the current cursor position."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (setf (scad-sketch-session-marks s)
           (list (copy-sequence (scad-sketch-session-point s)))))))

(defun scad-sketch-push-mark ()
  "Push current cursor position onto the mark stack."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (push (copy-sequence (scad-sketch-session-point s))
           (scad-sketch-session-marks s)))))

(defun scad-sketch-pop-mark ()
  "Pop most recent mark and jump cursor to it."
  (interactive)
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-marks session) (user-error "No marks set")))
  (scad-sketch--mutate
   (lambda (s)
     (setf (scad-sketch-session-point s)
           (copy-sequence (pop (scad-sketch-session-marks s)))))))

(defun scad-sketch-jump-to-mark ()
  "Move cursor to most recent mark (non-destructive)."
  (interactive)
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-marks session) (user-error "No marks set")))
  (scad-sketch--mutate
   (lambda (s)
     (setf (scad-sketch-session-point s)
           (copy-sequence (car (scad-sketch-session-marks s)))))))

(defun scad-sketch-clear-marks ()
  "Clear all marks and selections."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (setf (scad-sketch-session-marks s) nil)
     (setf (scad-sketch-session-selection s) nil))))

;;;; Focus and hover

(defun scad-sketch-focus-hovered ()
  "Focus the currently hovered shape (drill in)."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (hover   (car (scad-sketch-session-hover-stack session))))
    (unless hover (user-error "Nothing hovered"))
    (let ((path (scad-sketch--path-of-node session hover)))
      (when path
        (scad-sketch--mutate
         (lambda (s)
           (setf (scad-sketch-session-focused-path s) path)
           (setf (scad-sketch-session-selection s) nil)
           (setf (scad-sketch-session-hover-stack s) nil)))))))

(defun scad-sketch-unfocus ()
  "Pop one level of focus.  No-op if already at root."
  (interactive)
  (let ((session (scad-sketch--assert-session)))
    (when (scad-sketch-session-focused-path session)
      (scad-sketch--mutate
       (lambda (s)
         (let ((path (scad-sketch-session-focused-path s)))
           (setf (scad-sketch-session-focused-path s)
                 (when (cdr path) (butlast path)))
           (setf (scad-sketch-session-selection s) nil)
           (setf (scad-sketch-session-hover-stack s) nil)))))))

(defun scad-sketch-toggle-hovered-selection ()
  "Toggle the hovered shape/point in the selection set."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (hover   (car (scad-sketch-session-hover-stack session))))
    (unless hover (user-error "Nothing hovered"))
    (let ((path (scad-sketch--path-of-node session hover)))
      (when path
        (scad-sketch--mutate
         (lambda (s)
           (let ((ref (scad-sketch--sel-ref 'shape path)))
             (setf (scad-sketch-session-selection s)
                   (scad-sketch--sel-toggle ref (scad-sketch-session-selection s))))))))))

(defun scad-sketch--compute-hover-stack (session)
  "Recompute the hover stack from the current cursor position.
Returns a list of nodes (deepest first) that contain the cursor,
restricted to the currently focused subtree."
  (let* ((cursor (scad-sketch-session-point session))
         (cx (nth 0 cursor))
         (cy (nth 1 cursor))
         (active-nodes (scad-sketch--focused-tree session))
         stack)
    (dolist (node active-nodes)
      (scad-sketch-parse--walk
       node
       (lambda (n)
         (when (scad-sketch--point-in-node-p cx cy n session)
           (push n stack)))))
    ;; Sort by bounding-box area ascending so smallest (most specific) is first.
    (sort stack (lambda (a b)
                  (< (scad-sketch--node-bbox-area a session)
                     (scad-sketch--node-bbox-area b session))))))

(defun scad-sketch--point-in-node-p (x y node session)
  "Return non-nil if (X Y) is geometrically inside NODE."
  (pcase (plist-get node :type)
    ('circle
     (let* ((cx (plist-get node :cx)) (cy (plist-get node :cy))
            (r  (plist-get node :r))
            (dx (- x cx)) (dy (- y cy)))
       (< (+ (* dx dx) (* dy dy)) (* r r))))
    ('square
     (let ((sx (plist-get node :x)) (sy (plist-get node :y))
           (sw (plist-get node :w)) (sh (plist-get node :h)))
       (and (>= x sx) (<= x (+ sx sw))
            (>= y sy) (<= y (+ sy sh)))))
    ((or 'polygon 'array)
     (let ((pts (or (scad-sketch--polygon-points node session) '())))
       (scad-sketch--point-in-polygon-p x y pts)))
    ((or 'difference 'union 'intersection)
     ;; Hover the composition if cursor is within any child's bounding box.
     (cl-some (lambda (c) (scad-sketch--point-in-node-p x y c session))
              (plist-get node :children)))
    ('translate
     (scad-sketch--point-in-node-p
      (- x (plist-get node :tx))
      (- y (plist-get node :ty))
      (plist-get node :child) session))
    (_ nil)))

(defun scad-sketch--point-in-polygon-p (x y pts)
  "Ray-casting polygon containment test for point (X Y) in PTS."
  (let ((n (length pts)) (inside nil))
    (when (> n 2)
      (let ((j (1- n)))
        (dotimes (i n)
          (let* ((pi (nth i pts)) (pj (nth j pts))
                 (xi (nth 0 pi)) (yi (nth 1 pi))
                 (xj (nth 0 pj)) (yj (nth 1 pj)))
            (when (and (not (eq (> yi y) (> yj y)))
                       (< x (+ xi (* (- xj xi) (/ (- y yi) (- yj yi))))))
              (setq inside (not inside))))
          (setq j i))))
    inside))

(defun scad-sketch--node-bbox (node session)
  "Return (min-x max-x min-y max-y) bounding box for NODE."
  (pcase (plist-get node :type)
    ('circle
     (let ((cx (plist-get node :cx)) (cy (plist-get node :cy))
           (r  (plist-get node :r)))
       (list (- cx r) (+ cx r) (- cy r) (+ cy r))))
    ('square
     (list (plist-get node :x)
           (+ (plist-get node :x) (plist-get node :w))
           (plist-get node :y)
           (+ (plist-get node :y) (plist-get node :h))))
    ((or 'polygon 'array)
     (let ((pts (or (scad-sketch--polygon-points node session) '())))
       (if pts
           (list (apply #'min (mapcar #'car pts))
                 (apply #'max (mapcar #'car pts))
                 (apply #'min (mapcar #'cadr pts))
                 (apply #'max (mapcar #'cadr pts)))
         (list 0 1 0 1))))
    ((or 'difference 'union 'intersection)
     (let ((boxes (mapcar (lambda (c) (scad-sketch--node-bbox c session))
                          (plist-get node :children))))
       (list (apply #'min (mapcar #'car  boxes))
             (apply #'max (mapcar #'cadr boxes))
             (apply #'min (mapcar #'caddr boxes))
             (apply #'max (mapcar #'cadddr boxes)))))
    ('translate
     (let ((box (scad-sketch--node-bbox (plist-get node :child) session))
           (tx (plist-get node :tx)) (ty (plist-get node :ty)))
       (list (+ (nth 0 box) tx) (+ (nth 1 box) tx)
             (+ (nth 2 box) ty) (+ (nth 3 box) ty))))
    (_ (list 0 1 0 1))))

(defun scad-sketch--node-bbox-area (node session)
  "Return bounding box area of NODE (for hover sorting)."
  (let ((box (scad-sketch--node-bbox node session)))
    (* (- (nth 1 box) (nth 0 box))
       (- (nth 3 box) (nth 2 box)))))

;;;; TAB cycling

(defun scad-sketch-tab-next ()
  "Cycle hover forward.
At composition level: cycle through child shapes.
With a polygon focused: cycle through its vertices."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (focused (scad-sketch--focused-node session)))
    (if (and focused (eq (plist-get focused :type) 'polygon))
        ;; Cycle vertices within focused polygon
        (scad-sketch--mutate
         (lambda (s)
           (let* ((node  (scad-sketch--focused-node s))
                  (pts   (scad-sketch--polygon-points node s))
                  (n     (length pts))
                  (cur   (scad-sketch--poly-selected-index s node))
                  (next  (mod (1+ (or cur -1)) n))
                  (pt    (nth next pts)))
             (scad-sketch--poly-set-selected s node next)
             (setf (scad-sketch-session-point s)
                   (list (float (nth 0 pt)) (float (nth 1 pt)))))))
      ;; Cycle shapes in active context
      (scad-sketch--mutate
       (lambda (s)
         (let* ((nodes   (scad-sketch--focused-tree s))
                (hovered (car (scad-sketch-session-hover-stack s)))
                (cur-idx (when hovered
                           (cl-position hovered nodes :test #'eq)))
                (next-idx (mod (1+ (or cur-idx -1)) (max 1 (length nodes))))
                (next-node (nth next-idx nodes)))
           (setf (scad-sketch-session-hover-stack s)
                 (when next-node (list next-node)))
           (when next-node
             (let ((bbox (scad-sketch--node-bbox next-node s)))
               (setf (scad-sketch-session-point s)
                     (list (/ (+ (nth 0 bbox) (nth 1 bbox)) 2.0)
                           (/ (+ (nth 2 bbox) (nth 3 bbox)) 2.0)))))))))))

(defun scad-sketch-tab-prev ()
  "Cycle hover backward."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (focused (scad-sketch--focused-node session)))
    (if (and focused (eq (plist-get focused :type) 'polygon))
        (scad-sketch--mutate
         (lambda (s)
           (let* ((node  (scad-sketch--focused-node s))
                  (pts   (scad-sketch--polygon-points node s))
                  (n     (length pts))
                  (cur   (scad-sketch--poly-selected-index s node))
                  (prev  (mod (1- (or cur 0)) n))
                  (pt    (nth prev pts)))
             (scad-sketch--poly-set-selected s node prev)
             (setf (scad-sketch-session-point s)
                   (list (float (nth 0 pt)) (float (nth 1 pt)))))))
      (scad-sketch--mutate
       (lambda (s)
         (let* ((nodes   (scad-sketch--focused-tree s))
                (hovered (car (scad-sketch-session-hover-stack s)))
                (cur-idx (when hovered (cl-position hovered nodes :test #'eq)))
                (prev-idx (mod (1- (or cur-idx 0)) (max 1 (length nodes))))
                (prev-node (nth prev-idx nodes)))
           (setf (scad-sketch-session-hover-stack s)
                 (when prev-node (list prev-node)))
           (when prev-node
             (let ((bbox (scad-sketch--node-bbox prev-node s)))
               (setf (scad-sketch-session-point s)
                     (list (/ (+ (nth 0 bbox) (nth 1 bbox)) 2.0)
                           (/ (+ (nth 2 bbox) (nth 3 bbox)) 2.0)))))))))))

;;;; Polygon vertex editing
;; All of these require a focused polygon.

(defun scad-sketch--require-focused-polygon ()
  "Return the focused polygon node, or signal an error."
  (let* ((session (scad-sketch--assert-session))
         (focused (scad-sketch--focused-node session)))
    (unless (and focused (eq (plist-get focused :type) 'polygon))
      (user-error "Focus a polygon first (press RET on a polygon shape)"))
    focused))

(defun scad-sketch--replace-nth (n value list)
  (let ((copy (copy-sequence list)))
    (setf (nth n copy) value)
    copy))

(defun scad-sketch--make-point (xy &optional old-point)
  "Build [x y r] from XY, preserving radius from OLD-POINT."
  (list (float (nth 0 xy))
        (float (nth 1 xy))
        (float (or (and old-point (nth 2 old-point)) 0))))

(defun scad-sketch-append-point ()
  "Append cursor as a new vertex to the focused polygon."
  (interactive)
  (scad-sketch--require-focused-polygon)
  (scad-sketch--mutate
   (lambda (s)
     (let* ((node (scad-sketch--focused-node s))
            (pts  (or (scad-sketch--polygon-points node s) '()))
            (new-pt (scad-sketch--make-point (scad-sketch-session-point s)))
            (new-pts (append pts (list new-pt))))
       (scad-sketch--set-polygon-points s node new-pts)
       (scad-sketch--poly-set-selected s node (1- (length new-pts)))))))

(defun scad-sketch-insert-point-after-selected ()
  "Insert points after the selected vertex (marks oldest-first, then cursor)."
  (interactive)
  (scad-sketch--require-focused-polygon)
  (scad-sketch--mutate
   (lambda (s)
     (let* ((node      (scad-sketch--focused-node s))
            (pts       (or (scad-sketch--polygon-points node s) '()))
            (idx       (or (scad-sketch--poly-selected-index s node) -1))
            (insert-at (min (1+ idx) (length pts)))
            (mark-pts  (mapcar (lambda (m) (scad-sketch--make-point m))
                               (reverse (scad-sketch-session-marks s))))
            (cursor-pt (scad-sketch--make-point (scad-sketch-session-point s)))
            (new-pts   (append mark-pts (list cursor-pt)))
            (new-idx   (+ insert-at (length new-pts) -1)))
       (scad-sketch--set-polygon-points
        s node
        (append (cl-subseq pts 0 insert-at)
                new-pts
                (nthcdr insert-at pts)))
       (scad-sketch--poly-set-selected s node new-idx)))))

(defun scad-sketch-delete-selected ()
  "Delete the selected vertex from the focused polygon."
  (interactive)
  (scad-sketch--require-focused-polygon)
  (scad-sketch--mutate
   (lambda (s)
     (let* ((node (scad-sketch--focused-node s))
            (pts  (or (scad-sketch--polygon-points node s) '()))
            (idx  (or (scad-sketch--poly-selected-index s node)
                      (user-error "No selected vertex"))))
       (unless (< idx (length pts)) (user-error "Vertex out of range"))
       (let ((new-pts (append (cl-subseq pts 0 idx) (nthcdr (1+ idx) pts))))
         (scad-sketch--set-polygon-points s node new-pts)
         (scad-sketch--poly-set-selected
          s node
          (cond ((null new-pts) 0)
                ((>= idx (length new-pts)) (1- (length new-pts)))
                (t idx))))))))

(defun scad-sketch-line-from-mark ()
  "Append marks (oldest first) then cursor as new vertices."
  (interactive)
  (scad-sketch--require-focused-polygon)
  (scad-sketch--mutate
   (lambda (s)
     (unless (scad-sketch-session-marks s) (user-error "No marks set"))
     (let* ((node (scad-sketch--focused-node s))
            (pts  (or (scad-sketch--polygon-points node s) '()))
            (new-pts (append pts
                             (mapcar #'scad-sketch--make-point
                                     (reverse (scad-sketch-session-marks s)))
                             (list (scad-sketch--make-point
                                    (scad-sketch-session-point s))))))
       (scad-sketch--set-polygon-points s node new-pts)
       (scad-sketch--poly-set-selected s node (1- (length new-pts)))))))

(defun scad-sketch-rectangle-from-mark ()
  "Append rectangle corners from most recent mark to cursor."
  (interactive)
  (scad-sketch--require-focused-polygon)
  (scad-sketch--mutate
   (lambda (s)
     (let ((mark (or (car (scad-sketch-session-marks s)) (user-error "No marks set")))
           (pt   (scad-sketch-session-point s)))
       (let* ((node (scad-sketch--focused-node s))
              (pts  (or (scad-sketch--polygon-points node s) '()))
              (x1 (nth 0 mark)) (y1 (nth 1 mark))
              (x2 (nth 0 pt))   (y2 (nth 1 pt))
              (corners (list (list x1 y1) (list x2 y1) (list x2 y2) (list x1 y2)))
              (new-pts (append pts (mapcar #'scad-sketch--make-point corners))))
         (scad-sketch--set-polygon-points s node new-pts)
         (scad-sketch--poly-set-selected s node (1- (length new-pts))))))))

(defun scad-sketch-toggle-closed ()
  "Toggle closed/open for the focused polygon."
  (interactive)
  (scad-sketch--require-focused-polygon)
  (scad-sketch--mutate
   (lambda (s)
     (let ((node (scad-sketch--focused-node s)))
       (scad-sketch--poly-set-closed
        s node (not (scad-sketch--poly-closed s node)))))))

(defun scad-sketch-set-radius (radius)
  "Set polyRound radius on the selected vertex."
  (interactive (list (read-number "Radius: " 0)))
  (scad-sketch--require-focused-polygon)
  (scad-sketch--mutate
   (lambda (s)
     (let* ((node (scad-sketch--focused-node s))
            (pts  (or (scad-sketch--polygon-points node s) '()))
            (idx  (or (scad-sketch--poly-selected-index s node)
                      (user-error "No selected vertex")))
            (old  (nth idx pts))
            (new  (list (nth 0 old) (nth 1 old) (float radius))))
       (scad-sketch--set-polygon-points
        s node (scad-sketch--replace-nth idx new pts))))))

;;;; Coordinate-setting commands

(defun scad-sketch--set-point-axis (axis value)
  (scad-sketch--mutate
   (lambda (s)
     (let ((pt (copy-sequence (scad-sketch-session-point s))))
       (setf (nth axis pt) (float value))
       (setf (scad-sketch-session-point s) pt)))))

(defun scad-sketch-set-x (x)
  "Set cursor X."
  (interactive (list (read-number "X: " (nth 0 (scad-sketch-session-point (scad-sketch--assert-session))))))
  (scad-sketch--set-point-axis 0 x))

(defun scad-sketch-set-y (y)
  "Set cursor Y."
  (interactive (list (read-number "Y: " (nth 1 (scad-sketch-session-point (scad-sketch--assert-session))))))
  (scad-sketch--set-point-axis 1 y))

(defun scad-sketch--set-delta-axis (axis value)
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-marks session) (user-error "No marks set"))
    (scad-sketch--set-point-axis
     axis (+ (nth axis (car (scad-sketch-session-marks session))) (float value)))))

(defun scad-sketch-set-delta-x (dx)
  "Set cursor X to (mark X) + DX."
  (interactive (list (read-number "ΔX from mark: " 0)))
  (scad-sketch--set-delta-axis 0 dx))

(defun scad-sketch-set-delta-y (dy)
  "Set cursor Y to (mark Y) + DY."
  (interactive (list (read-number "ΔY from mark: " 0)))
  (scad-sketch--set-delta-axis 1 dy))

(defun scad-sketch-set-distance-from-mark (distance)
  "Set distance from mark to cursor, preserving angle."
  (interactive (list (read-number "Distance from mark: " 0)))
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-marks session) (user-error "No marks set")))
  (scad-sketch--mutate
   (lambda (s)
     (let* ((m (car (scad-sketch-session-marks s)))
            (p (scad-sketch-session-point s))
            (angle (atan (- (nth 1 p) (nth 1 m)) (- (nth 0 p) (nth 0 m)))))
       (setf (scad-sketch-session-point s)
             (list (+ (nth 0 m) (* (float distance) (cos angle)))
                   (+ (nth 1 m) (* (float distance) (sin angle)))))))))

(defun scad-sketch-set-angle-from-mark (degrees)
  "Set angle from mark to cursor in DEGREES, preserving distance."
  (interactive (list (read-number "Angle degrees from mark: " 0)))
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-marks session) (user-error "No marks set")))
  (scad-sketch--mutate
   (lambda (s)
     (let* ((m    (car (scad-sketch-session-marks s)))
            (p    (scad-sketch-session-point s))
            (dx   (- (nth 0 p) (nth 0 m)))
            (dy   (- (nth 1 p) (nth 1 m)))
            (dist (sqrt (+ (* dx dx) (* dy dy))))
            (ang  (* pi (/ (float degrees) 180.0))))
       (setf (scad-sketch-session-point s)
             (list (+ (nth 0 m) (* dist (cos ang)))
                   (+ (nth 1 m) (* dist (sin ang)))))))))

(defun scad-sketch-set-grid (grid)
  "Set the grid step."
  (interactive (list (read-number "Grid step: " (scad-sketch-session-grid (scad-sketch--assert-session)))))
  (scad-sketch--mutate
   (lambda (s) (setf (scad-sketch-session-grid s) (float grid)))))

;;;; Undo command

(defun scad-sketch-undo ()
  "Undo the last sketch edit."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (entry   (pop (scad-sketch-session-undo-stack session))))
    (unless entry (user-error "No sketch undo available"))
    (setf (scad-sketch-session-tree session)         (plist-get entry :tree))
    (setf (scad-sketch-session-point session)        (plist-get entry :point))
    (setf (scad-sketch-session-marks session)        (plist-get entry :marks))
    (setf (scad-sketch-session-focused-path session) (plist-get entry :focused-path))
    (setf (scad-sketch-session-selection session)    (plist-get entry :selection))
    (let ((states (plist-get entry :poly-states)))
      (when states
        (let ((tbl (make-hash-table :test 'eq)))
          (dolist (pair states) (puthash (car pair) (cdr pair) tbl))
          (setf (scad-sketch-session-named-marks session) tbl))))
    (setf (scad-sketch-session-dirty session) t)
    (scad-sketch--render)))

;;;; Rendering

(defun scad-sketch--all-points (session)
  "Collect all visible [x y] coords for auto-zoom bounds."
  (let (pts)
    (dolist (node (scad-sketch-session-tree session))
      (scad-sketch-parse--walk
       node
       (lambda (n)
         (pcase (plist-get n :type)
           ((or 'polygon 'array)
            (dolist (p (or (scad-sketch--polygon-points n session) '()))
              (push (list (nth 0 p) (nth 1 p)) pts)))
           ('circle
            (let ((cx (plist-get n :cx)) (cy (plist-get n :cy)) (r (plist-get n :r)))
              (push (list (- cx r) (- cy r)) pts)
              (push (list (+ cx r) (+ cy r)) pts)))
           ('square
            (push (list (plist-get n :x) (plist-get n :y)) pts)
            (push (list (+ (plist-get n :x) (plist-get n :w))
                        (+ (plist-get n :y) (plist-get n :h))) pts))))))
    pts))

(defun scad-sketch--bounds (session)
  "Return (min-x max-x min-y max-y) covering all content + cursor + marks."
  (let* ((geom  (scad-sketch--all-points session))
         (extra (delq nil (cons (scad-sketch-session-point session)
                                (scad-sketch-session-marks session))))
         (all   (append geom extra)))
    (if (null all) (list -10 10 -10 10)
      (let ((min-x (apply #'min (mapcar #'car  all)))
            (max-x (apply #'max (mapcar #'car  all)))
            (min-y (apply #'min (mapcar #'cadr all)))
            (max-y (apply #'max (mapcar #'cadr all))))
        (when (= min-x max-x) (setq min-x (- min-x 10) max-x (+ max-x 10)))
        (when (= min-y max-y) (setq min-y (- min-y 10) max-y (+ max-y 10)))
        (let ((px (max 1 (* 0.15 (- max-x min-x))))
              (py (max 1 (* 0.15 (- max-y min-y)))))
          (list (- min-x px) (+ max-x px) (- min-y py) (+ max-y py)))))))

(defun scad-sketch--transform (bounds)
  "Return a model→pixel transform closure for BOUNDS."
  (pcase-let ((`(,min-x ,max-x ,min-y ,max-y) bounds))
    (let* ((w scad-sketch-canvas-width) (h scad-sketch-canvas-height)
           (m scad-sketch-margin)
           (scale (min (/ (- w (* 2 m)) (- max-x min-x))
                       (/ (- h (* 2 m)) (- max-y min-y)))))
      (lambda (xy)
        (list (+ m (* (- (nth 0 xy) min-x) scale))
              (- h (+ m (* (- (nth 1 xy) min-y) scale))))))))

(defun scad-sketch--svg-line (svg tf a b &rest args)
  "Draw a model-space line A→B."
  (let ((pa (funcall tf a)) (pb (funcall tf b)))
    (apply #'svg-line svg (nth 0 pa) (nth 1 pa) (nth 0 pb) (nth 1 pb) args)))

(defun scad-sketch--draw-grid (svg bounds tf session)
  "Draw the background grid."
  (pcase-let ((`(,min-x ,max-x ,min-y ,max-y) bounds))
    (let* ((g (max 0.0001 (scad-sketch-session-grid session)))
           (x (* g (floor (/ min-x g))))
           (y (* g (floor (/ min-y g)))))
      (while (<= x (* g (ceiling (/ max-x g))))
        (scad-sketch--svg-line svg tf (list x min-y) (list x max-y) :stroke "#e8e8e8" :stroke-width 1)
        (setq x (+ x g)))
      (while (<= y (* g (ceiling (/ max-y g))))
        (scad-sketch--svg-line svg tf (list min-x y) (list max-x y) :stroke "#e8e8e8" :stroke-width 1)
        (setq y (+ y g)))
      (when (and (<= min-x 0) (<= 0 max-x))
        (scad-sketch--svg-line svg tf (list 0 min-y) (list 0 max-y) :stroke "#d0d0d0" :stroke-width 2))
      (when (and (<= min-y 0) (<= 0 max-y))
        (scad-sketch--svg-line svg tf (list min-x 0) (list max-x 0) :stroke "#d0d0d0" :stroke-width 2)))))

;;; polyRound geometry (carried over from v0.4)

(defun scad-sketch--corner-unit-vecs (A B C)
  (let* ((bx (nth 0 B)) (by (nth 1 B))
         (ba (list (- (nth 0 A) bx) (- (nth 1 A) by)))
         (bc (list (- (nth 0 C) bx) (- (nth 1 C) by)))
         (la (sqrt (+ (* (nth 0 ba) (nth 0 ba)) (* (nth 1 ba) (nth 1 ba)))))
         (lc (sqrt (+ (* (nth 0 bc) (nth 0 bc)) (* (nth 1 bc) (nth 1 bc))))))
    (when (and (> la 1e-10) (> lc 1e-10))
      (let* ((u (list (/ (nth 0 ba) la) (/ (nth 1 ba) la)))
             (v (list (/ (nth 0 bc) lc) (/ (nth 1 bc) lc)))
             (dot (max -1.0 (min 1.0 (+ (* (nth 0 u) (nth 0 v)) (* (nth 1 u) (nth 1 v))))))
             (half (/ (acos dot) 2)))
        (when (> (sin half) 1e-10) (list u v half))))))

(defun scad-sketch--corner-geometry-from-tlens (B u v half t1 t2)
  (let* ((bx (nth 0 B)) (by (nth 1 B))
         (tl (min t1 t2)) (ar (* tl (tan half)))
         (p1 (list (+ bx (* tl (nth 0 u))) (+ by (* tl (nth 1 u)))))
         (p2 (list (+ bx (* tl (nth 0 v))) (+ by (* tl (nth 1 v)))))
         (cross (- (* (nth 0 u) (nth 1 v)) (* (nth 1 u) (nth 0 v)))))
    (list :t1 p1 :t2 p2 :radius ar :sweep (if (> cross 0) 1 0))))

(defun scad-sketch--corner-geometry (A B C r)
  (when (and r (> r 0))
    (let ((uvh (scad-sketch--corner-unit-vecs A B C)))
      (when uvh
        (let* ((u (nth 0 uvh)) (v (nth 1 uvh)) (half (nth 2 uvh))
               (bx (nth 0 B)) (by (nth 1 B))
               (ba (list (- (nth 0 A) bx) (- (nth 1 A) by)))
               (bc (list (- (nth 0 C) bx) (- (nth 1 C) by)))
               (la (sqrt (+ (* (nth 0 ba) (nth 0 ba)) (* (nth 1 ba) (nth 1 ba)))))
               (lc (sqrt (+ (* (nth 0 bc) (nth 0 bc)) (* (nth 1 bc) (nth 1 bc)))))
               (tl (min (/ r (tan half)) (* la 0.49) (* lc 0.49))))
          (scad-sketch--corner-geometry-from-tlens B u v half tl tl))))))

(defun scad-sketch--pixel-radius (model-r tf)
  (let* ((o (funcall tf '(0 0))) (r (funcall tf (list model-r 0)))
         (dx (- (nth 0 r) (nth 0 o))) (dy (- (nth 1 r) (nth 1 o))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun scad-sketch--edge-len (P Q)
  (let ((dx (- (nth 0 Q) (nth 0 P))) (dy (- (nth 1 Q) (nth 1 P))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun scad-sketch--any-radius-p (points)
  (cl-some (lambda (p) (and (nth 2 p) (> (nth 2 p) 0))) points))

(defun scad-sketch--polyround-path-d (points closed tf)
  "Build SVG path data for POINTS with edge-aware polyRound clamping."
  (let ((n (length points)))
    (when (>= n 2)
      (let* ((t-out (make-vector n 0.0))
             (t-in  (make-vector n 0.0))
             (uvh-v (make-vector n nil)))
        (dotimes (i n)
          (let ((r (nth 2 (nth i points))))
            (when (and r (> r 0))
              (let* ((prev (cond ((> i 0) (nth (1- i) points)) (closed (nth (1- n) points))))
                     (next (cond ((< i (1- n)) (nth (1+ i) points)) (closed (nth 0 points)))))
                (when (and prev next)
                  (let* ((A (list (float (nth 0 prev)) (float (nth 1 prev))))
                         (B (list (float (nth 0 (nth i points))) (float (nth 1 (nth i points)))))
                         (C (list (float (nth 0 next)) (float (nth 1 next))))
                         (uvh (scad-sketch--corner-unit-vecs A B C)))
                    (when uvh
                      (aset uvh-v i uvh)
                      (let ((ti (/ r (tan (nth 2 uvh)))))
                        (aset t-in i ti) (aset t-out i ti)))))))))
        (dotimes (i n)
          (let* ((j (mod (1+ i) n))
                 (Pi (list (float (nth 0 (nth i points))) (float (nth 1 (nth i points)))))
                 (Pj (list (float (nth 0 (nth j points))) (float (nth 1 (nth j points)))))
                 (el (scad-sketch--edge-len Pi Pj))
                 (sm (+ (aref t-out i) (aref t-in j))))
            (when (and (or closed (< i (1- n))) (> sm (* el 0.999)))
              (let ((sc (/ (* el 0.499) sm)))
                (aset t-out i (* (aref t-out i) sc))
                (aset t-in  j (* (aref t-in  j) sc))))))
        (let ((corners (make-vector n nil)))
          (dotimes (i n)
            (let ((uvh (aref uvh-v i)))
              (when uvh
                (aset corners i
                      (scad-sketch--corner-geometry-from-tlens
                       (list (float (nth 0 (nth i points))) (float (nth 1 (nth i points))))
                       (nth 0 uvh) (nth 1 uvh) (nth 2 uvh)
                       (aref t-in i) (aref t-out i))))))
          (let* ((c0 (aref corners 0))
                 (start (if (and c0 closed)
                            (funcall tf (plist-get c0 :t1))
                          (funcall tf (list (float (nth 0 (nth 0 points)))
                                           (float (nth 1 (nth 0 points)))))))
                 (fmt (lambda (xy) (format "%.3f %.3f" (float (nth 0 xy)) (float (nth 1 xy)))))
                 (parts (list (format "M %s" (funcall fmt start)))))
            (dotimes (i n)
              (let* ((cor (aref corners i))
                     (ps  (funcall tf (list (float (nth 0 (nth i points)))
                                           (float (nth 1 (nth i points)))))))
                (if cor
                    (let* ((t1s (funcall tf (plist-get cor :t1)))
                           (t2s (funcall tf (plist-get cor :t2)))
                           (rs  (scad-sketch--pixel-radius (plist-get cor :radius) tf))
                           (sw  (plist-get cor :sweep)))
                      (push (format "L %s" (funcall fmt t1s)) parts)
                      (push (format "A %.3f %.3f 0 0 %d %s" rs rs sw (funcall fmt t2s)) parts))
                  (push (format "L %s" (funcall fmt ps)) parts))))
            (when closed (push "Z" parts))
            (mapconcat #'identity (nreverse parts) " ")))))))

;;; Node rendering

(defun scad-sketch--node-visual-state (node session)
  "Return :focused :hovered :selected :context or :normal for NODE."
  (let* ((focused-path (scad-sketch-session-focused-path session))
         (node-path    (scad-sketch--path-of-node session node))
         (hover-stack  (scad-sketch-session-hover-stack session))
         (selection    (scad-sketch-session-selection session)))
    (cond
     ;; Node is the focused node or its ancestor
     ((and focused-path node-path
           (equal (cl-subseq node-path 0 (min (length focused-path) (length node-path)))
                  (cl-subseq focused-path 0 (min (length focused-path) (length node-path)))))
      (if (equal node-path focused-path) :focused :focused))
     ;; Node is outside the focused subtree
     ((and focused-path node-path
           (not (equal (cl-subseq node-path 0 (min (length focused-path) (length node-path)))
                       (cl-subseq focused-path 0 (min (length focused-path) (length node-path))))))
      :context)
     ;; Hovered
     ((memq node hover-stack) :hovered)
     ;; Selected
     ((and node-path
           (scad-sketch--sel-member (scad-sketch--sel-ref 'shape node-path) selection))
      :selected)
     (t :normal))))

(defun scad-sketch--state-colors (state)
  "Return (stroke fill stroke-width) for a visual STATE."
  (pcase state
    (:focused  '("#111111" "none" 3))
    (:hovered  '("#0057c2" "none" 2))
    (:selected '("#d13f00" "none" 2))
    (:context  '("#cccccc" "none" 1))
    (_         '("#555555" "none" 1))))

(defun scad-sketch--draw-node (svg tf session node)
  "Render NODE onto SVG using transform TF."
  (let* ((state  (scad-sketch--node-visual-state node session))
         (colors (scad-sketch--state-colors state))
         (stroke (nth 0 colors))
         (sw     (nth 2 colors)))
    (pcase (plist-get node :type)
      ;; Polygon / array
      ((or 'polygon 'array)
       (let* ((pts    (or (scad-sketch--polygon-points node session) '()))
              (closed (if (eq (plist-get node :type) 'array)
                          t
                        (scad-sketch--poly-closed session node)))
              (focused (scad-sketch--focused-node session))
              (is-focused (eq node focused)))
         (when (>= (length pts) 2)
           (if (scad-sketch--any-radius-p pts)
               (let ((d (scad-sketch--polyround-path-d pts closed tf)))
                 (when d (svg-node svg 'path :d d :stroke stroke :stroke-width sw :fill "none")))
             (let ((xys (mapcar (lambda (p) (list (float (nth 0 p)) (float (nth 1 p)))) pts)))
               (cl-loop for a on xys for b = (cadr a) when b do
                        (scad-sketch--svg-line svg tf (car a) b :stroke stroke :stroke-width sw))
               (when (and closed (> (length xys) 2))
                 (scad-sketch--svg-line svg tf (car (last xys)) (car xys) :stroke stroke :stroke-width sw)))))
         ;; Draw vertex dots when this polygon is focused
         (when is-focused
           (let ((sel-idx (scad-sketch--poly-selected-index session node))
                 (n (length pts)))
             (dotimes (i n)
               (let* ((p      (nth i pts))
                      (xy     (list (float (nth 0 p)) (float (nth 1 p))))
                      (screen (funcall tf xy))
                      (sel    (= i (or sel-idx -1)))
                      (r-val  (or (nth 2 p) 0)))
                 (svg-circle svg (nth 0 screen) (nth 1 screen) (if sel 7 5)
                             :stroke (if sel "#d13f00" "#111111")
                             :stroke-width (if sel 3 2)
                             :fill (if sel "#fff0e8" "#ffffff"))
                 (svg-text svg (number-to-string i)
                           :x (+ (nth 0 screen) 8) :y (- (nth 1 screen) 8)
                           :font-size 12 :fill "#333333")
                 (when (> r-val 0)
                   (let* ((prev    (cond ((> i 0)     (nth (1- i) pts))
                                        (closed       (nth (1- n) pts))))
                          (next    (cond ((< i (1- n)) (nth (1+ i) pts))
                                        (closed       (nth 0 pts))))
                          (corner  (when (and prev next)
                                     (scad-sketch--corner-geometry
                                      (list (float (nth 0 prev)) (float (nth 1 prev)))
                                      xy
                                      (list (float (nth 0 next)) (float (nth 1 next)))
                                      r-val)))
                          (act-r   (if corner (plist-get corner :radius) r-val))
                          (capped  (and corner (< (+ act-r 0.001) r-val))))
                     (svg-circle svg (nth 0 screen) (nth 1 screen)
                                 (scad-sketch--pixel-radius act-r tf)
                                 :stroke (if capped "#c04000" "#804000")
                                 :stroke-width 1 :stroke-dasharray "3,3" :fill "none")
                     (svg-text svg (if capped
                                       (format "r=%s\u2192%s"
                                               (scad-sketch--fmt-num r-val)
                                               (scad-sketch--fmt-num act-r))
                                     (format "r=%s" (scad-sketch--fmt-num r-val)))
                               :x (+ (nth 0 screen) 8) :y (+ (nth 1 screen) 18)
                               :font-size 11 :fill (if capped "#c04000" "#804000"))))))))
       ;; Shape label when not focused
       (when (not (eq node (scad-sketch--focused-node session)))
         (let* ((bbox   (scad-sketch--node-bbox node session))
                (cx     (/ (+ (nth 0 bbox) (nth 1 bbox)) 2.0))
                (cy     (/ (+ (nth 2 bbox) (nth 3 bbox)) 2.0))
                (screen (funcall tf (list cx cy))))
           (svg-text svg "poly"
                     :x (nth 0 screen) :y (nth 1 screen)
                     :font-size 10 :fill stroke :text-anchor "middle"))))

      ;; Circle
      ('circle
       (let* ((cx (plist-get node :cx)) (cy (plist-get node :cy))
              (r  (plist-get node :r))
              (sc (funcall tf (list cx cy)))
              (pr (scad-sketch--pixel-radius r tf)))
         (svg-circle svg (nth 0 sc) (nth 1 sc) pr
                     :stroke stroke :stroke-width sw :fill "none")
         (svg-circle svg (nth 0 sc) (nth 1 sc) 3
                     :stroke stroke :stroke-width 1 :fill stroke)
         (svg-text svg (format "r=%s" (scad-sketch--fmt-num r))
                   :x (+ (nth 0 sc) (+ pr 4)) :y (nth 1 sc)
                   :font-size 10 :fill stroke)))

      ;; Square
      ('square
       (let* ((x (plist-get node :x)) (y (plist-get node :y))
              (w (plist-get node :w)) (h (plist-get node :h))
              (corners (list (list x y) (list (+ x w) y)
                             (list (+ x w) (+ y h)) (list x (+ y h))))
              (xys (mapcar tf corners)))
         (cl-loop for a on xys for b = (cadr a) when b do
                  (apply #'svg-line svg
                         (nth 0 (car a)) (nth 1 (car a)) (nth 0 b) (nth 1 b)
                         (list :stroke stroke :stroke-width sw)))
         (apply #'svg-line svg
                (nth 0 (car (last xys))) (nth 1 (car (last xys)))
                (nth 0 (car xys)) (nth 1 (car xys))
                (list :stroke stroke :stroke-width sw))))

      ;; Text
      ('text
       (let* ((sc (funcall tf (list (plist-get node :x) (plist-get node :y))))
              (sz (scad-sketch--pixel-radius (plist-get node :size) tf)))
         (svg-text svg (plist-get node :str)
                   :x (nth 0 sc) :y (nth 1 sc)
                   :font-size sz :fill stroke)))

      ;; Compositions — draw a label near centroid, children drawn separately
      ((or 'difference 'union 'intersection)
       (let* ((bbox   (scad-sketch--node-bbox node session))
              (cx     (/ (+ (nth 0 bbox) (nth 1 bbox)) 2.0))
              (cy     (/ (+ (nth 2 bbox) (nth 3 bbox)) 2.0))
              (screen (funcall tf (list cx cy))))
         (svg-text svg (symbol-name (plist-get node :type))
                   :x (nth 0 screen) :y (- (nth 1 screen) 8)
                   :font-size 10 :fill stroke :text-anchor "middle")))

      ;; Transforms — just draw child (with a small annotation)
      ((or 'translate 'rotate 'scale 'mirror)
       nil))))) ; children rendered separately by the walk below

(defun scad-sketch--draw-tree (svg tf session)
  "Draw all nodes in the session tree."
  (dolist (node (scad-sketch-session-tree session))
    (scad-sketch-parse--walk
     node
     (lambda (n)
       (scad-sketch--draw-node svg tf session n)))))

(defun scad-sketch--draw-cursor-and-marks (svg tf session)
  "Draw cursor crosshair and mark dots."
  (let* ((marks  (scad-sketch-session-marks session))
         (cursor (scad-sketch-session-point session)))
    ;; Mark chain
    (let ((ordered (reverse marks)))
      (cl-loop for a on ordered for b = (cadr a) when b do
               (scad-sketch--svg-line svg tf (car a) b
                                      :stroke "#008a2e" :stroke-width 1 :stroke-dasharray "4,4"))
      (when ordered
        (scad-sketch--svg-line svg tf (car (last ordered)) cursor
                               :stroke "#008a2e" :stroke-width 1 :stroke-dasharray "4,4")))
    (dolist (m (reverse marks))
      (let* ((s (funcall tf m))
             (cur (equal m (car marks)))
             (col (if cur "#008a2e" "#50a870")))
        (svg-circle svg (nth 0 s) (nth 1 s) 6 :stroke col :stroke-width 2 :fill "#e2ffe9")
        (when cur
          (svg-text svg "mark" :x (+ (nth 0 s) 10) :y (+ (nth 1 s) 4)
                    :font-size 12 :fill col))))
    ;; Cursor
    (let ((p (funcall tf cursor)))
      (svg-circle svg (nth 0 p) (nth 1 p) 5 :stroke "#0057c2" :stroke-width 2 :fill "#dfefff")
      (svg-line svg (- (nth 0 p) 10) (nth 1 p) (+ (nth 0 p) 10) (nth 1 p) :stroke "#0057c2" :stroke-width 2)
      (svg-line svg (nth 0 p) (- (nth 1 p) 10) (nth 0 p) (+ (nth 1 p) 10) :stroke "#0057c2" :stroke-width 2)
      (svg-text svg "point" :x (+ (nth 0 p) 12) :y (+ (nth 1 p) 4) :font-size 12 :fill "#0057c2"))))

(defun scad-sketch--fmt-num (n)
  "Format N compactly for OpenSCAD."
  (let ((x (float n)))
    (if (< (abs (- x (round x))) 0.000001)
        (number-to-string (round x))
      (let ((s (format "%.4f" x)))
        (setq s (replace-regexp-in-string "0+\\'" "" s))
        (setq s (replace-regexp-in-string "\\.\\'" "" s))
        (if (or (string= s "-0") (string= s "")) "0" s)))))

(defun scad-sketch--fmt-xy (xy)
  (format "(%s, %s)" (scad-sketch--fmt-num (nth 0 xy)) (scad-sketch--fmt-num (nth 1 xy))))

(defun scad-sketch--draw-hud (svg session)
  "Draw the status bar."
  (let* ((marks    (scad-sketch-session-marks session))
         (focused  (scad-sketch--focused-node session))
         (hover    (car (scad-sketch-session-hover-stack session)))
         (mark-str (cond ((null marks) "none")
                         ((= 1 (length marks)) (scad-sketch--fmt-xy (car marks)))
                         (t (format "%s (+%d)" (scad-sketch--fmt-xy (car marks)) (1- (length marks))))))
         (text (format "%s  grid=%s%s  point=%s  mark=%s  focus=%s  hover=%s  %s"
                       (or (when focused (plist-get focused :name))
                           (format "%d nodes" (length (scad-sketch-session-tree session))))
                       (scad-sketch--fmt-num (scad-sketch-session-grid session))
                       (scad-sketch-session-units session)
                       (scad-sketch--fmt-xy (scad-sketch-session-point session))
                       mark-str
                       (if focused (symbol-name (plist-get focused :type)) "none")
                       (if hover (symbol-name (plist-get hover :type)) "none")
                       (if (scad-sketch-session-dirty session) "*dirty*" "saved"))))
    (svg-rectangle svg 0 0 scad-sketch-canvas-width 28 :fill "#f8f8f8")
    (svg-text svg text :x 10 :y 19 :font-size 13 :fill "#111111")))

(defun scad-sketch--render ()
  "Re-render the editor buffer."
  (let* ((session   (scad-sketch--assert-session))
         ;; Recompute hover stack from cursor position
         (_ (setf (scad-sketch-session-hover-stack session)
                  (scad-sketch--compute-hover-stack session)))
         (svg       (svg-create scad-sketch-canvas-width scad-sketch-canvas-height))
         (bounds    (scad-sketch--bounds session))
         (tf        (scad-sketch--transform bounds)))
    (svg-rectangle svg 0 0 scad-sketch-canvas-width scad-sketch-canvas-height :fill "#ffffff")
    (scad-sketch--draw-grid svg bounds tf session)
    (scad-sketch--draw-tree svg tf session)
    (scad-sketch--draw-cursor-and-marks svg tf session)
    (scad-sketch--draw-hud svg session)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((beg (point)))
        (insert-image (svg-image svg :ascent 'center))
        (remove-text-properties beg (point) '(keymap nil)))
      (insert "\n\n")
      (insert (scad-sketch-unparse-top-level (scad-sketch-session-tree session)))
      (goto-char (point-min)))))

;;;; Write-back

(defun scad-sketch--sync-source-text (session)
  "Update session source-text from the source buffer."
  (when (buffer-live-p (scad-sketch-session-source-buffer session))
    (with-current-buffer (scad-sketch-session-source-buffer session)
      (setf (scad-sketch-session-source-text session)
            (buffer-substring-no-properties (point-min) (point-max))))))

(defun scad-sketch-write-back ()
  "Write the edited tree back to the source buffer."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (source  (scad-sketch-session-source-buffer session))
         (beg     (scad-sketch-session-content-beg session))
         (end     (scad-sketch-session-content-end session))
         (content (scad-sketch-unparse-top-level (scad-sketch-session-tree session))))
    (unless (buffer-live-p source) (user-error "Source buffer is gone"))
    (with-current-buffer source
      (save-excursion
        (goto-char beg)
        (delete-region beg end)
        (insert content)
        (set-marker end (point))))
    (scad-sketch--sync-source-text session)
    (setf (scad-sketch-session-dirty session) nil)
    (scad-sketch--render)
    (message "Wrote scad-sketch back to %s" (buffer-name source))))

(defun scad-sketch-quit ()
  "Quit the editor, restoring the window configuration."
  (interactive)
  (let ((session (scad-sketch--assert-session))
        (wconf   scad-sketch--window-config))
    (when (and (scad-sketch-session-dirty session)
               (y-or-n-p "Sketch has unwritten edits. Write back first? "))
      (scad-sketch-write-back))
    (kill-buffer (current-buffer))
    (when wconf (set-window-configuration wconf))))

(defun scad-sketch-help ()
  "Show key summary in the echo area."
  (interactive)
  (scad-sketch--assert-session)
  (message (concat "arrows=move  C-arrows=coarse  M-arrows=fine  S-arrows=move-vertex | "
                   "TAB/S-TAB=cycle-hover  RET=focus  ESC=unfocus  SPC=select | "
                   "p=append  i=insert  k=delete  R=radius  c=closed | "
                   "m/M/`/'/C=marks  x/y/X/Y/d/a=coords  g=grid | "
                   "w=write  u=undo  q=quit  C-h m=full help")))

;;;; Session construction and entry points

(defun scad-sketch--find-scad-target ()
  "Find the SCAD form at point for editing.
Returns plist :node-type :beg :end :source-text or signals user-error."
  (let* ((line-end (line-end-position))
         (buf-text (buffer-substring-no-properties (point-min) (point-max)))
         ;; Find a contiguous SCAD form that includes the current line.
         ;; Strategy: search backward for a top-level form start, parse forward.
         (origin (point))
         ;; Convert buffer position to 0-based offset in buf-text
         (origin-offset (1- origin)))
    (list :source-text buf-text :origin-offset origin-offset)))

(defun scad-sketch--session-from-source (source-text origin-offset source-buffer beg end)
  "Parse SOURCE-TEXT and create a session targeting the node at ORIGIN-OFFSET."
  (let* ((nodes       (scad-sketch-parse source-text))
         (target-node (scad-sketch-parse-node-at nodes origin-offset))
         ;; Determine focused-path: focus on the target node if it's a primitive,
         ;; leave unfocused if it's a composition or array.
         (target-path (when target-node
                        (scad-sketch--path-in-list nodes target-node)))
         (focused-path (when (and target-node
                                  (memq (plist-get target-node :type)
                                        '(polygon array circle square text)))
                         target-path))
         ;; Initial cursor: centroid of target node or (0,0).
         (init-pt     (if target-node
                          (let ((bbox (scad-sketch--bbox-of nodes target-node source-text)))
                            (list (/ (+ (nth 0 bbox) (nth 1 bbox)) 2.0)
                                  (/ (+ (nth 2 bbox) (nth 3 bbox)) 2.0)))
                        (list 0.0 0.0)))
         (session     (make-scad-sketch-session
                       :tree nodes
                       :source-text source-text
                       :focused-path focused-path
                       :hover-stack nil
                       :selection nil
                       :point init-pt
                       :marks nil
                       :named-marks nil
                       :grid (float scad-sketch-default-grid)
                       :fine-step (float scad-sketch-default-fine-step)
                       :coarse-step (float scad-sketch-default-coarse-step)
                       :units "mm"
                       :source-buffer source-buffer
                       :content-beg beg
                       :content-end end
                       :dirty nil
                       :undo-stack nil)))
    session))

(defun scad-sketch--path-in-list (nodes target)
  "Return the path (list of indices) to TARGET within the top-level NODES list."
  (cl-labels ((search (node-list prefix)
                (cl-loop for n in node-list for i from 0
                         when (eq n target)
                         return (append prefix (list i))
                         thereis (when (scad-sketch-parse--node-children n)
                                   (search (scad-sketch-parse--node-children n)
                                           (append prefix (list i)))))))
    (search nodes nil)))

(defun scad-sketch--bbox-of (nodes node source-text)
  "Return bounding box of NODE in the context of NODES."
  ;; Create a temporary session stub just for bbox computation.
  (let ((stub (make-scad-sketch-session
               :tree nodes :source-text source-text
               :focused-path nil :hover-stack nil :selection nil
               :point '(0 0) :marks nil :named-marks nil
               :grid 1 :fine-step 0.1 :coarse-step 5
               :units "mm" :source-buffer nil :content-beg nil :content-end nil
               :dirty nil :undo-stack nil)))
    (scad-sketch--node-bbox node stub)))

(defun scad-sketch--open-session (session)
  "Open an editor buffer for SESSION."
  (let ((wconf (current-window-configuration))
        (name  (or (when (scad-sketch-session-focused-path session)
                     (let ((n (scad-sketch--focused-node session)))
                       (or (plist-get n :name)
                           (symbol-name (plist-get n :type)))))
                   "sketch"))
        (buf   nil))
    (setq buf (get-buffer-create
               (format "%s%s*" scad-sketch--editor-buffer-prefix name)))
    (with-current-buffer buf
      (scad-sketch-editor-mode)
      (setq-local scad-sketch--session session)
      (setq-local scad-sketch--window-config wconf)
      (scad-sketch--render))
    (pop-to-buffer buf)))

(defun scad-sketch--find-form-region ()
  "Find the start/end buffer positions of the smallest SCAD form at point.
Searches backward for the form's opening token and forward for its close.
Returns (BEG END) or signals user-error if nothing is found."
  (save-excursion
    ;; Strategy: search backward for the nearest array assignment (name = [)
    ;; or known 2D keyword on its own line, from end-of-line so we catch the
    ;; opening line itself.
    (goto-char (line-end-position))
    (let ((found-beg nil) (found-end nil))
      ;; Try array assignment first: name = [
      (when (re-search-backward
             (rx (group (+ (any "A-Za-z0-9_$")))
                 (* space) "=" (* space) "[")
             nil t)
        (setq found-beg (match-beginning 0))
        ;; Find the closing ] then the ; after it
        (goto-char (1- (match-end 0)))  ; back to the [
        (condition-case nil
            (progn
              (forward-sexp 1)
              (skip-chars-forward "
")
              (when (= (char-after) ?\;)
                (forward-char 1)
                (setq found-end (point))))
          (error nil)))
      (unless (and found-beg found-end
                   (<= found-beg (point-original))
                   (<= (point-original) found-end))
        (setq found-beg nil found-end nil))
      (when (and found-beg found-end)
        (list found-beg found-end)))))

(defun scad-sketch--find-form-region ()
  "Find the buffer region of the best SCAD form target at point.
Tries, in order:
  1. The array assignment surrounding point (name = [...];)
  2. The innermost brace-delimited block containing point
     (for compositions like difference() { ... })
Returns (BEG END) buffer positions, or nil if nothing useful found."
  (let ((origin (point)))
    (save-excursion
      ;; --- Attempt 1: array assignment ---
      ;; Search backward from line-end so we catch the opening line itself.
      (goto-char (line-end-position))
      (let ((arr-beg nil) (arr-end nil))
        (when (re-search-backward
               (rx (+ (any "A-Za-z0-9_$")) (* space) "=" (* space) "[")
               nil t)
          (setq arr-beg (match-beginning 0))
          ;; Walk forward to find the matching ] and trailing ;
          (goto-char (- (match-end 0) 1))  ; position on the [
          (condition-case nil
              (progn
                (forward-sexp 1)
                (skip-chars-forward "
")
                (when (= (char-after) ?\;)
                  (forward-char 1)
                  (setq arr-end (point))))
            (error nil)))
        (when (and arr-beg arr-end
                   (<= arr-beg origin) (<= origin arr-end))
          (cl-return-from scad-sketch--find-form-region (list arr-beg arr-end))))
      ;; --- Attempt 2: brace-delimited block containing point ---
      ;; Walk backward counting braces to find the enclosing { ... }
      ;; then expand to include the keyword + () before it.
      (goto-char origin)
      (let ((depth 0) block-end block-beg)
        ;; Find closing } at or after origin
        (while (and (not block-end) (< (point) (point-max)))
          (let ((ch (char-after)))
            (cond
             ((= ch ?\{) (setq depth (1+ depth)) (forward-char 1))
             ((= ch ?\}) (if (= depth 0)
                             (progn (forward-char 1) (setq block-end (point)))
                           (setq depth (1- depth)) (forward-char 1)))
             (t (forward-char 1)))))
        (when block-end
          ;; Now find the matching { by going backward from block-end
          (goto-char (1- block-end))
          (condition-case nil
              (progn
                (backward-sexp 1)
                ;; Skip back over () and keyword
                (skip-chars-backward "
")
                (when (= (char-before) ?\))
                  (backward-sexp 1))
                (skip-chars-backward "
")
                ;; Now we should be just after the keyword; go to word start
                (skip-chars-backward "A-Za-z0-9_$")
                (setq block-beg (point)))
            (error nil))
          (when (and block-beg (<= block-beg origin) (<= origin block-end))
            (list block-beg block-end)))))))

;;;###autoload
(defun scad-sketch-at-point ()
  "Open the sketch editor for the 2D SCAD form at point.
Parses the entire buffer (skipping unknown constructs) so that variable
references like polygon(pts) can be resolved."
  (interactive)
  (unless (image-type-available-p 'svg)
    (user-error "This Emacs was not built with SVG image support"))
  (let* (;; Always parse the whole buffer — unknown forms are now skipped
         ;; silently, so include/use/module/function don't cause errors.
         (source-text (buffer-substring-no-properties (point-min) (point-max)))
         (origin-off  (1- (point)))
         (beg         (copy-marker (point-min)))
         (end         (copy-marker (point-max) t))
         (session     (condition-case err
                          (scad-sketch--session-from-source
                           source-text origin-off (current-buffer) beg end)
                        (user-error
                         (user-error "scad-sketch: %s" (cadr err))))))
    (unless (scad-sketch-session-tree session)
      (user-error "scad-sketch: no recognizable 2D forms found at or near point"))
    (scad-sketch--open-session session)))

;;;###autoload
(defun scad-sketch-insert-array-at-point (name)
  "Insert a new empty named array at point and open the sketch editor."
  (interactive "sArray name: ")
  (unless (image-type-available-p 'svg)
    (user-error "This Emacs was not built with SVG image support"))
  (let ((insert-pos (point)))
    (insert (format "%s = [];\n" name))
    (let* ((source-text (buffer-substring-no-properties (point-min) (point-max)))
           (origin-off  (1- insert-pos))
           (beg         (copy-marker (point-min)))
           (end         (copy-marker (point-max) t))
           (session     (scad-sketch--session-from-source
                         source-text origin-off (current-buffer) beg end)))
      (scad-sketch--open-session session))))

;;;###autoload
(defun scad-sketch-or-insert-at-point ()
  "Edit the SCAD form at point, or insert and open a new array if none found."
  (interactive)
  (unless (image-type-available-p 'svg)
    (user-error "This Emacs was not built with SVG image support"))
  (condition-case nil
      (scad-sketch-at-point)
    (user-error
     (call-interactively #'scad-sketch-insert-array-at-point))))

(provide 'scad-sketch)
;;; scad-sketch.el ends here
