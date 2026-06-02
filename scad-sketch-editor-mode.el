;;; scad-sketch-editor-mode.el --- Interactive SVG editor for scad-sketch -*- lexical-binding: t; -*-

;;; Commentary:

;; Implements `scad-sketch-editor-mode', the major mode used inside the
;; *scad-sketch: …* editor buffer.  Depends on:
;;
;;   scad-sketch-parse    — AST types and unparsing
;;   scad-sketch-geometry — spatial tests, bounding boxes, polyRound, fmt-num
;;   scad-sketch-session  — session struct and its accessors
;;
;; The file is intentionally self-contained with respect to interaction: every
;; interactive command, the render pipeline, and the write-back path live here.
;; Nothing in this file is needed by session construction or geometry code.
;;
;; INTERACTION MODEL
;; -----------------
;; All mutations go through `scad-sketch--mutate', which:
;;   1. Pushes the current state onto the undo stack.
;;   2. Calls the supplied lambda with the session.
;;   3. Marks the session dirty.
;;   4. Re-renders.
;;
;; THREE INTERACTION STATES (per mode docstring):
;;   Hovered  — node nearest cursor, recomputed each render.
;;   Selected — explicit set, toggled with SPC.
;;   Focused  — one subtree being edited; others dimmed.

;;; Code:

(require 'cl-lib)
(require 'svg)
(require 'scad-sketch-parse)
(require 'scad-sketch-geometry)
(require 'scad-sketch-session)

;;;; ── Editor-buffer locals ───────────────────────────────────────────────────

(defvar-local scad-sketch--session nil
  "The `scad-sketch-session' for the current editor buffer.")

(defvar-local scad-sketch--window-config nil
  "Window configuration to restore when the editor is quit.")

;;;; ── Keymap ─────────────────────────────────────────────────────────────────

(defvar scad-sketch-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    ;; ── Cursor movement ──
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
    ;; ── Selected vertex movement ──
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
    ;; ── Marks ──
    (define-key map (kbd "m")           #'scad-sketch-set-mark)
    (define-key map (kbd "M")           #'scad-sketch-push-mark)
    (define-key map (kbd "`")           #'scad-sketch-pop-mark)
    (define-key map (kbd "'")           #'scad-sketch-jump-to-mark)
    (define-key map (kbd "C")           #'scad-sketch-clear-marks)
    ;; ── Navigation / focus ──
    (define-key map (kbd "TAB")         #'scad-sketch-tab-next)
    (define-key map (kbd "<backtab>")   #'scad-sketch-tab-prev)
    (define-key map (kbd "RET")         #'scad-sketch-focus-hovered)
    (define-key map (kbd "ESC")         #'scad-sketch-unfocus)
    (define-key map (kbd "SPC")         #'scad-sketch-toggle-hovered-selection)
    ;; ── Vertex / polygon editing ──
    (define-key map (kbd "p")           #'scad-sketch-append-point)
    (define-key map (kbd "i")           #'scad-sketch-insert-point-after-selected)
    (define-key map (kbd "k")           #'scad-sketch-delete-selected)
    (define-key map (kbd "l")           #'scad-sketch-line-from-mark)
    (define-key map (kbd "r")           #'scad-sketch-rectangle-from-mark)
    (define-key map (kbd "c")           #'scad-sketch-toggle-closed)
    (define-key map (kbd "R")           #'scad-sketch-set-radius)
    ;; ── Coordinate entry ──
    (define-key map (kbd "x")           #'scad-sketch-set-x)
    (define-key map (kbd "y")           #'scad-sketch-set-y)
    (define-key map (kbd "X")           #'scad-sketch-set-delta-x)
    (define-key map (kbd "Y")           #'scad-sketch-set-delta-y)
    (define-key map (kbd "d")           #'scad-sketch-set-distance-from-mark)
    (define-key map (kbd "a")           #'scad-sketch-set-angle-from-mark)
    (define-key map (kbd "g")           #'scad-sketch-set-grid)
    ;; ── Session ──
    (define-key map (kbd "u")           #'scad-sketch-undo)
    (define-key map (kbd "w")           #'scad-sketch-write-back)
    (define-key map (kbd "q")           #'scad-sketch-quit)
    (define-key map (kbd "?")           #'scad-sketch-help)
    map)
  "Keymap for `scad-sketch-editor-mode'.")

;;;; ── Major mode ─────────────────────────────────────────────────────────────

(define-derived-mode scad-sketch-editor-mode special-mode "SCAD-Sketch"
  "Major mode for the scad-sketch visual SVG editor.

The buffer displays an SVG canvas with the current parse tree and a live
OpenSCAD source preview below it.

THREE INTERACTION STATES
  Hovered  — shape/point nearest the cursor crosshair (blue highlight).
             TAB / S-TAB cycle hover candidates within the active context.
  Selected — explicit set, toggled with SPC.  Shown in amber.
             C clears all selections and marks.
  Focused  — one subtree being edited; others are dimmed.
             RET focuses the hovered shape.  ESC pops one focus level.

CURSOR MOVEMENT
  <arrow>           grid step (snaps to grid)
  C-<arrow>         coarse step (snaps to grid)
  M-<arrow>         fine step (intentionally off-grid)
  S-<arrow>         move selected vertex (grid)
  M-S-<arrow>       move selected vertex (fine, off-grid)
  C-S-<arrow>       move selected vertex (coarse)

NAVIGATION
  TAB / S-TAB       cycle hover through shapes (at composition level)
                    or through vertices (when a polygon is focused)
  RET               focus the hovered shape (drill in)
  ESC               pop one focus level (no-op at root)
  SPC               toggle hovered shape / vertex in/out of selection

MARKS
  m    replace mark stack with cursor     M    push cursor onto mark stack
  `    pop most recent mark (jump to it)  '    jump to most recent mark (non-destructive)
  C    clear all marks and selections

EDITING (requires a focused polygon)
  p    append cursor as new vertex        i    insert after selected vertex
  k    delete selected vertex             R    set polyRound radius on selected vertex
  c    toggle closed / open              l    append marks then cursor as vertices
  r    append rectangle corners (mark → cursor)

COORDINATES
  x / y    set absolute cursor X or Y
  X / Y    set cursor relative to mark (ΔX / ΔY)
  d        set distance from mark (preserving angle)
  a        set angle from mark in degrees (preserving distance)

SESSION
  g    set grid step    u    undo    w    write back to source    q    quit
  ?    brief key summary in echo area    C-h m    this help

\\{scad-sketch-editor-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t))

;;;; ── Session guard ──────────────────────────────────────────────────────────

(defun scad-sketch--assert-session ()
  "Return the buffer-local session or signal `user-error'."
  (unless (and (boundp 'scad-sketch--session) scad-sketch--session)
    (user-error "No active scad-sketch session"))
  scad-sketch--session)

;;;; ── Convenience accessors ──────────────────────────────────────────────────

(defun scad-sketch--grid   (s) (float (scad-sketch-session-grid s)))
(defun scad-sketch--fine   (s) (float (scad-sketch-session-fine-step s)))
(defun scad-sketch--coarse (s) (float (scad-sketch-session-coarse-step s)))

;;;; ── Tree navigation ────────────────────────────────────────────────────────

(defun scad-sketch--node-at-path (session path)
  "Return the AST node reached by following PATH from the session root.
PATH is a list of child indices.  nil PATH returns the tree list itself."
  (let ((tree (scad-sketch-session-tree session)))
    (if (null path)
        tree
      (let ((node (nth (car path) tree)))
        (dolist (idx (cdr path))
          (setq node (nth idx (scad-sketch-parse--node-children node))))
        node))))

(defun scad-sketch--focused-node (session)
  "Return the currently focused AST node, or nil if nothing is focused."
  (let ((path (scad-sketch-session-focused-path session)))
    (when path (scad-sketch--node-at-path session path))))

(defun scad-sketch--focused-tree (session)
  "Return the list of nodes active for interaction.
If a composition is focused, returns its children.  If a primitive is
focused, returns a singleton list.  If nothing is focused, returns all
top-level nodes."
  (let ((focused (scad-sketch--focused-node session)))
    (if focused
        (let ((children (scad-sketch-parse--node-children focused)))
          (if children children (list focused)))
      (scad-sketch-session-tree session))))

(defun scad-sketch--path-of-node (session node)
  "Return the path (list of indices from root) to NODE, or nil."
  (let ((tree (scad-sketch-session-tree session)))
    (cl-labels ((search (nodes prefix)
                  (cl-loop for n in nodes for i from 0
                           when (eq n node)
                             return (append prefix (list i))
                           thereis
                             (when (scad-sketch-parse--node-children n)
                               (search (scad-sketch-parse--node-children n)
                                       (append prefix (list i)))))))
      (search tree nil))))

;;;; ── Polygon point resolution ───────────────────────────────────────────────

(defun scad-sketch--polygon-points (node session)
  "Return the resolved [x y r] point list for a polygon NODE.
Follows :source variable references via `scad-sketch-parse--lookup-variable'."
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
  "Update NODE's points in the session tree to NEW-POINTS.
If NODE has a :source variable reference, finds the corresponding array
node in the tree and updates that instead (so the variable definition is
what gets changed, not the polygon call site)."
  (let ((src (plist-get node :source)))
    (if src
        (let ((arr (cl-find-if
                    (lambda (n)
                      (and (eq (plist-get n :type) 'array)
                           (equal (plist-get n :name) src)))
                    (scad-sketch-session-tree session))))
          (when arr (plist-put arr :points new-points)))
      (plist-put node :points new-points))))

;;;; ── Geometry adapter ───────────────────────────────────────────────────────
;; The geometry module takes a polygon-points-fn rather than a session to stay
;; session-agnostic.  These wrappers bind the session and call through.

(defun scad-sketch--poly-points-fn (session)
  "Return a closure (node → points) suitable for geometry functions."
  (lambda (n) (scad-sketch--polygon-points n session)))

(defun scad-sketch--node-bbox (node session)
  "Return (min-x max-x min-y max-y) bounding box for NODE."
  (scad-sketch-geo--node-bbox node (scad-sketch--poly-points-fn session)))

(defun scad-sketch--node-bbox-area (node session)
  "Return bounding-box area of NODE."
  (scad-sketch-geo--node-bbox-area node (scad-sketch--poly-points-fn session)))

(defun scad-sketch--point-in-node-p (x y node session)
  "Return non-nil when (X Y) is geometrically inside NODE."
  (scad-sketch-geo--point-in-node-p x y node (scad-sketch--poly-points-fn session)))

;;;; ── Selection helpers ──────────────────────────────────────────────────────

(defun scad-sketch--sel-ref (kind path &optional index)
  "Build a selection reference plist (:kind KIND :path PATH :index INDEX)."
  (list :kind kind :path path :index index))

(defun scad-sketch--sel-equal (a b)
  "Return non-nil when selection refs A and B refer to the same thing."
  (and (eq    (plist-get a :kind)  (plist-get b :kind))
       (equal (plist-get a :path)  (plist-get b :path))
       (equal (plist-get a :index) (plist-get b :index))))

(defun scad-sketch--sel-member (ref selection)
  "Return non-nil when REF is present in SELECTION."
  (cl-some (lambda (r) (scad-sketch--sel-equal r ref)) selection))

(defun scad-sketch--sel-toggle (ref selection)
  "Return SELECTION with REF added (if absent) or removed (if present)."
  (if (scad-sketch--sel-member ref selection)
      (cl-remove-if (lambda (r) (scad-sketch--sel-equal r ref)) selection)
    (cons ref selection)))

;;;; ── Per-polygon edit state ─────────────────────────────────────────────────
;; Each polygon node gets a (selected-index . closed) cons stored in the
;; session's named-marks hash-table (repurposed from its original use).

(defun scad-sketch--poly-state (session node)
  "Return the (selected-index . closed) cons for polygon NODE.
Creates a default entry (index 0, closed t) if none exists yet."
  (let ((tbl (scad-sketch-session-named-marks session)))
    (unless (and tbl (hash-table-p tbl))
      (setf (scad-sketch-session-named-marks session) (make-hash-table :test 'eq))
      (setq tbl (scad-sketch-session-named-marks session)))
    (or (gethash node tbl)
        (let ((default (cons 0 t)))
          (puthash node default tbl)
          default))))

(defun scad-sketch--poly-selected-index (session node)
  (car (scad-sketch--poly-state session node)))
(defun scad-sketch--poly-closed (session node)
  (cdr (scad-sketch--poly-state session node)))
(defun scad-sketch--poly-set-selected (session node idx)
  (setcar (scad-sketch--poly-state session node) idx))
(defun scad-sketch--poly-set-closed (session node val)
  (setcdr (scad-sketch--poly-state session node) val))

;;;; ── Undo ───────────────────────────────────────────────────────────────────

(defun scad-sketch--push-undo (session)
  "Snapshot the mutable parts of SESSION onto its undo stack."
  (push (list :tree         (copy-tree (scad-sketch-session-tree session))
              :point        (copy-tree (scad-sketch-session-point session))
              :marks        (copy-tree (scad-sketch-session-marks session))
              :focused-path (copy-tree (scad-sketch-session-focused-path session))
              :selection    (copy-tree (scad-sketch-session-selection session))
              :poly-states  (when (hash-table-p (scad-sketch-session-named-marks session))
                              (let (acc)
                                (maphash (lambda (k v)
                                           (push (cons k (copy-tree v)) acc))
                                         (scad-sketch-session-named-marks session))
                                acc)))
        (scad-sketch-session-undo-stack session)))

(defun scad-sketch--mark-dirty (session)
  "Flag SESSION as having unsaved edits."
  (setf (scad-sketch-session-dirty session) t))

(defun scad-sketch--mutate (fn)
  "Run FN on the current session inside an undo boundary, then re-render.
FN receives the session as its sole argument.  State is pushed before FN
runs, so the undo entry reflects the state *before* the mutation."
  (let ((session (scad-sketch--assert-session)))
    (scad-sketch--push-undo session)
    (funcall fn session)
    (scad-sketch--mark-dirty session)
    (scad-sketch--render)))

;;;; ── Cursor movement ────────────────────────────────────────────────────────

(defun scad-sketch--move-xy (xy dx dy)
  (list (+ (float (nth 0 xy)) dx) (+ (float (nth 1 xy)) dy)))

(defun scad-sketch--snap-to-grid (v grid)
  (* grid (round (/ v grid))))

(defun scad-sketch--snap-xy (xy grid)
  (list (scad-sketch--snap-to-grid (nth 0 xy) grid)
        (scad-sketch--snap-to-grid (nth 1 xy) grid)))

(defun scad-sketch--move-point (dx dy &optional snap)
  "Move the cursor by (DX DY), snapping to grid when SNAP is non-nil."
  (scad-sketch--mutate
   (lambda (s)
     (let ((new (scad-sketch--move-xy (scad-sketch-session-point s) dx dy)))
       (setf (scad-sketch-session-point s)
             (if snap (scad-sketch--snap-xy new (scad-sketch--grid s)) new))))))

(defun scad-sketch-move-point-left ()         (interactive) (scad-sketch--move-point (- (scad-sketch--grid   (scad-sketch--assert-session))) 0 t))
(defun scad-sketch-move-point-right ()        (interactive) (scad-sketch--move-point    (scad-sketch--grid   (scad-sketch--assert-session))  0 t))
(defun scad-sketch-move-point-up ()           (interactive) (scad-sketch--move-point 0  (scad-sketch--grid   (scad-sketch--assert-session))    t))
(defun scad-sketch-move-point-down ()         (interactive) (scad-sketch--move-point 0 (- (scad-sketch--grid   (scad-sketch--assert-session))) t))
(defun scad-sketch-move-point-fine-left ()    (interactive) (scad-sketch--move-point (- (scad-sketch--fine   (scad-sketch--assert-session))) 0))
(defun scad-sketch-move-point-fine-right ()   (interactive) (scad-sketch--move-point    (scad-sketch--fine   (scad-sketch--assert-session))  0))
(defun scad-sketch-move-point-fine-up ()      (interactive) (scad-sketch--move-point 0  (scad-sketch--fine   (scad-sketch--assert-session))))
(defun scad-sketch-move-point-fine-down ()    (interactive) (scad-sketch--move-point 0 (- (scad-sketch--fine   (scad-sketch--assert-session)))))
(defun scad-sketch-move-point-coarse-left ()  (interactive) (scad-sketch--move-point (- (scad-sketch--coarse (scad-sketch--assert-session))) 0 t))
(defun scad-sketch-move-point-coarse-right () (interactive) (scad-sketch--move-point    (scad-sketch--coarse (scad-sketch--assert-session))  0 t))
(defun scad-sketch-move-point-coarse-up ()    (interactive) (scad-sketch--move-point 0  (scad-sketch--coarse (scad-sketch--assert-session))    t))
(defun scad-sketch-move-point-coarse-down ()  (interactive) (scad-sketch--move-point 0 (- (scad-sketch--coarse (scad-sketch--assert-session))) t))

;;;; ── Selected vertex movement ───────────────────────────────────────────────

(defun scad-sketch--move-selected (dx dy &optional snap)
  "Move the selected vertex of the focused polygon by (DX DY)."
  (let ((session (scad-sketch--assert-session)))
    (unless (let ((f (scad-sketch--focused-node session)))
              (and f (eq (plist-get f :type) 'polygon)))
      (user-error "No focused polygon to move a vertex in"))
    (scad-sketch--mutate
     (lambda (s)
       (let* ((node    (scad-sketch--focused-node s))
              (pts     (scad-sketch--polygon-points node s))
              (idx     (scad-sketch--poly-selected-index s node))
              (old     (or (nth idx pts) (user-error "No selected vertex")))
              (new-xy  (scad-sketch--move-xy
                        (list (float (nth 0 old)) (float (nth 1 old))) dx dy))
              (snapped (if snap
                           (scad-sketch--snap-xy new-xy (scad-sketch--grid s))
                         new-xy))
              (new-pt  (list (nth 0 snapped) (nth 1 snapped)
                             (float (or (nth 2 old) 0)))))
         (scad-sketch--set-polygon-points
          s node (scad-sketch--replace-nth idx new-pt pts))
         (setf (scad-sketch-session-point s) snapped))))))

(defun scad-sketch-move-selected-left ()         (interactive) (scad-sketch--move-selected (- (scad-sketch--grid   (scad-sketch--assert-session))) 0 t))
(defun scad-sketch-move-selected-right ()        (interactive) (scad-sketch--move-selected    (scad-sketch--grid   (scad-sketch--assert-session))  0 t))
(defun scad-sketch-move-selected-up ()           (interactive) (scad-sketch--move-selected 0  (scad-sketch--grid   (scad-sketch--assert-session))    t))
(defun scad-sketch-move-selected-down ()         (interactive) (scad-sketch--move-selected 0 (- (scad-sketch--grid   (scad-sketch--assert-session))) t))
(defun scad-sketch-move-selected-fine-left ()    (interactive) (scad-sketch--move-selected (- (scad-sketch--fine   (scad-sketch--assert-session))) 0))
(defun scad-sketch-move-selected-fine-right ()   (interactive) (scad-sketch--move-selected    (scad-sketch--fine   (scad-sketch--assert-session))  0))
(defun scad-sketch-move-selected-fine-up ()      (interactive) (scad-sketch--move-selected 0  (scad-sketch--fine   (scad-sketch--assert-session))))
(defun scad-sketch-move-selected-fine-down ()    (interactive) (scad-sketch--move-selected 0 (- (scad-sketch--fine   (scad-sketch--assert-session)))))
(defun scad-sketch-move-selected-coarse-left ()  (interactive) (scad-sketch--move-selected (- (scad-sketch--coarse (scad-sketch--assert-session))) 0 t))
(defun scad-sketch-move-selected-coarse-right () (interactive) (scad-sketch--move-selected    (scad-sketch--coarse (scad-sketch--assert-session))  0 t))
(defun scad-sketch-move-selected-coarse-up ()    (interactive) (scad-sketch--move-selected 0  (scad-sketch--coarse (scad-sketch--assert-session))    t))
(defun scad-sketch-move-selected-coarse-down ()  (interactive) (scad-sketch--move-selected 0 (- (scad-sketch--coarse (scad-sketch--assert-session))) t))

;;;; ── Mark commands ──────────────────────────────────────────────────────────

(defun scad-sketch-set-mark ()
  "Replace the mark stack with the current cursor position."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (setf (scad-sketch-session-marks s)
           (list (copy-sequence (scad-sketch-session-point s)))))))

(defun scad-sketch-push-mark ()
  "Push the current cursor position onto the mark stack."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (push (copy-sequence (scad-sketch-session-point s))
           (scad-sketch-session-marks s)))))

(defun scad-sketch-pop-mark ()
  "Pop the most recent mark and jump the cursor to it."
  (interactive)
  (unless (scad-sketch-session-marks (scad-sketch--assert-session))
    (user-error "No marks set"))
  (scad-sketch--mutate
   (lambda (s)
     (setf (scad-sketch-session-point s)
           (copy-sequence (pop (scad-sketch-session-marks s)))))))

(defun scad-sketch-jump-to-mark ()
  "Move the cursor to the most recent mark (non-destructive)."
  (interactive)
  (unless (scad-sketch-session-marks (scad-sketch--assert-session))
    (user-error "No marks set"))
  (scad-sketch--mutate
   (lambda (s)
     (setf (scad-sketch-session-point s)
           (copy-sequence (car (scad-sketch-session-marks s)))))))

(defun scad-sketch-clear-marks ()
  "Clear all marks and the selection set."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (setf (scad-sketch-session-marks    s) nil)
     (setf (scad-sketch-session-selection s) nil))))

;;;; ── Hover ──────────────────────────────────────────────────────────────────

(defun scad-sketch--compute-hover-stack (session)
  "Return nodes under the cursor, sorted smallest-bbox-first (deepest first).
Only nodes in the currently focused subtree are considered."
  (let* ((cursor (scad-sketch-session-point session))
         (cx     (nth 0 cursor))
         (cy     (nth 1 cursor))
         stack)
    (dolist (node (scad-sketch--focused-tree session))
      (scad-sketch-parse--walk
       node
       (lambda (n)
         (when (scad-sketch--point-in-node-p cx cy n session)
           (push n stack)))))
    (sort stack
          (lambda (a b)
            (< (scad-sketch--node-bbox-area a session)
               (scad-sketch--node-bbox-area b session))))))

;;;; ── Focus ──────────────────────────────────────────────────────────────────

(defun scad-sketch-focus-hovered ()
  "Focus (drill into) the currently hovered shape."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (hover   (car (scad-sketch-session-hover-stack session))))
    (unless hover (user-error "Nothing hovered"))
    (let ((path (scad-sketch--path-of-node session hover)))
      (when path
        (scad-sketch--mutate
         (lambda (s)
           (setf (scad-sketch-session-focused-path  s) path)
           (setf (scad-sketch-session-selection      s) nil)
           (setf (scad-sketch-session-hover-stack    s) nil)))))))

(defun scad-sketch-unfocus ()
  "Pop one level of focus.  No-op when already at the root."
  (interactive)
  (let ((session (scad-sketch--assert-session)))
    (when (scad-sketch-session-focused-path session)
      (scad-sketch--mutate
       (lambda (s)
         (let ((path (scad-sketch-session-focused-path s)))
           (setf (scad-sketch-session-focused-path s)
                 (when (cdr path) (butlast path)))
           (setf (scad-sketch-session-selection   s) nil)
           (setf (scad-sketch-session-hover-stack s) nil)))))))

(defun scad-sketch-toggle-hovered-selection ()
  "Toggle the hovered shape in / out of the selection set."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (hover   (car (scad-sketch-session-hover-stack session))))
    (unless hover (user-error "Nothing hovered"))
    (let ((path (scad-sketch--path-of-node session hover)))
      (when path
        (scad-sketch--mutate
         (lambda (s)
           (setf (scad-sketch-session-selection s)
                 (scad-sketch--sel-toggle
                  (scad-sketch--sel-ref 'shape path)
                  (scad-sketch-session-selection s)))))))))

;;;; ── TAB hover cycling ──────────────────────────────────────────────────────

(defun scad-sketch-tab-next ()
  "Cycle hover forward.
Inside a focused polygon: cycles through its vertices.
At composition level: cycles through child shapes."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (focused (scad-sketch--focused-node session)))
    (if (and focused (eq (plist-get focused :type) 'polygon))
        ;; Vertex cycling
        (scad-sketch--mutate
         (lambda (s)
           (let* ((node (scad-sketch--focused-node s))
                  (pts  (scad-sketch--polygon-points node s))
                  (n    (length pts))
                  (cur  (scad-sketch--poly-selected-index s node))
                  (next (mod (1+ (or cur -1)) n))
                  (pt   (nth next pts)))
             (scad-sketch--poly-set-selected s node next)
             (setf (scad-sketch-session-point s)
                   (list (float (nth 0 pt)) (float (nth 1 pt)))))))
      ;; Shape cycling
      (scad-sketch--mutate
       (lambda (s)
         (let* ((nodes    (scad-sketch--focused-tree s))
                (hovered  (car (scad-sketch-session-hover-stack s)))
                (cur-idx  (when hovered (cl-position hovered nodes :test #'eq)))
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
  "Cycle hover backward (see `scad-sketch-tab-next')."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (focused (scad-sketch--focused-node session)))
    (if (and focused (eq (plist-get focused :type) 'polygon))
        (scad-sketch--mutate
         (lambda (s)
           (let* ((node (scad-sketch--focused-node s))
                  (pts  (scad-sketch--polygon-points node s))
                  (n    (length pts))
                  (cur  (scad-sketch--poly-selected-index s node))
                  (prev (mod (1- (or cur 0)) n))
                  (pt   (nth prev pts)))
             (scad-sketch--poly-set-selected s node prev)
             (setf (scad-sketch-session-point s)
                   (list (float (nth 0 pt)) (float (nth 1 pt)))))))
      (scad-sketch--mutate
       (lambda (s)
         (let* ((nodes     (scad-sketch--focused-tree s))
                (hovered   (car (scad-sketch-session-hover-stack s)))
                (cur-idx   (when hovered (cl-position hovered nodes :test #'eq)))
                (prev-idx  (mod (1- (or cur-idx 0)) (max 1 (length nodes))))
                (prev-node (nth prev-idx nodes)))
           (setf (scad-sketch-session-hover-stack s)
                 (when prev-node (list prev-node)))
           (when prev-node
             (let ((bbox (scad-sketch--node-bbox prev-node s)))
               (setf (scad-sketch-session-point s)
                     (list (/ (+ (nth 0 bbox) (nth 1 bbox)) 2.0)
                           (/ (+ (nth 2 bbox) (nth 3 bbox)) 2.0)))))))))))

;;;; ── Polygon vertex editing ─────────────────────────────────────────────────

(defun scad-sketch--require-focused-polygon ()
  "Return the focused polygon node, or signal `user-error'."
  (let* ((session (scad-sketch--assert-session))
         (focused (scad-sketch--focused-node session)))
    (unless (and focused (eq (plist-get focused :type) 'polygon))
      (user-error "Focus a polygon first (press RET on a polygon shape)"))
    focused))

(defun scad-sketch--replace-nth (n value lst)
  "Return a copy of LST with element N replaced by VALUE."
  (let ((copy (copy-sequence lst)))
    (setf (nth n copy) value)
    copy))

(defun scad-sketch--make-point (xy &optional old-point)
  "Build a [x y r] point from XY, preserving radius from OLD-POINT."
  (list (float (nth 0 xy))
        (float (nth 1 xy))
        (float (or (and old-point (nth 2 old-point)) 0))))

(defun scad-sketch-append-point ()
  "Append the cursor position as a new vertex to the focused polygon."
  (interactive)
  (scad-sketch--require-focused-polygon)
  (scad-sketch--mutate
   (lambda (s)
     (let* ((node    (scad-sketch--focused-node s))
            (pts     (or (scad-sketch--polygon-points node s) '()))
            (new-pt  (scad-sketch--make-point (scad-sketch-session-point s)))
            (new-pts (append pts (list new-pt))))
       (scad-sketch--set-polygon-points s node new-pts)
       (scad-sketch--poly-set-selected s node (1- (length new-pts)))))))

(defun scad-sketch-insert-point-after-selected ()
  "Insert marks (oldest first) then cursor after the selected vertex."
  (interactive)
  (scad-sketch--require-focused-polygon)
  (scad-sketch--mutate
   (lambda (s)
     (let* ((node       (scad-sketch--focused-node s))
            (pts        (or (scad-sketch--polygon-points node s) '()))
            (idx        (or (scad-sketch--poly-selected-index s node) -1))
            (insert-at  (min (1+ idx) (length pts)))
            (mark-pts   (mapcar #'scad-sketch--make-point
                                (reverse (scad-sketch-session-marks s))))
            (cursor-pt  (scad-sketch--make-point (scad-sketch-session-point s)))
            (new-pts    (append mark-pts (list cursor-pt)))
            (new-idx    (+ insert-at (length new-pts) -1)))
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
     (let* ((node    (scad-sketch--focused-node s))
            (pts     (or (scad-sketch--polygon-points node s) '()))
            (idx     (or (scad-sketch--poly-selected-index s node)
                         (user-error "No selected vertex"))))
       (unless (< idx (length pts)) (user-error "Vertex out of range"))
       (let ((new-pts (append (cl-subseq pts 0 idx) (nthcdr (1+ idx) pts))))
         (scad-sketch--set-polygon-points s node new-pts)
         (scad-sketch--poly-set-selected s node
           (cond ((null new-pts)             0)
                 ((>= idx (length new-pts))  (1- (length new-pts)))
                 (t                          idx))))))))

(defun scad-sketch-line-from-mark ()
  "Append marks (oldest first) then the cursor as new vertices."
  (interactive)
  (scad-sketch--require-focused-polygon)
  (scad-sketch--mutate
   (lambda (s)
     (unless (scad-sketch-session-marks s) (user-error "No marks set"))
     (let* ((node    (scad-sketch--focused-node s))
            (pts     (or (scad-sketch--polygon-points node s) '()))
            (new-pts (append pts
                             (mapcar #'scad-sketch--make-point
                                     (reverse (scad-sketch-session-marks s)))
                             (list (scad-sketch--make-point
                                    (scad-sketch-session-point s))))))
       (scad-sketch--set-polygon-points s node new-pts)
       (scad-sketch--poly-set-selected s node (1- (length new-pts)))))))

(defun scad-sketch-rectangle-from-mark ()
  "Append rectangle corners from the most recent mark to the cursor."
  (interactive)
  (scad-sketch--require-focused-polygon)
  (scad-sketch--mutate
   (lambda (s)
     (let* ((mark    (or (car (scad-sketch-session-marks s)) (user-error "No marks set")))
            (pt      (scad-sketch-session-point s))
            (node    (scad-sketch--focused-node s))
            (pts     (or (scad-sketch--polygon-points node s) '()))
            (x1 (nth 0 mark)) (y1 (nth 1 mark))
            (x2 (nth 0 pt))   (y2 (nth 1 pt))
            (corners (list (list x1 y1) (list x2 y1) (list x2 y2) (list x1 y2)))
            (new-pts (append pts (mapcar #'scad-sketch--make-point corners))))
       (scad-sketch--set-polygon-points s node new-pts)
       (scad-sketch--poly-set-selected s node (1- (length new-pts)))))))

(defun scad-sketch-toggle-closed ()
  "Toggle the closed / open state of the focused polygon."
  (interactive)
  (scad-sketch--require-focused-polygon)
  (scad-sketch--mutate
   (lambda (s)
     (let ((node (scad-sketch--focused-node s)))
       (scad-sketch--poly-set-closed
        s node (not (scad-sketch--poly-closed s node)))))))

(defun scad-sketch-set-radius (radius)
  "Set the polyRound radius on the currently selected vertex."
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

;;;; ── Coordinate commands ────────────────────────────────────────────────────

(defun scad-sketch--set-point-axis (axis value)
  (scad-sketch--mutate
   (lambda (s)
     (let ((pt (copy-sequence (scad-sketch-session-point s))))
       (setf (nth axis pt) (float value))
       (setf (scad-sketch-session-point s) pt)))))

(defun scad-sketch-set-x (x)
  "Set the cursor X coordinate."
  (interactive (list (read-number "X: " (nth 0 (scad-sketch-session-point
                                                 (scad-sketch--assert-session))))))
  (scad-sketch--set-point-axis 0 x))

(defun scad-sketch-set-y (y)
  "Set the cursor Y coordinate."
  (interactive (list (read-number "Y: " (nth 1 (scad-sketch-session-point
                                                 (scad-sketch--assert-session))))))
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
  "Set distance from mark to cursor, preserving the current angle."
  (interactive (list (read-number "Distance from mark: " 0)))
  (unless (scad-sketch-session-marks (scad-sketch--assert-session))
    (user-error "No marks set"))
  (scad-sketch--mutate
   (lambda (s)
     (let* ((m     (car (scad-sketch-session-marks s)))
            (p     (scad-sketch-session-point s))
            (angle (atan (- (nth 1 p) (nth 1 m)) (- (nth 0 p) (nth 0 m)))))
       (setf (scad-sketch-session-point s)
             (list (+ (nth 0 m) (* (float distance) (cos angle)))
                   (+ (nth 1 m) (* (float distance) (sin angle)))))))))

(defun scad-sketch-set-angle-from-mark (degrees)
  "Set angle from mark to cursor in DEGREES, preserving the distance."
  (interactive (list (read-number "Angle degrees from mark: " 0)))
  (unless (scad-sketch-session-marks (scad-sketch--assert-session))
    (user-error "No marks set"))
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
  (interactive (list (read-number "Grid step: "
                                  (scad-sketch-session-grid
                                   (scad-sketch--assert-session)))))
  (scad-sketch--mutate
   (lambda (s) (setf (scad-sketch-session-grid s) (float grid)))))

;;;; ── Undo command ───────────────────────────────────────────────────────────

(defun scad-sketch-undo ()
  "Undo the last sketch edit."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (entry   (pop (scad-sketch-session-undo-stack session))))
    (unless entry (user-error "No sketch undo available"))
    (setf (scad-sketch-session-tree         session) (plist-get entry :tree))
    (setf (scad-sketch-session-point        session) (plist-get entry :point))
    (setf (scad-sketch-session-marks        session) (plist-get entry :marks))
    (setf (scad-sketch-session-focused-path session) (plist-get entry :focused-path))
    (setf (scad-sketch-session-selection    session) (plist-get entry :selection))
    (let ((states (plist-get entry :poly-states)))
      (when states
        (let ((tbl (make-hash-table :test 'eq)))
          (dolist (pair states) (puthash (car pair) (cdr pair) tbl))
          (setf (scad-sketch-session-named-marks session) tbl))))
    (setf (scad-sketch-session-dirty session) t)
    (scad-sketch--render)))

;;;; ── Rendering ──────────────────────────────────────────────────────────────

(defun scad-sketch--all-points (session)
  "Collect all visible model-space [x y] points for auto-zoom bounds."
  (let (pts)
    (dolist (node (scad-sketch-session-tree session))
      (scad-sketch-parse--walk
       node
       (lambda (n)
         (let ((type (plist-get n :type)))
           (cond
            ((memq type '(polygon array))
             (dolist (p (or (scad-sketch--polygon-points n session) '()))
               (push (list (nth 0 p) (nth 1 p)) pts)))
            ((eq type 'circle)
             (let ((cx (plist-get n :cx)) (cy (plist-get n :cy)) (r (plist-get n :r)))
               (push (list (- cx r) (- cy r)) pts)
               (push (list (+ cx r) (+ cy r)) pts)))
            ((eq type 'square)
             (push (list (plist-get n :x) (plist-get n :y)) pts)
             (push (list (+ (plist-get n :x) (plist-get n :w))
                         (+ (plist-get n :y) (plist-get n :h))) pts)))))))
    pts))

(defun scad-sketch--bounds (session)
  "Return (min-x max-x min-y max-y) covering all content, cursor, and marks."
  (let* ((geom  (scad-sketch--all-points session))
         (extra (delq nil (cons (scad-sketch-session-point session)
                                (scad-sketch-session-marks session))))
         (all   (append geom extra)))
    (if (null all)
        (list -10 10 -10 10)
      (let ((min-x (apply #'min (mapcar #'car  all)))
            (max-x (apply #'max (mapcar #'car  all)))
            (min-y (apply #'min (mapcar #'cadr all)))
            (max-y (apply #'max (mapcar #'cadr all))))
        (when (= min-x max-x) (setq min-x (- min-x 10) max-x (+ max-x 10)))
        (when (= min-y max-y) (setq min-y (- min-y 10) max-y (+ max-y 10)))
        (let ((px (max 1 (* 0.15 (- max-x min-x))))
              (py (max 1 (* 0.15 (- max-y min-y)))))
          (list (- min-x px) (+ max-x px) (- min-y py) (+ max-y py)))))))

(defun scad-sketch--make-transform (bounds)
  "Return a model→pixel transform closure for BOUNDS (min-x max-x min-y max-y)."
  (let ((min-x (nth 0 bounds)) (max-x (nth 1 bounds))
        (min-y (nth 2 bounds)) (max-y (nth 3 bounds)))
    (let* ((w     scad-sketch-canvas-width)
           (h     scad-sketch-canvas-height)
           (m     scad-sketch-margin)
           (scale (min (/ (- w (* 2 m)) (- max-x min-x))
                       (/ (- h (* 2 m)) (- max-y min-y)))))
      (lambda (xy)
        (list (+ m (* (- (nth 0 xy) min-x) scale))
              (- h (+ m (* (- (nth 1 xy) min-y) scale))))))))

(defun scad-sketch--svg-line (svg tf a b &rest args)
  "Draw a line from model-point A to B using transform TF."
  (let ((pa (funcall tf a)) (pb (funcall tf b)))
    (apply #'svg-line svg
           (nth 0 pa) (nth 1 pa) (nth 0 pb) (nth 1 pb)
           args)))

;;; Grid

(defun scad-sketch--draw-grid (svg bounds tf session)
  "Render the background grid lines onto SVG."
  (let ((min-x (nth 0 bounds)) (max-x (nth 1 bounds))
        (min-y (nth 2 bounds)) (max-y (nth 3 bounds)))
    (let* ((g (max 0.0001 (scad-sketch-session-grid session)))
           (x (* g (floor   (/ min-x g))))
           (y (* g (floor   (/ min-y g)))))
      (while (<= x (* g (ceiling (/ max-x g))))
        (scad-sketch--svg-line svg tf (list x min-y) (list x max-y)
                               :stroke "#e8e8e8" :stroke-width 1)
        (setq x (+ x g)))
      (while (<= y (* g (ceiling (/ max-y g))))
        (scad-sketch--svg-line svg tf (list min-x y) (list max-x y)
                               :stroke "#e8e8e8" :stroke-width 1)
        (setq y (+ y g)))
      ;; Heavier axis lines
      (when (and (<= min-x 0) (<= 0 max-x))
        (scad-sketch--svg-line svg tf (list 0 min-y) (list 0 max-y)
                               :stroke "#d0d0d0" :stroke-width 2))
      (when (and (<= min-y 0) (<= 0 max-y))
        (scad-sketch--svg-line svg tf (list min-x 0) (list max-x 0)
                               :stroke "#d0d0d0" :stroke-width 2)))))

;;; Node visual state

(defun scad-sketch--node-visual-state (node session)
  "Return :focused, :hovered, :selected, :context, or :normal for NODE."
  (let* ((focused-path (scad-sketch-session-focused-path session))
         (node-path    (scad-sketch--path-of-node session node))
         (hover-stack  (scad-sketch-session-hover-stack session))
         (selection    (scad-sketch-session-selection session))
         (fp-len       (length focused-path))
         (np-len       (length node-path))
         (shared       (min fp-len np-len)))
    (cond
     ((and focused-path node-path
           (equal (cl-subseq node-path 0 shared)
                  (cl-subseq focused-path 0 shared)))
      :focused)
     ((and focused-path node-path)
      :context)
     ((memq node hover-stack)
      :hovered)
     ((and node-path
           (scad-sketch--sel-member
            (scad-sketch--sel-ref 'shape node-path) selection))
      :selected)
     (t :normal))))

(defun scad-sketch--state-colors (state)
  "Return (stroke fill stroke-width) for a visual STATE keyword."
  (cond
   ((eq state :focused)  '("#111111" "none" 3))
   ((eq state :hovered)  '("#0057c2" "none" 2))
   ((eq state :selected) '("#d13f00" "none" 2))
   ((eq state :context)  '("#cccccc" "none" 1))
   (t                    '("#555555" "none" 1))))

;;; Node drawing

(defun scad-sketch--draw-node (svg tf session node)
  "Render a single AST NODE onto SVG using transform TF."
  (let* ((state  (scad-sketch--node-visual-state node session))
         (colors (scad-sketch--state-colors state))
         (stroke (nth 0 colors))
         (sw     (nth 2 colors))
         (type   (plist-get node :type)))
    (cond
     ;; ── Polygon / array ──
     ((memq type '(polygon array))
      (let* ((pts        (or (scad-sketch--polygon-points node session) '()))
             (closed     (if (eq type 'array)
                             t
                           (scad-sketch--poly-closed session node)))
             (is-focused (eq node (scad-sketch--focused-node session))))
        (when (>= (length pts) 2)
          (if (scad-sketch-geo--any-radius-p pts)
              (let ((d (scad-sketch-geo--polyround-path-d pts closed tf)))
                (when d
                  (svg-node svg 'path
                            :d d :stroke stroke :stroke-width sw :fill "none")))
            (let ((xys (mapcar (lambda (p)
                                 (list (float (nth 0 p)) (float (nth 1 p))))
                               pts)))
              (cl-loop for a on xys for b = (cadr a) when b do
                       (scad-sketch--svg-line svg tf (car a) b
                                             :stroke stroke :stroke-width sw))
              (when (and closed (> (length xys) 2))
                (scad-sketch--svg-line svg tf (car (last xys)) (car xys)
                                      :stroke stroke :stroke-width sw)))))
        ;; Vertex dots when this polygon is focused
        (when is-focused
          (let ((sel-idx (scad-sketch--poly-selected-index session node))
                (n       (length pts)))
            (dotimes (i n)
              (let* ((p      (nth i pts))
                     (xy     (list (float (nth 0 p)) (float (nth 1 p))))
                     (screen (funcall tf xy))
                     (sel    (= i (or sel-idx -1)))
                     (r-val  (or (nth 2 p) 0)))
                (svg-circle svg (nth 0 screen) (nth 1 screen) (if sel 7 5)
                            :stroke     (if sel "#d13f00" "#111111")
                            :stroke-width (if sel 3 2)
                            :fill       (if sel "#fff0e8" "#ffffff"))
                (svg-text svg (number-to-string i)
                          :x (+ (nth 0 screen) 8) :y (- (nth 1 screen) 8)
                          :font-size 12 :fill "#333333")
                (when (> r-val 0)
                  (let* ((prev   (cond ((> i 0)      (nth (1- i) pts))
                                       (closed        (nth (1- n) pts))))
                         (next   (cond ((< i (1- n)) (nth (1+ i) pts))
                                       (closed        (nth 0 pts))))
                         (corner (when (and prev next)
                                   (scad-sketch-geo--corner-geometry
                                    (list (float (nth 0 prev)) (float (nth 1 prev)))
                                    xy
                                    (list (float (nth 0 next)) (float (nth 1 next)))
                                    r-val)))
                         (act-r  (if corner (plist-get corner :radius) r-val))
                         (capped (and corner (< (+ act-r 0.001) r-val))))
                    (svg-circle svg (nth 0 screen) (nth 1 screen)
                                (scad-sketch-geo--pixel-radius act-r tf)
                                :stroke         (if capped "#c04000" "#804000")
                                :stroke-width   1
                                :stroke-dasharray "3,3"
                                :fill           "none")
                    (svg-text svg
                              (if capped
                                  (format "r=%s→%s"
                                          (scad-sketch-geo--fmt-num r-val)
                                          (scad-sketch-geo--fmt-num act-r))
                                (format "r=%s" (scad-sketch-geo--fmt-num r-val)))
                              :x (+ (nth 0 screen) 8) :y (+ (nth 1 screen) 18)
                              :font-size 11
                              :fill (if capped "#c04000" "#804000"))))))))
        ;; Shape label when not focused
        (when (not is-focused)
          (let* ((bbox   (scad-sketch--node-bbox node session))
                 (cx     (/ (+ (nth 0 bbox) (nth 1 bbox)) 2.0))
                 (cy     (/ (+ (nth 2 bbox) (nth 3 bbox)) 2.0))
                 (screen (funcall tf (list cx cy))))
            (svg-text svg "poly"
                      :x (nth 0 screen) :y (nth 1 screen)
                      :font-size 10 :fill stroke :text-anchor "middle")))))

     ;; ── Circle ──
     ((eq type 'circle)
      (let* ((cx (plist-get node :cx))
             (cy (plist-get node :cy))
             (r  (plist-get node :r))
             (sc (funcall tf (list cx cy)))
             (pr (scad-sketch-geo--pixel-radius r tf)))
        (svg-circle svg (nth 0 sc) (nth 1 sc) pr
                    :stroke stroke :stroke-width sw :fill "none")
        (svg-circle svg (nth 0 sc) (nth 1 sc) 3
                    :stroke stroke :stroke-width 1 :fill stroke)
        (svg-text svg (format "r=%s" (scad-sketch-geo--fmt-num r))
                  :x (+ (nth 0 sc) (+ pr 4)) :y (nth 1 sc)
                  :font-size 10 :fill stroke)))

     ;; ── Square ──
     ((eq type 'square)
      (let* ((x (plist-get node :x)) (y (plist-get node :y))
             (w (plist-get node :w)) (h (plist-get node :h))
             (corners (list (list x y) (list (+ x w) y)
                            (list (+ x w) (+ y h)) (list x (+ y h))))
             (xys (mapcar tf corners)))
        (cl-loop for a on xys for b = (cadr a) when b do
                 (apply #'svg-line svg
                        (nth 0 (car a)) (nth 1 (car a))
                        (nth 0 b) (nth 1 b)
                        (list :stroke stroke :stroke-width sw)))
        (apply #'svg-line svg
               (nth 0 (car (last xys))) (nth 1 (car (last xys)))
               (nth 0 (car xys))        (nth 1 (car xys))
               (list :stroke stroke :stroke-width sw))))

     ;; ── Text ──
     ((eq type 'text)
      (let* ((sc (funcall tf (list (plist-get node :x) (plist-get node :y))))
             (sz (scad-sketch-geo--pixel-radius (plist-get node :size) tf)))
        (svg-text svg (plist-get node :str)
                  :x (nth 0 sc) :y (nth 1 sc)
                  :font-size sz :fill stroke)))

     ;; ── Compositions — label at centroid; children rendered separately ──
     ((memq type '(difference union intersection))
      (let* ((bbox   (scad-sketch--node-bbox node session))
             (cx     (/ (+ (nth 0 bbox) (nth 1 bbox)) 2.0))
             (cy     (/ (+ (nth 2 bbox) (nth 3 bbox)) 2.0))
             (screen (funcall tf (list cx cy))))
        (svg-text svg (symbol-name type)
                  :x (nth 0 screen) :y (- (nth 1 screen) 8)
                  :font-size 10 :fill stroke :text-anchor "middle")))

     ;; Transforms have no own geometry; children are rendered by the tree walk.
     ((memq type '(translate rotate scale mirror)) nil)
     (t nil))))

(defun scad-sketch--draw-tree (svg tf session)
  "Walk the full node tree and render every node."
  (dolist (node (scad-sketch-session-tree session))
    (scad-sketch-parse--walk
     node
     (lambda (n) (scad-sketch--draw-node svg tf session n)))))

;;; Cursor and marks

(defun scad-sketch--draw-cursor-and-marks (svg tf session)
  "Render cursor crosshair and mark chain onto SVG."
  (let* ((marks  (scad-sketch-session-marks session))
         (cursor (scad-sketch-session-point session)))
    ;; Mark chain (dashed lines connecting marks to cursor)
    (let ((ordered (reverse marks)))
      (cl-loop for a on ordered for b = (cadr a) when b do
               (scad-sketch--svg-line svg tf (car a) b
                                     :stroke "#008a2e" :stroke-width 1
                                     :stroke-dasharray "4,4"))
      (when ordered
        (scad-sketch--svg-line svg tf (car (last ordered)) cursor
                               :stroke "#008a2e" :stroke-width 1
                               :stroke-dasharray "4,4")))
    ;; Mark dots
    (dolist (m (reverse marks))
      (let* ((s   (funcall tf m))
             (cur (equal m (car marks)))
             (col (if cur "#008a2e" "#50a870")))
        (svg-circle svg (nth 0 s) (nth 1 s) 6
                    :stroke col :stroke-width 2 :fill "#e2ffe9")
        (when cur
          (svg-text svg "mark"
                    :x (+ (nth 0 s) 10) :y (+ (nth 1 s) 4)
                    :font-size 12 :fill col))))
    ;; Cursor crosshair
    (let ((p (funcall tf cursor)))
      (svg-circle svg (nth 0 p) (nth 1 p) 5
                  :stroke "#0057c2" :stroke-width 2 :fill "#dfefff")
      (svg-line svg (- (nth 0 p) 10) (nth 1 p) (+ (nth 0 p) 10) (nth 1 p)
                :stroke "#0057c2" :stroke-width 2)
      (svg-line svg (nth 0 p) (- (nth 1 p) 10) (nth 0 p) (+ (nth 1 p) 10)
                :stroke "#0057c2" :stroke-width 2)
      (svg-text svg "point"
                :x (+ (nth 0 p) 12) :y (+ (nth 1 p) 4)
                :font-size 12 :fill "#0057c2"))))

;;; HUD status bar

(defun scad-sketch--draw-hud (svg session)
  "Render the status bar at the top of the SVG canvas."
  (let* ((marks    (scad-sketch-session-marks session))
         (focused  (scad-sketch--focused-node session))
         (hover    (car (scad-sketch-session-hover-stack session)))
         (mark-str (cond
                    ((null marks)
                     "none")
                    ((= 1 (length marks))
                     (scad-sketch-geo--fmt-xy (car marks)))
                    (t
                     (format "%s (+%d)"
                             (scad-sketch-geo--fmt-xy (car marks))
                             (1- (length marks))))))
         (text (format
                "%s  grid=%s%s  point=%s  mark=%s  focus=%s  hover=%s  %s"
                (or (when focused (plist-get focused :name))
                    (format "%d nodes"
                            (length (scad-sketch-session-tree session))))
                (scad-sketch-geo--fmt-num (scad-sketch-session-grid session))
                (scad-sketch-session-units session)
                (scad-sketch-geo--fmt-xy  (scad-sketch-session-point session))
                mark-str
                (if focused (symbol-name (plist-get focused :type)) "none")
                (if hover   (symbol-name (plist-get hover   :type)) "none")
                (if (scad-sketch-session-dirty session) "*dirty*" "saved"))))
    (svg-rectangle svg 0 0 scad-sketch-canvas-width 28 :fill "#f8f8f8")
    (svg-text svg text :x 10 :y 19 :font-size 13 :fill "#111111")))

;;; Main render entry point

(defun scad-sketch--render ()
  "Re-render the editor buffer: SVG canvas + live source preview."
  (let* ((session (scad-sketch--assert-session))
         (bounds  (scad-sketch--bounds session))
         (tf      (scad-sketch--make-transform bounds))
         (svg     (svg-create scad-sketch-canvas-width scad-sketch-canvas-height)))
    ;; Recompute hover stack before drawing
    (setf (scad-sketch-session-hover-stack session)
          (scad-sketch--compute-hover-stack session))
    (svg-rectangle svg 0 0 scad-sketch-canvas-width scad-sketch-canvas-height
                   :fill "#ffffff")
    (scad-sketch--draw-grid           svg bounds tf session)
    (scad-sketch--draw-tree           svg        tf session)
    (scad-sketch--draw-cursor-and-marks svg      tf session)
    (scad-sketch--draw-hud            svg           session)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((beg (point)))
        (insert-image (svg-image svg :ascent 'center))
        (remove-text-properties beg (point) '(keymap nil)))
      (insert "\n\n")
      (insert (scad-sketch-unparse-top-level
               (scad-sketch-session-tree session)))
      (goto-char (point-min)))))

;;;; ── Write-back ─────────────────────────────────────────────────────────────

(defun scad-sketch--sync-source-text (session)
  "Refresh session source-text from the live source buffer."
  (when (buffer-live-p (scad-sketch-session-source-buffer session))
    (with-current-buffer (scad-sketch-session-source-buffer session)
      (setf (scad-sketch-session-source-text session)
            (buffer-substring-no-properties (point-min) (point-max))))))

(defun scad-sketch-write-back ()
  "Write the edited AST back to the source buffer region and re-render."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (source  (scad-sketch-session-source-buffer session))
         (beg     (scad-sketch-session-content-beg session))
         (end     (scad-sketch-session-content-end session))
         (content (scad-sketch-unparse-top-level
                   (scad-sketch-session-tree session))))
    (unless (buffer-live-p source)
      (user-error "Source buffer is gone"))
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

;;;; ── Session / window commands ──────────────────────────────────────────────

(defun scad-sketch-quit ()
  "Quit the editor, restoring the previous window configuration.
Prompts to write back if there are unsaved edits."
  (interactive)
  (let ((session (scad-sketch--assert-session))
        (wconf   scad-sketch--window-config))
    (when (and (scad-sketch-session-dirty session)
               (y-or-n-p "Sketch has unwritten edits. Write back first? "))
      (scad-sketch-write-back))
    (kill-buffer (current-buffer))
    (when wconf (set-window-configuration wconf))))

(defun scad-sketch-help ()
  "Display a brief key-binding summary in the echo area."
  (interactive)
  (scad-sketch--assert-session)
  (message
   (concat "arrows=move  C-arrows=coarse  M-arrows=fine  S-arrows=move-vertex | "
           "TAB/S-TAB=cycle-hover  RET=focus  ESC=unfocus  SPC=select | "
           "p=append  i=insert  k=delete  R=radius  c=closed | "
           "m/M/`/'/C=marks  x/y/X/Y/d/a=coords  g=grid | "
           "w=write  u=undo  q=quit  C-h m=full help")))

(provide 'scad-sketch-editor-mode)
;;; scad-sketch-editor-mode.el ends here
