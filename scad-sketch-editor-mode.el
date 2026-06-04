;;; scad-sketch-editor-mode.el --- Major mode for editing scad-sketch sessions -*- lexical-binding: t; -*-

;;; Commentary:

;; Visual editor major mode for an already-established `scad-sketch-session'.
;; This file owns navigation/edit commands, SVG rendering, undo, write-back,
;; and editor buffer/window lifecycle.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'svg)
(require 'scad-sketch-session)
(require 'scad-sketch-geometry)

(defcustom scad-sketch-canvas-width 900
  "Sketch editor canvas width in pixels."
  :type 'integer :group 'scad-sketch)

(defcustom scad-sketch-canvas-height 650
  "Sketch editor canvas height in pixels."
  :type 'integer :group 'scad-sketch)

(defcustom scad-sketch-margin 48
  "Canvas margin in pixels."
  :type 'integer :group 'scad-sketch)

(defvar-local scad-sketch--window-config nil
  "Window configuration recorded just before the editor buffer was opened.")
(defvar scad-sketch--editor-buffer-prefix "*scad-sketch: ")

(defvar scad-sketch-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    ;; Cursor movement
    (define-key map (kbd "<left>")    #'scad-sketch-move-point-left)
    (define-key map (kbd "<right>")   #'scad-sketch-move-point-right)
    (define-key map (kbd "<up>")      #'scad-sketch-move-point-up)
    (define-key map (kbd "<down>")    #'scad-sketch-move-point-down)
    (define-key map (kbd "M-<left>")  #'scad-sketch-move-point-fine-left)
    (define-key map (kbd "M-<right>") #'scad-sketch-move-point-fine-right)
    (define-key map (kbd "M-<up>")    #'scad-sketch-move-point-fine-up)
    (define-key map (kbd "M-<down>")  #'scad-sketch-move-point-fine-down)
    (define-key map (kbd "C-<left>")  #'scad-sketch-move-point-coarse-left)
    (define-key map (kbd "C-<right>") #'scad-sketch-move-point-coarse-right)
    (define-key map (kbd "C-<up>")    #'scad-sketch-move-point-coarse-up)
    (define-key map (kbd "C-<down>")  #'scad-sketch-move-point-coarse-down)
    ;; Selected vertex movement
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
    (define-key map (kbd "m") #'scad-sketch-set-mark)
    (define-key map (kbd "M") #'scad-sketch-push-mark)
    (define-key map (kbd "`") #'scad-sketch-pop-mark)
    (define-key map (kbd "'") #'scad-sketch-jump-to-mark)
    (define-key map (kbd "C") #'scad-sketch-clear-marks)
    ;; Editing
    (define-key map (kbd "p")         #'scad-sketch-append-point)
    (define-key map (kbd "i")         #'scad-sketch-insert-point-after-selected)
    (define-key map (kbd "k")         #'scad-sketch-delete-selected)
    (define-key map (kbd "l")         #'scad-sketch-line-from-mark)
    (define-key map (kbd "r")         #'scad-sketch-rectangle-from-mark)
    (define-key map (kbd "c")         #'scad-sketch-toggle-closed)
    (define-key map (kbd "R")         #'scad-sketch-set-radius)
    (define-key map (kbd "TAB")       #'scad-sketch-next-selectable)
    (define-key map (kbd "<backtab>") #'scad-sketch-previous-selectable)
    (define-key map (kbd ".")         #'scad-sketch-next-hovered)
    (define-key map (kbd ",")         #'scad-sketch-previous-hovered)
    (define-key map (kbd "SPC")       #'scad-sketch-toggle-attention-selection)
    (define-key map (kbd "s")         #'scad-sketch-clear-selection)
    (define-key map (kbd "x")         #'scad-sketch-set-x)
    (define-key map (kbd "y")         #'scad-sketch-set-y)
    (define-key map (kbd "X")         #'scad-sketch-set-delta-x)
    (define-key map (kbd "Y")         #'scad-sketch-set-delta-y)
    (define-key map (kbd "d")         #'scad-sketch-set-distance-from-mark)
    (define-key map (kbd "a")         #'scad-sketch-set-angle-from-mark)
    (define-key map (kbd "g")         #'scad-sketch-set-grid)
    (define-key map (kbd "u")         #'scad-sketch-undo)
    (define-key map (kbd "w")         #'scad-sketch-write-back)
    (define-key map (kbd "q")         #'scad-sketch-quit)
    (define-key map (kbd "?")         #'scad-sketch-help)
    map)
  "Keymap for `scad-sketch-editor-mode'.")

(define-derived-mode scad-sketch-editor-mode special-mode "SCAD-Sketch"
  "Major mode for the scad-sketch visual editor.

The buffer shows an SVG canvas followed by a live OpenSCAD array preview.

The canvas displays:
  - a grid (step set with `g')
  - the polygon path with arcs where polyRound radii are set
  - vertex dots numbered from 0; the selected vertex is highlighted in orange
  - dashed radius circles on rounded vertices (orange = capped by edge length)
  - the cursor crosshair in blue, marks in green
  - a status bar: name, grid size, cursor coords, dirty flag

Two-column and three-column arrays are handled identically.  The rounding
radius defaults to 0.  On write-back, if every radius is 0 the array is
emitted as two-column; if any radius is non-zero it is emitted as three-column.

Movement:
  <arrow>             move cursor one grid step; snaps to grid
  C-<arrow>           move cursor one coarse step; snaps to grid
  M-<arrow>           move cursor one fine step; intentionally off-grid
  S-<arrow>           move selected vertex one grid step
  M-S-<arrow>         move selected vertex one fine step (off-grid)
  C-S-<arrow>         move selected vertex one coarse step

Vertex editing:
  TAB / S-TAB         select next / previous vertex (cursor jumps to it)
  p                   append cursor as a new vertex at end of array
  i                   insert cursor after selected vertex; if marks are set,
                        inserts each mark (oldest first) then cursor
  k                   delete the selected vertex

Marks:
  m                   replace all marks with cursor position
  M                   push cursor position onto mark stack
  `                   pop most recent mark and jump cursor there
  \'                   jump cursor to most recent mark (non-destructive)
  C                   clear all marks

Geometry:
  x / y               set cursor X or Y coordinate
  X / Y               set cursor X or Y relative to most recent mark (delta)
  d                   set distance from mark, preserving angle
  a                   set angle from mark in degrees, preserving distance
  R                   set polyRound radius on selected vertex
  c                   toggle closed / open polygon
  l                   append marks (oldest first) then cursor as vertices
  r                   append rectangle from most recent mark to cursor
  g                   change grid step

Session:
  w                   write array back to source buffer
  u                   undo
  q                   quit (offers to write back if dirty)
  ?                   key summary in the echo area

\\{scad-sketch-editor-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t))


;;; Opening the editor

(defun scad-sketch--open-session (session)
  "Open an editor buffer for SESSION, saving the current window configuration."
  (let ((wconf (current-window-configuration))
        (buf   (get-buffer-create
                (format "%s%s*" scad-sketch--editor-buffer-prefix
                        (scad-sketch-session-name session)))))
    (with-current-buffer buf
      (scad-sketch-editor-mode)
      (setq-local scad-sketch--session session)
      (setq-local scad-sketch--window-config wconf)
      (scad-sketch--render))
    (pop-to-buffer buf)))

;;; Undo
(defun scad-sketch--push-undo (session)
  "Push SESSION state onto the undo stack."
  (scad-sketch-session-sync-active-shape-from-points session)
  (push (list :points          (copy-tree (scad-sketch-session-points session))
              :point           (copy-tree (scad-sketch-session-point session))
              :marks           (copy-tree (scad-sketch-session-marks session))
              :named-marks     (copy-tree (scad-sketch-session-named-marks session))
              :selected-index  (scad-sketch-session-selected-index session)
              :closed          (scad-sketch-session-closed session)
              :shapes          (copy-tree (scad-sketch-session-shapes session))
              :active-shape-id (scad-sketch-session-active-shape-id session)
              :targets         (copy-tree (scad-sketch-session-targets session))
              :root-target-id  (scad-sketch-session-root-target-id session)
              :selection       (copy-tree (scad-sketch-session-selection session))
              :focus-ref       (copy-tree (scad-sketch-session-focus-ref session)))
        (scad-sketch-session-undo-stack session)))

(defun scad-sketch--mark-dirty (session)
  "Mark SESSION as having unsaved edits."
  (setf (scad-sketch-session-dirty session) t))

(defun scad-sketch--change (fn &optional source-mutation-p)
  "Call FN with session, then re-render.

When SOURCE-MUTATION-P is non-nil, push undo and mark the session dirty.
Cursor movement, mark movement, hover, focus, and selection are clean changes;
editing source geometry is dirty."
  (let ((session (scad-sketch--assert-session)))
    (when source-mutation-p
      (scad-sketch--push-undo session))
    (funcall fn session)
    (scad-sketch--normalize-attention session)
    (when source-mutation-p
      (scad-sketch--mark-dirty session))
    (scad-sketch--render)))

(defun scad-sketch--edit (fn)
  "Apply FN as a source-geometry mutation."
  (scad-sketch--change fn t))

(defun scad-sketch--clean-change (fn)
  "Apply FN as a clean UI/session change."
  (scad-sketch--change fn nil))

;; Backwards-compatible name.  From here on, use `scad-sketch--edit' for
;; mutations that should dirty the source.
(defun scad-sketch--mutate (fn)
  "Apply FN as a source-geometry mutation."
  (scad-sketch--edit fn))

;;; Point selection

(defun scad-sketch--selected-point (session)
  "Return the currently selected model point, or nil."
  (let ((idx (scad-sketch-session-selected-index session)))
    (when (and idx (>= idx 0)
               (< idx (length (scad-sketch-session-points session))))
      (nth idx (scad-sketch-session-points session)))))

(defun scad-sketch--set-selected-point (session point)
  "Replace the selected model point in SESSION with POINT."
  (let ((idx (scad-sketch-session-selected-index session)))
    (unless (and idx (>= idx 0)
                 (< idx (length (scad-sketch-session-points session))))
      (user-error "No selected point"))
    (setf (scad-sketch-session-points session)
          (scad-sketch--replace-nth idx point
                                    (scad-sketch-session-points session)))))

;;;; Selection / hover / attention

(defcustom scad-sketch-hover-radius-factor 0.75
  "Hover radius as a multiple of the current grid step."
  :type 'number :group 'scad-sketch)

(defun scad-sketch--shape-ref (&optional shape-id)
  "Return a shape selection ref for SHAPE-ID."
  (list :kind 'shape
        :shape-id (or shape-id
                      (scad-sketch-session-active-shape-id
                       (scad-sketch--assert-session)))))

(defun scad-sketch--point-ref (idx &optional shape-id)
  "Return a point selection ref for IDX in SHAPE-ID."
  (list :kind 'point
        :shape-id (or shape-id
                      (scad-sketch-session-active-shape-id
                       (scad-sketch--assert-session)))
        :index idx))

(defun scad-sketch--ref-kind (ref)
  "Return REF kind."
  (plist-get ref :kind))

(defun scad-sketch--ref-index (ref)
  "Return point index from REF."
  (plist-get ref :index))

(defun scad-sketch--ref-shape-id (ref)
  "Return shape id from REF."
  (plist-get ref :shape-id))

(defun scad-sketch--same-ref-p (a b)
  "Return non-nil if selection refs A and B describe the same object."
  (and a b
       (eq (scad-sketch--ref-kind a) (scad-sketch--ref-kind b))
       (eq (scad-sketch--ref-shape-id a) (scad-sketch--ref-shape-id b))
       (equal (scad-sketch--ref-index a) (scad-sketch--ref-index b))))

(defun scad-sketch--selection-contains-ref-p (session ref)
  "Return non-nil if SESSION selection explicitly contains REF."
  (cl-some (lambda (selected)
             (scad-sketch--same-ref-p selected ref))
           (scad-sketch-session-selection session)))

(defun scad-sketch--shape-selected-p (session &optional shape-id)
  "Return non-nil if SHAPE-ID is selected in SESSION."
  (scad-sketch--selection-contains-ref-p
   session (scad-sketch--shape-ref
            (or shape-id
                (scad-sketch-session-active-shape-id session)))))

(defun scad-sketch--point-selected-p (session shape-id idx)
  "Return non-nil if point IDX in SHAPE-ID is selected in SESSION.

A selected shape makes all of its points effectively selected."
  (or (scad-sketch--shape-selected-p session shape-id)
      (scad-sketch--selection-contains-ref-p
       session (scad-sketch--point-ref idx shape-id))))

(defun scad-sketch--remove-shape-and-subpoints (selection shape-id)
  "Return SELECTION with SHAPE-ID and all of its point refs removed."
  (cl-remove-if
   (lambda (ref)
     (eq (scad-sketch--ref-shape-id ref) shape-id))
   selection))

(defun scad-sketch--all-point-refs-except (session shape-id idx)
  "Return point refs for every point in SHAPE-ID except IDX."
  (let ((shape (scad-sketch-session-shape-by-id session shape-id))
        refs)
    (when shape
      (dotimes (i (length (scad-sketch-shape-points shape)))
        (unless (= i idx)
          (push (scad-sketch--point-ref i shape-id) refs))))
    (nreverse refs)))

(defun scad-sketch--toggle-ref-selection (session ref)
  "Toggle REF in SESSION selection.

Shape/point invariants:
  - A shape ref and its subpoint refs cannot coexist.
  - Toggling a shape selects the whole shape and removes subpoints.
  - Toggling a point while its shape is selected converts the shape selection
    into all point refs except that point, giving a convenient subtract flow."
  (let* ((kind (scad-sketch--ref-kind ref))
         (shape-id (scad-sketch--ref-shape-id ref))
         (selection (scad-sketch-session-selection session)))
    (pcase kind
      ('shape
       (if (scad-sketch--shape-selected-p session shape-id)
           (setf (scad-sketch-session-selection session)
                 (cl-remove-if (lambda (selected)
                                 (scad-sketch--same-ref-p selected ref))
                               selection))
         (setf (scad-sketch-session-selection session)
               (cons ref (scad-sketch--remove-shape-and-subpoints
                          selection shape-id)))))
      ('point
       (let ((idx (scad-sketch--ref-index ref)))
         (cond
          ;; Shape selected: subtract this point by expanding shape into all
          ;; other point refs.
          ((scad-sketch--shape-selected-p session shape-id)
           (setf (scad-sketch-session-selection session)
                 (append
                  (scad-sketch--all-point-refs-except session shape-id idx)
                  (scad-sketch--remove-shape-and-subpoints
                   selection shape-id))))
          ;; Point explicitly selected: remove it.
          ((scad-sketch--selection-contains-ref-p session ref)
           (setf (scad-sketch-session-selection session)
                 (cl-remove-if (lambda (selected)
                                 (scad-sketch--same-ref-p selected ref))
                               selection)))
          ;; Otherwise add it.
          (t
           (push ref (scad-sketch-session-selection session)))))))))

(defun scad-sketch--hover-radius (session)
  "Return hover radius in model units for SESSION."
  (max (scad-sketch--fine session)
       (* scad-sketch-hover-radius-factor
          (max 0.0001 (scad-sketch--grid session)))))

(defun scad-sketch--distance (a b)
  "Return Euclidean distance between points A and B."
  (let ((dx (- (nth 0 a) (nth 0 b)))
        (dy (- (nth 1 a) (nth 1 b))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun scad-sketch--distance-to-segment (p a b)
  "Return distance from point P to segment A-B."
  (let* ((px (nth 0 p))  (py (nth 1 p))
         (ax (nth 0 a))  (ay (nth 1 a))
         (bx (nth 0 b))  (by (nth 1 b))
         (vx (- bx ax))  (vy (- by ay))
         (wx (- px ax))  (wy (- py ay))
         (len2 (+ (* vx vx) (* vy vy))))
    (if (< len2 1e-12)
        (scad-sketch--distance p a)
      (let* ((raw (/ (+ (* wx vx) (* wy vy)) len2))
             (u (max 0.0 (min 1.0 raw)))
             (closest (list (+ ax (* u vx))
                            (+ ay (* u vy)))))
        (scad-sketch--distance p closest)))))

(defun scad-sketch--point-in-polygon-p (p xy-points)
  "Return non-nil if P is inside XY-POINTS using even/odd ray casting."
  (let ((inside nil)
        (j (1- (length xy-points)))
        (x (nth 0 p))
        (y (nth 1 p)))
    (dotimes (i (length xy-points))
      (let* ((pi (nth i xy-points))
             (pj (nth j xy-points))
             (xi (nth 0 pi)) (yi (nth 1 pi))
             (xj (nth 0 pj)) (yj (nth 1 pj)))
        (when (and (/= yi yj)
                   (<= (min yi yj) y)
                   (< y (max yi yj))
                   (< x (+ xi (/ (* (- y yi) (- xj xi))
                                  (- yj yi)))))
          (setq inside (not inside))))
      (setq j i))
    inside))

(defun scad-sketch--shape-center (session &optional shape-id)
  "Return the model-space center of SHAPE-ID in SESSION."
  (let ((shape (or (and shape-id
                        (scad-sketch-session-shape-by-id session shape-id))
                   (scad-sketch-session-active-shape session))))
    (pcase (and shape (scad-sketch-shape-kind shape))
      ('circle
       (let ((md (scad-sketch-shape-metadata shape)))
         (list (plist-get md :cx)
               (plist-get md :cy))))
      ('polygon
       (let ((points (mapcar #'scad-sketch--point-xy
                             (scad-sketch-shape-points shape))))
         (if points
             (let ((sx 0.0) (sy 0.0) (n 0))
               (dolist (p points)
                 (setq sx (+ sx (nth 0 p)))
                 (setq sy (+ sy (nth 1 p)))
                 (setq n (1+ n)))
               (list (/ sx n) (/ sy n)))
           (copy-sequence (scad-sketch-session-point session)))))
      (_
       (copy-sequence (scad-sketch-session-point session))))))

(defun scad-sketch--shape-hovered-p (session shape)
  "Return non-nil if SESSION point is on/near SHAPE."
  (let ((p (scad-sketch-session-point session))
        (r (scad-sketch--hover-radius session)))
    (pcase (scad-sketch-shape-kind shape)
      ('circle
       (let* ((md (scad-sketch-shape-metadata shape))
              (cx (plist-get md :cx))
              (cy (plist-get md :cy))
              (cr (plist-get md :r))
              (d (scad-sketch--distance p (list cx cy))))
         (or (<= (abs (- d cr)) r)
             (< d cr))))
      ('polygon
       (let* ((points (mapcar #'scad-sketch--point-xy
                              (scad-sketch-shape-points shape)))
              (n (length points))
              (near nil))
         (when (>= n 2)
           (cl-loop for rest on points
                    for a = (car rest)
                    for b = (cadr rest)
                    when (and b (<= (scad-sketch--distance-to-segment p a b) r))
                    do (setq near t))
           (when (and (not near)
                      (scad-sketch-shape-closed shape)
                      (> n 2)
                      (<= (scad-sketch--distance-to-segment
                           p (car (last points)) (car points))
                          r))
             (setq near t))
           (or near
               (and (scad-sketch-shape-closed shape)
                    (> n 2)
                    (scad-sketch--point-in-polygon-p p points))))))
      (_ nil))))

(defun scad-sketch--hover-candidates (session)
  "Return hovered refs under SESSION's current point.

Point refs are listed before shape refs so exact vertex hovers get attention
before the containing polygon."
  (let ((p (scad-sketch-session-point session))
        (r (scad-sketch--hover-radius session))
        candidates)
    (dolist (shape (scad-sketch-session-shapes session))
      (let ((shape-id (scad-sketch-shape-id shape)))
        (when (eq (scad-sketch-shape-kind shape) 'polygon)
          (cl-loop for model-point in (scad-sketch-shape-points shape)
                   for idx from 0
                   for xy = (scad-sketch--point-xy model-point)
                   when (<= (scad-sketch--distance p xy) r)
                   do (push (scad-sketch--point-ref idx shape-id) candidates)))
        (when (scad-sketch--shape-hovered-p session shape)
          (push (scad-sketch--shape-ref shape-id) candidates))))
    (nreverse candidates)))

(defun scad-sketch--selectable-refs (session)
  "Return all selectable refs for SESSION in tab-cycle order."
  (let (refs)
    (dolist (shape (scad-sketch-session-shapes session))
      (let ((shape-id (scad-sketch-shape-id shape)))
        (push (scad-sketch--shape-ref shape-id) refs)
        (when (eq (scad-sketch-shape-kind shape) 'polygon)
          (cl-loop for _pt in (scad-sketch-shape-points shape)
                   for idx from 0
                   do (push (scad-sketch--point-ref idx shape-id) refs)))))
    (nreverse refs)))

(defun scad-sketch--ref-anchor (session ref)
  "Return a model-space anchor point for REF."
  (pcase (scad-sketch--ref-kind ref)
    ('shape
     (scad-sketch--shape-center session (scad-sketch--ref-shape-id ref)))
    ('point
     (let* ((shape (scad-sketch-session-shape-by-id
                    session (scad-sketch--ref-shape-id ref)))
            (point (and shape
                        (nth (scad-sketch--ref-index ref)
                             (scad-sketch-shape-points shape)))))
       (if point
           (scad-sketch--point-xy point)
         (copy-sequence (scad-sketch-session-point session)))))
    (_
     (copy-sequence (scad-sketch-session-point session)))))

(defun scad-sketch--attention-ref (session)
  "Return the ref currently receiving attention in SESSION."
  (let* ((candidates (scad-sketch--hover-candidates session))
         (n (length candidates)))
    (if (> n 0)
        (nth (mod (or (scad-sketch-session-hover-index session) 0) n)
             candidates)
      (scad-sketch-session-focus-ref session))))

(defun scad-sketch--normalize-attention (session)
  "Clamp hover index and keep legacy selected-index aligned with attention."
  (let* ((candidates (scad-sketch--hover-candidates session))
         (n (length candidates)))
    (when (> n 0)
      (setf (scad-sketch-session-hover-index session)
            (mod (or (scad-sketch-session-hover-index session) 0) n)))
    (let ((attention (scad-sketch--attention-ref session)))
      (when attention
        (setf (scad-sketch-session-focus-ref session) attention)
        (when (scad-sketch--ref-shape-id attention)
          (scad-sketch-session-set-active-shape
           session (scad-sketch--ref-shape-id attention)))
        (if (eq (scad-sketch--ref-kind attention) 'point)
            (setf (scad-sketch-session-selected-index session)
                  (scad-sketch--ref-index attention))
          (setf (scad-sketch-session-selected-index session) nil))))))

(defun scad-sketch--selected-point-locs (session &optional fallback-to-active)
  "Return selected point locations in SESSION.

Each location is a cons (SHAPE-ID . INDEX).  Shape selections expand to all
points.  When no explicit selection exists and FALLBACK-TO-ACTIVE is non-nil,
return the legacy active point."
  (let (locs)
    (dolist (ref (scad-sketch-session-selection session))
      (pcase (scad-sketch--ref-kind ref)
        ('shape
         (let* ((shape-id (scad-sketch--ref-shape-id ref))
                (shape (scad-sketch-session-shape-by-id session shape-id)))
           (when shape
             (dotimes (i (length (scad-sketch-shape-points shape)))
               (push (cons shape-id i) locs)))))
        ('point
         (let* ((shape-id (scad-sketch--ref-shape-id ref))
                (idx (scad-sketch--ref-index ref))
                (shape (scad-sketch-session-shape-by-id session shape-id)))
           (when (and shape idx
                      (>= idx 0)
                      (< idx (length (scad-sketch-shape-points shape))))
             (push (cons shape-id idx) locs))))))
    (setq locs (delete-dups (nreverse locs)))
    (if (and (null locs) fallback-to-active
             (scad-sketch-session-active-shape-id session)
             (scad-sketch-session-selected-index session))
        (list (cons (scad-sketch-session-active-shape-id session)
                    (scad-sketch-session-selected-index session)))
      locs)))

(defun scad-sketch--selection-summary (session)
  "Return compact text describing SESSION selection."
  (let ((selection (scad-sketch-session-selection session)))
    (if (null selection)
        "none"
      (format "%d item%s"
              (length selection)
              (if (= 1 (length selection)) "" "s")))))

(defun scad-sketch--ref-summary (ref)
  "Return compact text for REF."
  (pcase (and ref (scad-sketch--ref-kind ref))
    ('shape (format "%s" (scad-sketch--ref-shape-id ref)))
    ('point (format "%s[%s]"
                    (scad-sketch--ref-shape-id ref)
                    (scad-sketch--ref-index ref)))
    (_ "none")))

;;; Movement

(defun scad-sketch--move-point (dx dy &optional snap)
  "Move cursor by DX, DY; snap to grid when SNAP is non-nil.

This is a clean operation: moving the editor cursor does not dirty source."
  (scad-sketch--clean-change
   (lambda (s)
     (let ((new (scad-sketch--move-xy (scad-sketch-session-point s) dx dy)))
       (setf (scad-sketch-session-point s)
             (if snap (scad-sketch--snap-xy new (scad-sketch--grid s)) new))
       (setf (scad-sketch-session-hover-index s) 0)))))

(defun scad-sketch--move-shape (shape dx dy &optional snap grid)
  "Move whole SHAPE by DX DY."
  (pcase (scad-sketch-shape-kind shape)
    ('polygon
     (setf (scad-sketch-shape-points shape)
           (mapcar (lambda (pt)
                     (let* ((xy (scad-sketch--move-xy
                                 (scad-sketch--point-xy pt) dx dy))
                            (xy (if snap (scad-sketch--snap-xy xy grid) xy)))
                       (scad-sketch--make-model-point xy pt)))
                   (scad-sketch-shape-points shape))))
    ('circle
     (let* ((md (scad-sketch-shape-metadata shape))
            (xy (scad-sketch--move-xy
                 (list (plist-get md :cx) (plist-get md :cy))
                 dx dy))
            (xy (if snap (scad-sketch--snap-xy xy grid) xy)))
       (setf (scad-sketch-shape-metadata shape)
             (plist-put md :cx (nth 0 xy)))
       (setf (scad-sketch-shape-metadata shape)
             (plist-put (scad-sketch-shape-metadata shape)
                        :cy (nth 1 xy)))))))

(defun scad-sketch--selected-shape-ids (session)
  "Return explicitly selected shape ids in SESSION."
  (delq nil
        (mapcar (lambda (ref)
                  (when (eq (scad-sketch--ref-kind ref) 'shape)
                    (scad-sketch--ref-shape-id ref)))
                (scad-sketch-session-selection session))))

(defun scad-sketch--move-selected (dx dy &optional snap)
  "Move selected vertices/shapes by DX, DY; snap to grid when SNAP is non-nil.

If no explicit selection exists, falls back to the active shape/point."
  (scad-sketch--edit
   (lambda (s)
     (let ((shape-ids (scad-sketch--selected-shape-ids s))
           (locs (scad-sketch--selected-point-locs s nil)))
       (cond
        (shape-ids
         (dolist (shape-id shape-ids)
           (let ((shape (scad-sketch-session-shape-by-id s shape-id)))
             (when shape
               (scad-sketch--move-shape shape dx dy snap
                                        (scad-sketch--grid s))))))
        (locs
         (dolist (loc locs)
           (let* ((shape-id (car loc))
                  (idx      (cdr loc))
                  (shape    (scad-sketch-session-shape-by-id s shape-id))
                  (points   (scad-sketch-shape-points shape))
                  (old      (nth idx points))
                  (new-xy   (scad-sketch--move-xy
                             (scad-sketch--point-xy old) dx dy))
                  (snapped  (if snap
                                (scad-sketch--snap-xy new-xy
                                                      (scad-sketch--grid s))
                              new-xy))
                  (new      (scad-sketch--make-model-point snapped old)))
             (setf (scad-sketch-shape-points shape)
                   (scad-sketch--replace-nth idx new points)))))
        (t
         (let ((shape (scad-sketch-session-active-shape s)))
           (unless shape
             (user-error "No selected point or shape"))
           (scad-sketch--move-shape shape dx dy snap
                                    (scad-sketch--grid s)))))))))

(defun scad-sketch--grid   (s) (float (scad-sketch-session-grid s)))
(defun scad-sketch--fine   (s) (float (scad-sketch-session-fine-step s)))
(defun scad-sketch--coarse (s) (float (scad-sketch-session-coarse-step s)))

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

;;; Mark commands

(defun scad-sketch-set-mark ()
  "Replace all marks with the current cursor position."
  (interactive)
  (scad-sketch--clean-change
   (lambda (s)
     (setf (scad-sketch-session-marks s)
           (list (copy-sequence (scad-sketch-session-point s)))))))

(defun scad-sketch-push-mark ()
  "Push the current cursor position onto the mark stack."
  (interactive)
  (scad-sketch--clean-change
   (lambda (s)
     (push (copy-sequence (scad-sketch-session-point s))
           (scad-sketch-session-marks s)))))

(defun scad-sketch-pop-mark ()
  "Pop the most recent mark and jump cursor to it."
  (interactive)
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-marks session) (user-error "No marks set")))
  (scad-sketch--clean-change
   (lambda (s)
     (setf (scad-sketch-session-point s)
           (copy-sequence (pop (scad-sketch-session-marks s))))
     (setf (scad-sketch-session-hover-index s) 0))))

(defun scad-sketch-jump-to-mark ()
  "Move cursor to the most recent mark without consuming it."
  (interactive)
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-marks session) (user-error "No marks set")))
  (scad-sketch--clean-change
   (lambda (s)
     (setf (scad-sketch-session-point s)
           (copy-sequence (car (scad-sketch-session-marks s))))
     (setf (scad-sketch-session-hover-index s) 0))))

(defun scad-sketch-clear-marks ()
  "Clear all marks."
  (interactive)
  (scad-sketch--clean-change
   (lambda (s) (setf (scad-sketch-session-marks s) nil))))

;;; Vertex editing
(defun scad-sketch--append-model-point (session point)
  "Append POINT to SESSION's active shape and focus/select it."
  (let* ((shape (scad-sketch-session-active-shape session))
         (shape-id (scad-sketch-shape-id shape))
         (points (append (scad-sketch-shape-points shape) (list point)))
         (idx (1- (length points))))
    (setf (scad-sketch-shape-points shape) points)
    (scad-sketch-session-set-active-shape session shape-id)
    (setf (scad-sketch-session-selected-index session) idx)
    (setf (scad-sketch-session-focus-ref session)
          (scad-sketch--point-ref idx shape-id))
    (setf (scad-sketch-session-selection session)
          (list (scad-sketch--point-ref idx shape-id)))))

(defun scad-sketch-append-point ()
  "Append the cursor position as a new vertex."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (scad-sketch--append-model-point
      s (scad-sketch--make-model-point (scad-sketch-session-point s))))))

(defun scad-sketch-insert-point-after-selected ()
  "Insert points after the selected vertex in the active shape.

With marks set, inserts each mark (oldest first) then the cursor.
Without marks, inserts only the cursor."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (let* ((shape     (scad-sketch-session-active-shape s))
            (shape-id  (scad-sketch-shape-id shape))
            (idx       (or (scad-sketch-session-selected-index s) -1))
            (points    (scad-sketch-shape-points shape))
            (insert-at (min (1+ idx) (length points)))
            (mark-pts  (mapcar (lambda (m) (scad-sketch--make-model-point m))
                                (reverse (scad-sketch-session-marks s))))
            (cursor-pt (scad-sketch--make-model-point
                        (scad-sketch-session-point s)))
            (new-pts   (append mark-pts (list cursor-pt)))
            (new-idx   (+ insert-at (length new-pts) -1)))
       (setf (scad-sketch-shape-points shape)
             (append (cl-subseq points 0 insert-at)
                     new-pts
                     (nthcdr insert-at points)))
       (scad-sketch-session-set-active-shape s shape-id)
       (setf (scad-sketch-session-selected-index s) new-idx)
       (setf (scad-sketch-session-focus-ref s)
             (scad-sketch--point-ref new-idx shape-id))))))

(defun scad-sketch-delete-selected ()
  "Delete selected vertices.

A selected shape deletes all vertices in that shape.  If no explicit selection
exists, deletes the active point."
  (interactive)
  (scad-sketch--edit
   (lambda (s)
     (let* ((locs (sort (copy-sequence
                         (scad-sketch--selected-point-locs s t))
                        (lambda (a b)
                          (if (eq (car a) (car b))
                              (> (cdr a) (cdr b))
                            (string> (symbol-name (car a))
                                     (symbol-name (car b))))))))
       (unless locs
         (user-error "No selected point or shape"))
       (dolist (loc locs)
         (let* ((shape-id (car loc))
                (idx      (cdr loc))
                (shape    (scad-sketch-session-shape-by-id s shape-id))
                (points   (and shape (scad-sketch-shape-points shape))))
           (when (and points (>= idx 0) (< idx (length points)))
             (setf (scad-sketch-shape-points shape)
                   (append (cl-subseq points 0 idx)
                           (nthcdr (1+ idx) points))))))

       ;; Drop empty shapes only in multi-shape sessions.  For a single-shape
       ;; session, an empty polygon is still a valid editing state.
       (when (> (length (scad-sketch-session-shapes s)) 1)
         (setf (scad-sketch-session-shapes s)
               (cl-remove-if (lambda (shape)
                               (null (scad-sketch-shape-points shape)))
                             (scad-sketch-session-shapes s))))

       (setf (scad-sketch-session-selection s) nil)

       (let ((active (or (scad-sketch-session-active-shape s)
                         (car (scad-sketch-session-shapes s)))))
         (when active
           (scad-sketch-session-set-active-shape
            s (scad-sketch-shape-id active))
           (setf (scad-sketch-session-selected-index s)
                 (if (scad-sketch-session-points s) 0 nil))
           (when (scad-sketch-session-points s)
             (setf (scad-sketch-session-point s)
                   (scad-sketch--point-xy
                    (car (scad-sketch-session-points s)))))))))))

(defun scad-sketch-line-from-mark ()
  "Create a new polygon shape from marks (oldest first) and cursor.

Normal polygon shapes are implicitly closed by OpenSCAD.  Open-path behavior
should be introduced later for beamChain/beamPoints-style objects."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (unless (scad-sketch-session-marks s)
       (user-error "No marks set"))
     (let ((points
            (append
             (mapcar #'scad-sketch--make-model-point
                     (reverse (scad-sketch-session-marks s)))
             (list (scad-sketch--make-model-point
                    (scad-sketch-session-point s))))))
       (scad-sketch-session-add-shape s points)))))

(defun scad-sketch-rectangle-from-mark ()
  "Create a new rectangle polygon shape from most recent mark to cursor."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (let ((mark (or (car (scad-sketch-session-marks s))
                     (user-error "No marks set")))
           (pt   (scad-sketch-session-point s)))
       (let* ((x1 (nth 0 mark)) (y1 (nth 1 mark))
              (x2 (nth 0 pt))   (y2 (nth 1 pt))
              (points
               (mapcar #'scad-sketch--make-model-point
                       (list (list x1 y1)
                             (list x2 y1)
                             (list x2 y2)
                             (list x1 y2)))))
         (scad-sketch-session-add-shape s points))))))

(defun scad-sketch-toggle-closed ()
  "Toggle the closed flag on the active shape.

For normal polygon write-back, OpenSCAD treats polygons as closed.  This remains
a visual/editor flag for now; future beamChain/beamPoints objects can serialize
open-path behavior explicitly."
  (interactive)
  (scad-sketch--clean-change
   (lambda (s)
     (let ((shape (scad-sketch-session-active-shape s)))
       (setf (scad-sketch-shape-closed shape)
             (not (scad-sketch-shape-closed shape)))
       (setf (scad-sketch-session-closed s)
             (scad-sketch-shape-closed shape))))))

(defun scad-sketch-set-radius (radius)
  "Set the polyRound radius of selected vertices.

If a shape is selected, applies to all of its vertices.  If no explicit
selection exists, applies to the active point."
  (interactive (list (read-number "Radius: " 0)))
  (scad-sketch--edit
   (lambda (s)
     (let ((locs (scad-sketch--selected-point-locs s t)))
       (unless locs
         (user-error "No selected point or shape"))
       (dolist (loc locs)
         (let* ((shape-id (car loc))
                (idx      (cdr loc))
                (shape    (scad-sketch-session-shape-by-id s shape-id))
                (points   (scad-sketch-shape-points shape))
                (pt       (nth idx points)))
           (setf (scad-sketch-shape-points shape)
                 (scad-sketch--replace-nth
                  idx
                  (list (nth 0 pt) (nth 1 pt) (float radius))
                  points))))))))

(defun scad-sketch--set-focus-ref (session ref)
  "Set SESSION focus to REF and move cursor to REF's anchor."
  (setf (scad-sketch-session-focus-ref session) ref)
  (when (scad-sketch--ref-shape-id ref)
    (scad-sketch-session-set-active-shape
     session (scad-sketch--ref-shape-id ref)))
  (setf (scad-sketch-session-point session)
        (copy-sequence (scad-sketch--ref-anchor session ref)))
  (setf (scad-sketch-session-hover-index session) 0)
  (if (eq (scad-sketch--ref-kind ref) 'point)
      (setf (scad-sketch-session-selected-index session)
            (scad-sketch--ref-index ref))
    (setf (scad-sketch-session-selected-index session) nil)))

(defun scad-sketch--cycle-selectable (delta)
  "Cycle focus by DELTA through all selectable refs."
  (scad-sketch--clean-change
   (lambda (s)
     (let* ((refs (scad-sketch--selectable-refs s))
            (n (length refs))
            (focus (or (scad-sketch-session-focus-ref s)
                       (car refs)))
            (idx (or (cl-position-if
                      (lambda (ref)
                        (scad-sketch--same-ref-p ref focus))
                      refs)
                     0)))
       (unless (> n 0)
         (user-error "No selectable objects"))
       (scad-sketch--set-focus-ref s
                                    (nth (mod (+ idx delta) n)
                                         refs))))))

(defun scad-sketch-next-selectable ()
  "Cycle focus to the next selectable shape/point."
  (interactive)
  (scad-sketch--cycle-selectable 1))

(defun scad-sketch-previous-selectable ()
  "Cycle focus to the previous selectable shape/point."
  (interactive)
  (scad-sketch--cycle-selectable -1))

(defun scad-sketch--cycle-hovered (delta)
  "Cycle attention by DELTA among hovered refs under point."
  (scad-sketch--clean-change
   (lambda (s)
     (let* ((candidates (scad-sketch--hover-candidates s))
            (n (length candidates)))
       (unless (> n 0)
         (user-error "No hovered objects under point"))
       (setf (scad-sketch-session-hover-index s)
             (mod (+ (or (scad-sketch-session-hover-index s) 0)
                     delta)
                  n))))))

(defun scad-sketch-next-hovered ()
  "Cycle attention to the next hovered object under point."
  (interactive)
  (scad-sketch--cycle-hovered 1))

(defun scad-sketch-previous-hovered ()
  "Cycle attention to the previous hovered object under point."
  (interactive)
  (scad-sketch--cycle-hovered -1))

(defun scad-sketch-toggle-attention-selection ()
  "Toggle the currently attended hovered/focused object in the selection."
  (interactive)
  (scad-sketch--clean-change
   (lambda (s)
     (let ((ref (scad-sketch--attention-ref s)))
       (unless ref
         (user-error "No focused or hovered object"))
       (scad-sketch--toggle-ref-selection s ref)))))

(defun scad-sketch-clear-selection ()
  "Clear the current selection."
  (interactive)
  (scad-sketch--clean-change
   (lambda (s)
     (setf (scad-sketch-session-selection s) nil))))


;;; Coordinate commands

(defun scad-sketch--set-point-axis (axis value)
  "Set cursor coordinate AXIS (0=x, 1=y) to VALUE."
  (scad-sketch--clean-change
   (lambda (s)
     (let ((pt (copy-sequence (scad-sketch-session-point s))))
       (setf (nth axis pt) (float value))
       (setf (scad-sketch-session-point s) pt)
       (setf (scad-sketch-session-hover-index s) 0)))))

(defun scad-sketch-set-x (x)
  "Set cursor X."
  (interactive (list (read-number "X: " (nth 0 (scad-sketch-session-point (scad-sketch--assert-session))))))
  (scad-sketch--set-point-axis 0 x))

(defun scad-sketch-set-y (y)
  "Set cursor Y."
  (interactive (list (read-number "Y: " (nth 1 (scad-sketch-session-point (scad-sketch--assert-session))))))
  (scad-sketch--set-point-axis 1 y))

(defun scad-sketch--set-delta-axis (axis value)
  "Set cursor AXIS to (most recent mark AXIS) + VALUE."
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-marks session) (user-error "No marks set"))
    (scad-sketch--set-point-axis
     axis (+ (nth axis (car (scad-sketch-session-marks session))) (float value)))))

(defun scad-sketch-set-delta-x (dx)
  "Set cursor X to (most recent mark X) + DX."
  (interactive (list (read-number "ΔX from mark: " 0)))
  (scad-sketch--set-delta-axis 0 dx))

(defun scad-sketch-set-delta-y (dy)
  "Set cursor Y to (most recent mark Y) + DY."
  (interactive (list (read-number "ΔY from mark: " 0)))
  (scad-sketch--set-delta-axis 1 dy))

(defun scad-sketch-set-distance-from-mark (distance)
  "Set distance from most recent mark to cursor, preserving angle."
  (interactive (list (read-number "Distance from mark: " 0)))
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-marks session) (user-error "No marks set")))
  (scad-sketch--clean-change
   (lambda (s)
     (let* ((m  (car (scad-sketch-session-marks s)))
            (p  (scad-sketch-session-point s))
            (angle (atan (- (nth 1 p) (nth 1 m))
                         (- (nth 0 p) (nth 0 m)))))
       (setf (scad-sketch-session-point s)
             (list (+ (nth 0 m) (* (float distance) (cos angle)))
                   (+ (nth 1 m) (* (float distance) (sin angle)))))
       (setf (scad-sketch-session-hover-index s) 0)))))

(defun scad-sketch-set-angle-from-mark (degrees)
  "Set angle from most recent mark to cursor in DEGREES, preserving distance."
  (interactive (list (read-number "Angle degrees from mark: " 0)))
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-marks session) (user-error "No marks set")))
  (scad-sketch--clean-change
   (lambda (s)
     (let* ((m     (car (scad-sketch-session-marks s)))
            (p     (scad-sketch-session-point s))
            (dx    (- (nth 0 p) (nth 0 m)))
            (dy    (- (nth 1 p) (nth 1 m)))
            (dist  (sqrt (+ (* dx dx) (* dy dy))))
            (angle (* pi (/ (float degrees) 180.0))))
       (setf (scad-sketch-session-point s)
             (list (+ (nth 0 m) (* dist (cos angle)))
                   (+ (nth 1 m) (* dist (sin angle)))))
       (setf (scad-sketch-session-hover-index s) 0)))))

(defun scad-sketch-set-grid (grid)
  "Set the grid step."
  (interactive
   (list (read-number "Grid step: "
                      (scad-sketch-session-grid
                       (scad-sketch--assert-session)))))
  (scad-sketch--clean-change
   (lambda (s)
     (setf (scad-sketch-session-grid s) (float grid))
     (setf (scad-sketch-session-hover-index s) 0))))

;;; Undo command
(defun scad-sketch-undo ()
  "Undo the last sketch edit."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (entry   (pop (scad-sketch-session-undo-stack session))))
    (unless entry (user-error "No sketch undo available"))
    (setf (scad-sketch-session-points session)         (plist-get entry :points))
    (setf (scad-sketch-session-point session)          (plist-get entry :point))
    (setf (scad-sketch-session-marks session)          (plist-get entry :marks))
    (setf (scad-sketch-session-named-marks session)    (plist-get entry :named-marks))
    (setf (scad-sketch-session-selected-index session) (plist-get entry :selected-index))
    (setf (scad-sketch-session-closed session)         (plist-get entry :closed))
    (setf (scad-sketch-session-shapes session)         (plist-get entry :shapes))
    (setf (scad-sketch-session-active-shape-id session)
          (plist-get entry :active-shape-id))
    (setf (scad-sketch-session-targets session)        (plist-get entry :targets))
    (setf (scad-sketch-session-root-target-id session)
          (plist-get entry :root-target-id))
    (setf (scad-sketch-session-selection session)      (plist-get entry :selection))
    (setf (scad-sketch-session-focus-ref session)      (plist-get entry :focus-ref))
    (setf (scad-sketch-session-dirty session) t)
    (scad-sketch--render)))


;;; Rendering
(defun scad-sketch--circle-bounds (shape)
  "Return bounds for circle SHAPE."
  (let* ((md (scad-sketch-shape-metadata shape))
         (cx (plist-get md :cx))
         (cy (plist-get md :cy))
         (r  (plist-get md :r)))
    (list (- cx r) (+ cx r) (- cy r) (+ cy r))))

(defun scad-sketch--shape-xy-points (shape)
  "Return representative XY points for SHAPE."
  (pcase (scad-sketch-shape-kind shape)
    ('polygon
     (mapcar #'scad-sketch--point-xy
             (scad-sketch-shape-points shape)))
    ('circle
     (let* ((b (scad-sketch--circle-bounds shape)))
       (list (list (nth 0 b) (nth 2 b))
             (list (nth 1 b) (nth 3 b)))))
    (_ nil)))

(defun scad-sketch--bounds (session)
  "Return (min-x max-x min-y max-y) for all shapes, marks, and cursor."
  (scad-sketch-session-sync-active-shape-from-points session)
  (let* ((shape-points
          (apply #'append
                 (mapcar #'scad-sketch--shape-xy-points
                         (scad-sketch-session-shapes session))))
         (extra (delq nil (cons (scad-sketch-session-point session)
                                (scad-sketch-session-marks session))))
         (all   (append shape-points extra)))
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
  "Return a pixel-coordinate closure for BOUNDS."
  (pcase-let ((`(,min-x ,max-x ,min-y ,max-y) bounds))
    (let* ((w scad-sketch-canvas-width) (h scad-sketch-canvas-height)
           (m scad-sketch-margin)
           (scale (min (/ (- w (* 2 m)) (- max-x min-x))
                       (/ (- h (* 2 m)) (- max-y min-y)))))
      (lambda (xy)
        (list (+ m (* (- (nth 0 xy) min-x) scale))
              (- h (+ m (* (- (nth 1 xy) min-y) scale))))))))

(defun scad-sketch--svg-line (svg transform a b &rest args)
  "Draw a model-space line A→B on SVG."
  (let ((pa (funcall transform a)) (pb (funcall transform b)))
    (apply #'svg-line svg (nth 0 pa) (nth 1 pa) (nth 0 pb) (nth 1 pb) args)))

(defun scad-sketch--draw-grid (svg bounds transform session)
  "Draw the background grid."
  (pcase-let ((`(,min-x ,max-x ,min-y ,max-y) bounds))
    (let* ((grid (max 0.0001 (scad-sketch-session-grid session)))
           (x (* grid (floor (/ min-x grid))))
           (y (* grid (floor (/ min-y grid)))))
      (while (<= x (* grid (ceiling (/ max-x grid))))
        (scad-sketch--svg-line svg transform (list x min-y) (list x max-y)
                               :stroke "#e8e8e8" :stroke-width 1)
        (setq x (+ x grid)))
      (while (<= y (* grid (ceiling (/ max-y grid))))
        (scad-sketch--svg-line svg transform (list min-x y) (list max-x y)
                               :stroke "#e8e8e8" :stroke-width 1)
        (setq y (+ y grid))))
    (when (and (<= min-x 0) (<= 0 max-x))
      (scad-sketch--svg-line svg transform (list 0 min-y) (list 0 max-y)
                             :stroke "#d0d0d0" :stroke-width 2))
    (when (and (<= min-y 0) (<= 0 max-y))
      (scad-sketch--svg-line svg transform (list min-x 0) (list max-x 0)
                             :stroke "#d0d0d0" :stroke-width 2))))

;;; polyRound arc geometry
(defun scad-sketch--draw-one-shape (svg transform session shape)
  "Draw one polygon SHAPE in SESSION."
  (let* ((points          (scad-sketch-shape-points shape))
         (closed          (scad-sketch-shape-closed shape))
         (shape-id        (scad-sketch-shape-id shape))
         (n               (length points))
         (idx             0)
         (shape-selected  (scad-sketch--shape-selected-p session shape-id))
         (attention       (scad-sketch--attention-ref session))
         (shape-attention (and attention
                               (eq (scad-sketch--ref-kind attention) 'shape)
                               (eq (scad-sketch--ref-shape-id attention)
                                   shape-id)))
         (active-shape    (eq shape-id
                               (scad-sketch-session-active-shape-id session)))
         (shape-stroke    (cond (shape-selected "#d13f00")
                                (shape-attention "#0057c2")
                                (active-shape "#333333")
                                (t "#777777")))
         (shape-width     (cond ((or shape-selected shape-attention) 5)
                                (active-shape 4)
                                (t 3))))
    (when (>= n 2)
      (if (scad-sketch--any-radius-p points)
          (let ((d (scad-sketch--polyround-path-d points closed transform)))
            (when d
              (svg-node svg 'path
                        :d d
                        :stroke shape-stroke
                        :stroke-width shape-width
                        :fill "none")))
        (let ((xy-points (mapcar #'scad-sketch--point-xy points)))
          (cl-loop for a on xy-points
                   for b = (cadr a)
                   when b do
                   (scad-sketch--svg-line svg transform (car a) b
                                          :stroke shape-stroke
                                          :stroke-width shape-width))
          (when (and closed (> n 2))
            (scad-sketch--svg-line svg transform
                                   (car (last xy-points))
                                   (car xy-points)
                                   :stroke shape-stroke
                                   :stroke-width shape-width)))))

    (when shape-attention
      (let ((center (funcall transform
                             (scad-sketch--shape-center session shape-id))))
        (svg-text svg (format "%s" shape-id)
                  :x (+ (nth 0 center) 10)
                  :y (+ (nth 1 center) 4)
                  :font-size 12
                  :fill "#0057c2")))

    (dolist (pt points)
      (let* ((xy        (scad-sketch--point-xy pt))
             (screen    (funcall transform xy))
             (point-ref (scad-sketch--point-ref idx shape-id))
             (sel       (scad-sketch--point-selected-p session shape-id idx))
             (attn      (and attention
                              (scad-sketch--same-ref-p attention point-ref)))
             (radius    (scad-sketch--point-radius pt)))
        (svg-circle svg (nth 0 screen) (nth 1 screen)
                    (cond (sel 8)
                          (attn 7)
                          (active-shape 6)
                          (t 5))
                    :stroke (cond (sel "#d13f00")
                                  (attn "#0057c2")
                                  (active-shape "#333333")
                                  (t "#777777"))
                    :stroke-width (cond (sel 3)
                                        (attn 3)
                                        (t 2))
                    :fill (cond (sel "#fff0e8")
                                (attn "#dfefff")
                                (active-shape "#ffffff")
                                (t "#f8f8f8")))
        (svg-text svg (format "%s:%d" shape-id idx)
                  :x (+ (nth 0 screen) 8)
                  :y (- (nth 1 screen) 8)
                  :font-size 11
                  :fill "#333333")
        (when (> radius 0)
          (let* ((prev     (cond ((> idx 0)      (nth (1- idx) points))
                                 (closed         (nth (1- n)   points))))
                 (next     (cond ((< idx (1- n)) (nth (1+ idx) points))
                                 (closed         (nth 0        points))))
                 (corner   (when (and prev next)
                             (scad-sketch--corner-geometry
                              (scad-sketch--point-xy prev)
                              xy
                              (scad-sketch--point-xy next)
                              radius)))
                 (actual-r (if corner (plist-get corner :radius) radius))
                 (capped   (and corner (< (+ actual-r 0.001) radius))))
            (svg-circle svg (nth 0 screen) (nth 1 screen)
                        (scad-sketch--pixel-radius actual-r transform)
                        :stroke (if capped "#c04000" "#804000")
                        :stroke-width 1
                        :stroke-dasharray "3,3"
                        :fill "none")
            (svg-text svg (if capped
                              (format "r=%s\u2192%s"
                                      (scad-sketch--fmt-num radius)
                                      (scad-sketch--fmt-num actual-r))
                            (format "r=%s"
                                    (scad-sketch--fmt-num actual-r)))
                      :x (+ (nth 0 screen) 8)
                      :y (+ (nth 1 screen) 18)
                      :font-size 11
                      :fill (if capped "#c04000" "#804000")))))
      (setq idx (1+ idx)))))

(defun scad-sketch--draw-one-polygon-shape (svg transform session shape)
  "Draw one polygon SHAPE in SESSION."
  (let* ((points          (scad-sketch-shape-points shape))
         (closed          (scad-sketch-shape-closed shape))
         (shape-id        (scad-sketch-shape-id shape))
         (n               (length points))
         (idx             0)
         (shape-selected  (scad-sketch--shape-selected-p session shape-id))
         (attention       (scad-sketch--attention-ref session))
         (shape-attention (and attention
                               (eq (scad-sketch--ref-kind attention) 'shape)
                               (eq (scad-sketch--ref-shape-id attention)
                                   shape-id)))
         (active-shape    (eq shape-id
                               (scad-sketch-session-active-shape-id session)))
         (shape-stroke    (cond (shape-selected "#d13f00")
                                (shape-attention "#0057c2")
                                (active-shape "#333333")
                                (t "#777777")))
         (shape-width     (cond ((or shape-selected shape-attention) 5)
                                (active-shape 4)
                                (t 3))))
    (when (>= n 2)
      (if (scad-sketch--any-radius-p points)
          (let ((d (scad-sketch--polyround-path-d points closed transform)))
            (when d
              (svg-node svg 'path
                        :d d
                        :stroke shape-stroke
                        :stroke-width shape-width
                        :fill "none")))
        (let ((xy-points (mapcar #'scad-sketch--point-xy points)))
          (cl-loop for a on xy-points
                   for b = (cadr a)
                   when b do
                   (scad-sketch--svg-line svg transform (car a) b
                                          :stroke shape-stroke
                                          :stroke-width shape-width))
          (when (and closed (> n 2))
            (scad-sketch--svg-line svg transform
                                   (car (last xy-points))
                                   (car xy-points)
                                   :stroke shape-stroke
                                   :stroke-width shape-width)))))

    (dolist (pt points)
      (let* ((xy        (scad-sketch--point-xy pt))
             (screen    (funcall transform xy))
             (point-ref (scad-sketch--point-ref idx shape-id))
             (sel       (scad-sketch--point-selected-p session shape-id idx))
             (attn      (and attention
                              (scad-sketch--same-ref-p attention point-ref)))
             (radius    (scad-sketch--point-radius pt)))
        (svg-circle svg (nth 0 screen) (nth 1 screen)
                    (cond (sel 8)
                          (attn 7)
                          (active-shape 6)
                          (t 5))
                    :stroke (cond (sel "#d13f00")
                                  (attn "#0057c2")
                                  (active-shape "#333333")
                                  (t "#777777"))
                    :stroke-width (cond (sel 3)
                                        (attn 3)
                                        (t 2))
                    :fill (cond (sel "#fff0e8")
                                (attn "#dfefff")
                                (active-shape "#ffffff")
                                (t "#f8f8f8")))
        (svg-text svg (format "%s:%d" shape-id idx)
                  :x (+ (nth 0 screen) 8)
                  :y (- (nth 1 screen) 8)
                  :font-size 11
                  :fill "#333333")
        (when (> radius 0)
          (let* ((prev     (cond ((> idx 0)      (nth (1- idx) points))
                                 (closed         (nth (1- n)   points))))
                 (next     (cond ((< idx (1- n)) (nth (1+ idx) points))
                                 (closed         (nth 0        points))))
                 (corner   (when (and prev next)
                             (scad-sketch--corner-geometry
                              (scad-sketch--point-xy prev)
                              xy
                              (scad-sketch--point-xy next)
                              radius)))
                 (actual-r (if corner (plist-get corner :radius) radius))
                 (capped   (and corner (< (+ actual-r 0.001) radius))))
            (svg-circle svg (nth 0 screen) (nth 1 screen)
                        (scad-sketch--pixel-radius actual-r transform)
                        :stroke (if capped "#c04000" "#804000")
                        :stroke-width 1
                        :stroke-dasharray "3,3"
                        :fill "none")
            (svg-text svg (if capped
                              (format "r=%s\u2192%s"
                                      (scad-sketch--fmt-num radius)
                                      (scad-sketch--fmt-num actual-r))
                            (format "r=%s"
                                    (scad-sketch--fmt-num actual-r)))
                      :x (+ (nth 0 screen) 8)
                      :y (+ (nth 1 screen) 18)
                      :font-size 11
                      :fill (if capped "#c04000" "#804000")))))
      (setq idx (1+ idx)))))

(defun scad-sketch--draw-one-circle-shape (svg transform session shape)
  "Draw one circle SHAPE in SESSION."
  (let* ((shape-id        (scad-sketch-shape-id shape))
         (md              (scad-sketch-shape-metadata shape))
         (center          (list (plist-get md :cx) (plist-get md :cy)))
         (r               (plist-get md :r))
         (screen          (funcall transform center))
         (pr              (scad-sketch--pixel-radius r transform))
         (shape-selected  (scad-sketch--shape-selected-p session shape-id))
         (attention       (scad-sketch--attention-ref session))
         (shape-attention (and attention
                               (eq (scad-sketch--ref-kind attention) 'shape)
                               (eq (scad-sketch--ref-shape-id attention)
                                   shape-id)))
         (active-shape    (eq shape-id
                               (scad-sketch-session-active-shape-id session))))
    (svg-circle svg (nth 0 screen) (nth 1 screen) pr
                :stroke (cond (shape-selected "#d13f00")
                              (shape-attention "#0057c2")
                              (active-shape "#333333")
                              (t "#777777"))
                :stroke-width (cond ((or shape-selected shape-attention) 5)
                                    (active-shape 4)
                                    (t 3))
                :fill "none")
    (svg-circle svg (nth 0 screen) (nth 1 screen)
                (cond (shape-selected 8)
                      (shape-attention 7)
                      (active-shape 6)
                      (t 5))
                :stroke (cond (shape-selected "#d13f00")
                              (shape-attention "#0057c2")
                              (active-shape "#333333")
                              (t "#777777"))
                :stroke-width 2
                :fill (cond (shape-selected "#fff0e8")
                            (shape-attention "#dfefff")
                            (active-shape "#ffffff")
                            (t "#f8f8f8")))
    (svg-text svg (format "%s" shape-id)
              :x (+ (nth 0 screen) 8)
              :y (- (nth 1 screen) 8)
              :font-size 11
              :fill "#333333")))

(defun scad-sketch--shape-bounds (shape)
  "Return model-space bounds for SHAPE as (MIN-X MAX-X MIN-Y MAX-Y)."
  (pcase (scad-sketch-shape-kind shape)
    ('circle
     (scad-sketch--circle-bounds shape))
    ('polygon
     (let ((points (mapcar #'scad-sketch--point-xy
                           (scad-sketch-shape-points shape))))
       (when points
         (list (apply #'min (mapcar #'car points))
               (apply #'max (mapcar #'car points))
               (apply #'min (mapcar #'cadr points))
               (apply #'max (mapcar #'cadr points))))))
    (_ nil)))

(defun scad-sketch--merge-bounds (bounds-list)
  "Merge BOUNDS-LIST into one bounds tuple."
  (let ((bounds-list (delq nil bounds-list)))
    (when bounds-list
      (list (apply #'min (mapcar #'car bounds-list))
            (apply #'max (mapcar #'cadr bounds-list))
            (apply #'min (mapcar #'caddr bounds-list))
            (apply #'max (mapcar #'cadddr bounds-list))))))

(defun scad-sketch--tree-bounds (session tree)
  "Return bounds for TREE in SESSION."
  (pcase (plist-get tree :kind)
    ('shape
     (let ((shape (scad-sketch-session-shape-by-id
                   session (plist-get tree :shape-id))))
       (and shape (scad-sketch--shape-bounds shape))))
    ('boolean
     (scad-sketch--merge-bounds
      (mapcar (lambda (child)
                (scad-sketch--tree-bounds session child))
              (plist-get tree :children))))
    (_ nil)))

(defun scad-sketch--draw-boolean-boxes (svg transform session tree)
  "Draw dotted boolean bounding boxes for TREE."
  (when (eq (plist-get tree :kind) 'boolean)
    (let* ((op (plist-get tree :op))
           (bounds (scad-sketch--tree-bounds session tree)))
      (when bounds
        (pcase-let ((`(,min-x ,max-x ,min-y ,max-y) bounds))
          (let* ((p0 (funcall transform (list min-x min-y)))
                 (p1 (funcall transform (list max-x max-y)))
                 (x (min (nth 0 p0) (nth 0 p1)))
                 (y (min (nth 1 p0) (nth 1 p1)))
                 (w (abs (- (nth 0 p1) (nth 0 p0))))
                 (h (abs (- (nth 1 p1) (nth 1 p0)))))
            (svg-rectangle svg x y w h
                           :stroke "#6a5acd"
                           :stroke-width 1
                           :stroke-dasharray "6,4"
                           :fill "none")
            (svg-text svg (format "%s" op)
                      :x (+ x 6)
                      :y (+ y 14)
                      :font-size 12
                      :fill "#6a5acd")))))
    (dolist (child (plist-get tree :children))
      (scad-sketch--draw-boolean-boxes svg transform session child))))

(defun scad-sketch--draw-path (svg transform session)
  "Draw all shapes and boolean boxes in SESSION."
  (scad-sketch-session-sync-active-shape-from-points session)
  (dolist (shape (scad-sketch-session-shapes session))
    (pcase (scad-sketch-shape-kind shape)
      ('polygon
       (scad-sketch--draw-one-polygon-shape svg transform session shape))
      ('circle
       (scad-sketch--draw-one-circle-shape svg transform session shape))))
  (when (scad-sketch-session-tree session)
    (scad-sketch--draw-boolean-boxes
     svg transform session (scad-sketch-session-tree session))))

(defun scad-sketch--draw-point-and-marks (svg transform session)
  "Draw all marks and the cursor point."
  (let* ((marks  (scad-sketch-session-marks session))
         (cursor (scad-sketch-session-point session)))
    (let ((ordered (reverse marks)))
      (cl-loop for a on ordered for b = (cadr a) when b do
               (scad-sketch--svg-line svg transform (car a) b
                                      :stroke "#008a2e" :stroke-width 1 :stroke-dasharray "4,4"))
      (when ordered
        (scad-sketch--svg-line svg transform (car (last ordered)) cursor
                               :stroke "#008a2e" :stroke-width 1 :stroke-dasharray "4,4")))
    (dolist (m (reverse marks))
      (let* ((screen  (funcall transform m))
             (current (equal m (car marks)))
             (color   (if current "#008a2e" "#50a870")))
        (svg-circle svg (nth 0 screen) (nth 1 screen) 6
                    :stroke color :stroke-width 2 :fill "#e2ffe9")
        (when current
          (svg-text svg "mark" :x (+ (nth 0 screen) 10) :y (+ (nth 1 screen) 4)
                    :font-size 12 :fill color))))
    (let ((p (funcall transform cursor)))
      (svg-circle svg (nth 0 p) (nth 1 p) 5
                  :stroke "#0057c2" :stroke-width 2 :fill "#dfefff")
      (svg-line svg (- (nth 0 p) 10) (nth 1 p) (+ (nth 0 p) 10) (nth 1 p)
                :stroke "#0057c2" :stroke-width 2)
      (svg-line svg (nth 0 p) (- (nth 1 p) 10) (nth 0 p) (+ (nth 1 p) 10)
                :stroke "#0057c2" :stroke-width 2)
      (svg-text svg "point" :x (+ (nth 0 p) 12) :y (+ (nth 1 p) 4)
                :font-size 12 :fill "#0057c2"))))

(defun scad-sketch--fmt-xy (xy)
  "Format XY pair compactly."
  (format "(%s, %s)" (scad-sketch--fmt-num (nth 0 xy)) (scad-sketch--fmt-num (nth 1 xy))))

(defun scad-sketch--draw-hud (svg session)
  "Draw the status bar."
  (let* ((marks     (scad-sketch-session-marks session))
         (mark-str  (cond ((null marks) "none")
                          ((= 1 (length marks))
                           (scad-sketch--fmt-xy (car marks)))
                          (t
                           (format "%s (+%d)"
                                   (scad-sketch--fmt-xy (car marks))
                                   (1- (length marks))))))
         (attention (scad-sketch--attention-ref session))
         (root      (or (and (scad-sketch-session-root-target session)
                             (scad-sketch-target-kind
                              (scad-sketch-session-root-target session)))
                        'array-only))
         (text      (format "%s  root=%s  shapes=%d  active=%s  grid=%s%s  point=%s  mark=%s  attn=%s  sel=%s  %s"
                            (scad-sketch-session-name session)
                            root
                            (length (scad-sketch-session-shapes session))
                            (scad-sketch-session-active-shape-id session)
                            (scad-sketch--fmt-num
                             (scad-sketch-session-grid session))
                            (scad-sketch-session-units session)
                            (scad-sketch--fmt-xy
                             (scad-sketch-session-point session))
                            mark-str
                            (scad-sketch--ref-summary attention)
                            (scad-sketch--selection-summary session)
                            (if (scad-sketch-session-dirty session)
                                "*dirty*"
                              "saved"))))
    (svg-rectangle svg 0 0 scad-sketch-canvas-width 28
                   :fill "#f8f8f8")
    (svg-text svg text
              :x 10
              :y 19
              :font-size 13
              :fill "#111111")))

(defun scad-sketch--render ()
  "Re-render the editor buffer."
  (let* ((session   (scad-sketch--assert-session))
         (svg       (svg-create scad-sketch-canvas-width scad-sketch-canvas-height))
         (bounds    (scad-sketch--bounds session))
         (transform (scad-sketch--transform bounds)))
    (svg-rectangle svg 0 0 scad-sketch-canvas-width scad-sketch-canvas-height :fill "#ffffff")
    (scad-sketch--draw-grid svg bounds transform session)
    (scad-sketch--draw-path svg transform session)
    (scad-sketch--draw-point-and-marks svg transform session)
    (scad-sketch--draw-hud svg session)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((beg (point)))
        (insert-image (svg-image svg :ascent 'center))
        (remove-text-properties beg (point) '(keymap nil)))
      (insert "\n\n")
      (insert (scad-sketch--emit-content session))
      (goto-char (point-min)))))


(defun scad-sketch--emit-content (session)
  "Return the live source preview for SESSION."
  (scad-sketch-session-preview session))

;;; Write-back / quit
(defun scad-sketch-write-back ()
  "Write the edited sketch back to the source buffer."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (source  (scad-sketch-session-source-buffer session)))
    (scad-sketch-session-write-back session)
    (scad-sketch--render)
    (message "Wrote scad-sketch `%s' back to %s"
             (scad-sketch-session-name session)
             (if (buffer-live-p source) (buffer-name source) "<dead buffer>"))))

(defun scad-sketch-quit ()
  "Quit the sketch editor and restore the window configuration."
  (interactive)
  (let ((session (scad-sketch--assert-session))
        (wconf   scad-sketch--window-config))
    (when (and (scad-sketch-session-dirty session)
               (y-or-n-p "Sketch has unwritten edits. Write back first? "))
      (scad-sketch-write-back))
    (kill-buffer (current-buffer))
    (when wconf
      (set-window-configuration wconf))))

(defun scad-sketch-help ()
  "Display a key binding summary in the echo area.
For full documentation use \\[describe-mode]."
  (interactive)
  (scad-sketch--assert-session)
  (message
   (concat "arrows=move cursor(clean)  C-arrows=coarse  M-arrows=fine | "
           "TAB/S-TAB=focus shape/point  ./,=cycle hovered  SPC=toggle selection  s=clear selection | "
           "S-arrows=move selected geometry(dirty) | "
           "p=append  i=insert  k=delete | "
           "m=set-mark  M=push  `=pop  '=jump  C=clear marks | "
           "R=radius  c=closed  l=line  r=rect | "
           "x/y=coord  X/Y=delta  d=dist  a=angle  g=grid | "
           "w=write  u=undo  q=quit  C-h m=full help")))


(provide 'scad-sketch-editor-mode)
;;; scad-sketch-editor-mode.el ends here
