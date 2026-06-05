;;; scad-sketch-editor--selection.el --- Selection/hover/attention -*- lexical-binding: t; -*-

;;; Commentary:

;; Manages which objects are selected, hovered, or receiving attention.
;;
;; Terminology:
;;   selection  - the explicit multi-object set the user has built up
;;   hover      - objects geometrically near the cursor point
;;   attention  - the single ref currently receiving keyboard focus;
;;                prefers the hovered object, falls back to focus-ref
;;
;; This file owns:
;;   - membership predicates for the selection set
;;   - toggle/expand logic (shape ↔ point invariants)
;;   - hover candidate computation (geometry)
;;   - attention resolution and normalization
;;   - selectable-ref enumeration for TAB cycling
;;   - selected-point-locs expansion (selection → (shape-id . index) pairs)
;;   - summary helpers

;;; Code:

(require 'cl-lib)
(require 'scad-sketch-session)
(require 'scad-sketch-editor--refs)

;;; Hover geometry

(defcustom scad-sketch-hover-radius-factor 0.75
  "Hover radius as a multiple of the current grid step."
  :type 'number :group 'scad-sketch)

(defun scad-sketch--hover-radius (session)
  "Return hover radius in model units for SESSION."
  (max (float (scad-sketch-session-fine-step session))
       (* scad-sketch-hover-radius-factor
          (max 0.0001 (float (scad-sketch-session-grid session))))))

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
             (u   (max 0.0 (min 1.0 raw)))
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
                   (<  y (max yi yj))
                   (<  x (+ xi (/ (* (- y yi) (- xj xi))
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
         (list (plist-get md :cx) (plist-get md :cy))))
      ('square
       (let ((pts (scad-sketch--square-corner-points shape)))
         (list (/ (+ (nth 0 (nth 0 pts)) (nth 0 (nth 2 pts))) 2.0)
               (/ (+ (nth 1 (nth 0 pts)) (nth 1 (nth 2 pts))) 2.0))))
      ('text
       (pcase-let ((`(,min-x ,max-x ,min-y ,max-y)
                    (scad-sketch--text-rough-bounds shape)))
         (list (/ (+ min-x max-x) 2.0)
               (/ (+ min-y max-y) 2.0))))
      ('polygon
       (let ((points (mapcar #'scad-sketch--point-xy
                             (scad-sketch-shape-points shape))))
         (if points
             (let ((sx 0.0) (sy 0.0) (n 0))
               (dolist (p points)
                 (setq sx (+ sx (nth 0 p)))
                 (setq sy (+ sy (nth 1 p)))
                 (setq n  (1+ n)))
               (list (/ sx n) (/ sy n)))
           (copy-sequence (scad-sketch-session-point session)))))
      (_ (copy-sequence (scad-sketch-session-point session))))))

(defun scad-sketch--shape-hovered-p (session shape)
  "Return non-nil if SESSION cursor is on/near SHAPE."
  (let ((p (scad-sketch-session-point session))
        (r (scad-sketch--hover-radius session)))
    (pcase (scad-sketch-shape-kind shape)
      ('circle
       (let* ((md (scad-sketch-shape-metadata shape))
              (cx (plist-get md :cx))
              (cy (plist-get md :cy))
              (cr (plist-get md :r))
              (d  (scad-sketch--distance p (list cx cy))))
         (or (<= (abs (- d cr)) r) (< d cr))))
      ('square
       (let* ((pts  (scad-sketch--square-corner-points shape))
              (near nil))
         (dotimes (i 4)
           (let ((a (nth i pts))
                 (b (nth (mod (1+ i) 4) pts)))
             (when (<= (scad-sketch--distance-to-segment p a b) r)
               (setq near t))))
         (or near (scad-sketch--point-in-polygon-p p pts))))
      ('text
       (pcase-let ((`(,min-x ,max-x ,min-y ,max-y)
                    (scad-sketch--text-rough-bounds shape)))
         (and (<= (- min-x r) (nth 0 p) (+ max-x r))
              (<= (- min-y r) (nth 1 p) (+ max-y r)))))
      ('polygon
       (let* ((points (mapcar #'scad-sketch--point-xy
                              (scad-sketch-shape-points shape)))
              (n      (length points))
              (near   nil))
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

(defun scad-sketch--square-corner-points (shape)
  "Return square SHAPE corner points in model coordinates.

Corners are ordered like a polygon:
  0 lower-left/origin, 1 lower-right, 2 upper-right, 3 upper-left."
  (let* ((md    (scad-sketch-shape-metadata shape))
         (x     (float (or (plist-get md :x) 0.0)))
         (y     (float (or (plist-get md :y) 0.0)))
         (w     (float (or (plist-get md :w) 0.0)))
         (h     (float (or (plist-get md :h) 0.0)))
         (a     (* pi (/ (float (or (plist-get md :angle) 0.0)) 180.0)))
         (ux    (cos a))
         (uy    (sin a))
         (vx    (- (sin a)))
         (vy    (cos a))
         (p0    (list x y))
         (p1    (list (+ x (* ux w))
                      (+ y (* uy w))))
         (p2    (list (+ x (* ux w) (* vx h))
                      (+ y (* uy w) (* vy h))))
         (p3    (list (+ x (* vx h))
                      (+ y (* vy h)))))
    (list p0 p1 p2 p3)))

(defun scad-sketch--primitive-handle-count (shape)
  "Return the number of point-like handles for primitive SHAPE."
  (pcase (scad-sketch-shape-kind shape)
    ('circle 1)
    ('square 4)
    (_ 0)))

(defun scad-sketch--primitive-handle-xy (shape idx)
  "Return model-space XY for primitive SHAPE handle IDX, or nil."
  (pcase (scad-sketch-shape-kind shape)
    ('circle
     (when (= idx 0)
       (let* ((md (scad-sketch-shape-metadata shape))
              (cx (float (or (plist-get md :cx) 0.0)))
              (cy (float (or (plist-get md :cy) 0.0)))
              (r  (float (or (plist-get md :r) 0.0))))
         (list (+ cx r) cy))))
    ('square
     (nth idx (scad-sketch--square-corner-points shape)))
    (_ nil)))

(defun scad-sketch--text-rough-bounds (shape)
  "Return approximate bounds for text SHAPE as (MIN-X MAX-X MIN-Y MAX-Y)."
  (let* ((md     (scad-sketch-shape-metadata shape))
         (str    (or (plist-get md :str) ""))
         (x      (float (or (plist-get md :x) 0.0)))
         (y      (float (or (plist-get md :y) 0.0)))
         (size   (float (or (plist-get md :size) 10.0)))
         (width  (max size (* size 0.6 (max 1 (string-width str)))))
         (height size))
    (list x (+ x width) y (+ y height))))

(defun scad-sketch--hover-candidates (session)
  "Return hovered refs under SESSION's current point.

Point refs are listed before shape refs so exact vertex/handle hovers take
attention priority over the containing shape."
  (let ((p          (scad-sketch-session-point session))
        (r          (scad-sketch--hover-radius session))
        candidates)
    (dolist (shape (scad-sketch-session-shapes session))
      (let ((shape-id (scad-sketch-shape-id shape)))
        (pcase (scad-sketch-shape-kind shape)
          ('polygon
           (cl-loop for model-point in (scad-sketch-shape-points shape)
                    for idx from 0
                    for xy = (scad-sketch--point-xy model-point)
                    when (<= (scad-sketch--distance p xy) r)
                    do (push (scad-sketch--point-ref idx shape-id) candidates)))
          ((or 'circle 'square)
           (dotimes (idx (scad-sketch--primitive-handle-count shape))
             (let ((xy (scad-sketch--primitive-handle-xy shape idx)))
               (when (and xy (<= (scad-sketch--distance p xy) r))
                 (push (scad-sketch--point-ref idx shape-id) candidates))))))
        (when (scad-sketch--shape-hovered-p session shape)
          (push (scad-sketch--shape-ref shape-id) candidates))))
    (nreverse candidates)))

;;; Selectable refs (TAB cycle)
(defun scad-sketch--selectable-refs (session)
  "Return all selectable refs for SESSION in tab-cycle order."
  (let (refs)
    (dolist (shape (scad-sketch-session-shapes session))
      (let ((shape-id (scad-sketch-shape-id shape)))
        (push (scad-sketch--shape-ref shape-id) refs)
        (pcase (scad-sketch-shape-kind shape)
          ('polygon
           (cl-loop for _pt in (scad-sketch-shape-points shape)
                    for idx from 0
                    do (push (scad-sketch--point-ref idx shape-id) refs)))
          ((or 'circle 'square)
           (dotimes (idx (scad-sketch--primitive-handle-count shape))
             (push (scad-sketch--point-ref idx shape-id) refs))))))
    (nreverse refs)))

(defun scad-sketch--ref-anchor (session ref)
  "Return a model-space anchor point for REF."
  (pcase (scad-sketch--ref-kind ref)
    ('shape
     (scad-sketch--shape-center session (scad-sketch--ref-shape-id ref)))
    ('point
     (let* ((shape (scad-sketch-session-shape-by-id
                    session (scad-sketch--ref-shape-id ref)))
            (idx   (scad-sketch--ref-index ref)))
       (pcase (and shape (scad-sketch-shape-kind shape))
         ('polygon
          (let ((point (nth idx (scad-sketch-shape-points shape))))
            (if point
                (scad-sketch--point-xy point)
              (copy-sequence (scad-sketch-session-point session)))))
         ((or 'circle 'square)
          (or (scad-sketch--primitive-handle-xy shape idx)
              (copy-sequence (scad-sketch-session-point session))))
         (_
          (copy-sequence (scad-sketch-session-point session))))))
    (_ (copy-sequence (scad-sketch-session-point session)))))

;;; Selection membership predicates

(defun scad-sketch--selection-contains-ref-p (session ref)
  "Return non-nil if SESSION selection explicitly contains REF."
  (cl-some (lambda (selected)
             (scad-sketch--same-ref-p selected ref))
           (scad-sketch-session-selection session)))

(defun scad-sketch--shape-selected-p (session &optional shape-id)
  "Return non-nil if SHAPE-ID is explicitly selected in SESSION."
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

;;; Toggle helpers

(defun scad-sketch--remove-shape-and-subpoints (selection shape-id)
  "Return SELECTION with SHAPE-ID and all of its point refs removed."
  (cl-remove-if (lambda (ref)
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

Invariants:
  - A shape ref and its subpoint refs cannot coexist.
  - Toggling a shape selects the whole shape and removes subpoints.
  - Toggling a point while its shape is selected converts the shape
    selection into all point refs except that point."
  (let* ((kind      (scad-sketch--ref-kind ref))
         (shape-id  (scad-sketch--ref-shape-id ref))
         (selection (scad-sketch-session-selection session)))
    (pcase kind
      ('shape
       (if (scad-sketch--shape-selected-p session shape-id)
           (setf (scad-sketch-session-selection session)
                 (cl-remove-if (lambda (s) (scad-sketch--same-ref-p s ref))
                               selection))
         (setf (scad-sketch-session-selection session)
               (cons ref (scad-sketch--remove-shape-and-subpoints
                          selection shape-id)))))
      ('point
       (let ((idx (scad-sketch--ref-index ref)))
         (cond
          ((scad-sketch--shape-selected-p session shape-id)
           (setf (scad-sketch-session-selection session)
                 (append
                  (scad-sketch--all-point-refs-except session shape-id idx)
                  (scad-sketch--remove-shape-and-subpoints
                   selection shape-id))))
          ((scad-sketch--selection-contains-ref-p session ref)
           (setf (scad-sketch-session-selection session)
                 (cl-remove-if (lambda (s) (scad-sketch--same-ref-p s ref))
                               selection)))
          (t
           (push ref (scad-sketch-session-selection session)))))))))

;;; Attention

(defun scad-sketch--attention-ref (session)
  "Return the ref currently receiving attention in SESSION."
  (let* ((candidates (scad-sketch--hover-candidates session))
         (n          (length candidates)))
    (if (> n 0)
        (nth (mod (or (scad-sketch-session-hover-index session) 0) n)
             candidates)
      (scad-sketch-session-focus-ref session))))

(defun scad-sketch--normalize-attention (session)
  "Clamp hover index and keep legacy selected-index aligned with attention."
  (let* ((candidates (scad-sketch--hover-candidates session))
         (n          (length candidates)))
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

;;; Selected point expansion

(defun scad-sketch--selected-shape-ids (session)
  "Return explicitly selected shape ids in SESSION."
  (delq nil
        (mapcar (lambda (ref)
                  (when (eq (scad-sketch--ref-kind ref) 'shape)
                    (scad-sketch--ref-shape-id ref)))
                (scad-sketch-session-selection session))))

(defun scad-sketch--selected-point-locs (session &optional fallback-to-active)
  "Return selected point/handle locations as (SHAPE-ID . INDEX) conses.

Polygon shape selections expand to all vertices.  Primitive shape selections
remain shape selections and are moved as whole shapes by `scad-sketch--move-selected'."
  (let (locs)
    (dolist (ref (scad-sketch-session-selection session))
      (pcase (scad-sketch--ref-kind ref)
        ('shape
         (let* ((shape-id (scad-sketch--ref-shape-id ref))
                (shape    (scad-sketch-session-shape-by-id session shape-id)))
           (when (and shape (eq (scad-sketch-shape-kind shape) 'polygon))
             (dotimes (i (length (scad-sketch-shape-points shape)))
               (push (cons shape-id i) locs)))))
        ('point
         (let* ((shape-id (scad-sketch--ref-shape-id ref))
                (idx      (scad-sketch--ref-index ref))
                (shape    (scad-sketch-session-shape-by-id session shape-id)))
           (when (and shape idx (>= idx 0))
             (pcase (scad-sketch-shape-kind shape)
               ('polygon
                (when (< idx (length (scad-sketch-shape-points shape)))
                  (push (cons shape-id idx) locs)))
               ((or 'circle 'square)
                (when (< idx (scad-sketch--primitive-handle-count shape))
                  (push (cons shape-id idx) locs)))))))))
    (setq locs (delete-dups (nreverse locs)))
    (if (and (null locs) fallback-to-active
             (scad-sketch-session-active-shape-id session)
             (scad-sketch-session-selected-index session))
        (list (cons (scad-sketch-session-active-shape-id session)
                    (scad-sketch-session-selected-index session)))
      locs)))

;;; Summaries

(defun scad-sketch--selection-summary (session)
  "Return compact text describing SESSION selection."
  (let ((selection (scad-sketch-session-selection session)))
    (if (null selection)
        "none"
      (format "%d item%s"
              (length selection)
              (if (= 1 (length selection)) "" "s")))))

(provide 'scad-sketch-editor--selection)
;;; scad-sketch-editor--selection.el ends here
