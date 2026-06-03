;;; scad-sketch-geometry.el --- Pure geometry helpers for scad-sketch -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure model/geometry helpers for scad-sketch.  This file should not touch
;; source buffers, editor buffers, windows, or SVG drawing objects.
;;
;; Model points use the parser/editor convention: (x y r), where r defaults to
;; 0 and represents a polyRound-style corner radius.

;;; Code:

(require 'cl-lib)

;;;; Point helpers

(defun scad-sketch--point-xy (point)
  "Return the visible [x y] of model POINT."
  (list (float (or (nth 0 point) 0))
        (float (or (nth 1 point) 0))))

(defun scad-sketch--point-radius (point)
  "Return the polyRound radius of POINT, or 0."
  (or (nth 2 point) 0))

(defun scad-sketch--make-model-point (xy &optional old-point)
  "Build a model [x y r] point from visible XY.
Preserves radius from OLD-POINT if provided."
  (list (float (nth 0 xy))
        (float (nth 1 xy))
        (float (or (nth 2 old-point) 0))))

(defun scad-sketch--replace-nth (n value list)
  "Return LIST with element N replaced by VALUE."
  (let ((copy (copy-sequence list)))
    (setf (nth n copy) value)
    copy))

;;;; Selection / hover / attention

(defcustom scad-sketch-hover-radius-factor 0.75
  "Hover radius as a multiple of the current grid step."
  :type 'number :group 'scad-sketch)

(defun scad-sketch--shape-id ()
  "Return the current single-shape id.

This is intentionally a function so the later object-tree implementation has
one obvious place to start replacing the current single-shape assumption."
  'shape-0)

(defun scad-sketch--shape-ref ()
  "Return the current shape selection ref."
  (list :kind 'shape :shape-id (scad-sketch--shape-id)))

(defun scad-sketch--point-ref (idx)
  "Return a point selection ref for IDX."
  (list :kind 'point :shape-id (scad-sketch--shape-id) :index idx))

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
  (let ((shape-ref (list :kind 'shape
                         :shape-id (or shape-id (scad-sketch--shape-id)))))
    (scad-sketch--selection-contains-ref-p session shape-ref)))

(defun scad-sketch--point-selected-p (session idx)
  "Return non-nil if point IDX is selected in SESSION.

A selected shape makes all of its points effectively selected."
  (or (scad-sketch--shape-selected-p session)
      (scad-sketch--selection-contains-ref-p
       session (scad-sketch--point-ref idx))))

(defun scad-sketch--remove-shape-and-subpoints (selection shape-id)
  "Return SELECTION with SHAPE-ID and all of its point refs removed."
  (cl-remove-if
   (lambda (ref)
     (eq (scad-sketch--ref-shape-id ref) shape-id))
   selection))

(defun scad-sketch--all-point-refs-except (session idx)
  "Return point refs for every SESSION point except IDX."
  (let (refs)
    (dotimes (i (length (scad-sketch-session-points session)))
      (unless (= i idx)
        (push (scad-sketch--point-ref i) refs)))
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
                  (scad-sketch--all-point-refs-except session idx)
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

(defun scad-sketch--shape-center (session)
  "Return the model-space center of SESSION's current shape."
  (let ((points (mapcar #'scad-sketch--point-xy
                        (scad-sketch-session-points session))))
    (if points
        (let ((sx 0.0) (sy 0.0) (n 0))
          (dolist (p points)
            (setq sx (+ sx (nth 0 p)))
            (setq sy (+ sy (nth 1 p)))
            (setq n (1+ n)))
          (list (/ sx n) (/ sy n)))
      (copy-sequence (scad-sketch-session-point session)))))

(defun scad-sketch--shape-hovered-p (session)
  "Return non-nil if SESSION point is on/near the current shape."
  (let* ((p (scad-sketch-session-point session))
         (points (mapcar #'scad-sketch--point-xy
                         (scad-sketch-session-points session)))
         (n (length points))
         (r (scad-sketch--hover-radius session))
         (near nil))
    (when (>= n 2)
      (cl-loop for rest on points
               for a = (car rest)
               for b = (cadr rest)
               when (and b (<= (scad-sketch--distance-to-segment p a b) r))
               do (setq near t))
      (when (and (not near)
                 (scad-sketch-session-closed session)
                 (> n 2)
                 (<= (scad-sketch--distance-to-segment
                      p (car (last points)) (car points))
                     r))
        (setq near t))
      (or near
          (and (scad-sketch-session-closed session)
               (> n 2)
               (scad-sketch--point-in-polygon-p p points))))))

(defun scad-sketch--hover-candidates (session)
  "Return hovered refs under SESSION's current point.

Point refs are listed before the shape ref so exact vertex hovers get
attention before the containing polygon."
  (let ((p (scad-sketch-session-point session))
        (r (scad-sketch--hover-radius session))
        candidates)
    (cl-loop for model-point in (scad-sketch-session-points session)
             for idx from 0
             for xy = (scad-sketch--point-xy model-point)
             when (<= (scad-sketch--distance p xy) r)
             do (push (scad-sketch--point-ref idx) candidates))
    (setq candidates (nreverse candidates))
    (when (scad-sketch--shape-hovered-p session)
      (setq candidates (append candidates (list (scad-sketch--shape-ref)))))
    candidates))

(defun scad-sketch--selectable-refs (session)
  "Return all selectable refs for SESSION in tab-cycle order."
  (append
   (list (scad-sketch--shape-ref))
   (cl-loop for _pt in (scad-sketch-session-points session)
            for idx from 0
            collect (scad-sketch--point-ref idx))))

(defun scad-sketch--ref-anchor (session ref)
  "Return a model-space anchor point for REF."
  (pcase (scad-sketch--ref-kind ref)
    ('shape
     (scad-sketch--shape-center session))
    ('point
     (let ((point (nth (scad-sketch--ref-index ref)
                       (scad-sketch-session-points session))))
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
        (if (eq (scad-sketch--ref-kind attention) 'point)
            (setf (scad-sketch-session-selected-index session)
                  (scad-sketch--ref-index attention))
          (setf (scad-sketch-session-selected-index session) nil))))))

(defun scad-sketch--selected-point-indices (session &optional fallback-to-active)
  "Return selected point indices in SESSION.

Shape selections expand to all points.  When no explicit selection exists and
FALLBACK-TO-ACTIVE is non-nil, return the legacy active point."
  (let (indices)
    (dolist (ref (scad-sketch-session-selection session))
      (pcase (scad-sketch--ref-kind ref)
        ('shape
         (dotimes (i (length (scad-sketch-session-points session)))
           (push i indices)))
        ('point
         (let ((idx (scad-sketch--ref-index ref)))
           (when (and idx
                      (>= idx 0)
                      (< idx (length (scad-sketch-session-points session))))
             (push idx indices))))))
    (setq indices (delete-dups (nreverse indices)))
    (if (and (null indices) fallback-to-active
             (scad-sketch-session-selected-index session))
        (list (scad-sketch-session-selected-index session))
      indices)))

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
    ('shape "shape")
    ('point (format "point[%s]" (scad-sketch--ref-index ref)))
    (_ "none")))

;;;; Movement and construction helpers

(defun scad-sketch--move-xy (xy dx dy)
  "Return XY shifted by DX, DY."
  (list (+ (float (nth 0 xy)) dx) (+ (float (nth 1 xy)) dy)))

(defun scad-sketch--snap-to-grid (v grid)
  "Round V to the nearest multiple of GRID."
  (* grid (round (/ v grid))))

(defun scad-sketch--snap-xy (xy grid)
  "Snap both coordinates of XY to GRID."
  (list (scad-sketch--snap-to-grid (nth 0 xy) grid)
        (scad-sketch--snap-to-grid (nth 1 xy) grid)))

(defun scad-sketch--geometry-line-points (marks point)
  "Return model points for MARKS followed by POINT.
MARKS are stored newest-first by the editor; the returned geometry uses oldest
mark first, then POINT.  Signals if MARKS is nil."
  (unless marks (user-error "No marks set"))
  (append (mapcar #'scad-sketch--make-model-point (reverse marks))
          (list (scad-sketch--make-model-point point))))

(defun scad-sketch--geometry-rectangle-points (mark point)
  "Return rectangle corner model points from MARK to POINT."
  (unless mark (user-error "No marks set"))
  (let ((x1 (nth 0 mark)) (y1 (nth 1 mark))
        (x2 (nth 0 point)) (y2 (nth 1 point)))
    (mapcar #'scad-sketch--make-model-point
            (list (list x1 y1) (list x2 y1) (list x2 y2) (list x1 y2)))))

(defun scad-sketch--geometry-point-at-distance (mark point distance)
  "Return POINT moved to DISTANCE from MARK, preserving current angle."
  (let* ((angle (atan (- (nth 1 point) (nth 1 mark))
                      (- (nth 0 point) (nth 0 mark)))))
    (list (+ (nth 0 mark) (* (float distance) (cos angle)))
          (+ (nth 1 mark) (* (float distance) (sin angle))))))

(defun scad-sketch--geometry-point-at-angle (mark point degrees)
  "Return POINT rotated around MARK to DEGREES, preserving distance."
  (let* ((dx    (- (nth 0 point) (nth 0 mark)))
         (dy    (- (nth 1 point) (nth 1 mark)))
         (dist  (sqrt (+ (* dx dx) (* dy dy))))
         (angle (* pi (/ (float degrees) 180.0))))
    (list (+ (nth 0 mark) (* dist (cos angle)))
          (+ (nth 1 mark) (* dist (sin angle))))))

;;;; Local-coordinate transform helpers for parser AST nodes

(defun scad-sketch-geometry-translate-point (point tx ty)
  "Translate model POINT by TX and TY, preserving radius."
  (list (+ (float (nth 0 point)) (float tx))
        (+ (float (nth 1 point)) (float ty))
        (float (or (nth 2 point) 0))))

(defun scad-sketch-geometry-rotate-point (point degrees)
  "Rotate model POINT around the origin by DEGREES, preserving radius."
  (let* ((angle (* pi (/ (float degrees) 180.0)))
         (x (float (nth 0 point)))
         (y (float (nth 1 point))))
    (list (- (* x (cos angle)) (* y (sin angle)))
          (+ (* x (sin angle)) (* y (cos angle)))
          (float (or (nth 2 point) 0)))))

(defun scad-sketch-geometry-scale-point (point sx sy)
  "Scale model POINT by SX and SY, preserving radius for now."
  (list (* (float (nth 0 point)) (float sx))
        (* (float (nth 1 point)) (float sy))
        (float (or (nth 2 point) 0))))

(defun scad-sketch-geometry-mirror-point (point mx my)
  "Mirror model POINT across axes indicated by MX and MY.
This matches the sketch editor's local-coordinate representation and preserves
radius.  Non-zero MX negates X; non-zero MY negates Y."
  (list (* (float (nth 0 point)) (if (zerop (float mx)) 1 -1))
        (* (float (nth 1 point)) (if (zerop (float my)) 1 -1))
        (float (or (nth 2 point) 0))))

(defun scad-sketch-geometry-transform-points (points fn &rest args)
  "Apply point transform FN with ARGS to POINTS."
  (mapcar (lambda (p) (apply fn (cons p args))) points))

;;;; polyRound arc geometry

;;; polyRound arc geometry

(defun scad-sketch--corner-unit-vecs (A B C)
  "Return (U V HALF-ANGLE) for the corner at B, or nil if degenerate."
  (let* ((bx (nth 0 B)) (by (nth 1 B))
         (ba (list (- (nth 0 A) bx) (- (nth 1 A) by)))
         (bc (list (- (nth 0 C) bx) (- (nth 1 C) by)))
         (len-ba (sqrt (+ (* (nth 0 ba) (nth 0 ba)) (* (nth 1 ba) (nth 1 ba)))))
         (len-bc (sqrt (+ (* (nth 0 bc) (nth 0 bc)) (* (nth 1 bc) (nth 1 bc))))))
    (when (and (> len-ba 1e-10) (> len-bc 1e-10))
      (let* ((u    (list (/ (nth 0 ba) len-ba) (/ (nth 1 ba) len-ba)))
             (v    (list (/ (nth 0 bc) len-bc) (/ (nth 1 bc) len-bc)))
             (dot  (max -1.0 (min 1.0 (+ (* (nth 0 u) (nth 0 v))
                                          (* (nth 1 u) (nth 1 v))))))
             (half (/ (acos dot) 2)))
        (when (> (sin half) 1e-10)
          (list u v half))))))

(defun scad-sketch--corner-geometry-from-tlens (B u v half t1-len t2-len)
  "Build a corner plist from pre-clamped tangent lengths."
  (let* ((bx (nth 0 B)) (by (nth 1 B))
         (t-len   (min t1-len t2-len))
         (actual-r (* t-len (tan half)))
         (t1    (list (+ bx (* t-len (nth 0 u))) (+ by (* t-len (nth 1 u)))))
         (t2    (list (+ bx (* t-len (nth 0 v))) (+ by (* t-len (nth 1 v)))))
         (cross (- (* (nth 0 u) (nth 1 v)) (* (nth 1 u) (nth 0 v))))
         (sweep (if (> cross 0) 1 0)))
    (list :t1 t1 :t2 t2 :radius actual-r :sweep sweep)))

(defun scad-sketch--corner-geometry (A B C r)
  "Compute polyRound arc geometry for corner at B with radius R.
Returns plist (:t1 :t2 :radius :sweep), or nil if degenerate."
  (when (and r (> r 0))
    (let ((uvh (scad-sketch--corner-unit-vecs A B C)))
      (when uvh
        (let* ((u    (nth 0 uvh)) (v (nth 1 uvh)) (half (nth 2 uvh))
               (bx   (nth 0 B))  (by (nth 1 B))
               (ba   (list (- (nth 0 A) bx) (- (nth 1 A) by)))
               (bc   (list (- (nth 0 C) bx) (- (nth 1 C) by)))
               (l-ba (sqrt (+ (* (nth 0 ba) (nth 0 ba)) (* (nth 1 ba) (nth 1 ba)))))
               (l-bc (sqrt (+ (* (nth 0 bc) (nth 0 bc)) (* (nth 1 bc) (nth 1 bc)))))
               (t-len (min (/ r (tan half)) (* l-ba 0.49) (* l-bc 0.49))))
          (scad-sketch--corner-geometry-from-tlens B u v half t-len t-len))))))

(defun scad-sketch--pixel-radius (model-r transform)
  "Convert model-space radius MODEL-R to screen pixels via TRANSFORM."
  (let* ((o  (funcall transform '(0 0)))
         (r  (funcall transform (list model-r 0)))
         (dx (- (nth 0 r) (nth 0 o)))
         (dy (- (nth 1 r) (nth 1 o))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun scad-sketch--edge-len (P Q)
  "Model-space distance between points P and Q."
  (let ((dx (- (nth 0 Q) (nth 0 P)))
        (dy (- (nth 1 Q) (nth 1 P))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun scad-sketch--any-radius-p (points)
  "Return non-nil if any point in POINTS has a non-zero radius."
  (cl-some (lambda (p) (and (nth 2 p) (> (nth 2 p) 0))) points))

(defun scad-sketch--polyround-path-d (points closed transform)
  "Build an SVG path data string for POINTS with polyRound radii.
Uses edge-aware tangent-length clamping to avoid crossed segments."
  (let ((n (length points)))
    (when (>= n 2)
      (let* ((t-out (make-vector n 0.0))
             (t-in  (make-vector n 0.0))
             (uvh-vec (make-vector n nil)))
        (dotimes (i n)
          (let ((r (nth 2 (nth i points))))
            (when (and r (> r 0))
              (let* ((prev (cond ((> i 0)      (nth (1- i) points))
                                 (closed        (nth (1- n) points))))
                     (next (cond ((< i (1- n)) (nth (1+ i) points))
                                 (closed        (nth 0 points)))))
                (when (and prev next)
                  (let* ((A   (scad-sketch--point-xy prev))
                         (B   (scad-sketch--point-xy (nth i points)))
                         (C   (scad-sketch--point-xy next))
                         (uvh (scad-sketch--corner-unit-vecs A B C)))
                    (when uvh
                      (aset uvh-vec i uvh)
                      (let ((t-ideal (/ r (tan (nth 2 uvh)))))
                        (aset t-in  i t-ideal)
                        (aset t-out i t-ideal)))))))))
        (dotimes (i n)
          (let* ((j    (mod (1+ i) n))
                 (Pi   (scad-sketch--point-xy (nth i points)))
                 (Pj   (scad-sketch--point-xy (nth j points)))
                 (edge (scad-sketch--edge-len Pi Pj))
                 (sum  (+ (aref t-out i) (aref t-in j))))
            (when (and (or closed (< i (1- n)))
                       (> sum (* edge 0.999)))
              (let ((scale (/ (* edge 0.499) sum)))
                (aset t-out i (* (aref t-out i) scale))
                (aset t-in  j (* (aref t-in  j) scale))))))
        (let ((corners (make-vector n nil)))
          (dotimes (i n)
            (let ((uvh (aref uvh-vec i)))
              (when uvh
                (aset corners i
                      (scad-sketch--corner-geometry-from-tlens
                       (scad-sketch--point-xy (nth i points))
                       (nth 0 uvh) (nth 1 uvh) (nth 2 uvh)
                       (aref t-in i) (aref t-out i))))))
          (let* ((c0       (aref corners 0))
                 (start-xy (if (and c0 closed)
                               (funcall transform (plist-get c0 :t1))
                             (funcall transform (scad-sketch--point-xy (nth 0 points)))))
                 (fmt      (lambda (xy)
                             (format "%.3f %.3f" (float (nth 0 xy)) (float (nth 1 xy)))))
                 (parts    (list (format "M %s" (funcall fmt start-xy)))))
            (dotimes (i n)
              (let* ((corner (aref corners i))
                     (pt-s   (funcall transform (scad-sketch--point-xy (nth i points)))))
                (if corner
                    (let* ((t1s   (funcall transform (plist-get corner :t1)))
                           (t2s   (funcall transform (plist-get corner :t2)))
                           (rs    (scad-sketch--pixel-radius
                                   (plist-get corner :radius) transform))
                           (sweep (plist-get corner :sweep)))
                      (push (format "L %s" (funcall fmt t1s)) parts)
                      (push (format "A %.3f %.3f 0 0 %d %s" rs rs sweep (funcall fmt t2s)) parts))
                  (push (format "L %s" (funcall fmt pt-s)) parts))))
            (when closed (push "Z" parts))
            (mapconcat #'identity (nreverse parts) " ")))))))


;;;; Formatting helpers

;;; Output

(defun scad-sketch--fmt-num (n)
  "Format N compactly for OpenSCAD."
  (let ((x (float n)))
    (if (< (abs (- x (round x))) 0.000001)
        (number-to-string (round x))
      (let ((s (format "%.4f" x)))
        (setq s (replace-regexp-in-string "0+\\'" "" s))
        (setq s (replace-regexp-in-string "\\.\\'" "" s))
        (if (or (string= s "-0") (string= s "")) "0" s)))))

(defun scad-sketch--emit-point (point use-radii)
  "Format one model POINT.  When USE-RADII is non-nil emit [x, y, r]."
  (if use-radii
      (format "[%s, %s, %s]"
              (scad-sketch--fmt-num (nth 0 point))
              (scad-sketch--fmt-num (nth 1 point))
              (scad-sketch--fmt-num (nth 2 point)))
    (format "[%s, %s]"
            (scad-sketch--fmt-num (nth 0 point))
            (scad-sketch--fmt-num (nth 1 point)))))


(provide 'scad-sketch-geometry)
;;; scad-sketch-geometry.el ends here
