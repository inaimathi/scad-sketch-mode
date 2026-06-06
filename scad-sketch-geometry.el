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
         (angle (* float-pi (/ (float degrees) 180.0))))
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
  (let* ((angle (* float-pi (/ (float degrees) 180.0)))
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
