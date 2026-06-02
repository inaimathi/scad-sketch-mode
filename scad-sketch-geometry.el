;;; scad-sketch-geometry.el --- Spatial geometry for scad-sketch -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure geometric functions used by the scad-sketch editor.  Nothing in this
;; file touches Emacs buffers, sessions, or UI state.  The only dependency is
;; `scad-sketch-parse' (for node plist shape types and the polygon-points
;; resolver passed in by callers).
;;
;; PUBLIC API
;; ----------
;; Point / polygon tests:
;;   scad-sketch-geo--point-in-polygon-p  X Y PTS
;;   scad-sketch-geo--point-in-node-p     X Y NODE POLYGON-POINTS-FN
;;
;; Bounding boxes  →  (min-x max-x min-y max-y):
;;   scad-sketch-geo--node-bbox      NODE POLYGON-POINTS-FN
;;   scad-sketch-geo--node-bbox-area NODE POLYGON-POINTS-FN
;;
;; polyRound corner geometry:
;;   scad-sketch-geo--corner-unit-vecs            A B C
;;   scad-sketch-geo--corner-geometry-from-tlens  B U V HALF T1 T2
;;   scad-sketch-geo--corner-geometry             A B C R
;;   scad-sketch-geo--any-radius-p                POINTS
;;   scad-sketch-geo--edge-len                    P Q
;;
;; SVG pixel helpers (depend on a transform closure TF):
;;   scad-sketch-geo--pixel-radius  MODEL-R TF
;;   scad-sketch-geo--polyround-path-d  POINTS CLOSED TF
;;
;; Number formatting (shared with render layer):
;;   scad-sketch-geo--fmt-num  N
;;   scad-sketch-geo--fmt-xy   XY

;;; Code:

(require 'cl-lib)

;;;; ── Point / polygon containment ───────────────────────────────────────────

(defun scad-sketch-geo--point-in-polygon-p (x y pts)
  "Ray-casting test: non-nil when (X Y) is inside polygon PTS.
PTS is a list of (x y …) points.  Requires at least three points."
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

(defun scad-sketch-geo--point-in-node-p (x y node polygon-points-fn)
  "Return non-nil when (X Y) is geometrically inside NODE.
POLYGON-POINTS-FN is called as (fn node) and should return the resolved
list of [x y r] points for polygon/array nodes."
  (let ((type (plist-get node :type)))
    (cond
     ((eq type 'circle)
      (let* ((cx (plist-get node :cx))
             (cy (plist-get node :cy))
             (r  (plist-get node :r))
             (dx (- x cx)) (dy (- y cy)))
        (< (+ (* dx dx) (* dy dy)) (* r r))))
     ((eq type 'square)
      (let ((sx (plist-get node :x)) (sy (plist-get node :y))
            (sw (plist-get node :w)) (sh (plist-get node :h)))
        (and (>= x sx) (<= x (+ sx sw))
             (>= y sy) (<= y (+ sy sh)))))
     ((memq type '(polygon array))
      (scad-sketch-geo--point-in-polygon-p
       x y (or (funcall polygon-points-fn node) '())))
     ((memq type '(difference union intersection))
      (cl-some (lambda (c)
                 (scad-sketch-geo--point-in-node-p x y c polygon-points-fn))
               (plist-get node :children)))
     ((eq type 'translate)
      (scad-sketch-geo--point-in-node-p
       (- x (plist-get node :tx))
       (- y (plist-get node :ty))
       (plist-get node :child)
       polygon-points-fn))
     (t nil))))

;;;; ── Bounding boxes ─────────────────────────────────────────────────────────

(defun scad-sketch-geo--node-bbox (node polygon-points-fn)
  "Return (min-x max-x min-y max-y) bounding box for NODE.
POLYGON-POINTS-FN is called as (fn node) for polygon/array nodes."
  (let ((type (plist-get node :type)))
    (cond
     ((eq type 'circle)
      (let ((cx (plist-get node :cx))
            (cy (plist-get node :cy))
            (r  (plist-get node :r)))
        (list (- cx r) (+ cx r) (- cy r) (+ cy r))))
     ((eq type 'square)
      (list (plist-get node :x)
            (+ (plist-get node :x) (plist-get node :w))
            (plist-get node :y)
            (+ (plist-get node :y) (plist-get node :h))))
     ((memq type '(polygon array))
      (let ((pts (or (funcall polygon-points-fn node) '())))
        (if pts
            (list (apply #'min (mapcar #'car  pts))
                  (apply #'max (mapcar #'car  pts))
                  (apply #'min (mapcar #'cadr pts))
                  (apply #'max (mapcar #'cadr pts)))
          (list 0 1 0 1))))
     ((memq type '(difference union intersection))
      (let ((boxes (mapcar (lambda (c)
                             (scad-sketch-geo--node-bbox c polygon-points-fn))
                           (plist-get node :children))))
        (if boxes
            (list (apply #'min (mapcar #'car    boxes))
                  (apply #'max (mapcar #'cadr   boxes))
                  (apply #'min (mapcar #'caddr  boxes))
                  (apply #'max (mapcar #'cadddr boxes)))
          (list 0 1 0 1))))
     ((eq type 'translate)
      (let ((box (scad-sketch-geo--node-bbox (plist-get node :child) polygon-points-fn))
            (tx  (plist-get node :tx))
            (ty  (plist-get node :ty)))
        (list (+ (nth 0 box) tx) (+ (nth 1 box) tx)
              (+ (nth 2 box) ty) (+ (nth 3 box) ty))))
     (t (list 0 1 0 1)))))

(defun scad-sketch-geo--node-bbox-area (node polygon-points-fn)
  "Return bounding-box area of NODE (used for hover depth-sorting)."
  (let ((box (scad-sketch-geo--node-bbox node polygon-points-fn)))
    (* (- (nth 1 box) (nth 0 box))
       (- (nth 3 box) (nth 2 box)))))

;;;; ── polyRound corner geometry ──────────────────────────────────────────────

(defun scad-sketch-geo--corner-unit-vecs (A B C)
  "Compute unit vectors and half-angle for the corner at B between edges BA, BC.
Returns (U V HALF-ANGLE) or nil if the geometry is degenerate."
  (let* ((bx (nth 0 B)) (by (nth 1 B))
         (ba (list (- (nth 0 A) bx) (- (nth 1 A) by)))
         (bc (list (- (nth 0 C) bx) (- (nth 1 C) by)))
         (la (sqrt (+ (* (nth 0 ba) (nth 0 ba)) (* (nth 1 ba) (nth 1 ba)))))
         (lc (sqrt (+ (* (nth 0 bc) (nth 0 bc)) (* (nth 1 bc) (nth 1 bc))))))
    (when (and (> la 1e-10) (> lc 1e-10))
      (let* ((u   (list (/ (nth 0 ba) la) (/ (nth 1 ba) la)))
             (v   (list (/ (nth 0 bc) lc) (/ (nth 1 bc) lc)))
             (dot (max -1.0 (min 1.0 (+ (* (nth 0 u) (nth 0 v))
                                        (* (nth 1 u) (nth 1 v))))))
             (half (/ (acos dot) 2)))
        (when (> (sin half) 1e-10)
          (list u v half))))))

(defun scad-sketch-geo--corner-geometry-from-tlens (B u v half t1 t2)
  "Build corner arc geometry for point B given unit vecs U V, half-angle HALF.
T1 and T2 are the tangent lengths along U and V respectively.
Returns plist (:t1 PT :t2 PT :radius NUM :sweep 0-or-1)."
  (let* ((bx   (nth 0 B)) (by (nth 1 B))
         (tl   (min t1 t2))
         (ar   (* tl (tan half)))
         (p1   (list (+ bx (* tl (nth 0 u))) (+ by (* tl (nth 1 u)))))
         (p2   (list (+ bx (* tl (nth 0 v))) (+ by (* tl (nth 1 v)))))
         (cross (- (* (nth 0 u) (nth 1 v)) (* (nth 1 u) (nth 0 v)))))
    (list :t1 p1 :t2 p2 :radius ar :sweep (if (> cross 0) 1 0))))

(defun scad-sketch-geo--corner-geometry (A B C r)
  "Compute arc geometry for a polyRound corner at B with radius R.
A and C are the adjacent vertices.  Returns the same plist as
`scad-sketch-geo--corner-geometry-from-tlens', or nil if degenerate."
  (when (and r (> r 0))
    (let ((uvh (scad-sketch-geo--corner-unit-vecs A B C)))
      (when uvh
        (let* ((u    (nth 0 uvh)) (v (nth 1 uvh)) (half (nth 2 uvh))
               (bx   (nth 0 B))  (by (nth 1 B))
               (ba   (list (- (nth 0 A) bx) (- (nth 1 A) by)))
               (bc   (list (- (nth 0 C) bx) (- (nth 1 C) by)))
               (la   (sqrt (+ (* (nth 0 ba) (nth 0 ba)) (* (nth 1 ba) (nth 1 ba)))))
               (lc   (sqrt (+ (* (nth 0 bc) (nth 0 bc)) (* (nth 1 bc) (nth 1 bc)))))
               (tl   (min (/ r (tan half)) (* la 0.49) (* lc 0.49))))
          (scad-sketch-geo--corner-geometry-from-tlens B u v half tl tl))))))

(defun scad-sketch-geo--any-radius-p (points)
  "Return non-nil if any point in POINTS has a non-zero third element."
  (cl-some (lambda (p) (and (nth 2 p) (> (nth 2 p) 0))) points))

(defun scad-sketch-geo--edge-len (P Q)
  "Return the Euclidean distance between points P and Q."
  (let ((dx (- (nth 0 Q) (nth 0 P)))
        (dy (- (nth 1 Q) (nth 1 P))))
    (sqrt (+ (* dx dx) (* dy dy)))))

;;;; ── SVG pixel helpers ───────────────────────────────────────────────────────

(defun scad-sketch-geo--pixel-radius (model-r tf)
  "Convert MODEL-R (model-space radius) to pixel radius via transform TF."
  (let* ((o  (funcall tf '(0 0)))
         (r  (funcall tf (list model-r 0)))
         (dx (- (nth 0 r) (nth 0 o)))
         (dy (- (nth 1 r) (nth 1 o))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun scad-sketch-geo--polyround-path-d (points closed tf)
  "Build an SVG path `d' string for POINTS with polyRound corner arcs.
CLOSED non-nil closes the path with Z.  TF is the model→pixel transform.
Returns a string suitable for the `d' attribute of an SVG <path>, or nil
if there are fewer than 2 points."
  (let ((n (length points)))
    (when (>= n 2)
      (let* ((t-out  (make-vector n 0.0))
             (t-in   (make-vector n 0.0))
             (uvh-v  (make-vector n nil)))
        ;; --- Pass 1: compute raw tangent lengths at each rounded corner ---
        (dotimes (i n)
          (let ((r (nth 2 (nth i points))))
            (when (and r (> r 0))
              (let* ((prev (cond ((> i 0)      (nth (1- i) points))
                                 (closed        (nth (1- n) points))))
                     (next (cond ((< i (1- n)) (nth (1+ i) points))
                                 (closed        (nth 0 points)))))
                (when (and prev next)
                  (let* ((A   (list (float (nth 0 prev))         (float (nth 1 prev))))
                         (B   (list (float (nth 0 (nth i points))) (float (nth 1 (nth i points)))))
                         (C   (list (float (nth 0 next))          (float (nth 1 next))))
                         (uvh (scad-sketch-geo--corner-unit-vecs A B C)))
                    (when uvh
                      (aset uvh-v i uvh)
                      (let ((ti (/ r (tan (nth 2 uvh)))))
                        (aset t-in i ti)
                        (aset t-out i ti)))))))))
        ;; --- Pass 2: clamp tangent lengths so arcs don't overlap edges ---
        (dotimes (i n)
          (let* ((j  (mod (1+ i) n))
                 (Pi (list (float (nth 0 (nth i points))) (float (nth 1 (nth i points)))))
                 (Pj (list (float (nth 0 (nth j points))) (float (nth 1 (nth j points)))))
                 (el (scad-sketch-geo--edge-len Pi Pj))
                 (sm (+ (aref t-out i) (aref t-in j))))
            (when (and (or closed (< i (1- n))) (> sm (* el 0.999)))
              (let ((sc (/ (* el 0.499) sm)))
                (aset t-out i (* (aref t-out i) sc))
                (aset t-in  j (* (aref t-in  j) sc))))))
        ;; --- Pass 3: build corner arc descriptors ---
        (let ((corners (make-vector n nil)))
          (dotimes (i n)
            (let ((uvh (aref uvh-v i)))
              (when uvh
                (aset corners i
                      (scad-sketch-geo--corner-geometry-from-tlens
                       (list (float (nth 0 (nth i points)))
                             (float (nth 1 (nth i points))))
                       (nth 0 uvh) (nth 1 uvh) (nth 2 uvh)
                       (aref t-in i) (aref t-out i))))))
          ;; --- Pass 4: emit SVG path segments ---
          (let* ((c0    (aref corners 0))
                 (start (if (and c0 closed)
                            (funcall tf (plist-get c0 :t1))
                          (funcall tf (list (float (nth 0 (nth 0 points)))
                                           (float (nth 1 (nth 0 points)))))))
                 (fmt   (lambda (xy)
                          (format "%.3f %.3f"
                                  (float (nth 0 xy)) (float (nth 1 xy)))))
                 (parts (list (format "M %s" (funcall fmt start)))))
            (dotimes (i n)
              (let* ((cor (aref corners i))
                     (ps  (funcall tf
                                   (list (float (nth 0 (nth i points)))
                                         (float (nth 1 (nth i points)))))))
                (if cor
                    (let* ((t1s (funcall tf (plist-get cor :t1)))
                           (t2s (funcall tf (plist-get cor :t2)))
                           (rs  (scad-sketch-geo--pixel-radius
                                 (plist-get cor :radius) tf))
                           (sw  (plist-get cor :sweep)))
                      (push (format "L %s"                     (funcall fmt t1s)) parts)
                      (push (format "A %.3f %.3f 0 0 %d %s" rs rs sw (funcall fmt t2s)) parts))
                  (push (format "L %s" (funcall fmt ps)) parts))))
            (when closed (push "Z" parts))
            (mapconcat #'identity (nreverse parts) " ")))))))

;;;; ── Number formatting ───────────────────────────────────────────────────────

(defun scad-sketch-geo--fmt-num (n)
  "Format number N compactly: integers without decimal, floats up to 4 dp."
  (let ((x (float n)))
    (if (< (abs (- x (round x))) 0.000001)
        (number-to-string (round x))
      (let ((s (format "%.4f" x)))
        (setq s (replace-regexp-in-string "0+\\'" "" s))
        (setq s (replace-regexp-in-string "\\.\\'" "" s))
        (if (or (string= s "-0") (string= s "")) "0" s)))))

(defun scad-sketch-geo--fmt-xy (xy)
  "Format XY as \"(x, y)\" using `scad-sketch-geo--fmt-num'."
  (format "(%s, %s)"
          (scad-sketch-geo--fmt-num (nth 0 xy))
          (scad-sketch-geo--fmt-num (nth 1 xy))))

(provide 'scad-sketch-geometry)
;;; scad-sketch-geometry.el ends here
