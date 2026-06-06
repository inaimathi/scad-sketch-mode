;;; scad-sketch-editor--rendering.el --- SVG canvas rendering -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns everything that produces pixels: bounds/transform computation,
;; grid drawing, per-shape drawing, boolean tree preview with proper SVG
;; compositing, marks/cursor overlay, HUD, and `scad-sketch--render'.
;;
;; Boolean compositing strategy
;; ────────────────────────────
;; The fundamental technique for union, and for the positive child of
;; difference/intersection, is a TWO-PASS COMPOUND PATH:
;;
;;   Pass 1 – fill:  one <path> element, d = ALL leaf path-data strings
;;             joined into a single compound path (M…Z M…Z …),
;;             fill="white" fill-rule="nonzero" stroke="none".
;;             This floods the entire union region white, including the
;;             internal seam areas where shapes overlap.
;;
;;   Pass 2 – stroke: the SAME compound path, fill="none",
;;             stroke=color stroke-width=2.
;;             SVG strokes every subpath boundary.  Strokes that fall on
;;             internal seams (edges inside the filled white region) are
;;             rendered white-on-white and therefore invisible.  Only
;;             strokes on the exterior boundary (edges outside all other
;;             subpaths) are visible against the non-white background.
;;
;; This produces a true merged silhouette outline with no internal seam
;; lines, without any path-boolean geometry computation.  It relies on
;; the background behind the shape being white, which it always is here.
;;
;;   union       – two-pass compound path of all leaf children.
;;   difference  – two-pass compound path for the positive child (inside
;;                 an SVG <mask> that blacks out subtract shapes);
;;                 subtract shapes: dashed compound outline outside mask.
;;   intersection – two-pass compound path for primary child inside an
;;                  SVG <clipPath> from non-primary children;
;;                  non-primary: dashed compound ghost outline.
;;
;; Overlay / highlight strategy
;; ────────────────────────────
;;   • Per-shape path strokes are SUPPRESSED in boolean sessions.
;;     The preview layer carries all geometry.  Vertex dots render always.
;;
;;   • Attention on a shape ref highlights the DEEPEST BOOLEAN GROUP
;;     containing that shape, using the same two-pass compound technique
;;     but in the attention/selection colour.
;;
;;   • Attention on a point ref re-enables per-shape stroke for that
;;     individual shape only (drilling into a vertex).
;;
;; Read-only over session state: queries but never mutates.

;;; Code:

(require 'cl-lib)
(require 'svg)
(require 'scad-sketch-session)
(require 'scad-sketch-geometry)
(require 'scad-sketch-editor--refs)
(require 'scad-sketch-editor--selection)
(require 'scad-sketch-editor-core)

;;;; ── Canvas constants ────────────────────────────────────────────────────────

(defcustom scad-sketch-canvas-width 900
  "Sketch editor canvas width in pixels."
  :type 'integer :group 'scad-sketch)

(defcustom scad-sketch-canvas-height 650
  "Sketch editor canvas height in pixels."
  :type 'integer :group 'scad-sketch)

(defcustom scad-sketch-margin 48
  "Canvas margin in pixels."
  :type 'integer :group 'scad-sketch)

;;;; ── Numeric formatting ─────────────────────────────────────────────────────

(defun scad-sketch--fmt-num (n)
  "Format N compactly (drop trailing zeros after decimal point)."
  (let ((s (format "%.4f" n)))
    (string-trim-right (string-trim-right s "0") "\\.")))

(defun scad-sketch--fmt-xy (xy)
  "Format XY pair as \"(x, y)\"."
  (format "(%s, %s)"
          (scad-sketch--fmt-num (nth 0 xy))
          (scad-sketch--fmt-num (nth 1 xy))))

;;;; ── Bounds and coordinate transform ────────────────────────────────────────
(defun scad-sketch--transform-xy-points (matrix xy-points)
  "Apply affine MATRIX to XY-POINTS."
  (mapcar (lambda (xy)
            (scad-sketch-session--mat-apply matrix xy))
          xy-points))

(defun scad-sketch--mirror-transform (mx my transform)
  "Return a transform closure that mirrors model XY before TRANSFORM."
  (let ((m (scad-sketch-session--mat-mirror mx my)))
    (lambda (xy)
      (funcall transform
               (scad-sketch-session--mat-apply m xy)))))

(defun scad-sketch--circle-bounds (shape)
  "Return (MIN-X MAX-X MIN-Y MAX-Y) for circle SHAPE."
  (let* ((md (scad-sketch-shape-metadata shape))
         (cx (plist-get md :cx))
         (cy (plist-get md :cy))
         (r  (plist-get md :r)))
    (list (- cx r) (+ cx r) (- cy r) (+ cy r))))

(defun scad-sketch--text-bounds (shape)
  "Return approximate (MIN-X MAX-X MIN-Y MAX-Y) bounds for text SHAPE."
  (let* ((md     (scad-sketch-shape-metadata shape))
         (str    (or (plist-get md :str) ""))
         (x      (float (or (plist-get md :x) 0.0)))
         (y      (float (or (plist-get md :y) 0.0)))
         (size   (max 0.0001 (float (or (plist-get md :size) 10.0))))
         (angle  (* pi (/ (float (or (plist-get md :angle) 0.0)) 180.0)))
         (width  (max size (* size 0.6 (max 1 (string-width str)))))
         (height size)
         (co     (cos angle))
         (si     (sin angle))
         (corners (list (list 0.0 0.0)
                        (list width 0.0)
                        (list width height)
                        (list 0.0 height)))
         xs ys)
    (dolist (c corners)
      (let* ((cx (+ x (- (* (nth 0 c) co) (* (nth 1 c) si))))
             (cy (+ y (+ (* (nth 0 c) si) (* (nth 1 c) co)))))
        (push cx xs)
        (push cy ys)))
    (list (apply #'min xs)
          (apply #'max xs)
          (apply #'min ys)
          (apply #'max ys))))

(defun scad-sketch--shape-xy-points (shape)
  "Return representative XY points for SHAPE, for bounds computation."
  (pcase (scad-sketch-shape-kind shape)
    ('polygon
     (mapcar #'scad-sketch--point-xy
             (scad-sketch-shape-points shape)))

    ('circle
     (let* ((md (scad-sketch-shape-metadata shape))
            (cx (float (or (plist-get md :cx) 0.0)))
            (cy (float (or (plist-get md :cy) 0.0)))
            (r  (float (or (plist-get md :r) 0.0))))
       (list (list (- cx r) cy)
             (list (+ cx r) cy)
             (list cx (- cy r))
             (list cx (+ cy r)))))

    ('square
     (scad-sketch--square-corner-points shape))

    ('text
     (pcase-let ((`(,min-x ,max-x ,min-y ,max-y)
                  (scad-sketch--text-rough-bounds shape)))
       (list (list min-x min-y)
             (list max-x min-y)
             (list max-x max-y)
             (list min-x max-y))))

    (_ nil)))

(defun scad-sketch--bounds (session)
  "Return (MIN-X MAX-X MIN-Y MAX-Y) covering the visible sketch scene.

Bounds include:
  - source-side editable shapes
  - semantic tree preview geometry, including mirrored output
  - mirror axis handles
  - marks
  - cursor point"
  (scad-sketch-session-sync-active-shape-from-points session)
  (let* ((source-shape-points
          (apply #'append
                 (mapcar #'scad-sketch--shape-xy-points
                         (scad-sketch-session-shapes session))))

         ;; This is the important part for mirror support.  The mirrored object
         ;; is not present in `session-shapes'; it only exists through the tree.
         (tree-points
          (scad-sketch--tree-xy-points
           session
           (scad-sketch-session-tree session)))

         (mirror-handle-points
          (scad-sketch--mirror-handle-xy-points session))

         (extra
          (delq nil
                (cons (scad-sketch-session-point session)
                      (scad-sketch-session-marks session))))

         (all
          (append source-shape-points
                  tree-points
                  mirror-handle-points
                  extra)))

    (if (null all)
        (list -10 10 -10 10)
      (let ((min-x (apply #'min (mapcar #'car  all)))
            (max-x (apply #'max (mapcar #'car  all)))
            (min-y (apply #'min (mapcar #'cadr all)))
            (max-y (apply #'max (mapcar #'cadr all))))
        (when (= min-x max-x)
          (setq min-x (- min-x 10)
                max-x (+ max-x 10)))
        (when (= min-y max-y)
          (setq min-y (- min-y 10)
                max-y (+ max-y 10)))
        (let ((px (max 1 (* 0.15 (- max-x min-x))))
              (py (max 1 (* 0.15 (- max-y min-y)))))
          (list (- min-x px) (+ max-x px)
                (- min-y py) (+ max-y py)))))))

(defun scad-sketch--transform (bounds)
  "Return a pixel-coordinate closure for BOUNDS (Y-flipped for screen)."
  (pcase-let ((`(,min-x ,max-x ,min-y ,max-y) bounds))
    (let* ((w     scad-sketch-canvas-width)
           (h     scad-sketch-canvas-height)
           (m     scad-sketch-margin)
           (scale (min (/ (- w (* 2 m)) (- max-x min-x))
                       (/ (- h (* 2 m)) (- max-y min-y)))))
      (lambda (xy)
        (list (+ m (* (- (nth 0 xy) min-x) scale))
              (- h (+ m (* (- (nth 1 xy) min-y) scale))))))))

(defun scad-sketch--pixel-radius (r transform)
  "Convert model radius R to pixels using TRANSFORM."
  (let* ((o (funcall transform '(0 0)))
         (p (funcall transform (list r 0))))
    (abs (- (nth 0 p) (nth 0 o)))))

;;;; ── SVG primitive helpers ──────────────────────────────────────────────────

(defun scad-sketch--svg-line (svg transform a b &rest args)
  "Draw model-space line A→B on SVG using TRANSFORM."
  (let ((pa (funcall transform a))
        (pb (funcall transform b)))
    (apply #'svg-line svg
           (nth 0 pa) (nth 1 pa)
           (nth 0 pb) (nth 1 pb)
           args)))

;;;; ── Shape path generation ──────────────────────────────────────────────────
(defun scad-sketch--shape-path-d (shape transform)
  "Return an SVG path-data string for SHAPE under TRANSFORM, or nil."
  (pcase (scad-sketch-shape-kind shape)
    ('polygon
     (let* ((points (scad-sketch-shape-points shape))
            (closed (scad-sketch-shape-closed shape)))
       (if (scad-sketch--any-radius-p points)
           (scad-sketch--polyround-path-d points closed transform)
         (let* ((xy-pts     (mapcar #'scad-sketch--point-xy points))
                (screen-pts (mapcar transform xy-pts)))
           (when screen-pts
             (concat
              (format "M %s %s"
                      (scad-sketch--fmt-num (nth 0 (car screen-pts)))
                      (scad-sketch--fmt-num (nth 1 (car screen-pts))))
              (mapconcat
               (lambda (p)
                 (format " L %s %s"
                         (scad-sketch--fmt-num (nth 0 p))
                         (scad-sketch--fmt-num (nth 1 p))))
               (cdr screen-pts) "")
              (if (and closed (> (length screen-pts) 2)) " Z" "")))))))
    ('circle
     (let* ((md     (scad-sketch-shape-metadata shape))
            (center (funcall transform
                             (list (plist-get md :cx) (plist-get md :cy))))
            (r      (scad-sketch--pixel-radius (plist-get md :r) transform))
            (cx     (nth 0 center))
            (cy     (nth 1 center)))
       (format "M %s %s A %s %s 0 1 0 %s %s A %s %s 0 1 0 %s %s Z"
               (scad-sketch--fmt-num (- cx r)) (scad-sketch--fmt-num cy)
               (scad-sketch--fmt-num r)         (scad-sketch--fmt-num r)
               (scad-sketch--fmt-num (+ cx r)) (scad-sketch--fmt-num cy)
               (scad-sketch--fmt-num r)         (scad-sketch--fmt-num r)
               (scad-sketch--fmt-num (- cx r)) (scad-sketch--fmt-num cy))))
    ('square
     (let* ((pts (mapcar transform (scad-sketch--square-corner-points shape)))
            (p0 (nth 0 pts))
            (p1 (nth 1 pts))
            (p2 (nth 2 pts))
            (p3 (nth 3 pts)))
       (format "M %s %s L %s %s L %s %s L %s %s Z"
               (scad-sketch--fmt-num (nth 0 p0))
               (scad-sketch--fmt-num (nth 1 p0))
               (scad-sketch--fmt-num (nth 0 p1))
               (scad-sketch--fmt-num (nth 1 p1))
               (scad-sketch--fmt-num (nth 0 p2))
               (scad-sketch--fmt-num (nth 1 p2))
               (scad-sketch--fmt-num (nth 0 p3))
               (scad-sketch--fmt-num (nth 1 p3)))))
    ('text
     (pcase-let ((`(,min-x ,max-x ,min-y ,max-y)
                  (scad-sketch--text-rough-bounds shape)))
       (let* ((pts (mapcar transform
                           (list (list min-x min-y)
                                 (list max-x min-y)
                                 (list max-x max-y)
                                 (list min-x max-y))))
              (p0 (nth 0 pts))
              (p1 (nth 1 pts))
              (p2 (nth 2 pts))
              (p3 (nth 3 pts)))
         (format "M %s %s L %s %s L %s %s L %s %s Z"
                 (scad-sketch--fmt-num (nth 0 p0))
                 (scad-sketch--fmt-num (nth 1 p0))
                 (scad-sketch--fmt-num (nth 0 p1))
                 (scad-sketch--fmt-num (nth 1 p1))
                 (scad-sketch--fmt-num (nth 0 p2))
                 (scad-sketch--fmt-num (nth 1 p2))
                 (scad-sketch--fmt-num (nth 0 p3))
                 (scad-sketch--fmt-num (nth 1 p3))))))
    (_ nil)))

;;;; ── Tree queries ────────────────────────────────────────────────────────────

(defun scad-sketch--root-is-boolean-p (session)
  "Return non-nil if SESSION has a boolean root target."
  (let ((root (scad-sketch-session-root-target session)))
    (and root (eq (scad-sketch-target-kind root) 'boolean))))

(defun scad-sketch--tree-xy-points (session tree &optional matrix)
  "Return representative model-space XY points for TREE.

Unlike `scad-sketch-session-shapes', TREE includes semantic preview geometry
such as mirror output.  MATRIX is an accumulated model-space affine transform."
  (let ((matrix (or matrix (scad-sketch-session--mat-identity))))
    (pcase (and tree (plist-get tree :kind))
      ('shape
       (let ((shape (scad-sketch-session-shape-by-id
                     session (plist-get tree :shape-id))))
         (if shape
             (scad-sketch--transform-xy-points
              matrix
              (scad-sketch--shape-xy-points shape))
           nil)))

      ('boolean
       (apply #'append
              (mapcar (lambda (child)
                        (scad-sketch--tree-xy-points session child matrix))
                      (plist-get tree :children))))

      ('mirror
       (let* ((mx (float (or (plist-get tree :mx) 1.0)))
              (my (float (or (plist-get tree :my) 0.0)))
              (mirror-matrix (scad-sketch-session--mat-mirror mx my))
              (next-matrix   (scad-sketch-session--mat-mul
                              matrix mirror-matrix)))
         (scad-sketch--tree-xy-points
          session
          (plist-get tree :child)
          next-matrix)))

      (_ nil))))

(defun scad-sketch--tree-path-ds (session transform tree)
  "Return all SVG path-data strings for shape leaves in TREE.

For mirror nodes, returns the mirrored output paths."
  (pcase (plist-get tree :kind)
    ('shape
     (let* ((shape (scad-sketch-session-shape-by-id
                    session (plist-get tree :shape-id)))
            (d     (and shape (scad-sketch--shape-path-d shape transform))))
       (if d (list d) nil)))

    ('boolean
     (apply #'append
            (mapcar (lambda (child)
                      (scad-sketch--tree-path-ds session transform child))
                    (plist-get tree :children))))

    ('mirror
     (let* ((mx (plist-get tree :mx))
            (my (plist-get tree :my))
            (child-transform (scad-sketch--mirror-transform mx my transform)))
       (scad-sketch--tree-path-ds
        session child-transform (plist-get tree :child))))

    (_ nil)))

(defun scad-sketch--compound-d (path-ds)
  "Join PATH-DS strings into one compound SVG path-data string."
  (mapconcat #'identity (delq nil path-ds) " "))

(defun scad-sketch--tree-contains-shape-p (tree shape-id)
  "Return non-nil if TREE contains a leaf with SHAPE-ID."
  (pcase (plist-get tree :kind)
    ('shape
     (eq (plist-get tree :shape-id) shape-id))
    ('boolean
     (cl-some (lambda (child)
                (scad-sketch--tree-contains-shape-p child shape-id))
              (plist-get tree :children)))
    ('mirror
     (scad-sketch--tree-contains-shape-p
      (plist-get tree :child) shape-id))
    (_ nil)))

(defun scad-sketch--deepest-containing-group (tree shape-id)
  "Return the deepest boolean node in TREE that contains SHAPE-ID, or nil."
  (when (scad-sketch--tree-contains-shape-p tree shape-id)
    (pcase (plist-get tree :kind)
      ('boolean
       (or (cl-some (lambda (child)
                      (scad-sketch--deepest-containing-group child shape-id))
                    (plist-get tree :children))
           tree))
      (_ nil))))

;;;; ── Two-pass compound path drawing ─────────────────────────────────────────
;;
;; This is the core primitive for all boolean preview rendering.
;; See the Commentary section for the explanation of why this works.
(defun scad-sketch--mirror-handle-xy-points (session)
  "Return model-space XY points for all mirror handles in SESSION."
  (let (points)
    (dolist (mirror (scad-sketch-session--tree-mirrors
                     (scad-sketch-session-tree session)))
      (dotimes (idx 2)
        (let ((xy (scad-sketch--mirror-handle-xy session mirror idx)))
          (when xy
            (push xy points)))))
    (nreverse points)))

(defun scad-sketch--draw-mirror-axis (svg transform session mirror)
  "Draw MIRROR axis and editable normal handles."
  (let* ((mirror-id (plist-get mirror :mirror-id))
         (axis      (scad-sketch--mirror-axis-segment mirror))
         (a         (nth 0 axis))
         (b         (nth 1 axis))
         (attention (scad-sketch--attention-ref session))
         (axis-ref  (scad-sketch--mirror-ref mirror-id))
         (axis-attn (and attention
                         (scad-sketch--same-ref-p attention axis-ref)))
         (axis-sel  (scad-sketch--mirror-ref-selected-p session mirror-id))
         (stroke    (cond (axis-sel  "#d13f00")
                          (axis-attn "#0057c2")
                          (t         "#0057c2"))))
    (scad-sketch--svg-line svg transform a b
                           :stroke stroke
                           :stroke-width (cond (axis-sel 4)
                                               (axis-attn 3)
                                               (t 2))
                           :stroke-dasharray "14,8"
                           :stroke-opacity (if (or axis-sel axis-attn) 0.95 0.55))

    (dotimes (idx 2)
      (let* ((xy        (scad-sketch--mirror-handle-xy session mirror idx))
             (screen    (funcall transform xy))
             (point-ref (scad-sketch--mirror-point-ref idx mirror-id))
             (sel       (scad-sketch--mirror-point-selected-p
                         session mirror-id idx))
             (attn      (and attention
                             (scad-sketch--same-ref-p attention point-ref))))
        (when (and (fboundp 'scad-sketch--hover-attention-p)
                   (scad-sketch--hover-attention-p session point-ref))
          (scad-sketch--draw-attention-halo svg screen 11))
        (svg-circle svg (nth 0 screen) (nth 1 screen)
                    (cond (sel 8) (attn 7) (t 6))
                    :stroke (cond (sel "#d13f00")
                                  (attn "#0057c2")
                                  (t "#0057c2"))
                    :stroke-width 2
                    :fill (cond (sel "#fff0e8")
                                (attn "#dfefff")
                                (t "#ffffff")))
        (scad-sketch--draw-label
         svg
         (format "%s:axis%d" mirror-id idx)
         (+ (nth 0 screen) 8)
         (- (nth 1 screen) 8)
         sel attn axis-attn)))))

(defun scad-sketch--draw-mirror-axes (svg transform session)
  "Draw all mirror axes in SESSION."
  (dolist (mirror (scad-sketch-session--tree-mirrors
                   (scad-sketch-session-tree session)))
    (scad-sketch--draw-mirror-axis svg transform session mirror)))

(defun scad-sketch--draw-compound (parent-node compound-d
                                               fill-color stroke-color stroke-width
                                               &optional stroke-dash)
  "Draw COMPOUND-D as a two-pass compound path into PARENT-NODE.

Pass 1: fill=FILL-COLOR, stroke=none (floods the union region).
Pass 2: fill=none, stroke=STROKE-COLOR width=STROKE-WIDTH (outer boundary only).
Optional STROKE-DASH sets stroke-dasharray on both passes."
  (when (and compound-d (not (string-empty-p compound-d)))
    ;; Pass 1: fill floods interior including internal seam areas.
    (apply #'svg-node parent-node 'path
           (append (list :d            compound-d
                         :fill         fill-color
                         :fill-rule    "nonzero"
                         :stroke       "none")
                   (when stroke-dash (list :stroke-dasharray stroke-dash))))
    ;; Pass 2: stroke follows subpath boundaries; internal strokes land
    ;; on white fill and vanish; only exterior boundary strokes are visible.
    (apply #'svg-node parent-node 'path
           (append (list :d            compound-d
                         :fill         "none"
                         :fill-rule    "nonzero"
                         :stroke       stroke-color
                         :stroke-width stroke-width)
                   (when stroke-dash (list :stroke-dasharray stroke-dash))))))

;;;; ── Boolean tree SVG preview ───────────────────────────────────────────────

(defvar scad-sketch--svg-id-counter 0
  "Counter for unique SVG mask/clip IDs within one render pass.")

(defun scad-sketch--next-svg-id (prefix)
  "Return a fresh unique SVG element id starting with PREFIX."
  (format "%s-%d" prefix (cl-incf scad-sketch--svg-id-counter)))

(defun scad-sketch--ensure-defs (svg)
  "Return the <defs> child of SVG, creating it if absent."
  (or (cl-find 'defs (cddr svg) :key #'car)
      (svg-node svg 'defs)))

(defun scad-sketch--render-tree (svg defs transform session tree)
  "Render TREE into SVG using masks/clips stored in DEFS.
Returns the outermost <g> node created, or nil."
  (pcase (plist-get tree :kind)

    ('shape
     (let* ((shape (scad-sketch-session-shape-by-id
                    session (plist-get tree :shape-id)))
            (d     (and shape (scad-sketch--shape-path-d shape transform))))
       (when d
         (let ((g (svg-node svg 'g)))
           (scad-sketch--draw-compound g d "#ffffff" "#888888" 2)
           g))))

    ('mirror
     (let* ((g        (svg-node svg 'g))
            (mx       (plist-get tree :mx))
            (my       (plist-get tree :my))
            (child    (plist-get tree :child))
            ;; Actual mirrored output.
            (mxf      (scad-sketch--mirror-transform mx my transform))
            (mirror-d (scad-sketch--compound-d
                       (scad-sketch--tree-path-ds session mxf child))))
       ;; The source-side child is intentionally left to the normal per-shape
       ;; overlay layer.  The mirror result gets a dashed secondary outline.
       (scad-sketch--draw-compound g mirror-d "#ffffff" "#0057c2" 2 "14,8")
       g))

    ('boolean
     (let ((op       (plist-get tree :op))
           (children (plist-get tree :children)))
       (pcase op
         ('union
          (let* ((g        (svg-node svg 'g))
                 (all-ds   (scad-sketch--tree-path-ds session transform tree))
                 (compound (scad-sketch--compound-d all-ds)))
            (scad-sketch--draw-compound g compound "#ffffff" "#888888" 2)
            g))

         ('difference
          (when children
            (let* ((mask-id  (scad-sketch--next-svg-id "diff-mask"))
                   (mask     (svg-node defs 'mask :id mask-id))
                   (g        (svg-node svg 'g :mask (format "url(#%s)" mask-id)))
                   (pos-ds   (scad-sketch--tree-path-ds
                              session transform (car children)))
                   (pos-d    (scad-sketch--compound-d pos-ds))
                   (sub-ds   (apply #'append
                                    (mapcar (lambda (sub)
                                              (scad-sketch--tree-path-ds
                                               session transform sub))
                                            (cdr children))))
                   (sub-d    (scad-sketch--compound-d sub-ds)))
              (svg-node mask 'rect :x 0 :y 0
                        :width scad-sketch-canvas-width
                        :height scad-sketch-canvas-height
                        :fill "white")
              (when (and sub-d (not (string-empty-p sub-d)))
                (svg-node mask 'path :d sub-d
                          :fill "black" :fill-rule "nonzero"
                          :stroke "black" :stroke-width 1))
              (scad-sketch--draw-compound g pos-d "#ffffff" "#888888" 2)
              (when (and sub-d (not (string-empty-p sub-d)))
                (scad-sketch--draw-compound svg sub-d "none" "#aaaaaa" 1 "6,4"))
              g)))

         ('intersection
          (when children
            (let* ((clip-id (scad-sketch--next-svg-id "intersect-clip"))
                   (clip    (svg-node defs 'clipPath :id clip-id))
                   (g       (svg-node svg 'g :clip-path (format "url(#%s)" clip-id)))
                   (first-d (scad-sketch--compound-d
                             (scad-sketch--tree-path-ds
                              session transform (car children))))
                   (rest-ds (apply #'append
                                   (mapcar (lambda (sub)
                                             (scad-sketch--tree-path-ds
                                              session transform sub))
                                           (cdr children))))
                   (rest-d  (scad-sketch--compound-d rest-ds)))
              (when (and rest-d (not (string-empty-p rest-d)))
                (svg-node clip 'path :d rest-d :fill "black" :fill-rule "nonzero"))
              (scad-sketch--draw-compound g first-d "#ffffff" "#888888" 2)
              (when (and rest-d (not (string-empty-p rest-d)))
                (scad-sketch--draw-compound svg rest-d "none" "#aaaaaa" 1 "6,4"))
              g)))

         (_ nil))))

    (_ nil)))

;;;; ── Group-level attention highlight ────────────────────────────────────────
;;
;; When attention is on a shape ref inside a boolean tree, highlight the
;; deepest boolean group containing that shape, using two-pass compound path
;; in the attention/selection colour.  This makes "select the rectangle" light
;; up the whole union silhouette in orange/blue, not just the rect outline.
(defun scad-sketch--hover-attention-ref (session)
  "Return the ref that should receive an attention halo, or nil.

Unlike `scad-sketch--attention-ref', this does not fall back to global focus.
Halos should only appear for actual hover attention."
  (scad-sketch--hover-ref session))

(defun scad-sketch--hover-attention-p (session ref)
  "Return non-nil if REF is the currently hovered attention ref."
  (scad-sketch--same-ref-p (scad-sketch--hover-attention-ref session) ref))

(defun scad-sketch--tree-find-group-by-id (tree group-id)
  "Return the boolean TREE node with GROUP-ID, or nil."
  (when tree
    (pcase (plist-get tree :kind)
      ('boolean
       (or (and (equal (plist-get tree :group-id) group-id)
                tree)
           (cl-some (lambda (child)
                      (scad-sketch--tree-find-group-by-id child group-id))
                    (plist-get tree :children))))
      (_ nil))))

(defun scad-sketch--draw-group-box-halo (svg transform session group)
  "Draw a dashed rectangular halo around boolean GROUP itself."
  (let ((bounds (scad-sketch--tree-bounds session group)))
    (when bounds
      (pcase-let ((`(,min-x ,max-x ,min-y ,max-y) bounds))
        (let* ((p0 (funcall transform (list min-x min-y)))
               (p1 (funcall transform (list max-x max-y)))
               (x  (min (nth 0 p0) (nth 0 p1)))
               (y  (min (nth 1 p0) (nth 1 p1)))
               (w  (abs (- (nth 0 p1) (nth 0 p0))))
               (h  (abs (- (nth 1 p1) (nth 1 p0)))))
          ;; Soft box halo.
          (svg-rectangle svg x y w h
                         :fill "none"
                         :stroke "#0057c2"
                         :stroke-width 9
                         :stroke-opacity 0.12
                         :stroke-dasharray "14,8")
          ;; Crisp dashed group rectangle.
          (svg-rectangle svg x y w h
                         :fill "none"
                         :stroke "#0057c2"
                         :stroke-width 2.5
                         :stroke-opacity 0.85
                         :stroke-dasharray "14,8"))))))

(defun scad-sketch--draw-compound-halo (parent-node compound-d)
  "Draw an attention halo around COMPOUND-D.

This is for non-point attention: shapes and boolean groups.  Point attention
uses `scad-sketch--draw-attention-halo' instead."
  (when (and compound-d (not (string-empty-p compound-d)))
    ;; Fill pass hides internal seams in compound paths, matching the boolean
    ;; preview/highlight strategy.
    (svg-node parent-node 'path
              :d compound-d
              :fill "#ffffff"
              :fill-rule "nonzero"
              :stroke "none")
    ;; Wide soft halo.
    (svg-node parent-node 'path
              :d compound-d
              :fill "none"
              :fill-rule "nonzero"
              :stroke "#0057c2"
              :stroke-width 14
              :stroke-opacity 0.12
              :stroke-linejoin "round"
              :stroke-linecap "round")
    ;; Mid halo.
    (svg-node parent-node 'path
              :d compound-d
              :fill "none"
              :fill-rule "nonzero"
              :stroke "#0057c2"
              :stroke-width 7
              :stroke-opacity 0.28
              :stroke-linejoin "round"
              :stroke-linecap "round")
    ;; Crisp edge.
    (svg-node parent-node 'path
              :d compound-d
              :fill "none"
              :fill-rule "nonzero"
              :stroke "#0057c2"
              :stroke-width 2.5
              :stroke-opacity 0.85
              :stroke-linejoin "round"
              :stroke-linecap "round")))

(defun scad-sketch--draw-ref-geometry-halo (svg transform session ref)
  "Draw a geometry-level attention halo for REF.

Point refs intentionally do nothing here; point refs are haloed at the point
renderer.

Boolean group wrapper refs get a dashed group rectangle.
Boolean member refs get the child-object compound halo."
  (when ref
    (pcase (scad-sketch--ref-kind ref)
      ('point
       nil)

      ('shape
       (let* ((shape-id (scad-sketch--ref-shape-id ref))
              (shape    (scad-sketch-session-shape-by-id session shape-id))
              (d        (and shape
                             (scad-sketch--shape-path-d shape transform))))
         (when d
           (scad-sketch--draw-compound-halo svg d))))

      ('boolean
       (let* ((group-id (scad-sketch--ref-group-id ref))
              (group    (and group-id
                              (scad-sketch-session--tree-find-group
                               (scad-sketch-session-tree session)
                               group-id))))
         (when group
           (scad-sketch--draw-group-box-halo svg transform session group))))

      ('boolean-members
       (let* ((group-id (scad-sketch--ref-group-id ref))
              (group    (and group-id
                              (scad-sketch-session--tree-find-group
                               (scad-sketch-session-tree session)
                               group-id)))
              (ds       (and group
                             (scad-sketch--tree-path-ds
                              session transform group)))
              (compound (and ds (scad-sketch--compound-d ds))))
         (when compound
           (scad-sketch--draw-compound-halo svg compound))))

      ('mirror
       (let* ((mirror-id (scad-sketch--ref-mirror-id ref))
              (mirror    (and mirror-id
                              (scad-sketch-session--tree-find-mirror
                               (scad-sketch-session-tree session)
                               mirror-id))))
         (when mirror
           (scad-sketch--draw-mirror-axis svg transform session mirror)))))))

(defun scad-sketch--draw-attention-halo (svg screen &optional radius)
  "Draw a visible attention halo around SCREEN.

This is intentionally separate from selected/active styling so the currently
attended object remains obvious even when it is already selected."
  (let ((r (or radius 11)))
    (svg-circle svg (nth 0 screen) (nth 1 screen)
                (+ r 4)
                :stroke "#0057c2"
                :stroke-width 5
                :stroke-opacity 0.18
                :fill "none")
    (svg-circle svg (nth 0 screen) (nth 1 screen)
                r
                :stroke "#0057c2"
                :stroke-width 2
                :stroke-opacity 0.65
                :fill "none")))

(defun scad-sketch--draw-group-highlight (svg transform session)
  "Draw boolean group-level highlight for selected refs.

Returns the shape ids whose groups were highlighted.  Attention is not handled
here; `scad-sketch--draw-ref-geometry-halo' handles the current attention ref."
  (let ((tree (scad-sketch-session-tree session))
        highlighted)
    (when (and tree (scad-sketch--root-is-boolean-p session))
      (dolist (ref (scad-sketch-session-selection session))
        (pcase (scad-sketch--ref-kind ref)
          ('shape
           (let* ((shape-id (scad-sketch--ref-shape-id ref))
                  (group    (and shape-id
                                  (scad-sketch--deepest-containing-group
                                   tree shape-id))))
             (when group
               (let* ((all-ds   (scad-sketch--tree-path-ds
                                  session transform group))
                      (compound (scad-sketch--compound-d all-ds)))
                 (scad-sketch--draw-compound svg compound "#ffffff" "#d13f00" 5))
               (push shape-id highlighted))))

          ('boolean
           (let* ((group-id (plist-get ref :group-id))
                  (group    (and group-id
                                  (scad-sketch--tree-find-group-by-id
                                   tree group-id))))
             (when group
               (let* ((all-ds   (scad-sketch--tree-path-ds
                                  session transform group))
                      (compound (scad-sketch--compound-d all-ds)))
                 (scad-sketch--draw-compound svg compound "#ffffff" "#d13f00" 5))))))))
    highlighted))

(defun scad-sketch--label-gradient (defs id color-a color-b)
  "Ensure text gradient ID exists in DEFS and return url(#ID)."
  (unless (cl-find-if
           (lambda (node)
             (and (consp node)
                  (eq (car node) 'linearGradient)
                  (equal (plist-get (cadr node) :id) id)))
           (cddr defs))
    (let ((grad (svg-node defs 'linearGradient
                          :id id
                          :x1 "0%" :y1 "0%"
                          :x2 "100%" :y2 "0%")))
      (svg-node grad 'stop
                :offset "0%"
                :stop-color color-a)
      (svg-node grad 'stop
                :offset "100%"
                :stop-color color-b)))
  (format "url(#%s)" id))

(defun scad-sketch--boolean-group-label-state (session group)
  "Return plist state for GROUP's label.

The label is attention-highlighted when either the group wrapper or group
members ref has attention.  It is selection-highlighted when all child shapes
are selected directly."
  (let* ((group-id    (plist-get group :group-id))
         (attention   (scad-sketch--attention-ref session))
         (group-ref   (scad-sketch--boolean-ref group-id))
         (members-ref (scad-sketch--boolean-members-ref group-id))
         (shape-ids   (scad-sketch-session--tree-shape-ids group))
         (selected    (and shape-ids
                           (cl-every
                            (lambda (shape-id)
                              (scad-sketch--shape-selected-p session shape-id))
                            shape-ids)))
         (attn        (or (and attention
                               (scad-sketch--same-ref-p attention group-ref))
                          (and attention
                               (scad-sketch--same-ref-p attention members-ref)))))
    (list :selected selected
          :attention attn)))

(defun scad-sketch--draw-label (svg label x y &optional selected attention active)
  "Draw LABEL at X Y with unobtrusive state-aware styling.

Plain labels are small and gray.  Selected labels use an orange gradient.
Attention labels use a blue gradient matching the attention halo.  Active labels
get a slightly darker neutral treatment.  This deliberately does not draw a
label background box."
  (let* ((text (format "%s" label))
         (defs (scad-sketch--ensure-defs svg))
         (fill (cond
                (attention
                 (scad-sketch--label-gradient defs
                                              "scad-sketch-label-attention"
                                              "#0057c2"
                                              "#6fa8ff"))
                (selected
                 (scad-sketch--label-gradient defs
                                              "scad-sketch-label-selected"
                                              "#d13f00"
                                              "#ff9a55"))
                (active
                 "#333333")
                (t
                 "#555555")))
         (stroke (cond
                  (attention "#dfefff")
                  (selected  "#fff0e8")
                  (t         nil))))
    (apply #'svg-text svg text
           (append
            (list :x x
                  :y y
                  :font-size 11
                  :font-weight (if (or selected attention) "bold" "normal")
                  :fill fill)
            ;; A very thin pale stroke keeps gradient text readable without
            ;; turning labels into badges/boxes.
            (when stroke
              (list :stroke stroke
                    :stroke-width 0.35))))))

;;;; ── Grid ───────────────────────────────────────────────────────────────────

(defun scad-sketch--draw-grid (svg bounds transform session)
  "Draw background grid lines and axes."
  (pcase-let ((`(,min-x ,max-x ,min-y ,max-y) bounds))
    (let* ((grid (max 0.0001 (scad-sketch-session-grid session)))
           (x    (* grid (floor (/ min-x grid))))
           (y    (* grid (floor (/ min-y grid)))))
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

;;;; ── Per-shape editor overlays ──────────────────────────────────────────────
;;
;; In boolean sessions, per-shape path strokes are suppressed.  The preview
;; and group-highlight layers carry geometry.  Vertex dots always render.
;; In non-boolean sessions, full per-shape strokes are drawn as before.
(defun scad-sketch--draw-vertex-dot (svg screen point-ref sel attn
                                         active-shape session pt transform
                                         closed points idx n)
  "Draw one vertex dot and its polyRound radius annotation."
  (let ((radius (scad-sketch--point-radius pt))
        (xy     (scad-sketch--point-xy pt)))
    (when (scad-sketch--hover-attention-p session point-ref)
      (scad-sketch--draw-attention-halo svg screen 11))
    (svg-circle svg (nth 0 screen) (nth 1 screen)
                (cond (sel 8) (attn 7) (active-shape 6) (t 5))
                :stroke       (cond (sel          "#d13f00")
                                    (attn         "#0057c2")
                                    (active-shape "#333333")
                                    (t            "#777777"))
                :stroke-width (cond (sel 3) (attn 3) (t 2))
                :fill         (cond (sel          "#fff0e8")
                                    (attn         "#dfefff")
                                    (active-shape "#ffffff")
                                    (t            "#f8f8f8")))
    (scad-sketch--draw-label
     svg
     (format "%s:%d" (scad-sketch--ref-shape-id point-ref) idx)
     (+ (nth 0 screen) 8)
     (- (nth 1 screen) 8)
     sel attn active-shape)
    (when (> radius 0)
      (let* ((prev    (cond ((> idx 0)      (nth (1- idx) points))
                            (closed         (nth (1- n)   points))))
             (next    (cond ((< idx (1- n)) (nth (1+ idx) points))
                            (closed         (nth 0        points))))
             (corner  (when (and prev next)
                        (scad-sketch--corner-geometry
                         (scad-sketch--point-xy prev) xy
                         (scad-sketch--point-xy next) radius)))
             (actual-r (if corner (plist-get corner :radius) radius))
             (capped   (and corner (< (+ actual-r 0.001) radius)))
             (r-label  (if capped
                           (format "r=%s→%s"
                                   (scad-sketch--fmt-num radius)
                                   (scad-sketch--fmt-num actual-r))
                         (format "r=%s" (scad-sketch--fmt-num actual-r)))))
        (svg-circle svg (nth 0 screen) (nth 1 screen)
                    (scad-sketch--pixel-radius actual-r transform)
                    :stroke (if capped "#c04000" "#804000")
                    :stroke-width 1 :stroke-dasharray "3,3" :fill "none")
        (scad-sketch--draw-label
         svg r-label
         (+ (nth 0 screen) 8)
         (+ (nth 1 screen) 18)
         sel attn active-shape)))))

(defun scad-sketch--draw-one-polygon-shape (svg transform session shape
                                                &optional suppress-stroke)
  "Draw polygon SHAPE editor overlay.
When SUPPRESS-STROKE is non-nil, skip path stroke; vertex dots still drawn."
  (let* ((points      (scad-sketch-shape-points shape))
         (closed      (scad-sketch-shape-closed shape))
         (shape-id    (scad-sketch-shape-id shape))
         (n           (length points))
         (shape-sel   (scad-sketch--shape-selected-p session shape-id))
         (attention   (scad-sketch--attention-ref session))
         (shape-attn  (and attention
                           (eq (scad-sketch--ref-kind attention) 'shape)
                           (eq (scad-sketch--ref-shape-id attention) shape-id)))
         (active-shape (eq shape-id
                           (scad-sketch-session-active-shape-id session))))
    (when (and (not suppress-stroke) (>= n 2))
      (let* ((boolean-session (scad-sketch--root-is-boolean-p session))
             (ghosted     (and boolean-session
                               (not shape-sel)
                               (not shape-attn)
                               (not active-shape)))
             (stroke      (cond (shape-sel    "#d13f00")
                                (shape-attn   "#0057c2")
                                (active-shape "#333333")
                                (ghosted      "#9a9a9a")
                                (t            "#777777")))
             (width       (cond ((or shape-sel shape-attn) 5)
                                (active-shape 4) (ghosted 1.5) (t 3)))
             (dash        (and ghosted "6,4"))
             (path-attrs  (append (list :stroke stroke :stroke-width width
                                        :fill "none")
                                  (when dash (list :stroke-dasharray dash)))))
        (if (scad-sketch--any-radius-p points)
            (let ((d (scad-sketch--polyround-path-d points closed transform)))
              (when d (apply #'svg-node svg 'path :d d path-attrs)))
          (let ((xy-pts (mapcar #'scad-sketch--point-xy points)))
            (cl-loop for a on xy-pts for b = (cadr a) when b do
                     (apply #'scad-sketch--svg-line svg transform (car a) b
                            path-attrs))
            (when (and closed (> n 2))
              (apply #'scad-sketch--svg-line
                     svg transform (car (last xy-pts)) (car xy-pts)
                     path-attrs))))))
    (when shape-attn
      (let ((center (funcall transform
                             (scad-sketch--shape-center session shape-id))))
        (svg-text svg (format "%s" shape-id)
                  :x (+ (nth 0 center) 10) :y (+ (nth 1 center) 4)
                  :font-size 12 :fill "#0057c2")))
    (let ((idx 0))
      (dolist (pt points)
        (let* ((xy        (scad-sketch--point-xy pt))
               (screen    (funcall transform xy))
               (point-ref (scad-sketch--point-ref idx shape-id))
               (sel       (scad-sketch--point-selected-p session shape-id idx))
               (attn      (and attention
                               (scad-sketch--same-ref-p attention point-ref))))
          (scad-sketch--draw-vertex-dot
           svg screen point-ref sel attn active-shape session pt transform
           closed points idx n))
        (setq idx (1+ idx))))))

(defun scad-sketch--draw-one-square-shape (svg transform session shape
                                               &optional suppress-stroke)
  "Draw square SHAPE editor overlay, including corner and center handles."
  (let* ((shape-id     (scad-sketch-shape-id shape))
         (pts          (scad-sketch--square-corner-points shape))
         (shape-sel    (scad-sketch--shape-selected-p session shape-id))
         (attention    (scad-sketch--attention-ref session))
         (shape-attn   (and attention
                            (eq (scad-sketch--ref-kind attention) 'shape)
                            (eq (scad-sketch--ref-shape-id attention) shape-id)))
         (active-shape (eq shape-id
                           (scad-sketch-session-active-shape-id session))))
    (unless suppress-stroke
      (let* ((boolean-session (scad-sketch--root-is-boolean-p session))
             (ghosted (and boolean-session
                           (not shape-sel)
                           (not shape-attn)
                           (not active-shape)))
             (stroke  (cond (shape-sel    "#d13f00")
                            (shape-attn   "#0057c2")
                            (active-shape "#333333")
                            (ghosted      "#9a9a9a")
                            (t            "#777777")))
             (attrs   (append (list :stroke stroke
                                     :stroke-width (cond (shape-sel 5)
                                                         (shape-attn 4)
                                                         (t 2)))
                              (when ghosted
                                (list :stroke-dasharray "6,4")))))
        (dotimes (i 4)
          (apply #'scad-sketch--svg-line
                 svg transform
                 (nth i pts)
                 (nth (mod (1+ i) 4) pts)
                 attrs))))
    (dotimes (idx (scad-sketch--primitive-handle-count shape))
      (let* ((xy         (scad-sketch--primitive-handle-xy shape idx))
             (screen     (funcall transform xy))
             (point-ref  (scad-sketch--point-ref idx shape-id))
             (sel        (scad-sketch--point-selected-p session shape-id idx))
             (attn       (and attention
                              (scad-sketch--same-ref-p attention point-ref)))
             (is-center  (= idx 4))
             (label      (if is-center
                             (format "%s:center" shape-id)
                           (format "%s:%d" shape-id idx))))
        (when (scad-sketch--hover-attention-p session point-ref)
          (scad-sketch--draw-attention-halo svg screen 11))
        (svg-circle svg (nth 0 screen) (nth 1 screen)
                    (cond (sel 8) (attn 7) (active-shape 6) (t 5))
                    :stroke (cond (sel "#d13f00")
                                  (attn "#0057c2")
                                  (active-shape "#333333")
                                  (is-center "#555555")
                                  (t "#777777"))
                    :stroke-width 2
                    :fill (cond (sel "#fff0e8")
                                (attn "#dfefff")
                                (active-shape "#ffffff")
                                (t "#f8f8f8")))
        (scad-sketch--draw-label
         svg label
         (+ (nth 0 screen) 8)
         (- (nth 1 screen) 8)
         sel attn active-shape)))))

(defun scad-sketch--draw-one-circle-shape (svg transform session shape
                                               &optional suppress-stroke)
  "Draw circle SHAPE editor overlay.

Circle handle indices:
  0 center
  1 east radius
  2 north radius"
  (let* ((shape-id     (scad-sketch-shape-id shape))
         (md           (scad-sketch-shape-metadata shape))
         (center       (list (float (or (plist-get md :cx) 0.0))
                             (float (or (plist-get md :cy) 0.0))))
         (r            (float (or (plist-get md :r) 0.0)))
         (screen       (funcall transform center))
         (pr           (scad-sketch--pixel-radius r transform))
         (shape-sel    (scad-sketch--shape-selected-p session shape-id))
         (attention    (scad-sketch--attention-ref session))
         (shape-attn   (and attention
                             (eq (scad-sketch--ref-kind attention) 'shape)
                             (eq (scad-sketch--ref-shape-id attention) shape-id)))
         (active-shape (eq shape-id
                            (scad-sketch-session-active-shape-id session)))
         (boolean-session (scad-sketch--root-is-boolean-p session))
         (ghosted      (and boolean-session
                             (not shape-sel)
                             (not shape-attn)
                             (not active-shape)))
         (stroke       (cond (shape-sel    "#d13f00")
                             (shape-attn   "#0057c2")
                             (active-shape "#333333")
                             (ghosted      "#9a9a9a")
                             (t            "#777777"))))

    (unless suppress-stroke
      (svg-circle svg (nth 0 screen) (nth 1 screen) pr
                  :stroke stroke
                  :stroke-width (cond (shape-sel 5)
                                      (shape-attn 4)
                                      (active-shape 3)
                                      (t 2))
                  :stroke-dasharray (if ghosted "6,4" nil)
                  :fill "none"))

    (dolist (idx '(1 2))
      (let ((handle-xy (scad-sketch--primitive-handle-xy shape idx)))
        (when handle-xy
          (scad-sketch--svg-line svg transform center handle-xy
                                 :stroke "#999999"
                                 :stroke-width 1
                                 :stroke-dasharray "3,3"))))

    (dotimes (idx (scad-sketch--primitive-handle-count shape))
      (let* ((handle-xy  (scad-sketch--primitive-handle-xy shape idx))
             (handle-ref (scad-sketch--point-ref idx shape-id)))
        (when handle-xy
          (let* ((handle-s (funcall transform handle-xy))
                 (sel      (scad-sketch--point-selected-p session shape-id idx))
                 (attn     (and attention
                                (scad-sketch--same-ref-p attention handle-ref)))
                 (is-center (= idx 0))
                 (radius   (cond (sel 8)
                                 (attn 7)
                                 (active-shape 6)
                                 (is-center 5)
                                 (t 5)))
                 (handle-stroke
                  (cond (sel "#d13f00")
                        (attn "#0057c2")
                        (active-shape "#333333")
                        (is-center "#555555")
                        (t "#777777")))
                 (handle-fill
                  (cond (sel "#fff0e8")
                        (attn "#dfefff")
                        (is-center "#ffffff")
                        (t "#f8f8f8")))
                 (label
                  (pcase idx
                    (0 (format "%s:center" shape-id))
                    (1 (format "%s:r-east" shape-id))
                    (2 (format "%s:r-north" shape-id))
                    (_ (format "%s:%d" shape-id idx)))))
            (when (scad-sketch--hover-attention-p session handle-ref)
              (scad-sketch--draw-attention-halo svg handle-s 11))
            (svg-circle svg
                        (nth 0 handle-s)
                        (nth 1 handle-s)
                        radius
                        :stroke handle-stroke
                        :stroke-width 2
                        :fill handle-fill)
            (scad-sketch--draw-label
             svg label
             (+ (nth 0 handle-s) 8)
             (- (nth 1 handle-s) 8)
             sel attn active-shape)))))

    (let ((east (scad-sketch--primitive-handle-xy shape 1)))
      (when east
        (let ((east-s (funcall transform east)))
          (scad-sketch--draw-label
           svg
           (format "r=%s" (scad-sketch--fmt-num r))
           (+ (nth 0 east-s) 8)
           (+ (nth 1 east-s) 16)
           shape-sel shape-attn active-shape))))))

(defun scad-sketch--draw-one-text-shape (svg transform session shape
                                             &optional suppress-stroke)
  "Draw text SHAPE editor overlay."
  (let* ((shape-id    (scad-sketch-shape-id shape))
         (md          (scad-sketch-shape-metadata shape))
         (str         (or (plist-get md :str) ""))
         (x           (float (or (plist-get md :x) 0.0)))
         (y           (float (or (plist-get md :y) 0.0)))
         (size        (float (or (plist-get md :size) 10.0)))
         (angle       (float (or (plist-get md :angle) 0.0)))
         (font        (plist-get md :font))
         (screen      (funcall transform (list x y)))
         (font-px     (max 8 (scad-sketch--pixel-radius size transform)))
         (shape-sel   (scad-sketch--shape-selected-p session shape-id))
         (attention   (scad-sketch--attention-ref session))
         (shape-attn  (and attention
                           (eq (scad-sketch--ref-kind attention) 'shape)
                           (eq (scad-sketch--ref-shape-id attention) shape-id)))
         (origin-ref  (scad-sketch--point-ref 0 shape-id))
         (origin-sel  (scad-sketch--point-selected-p session shape-id 0))
         (origin-attn (and attention
                           (scad-sketch--same-ref-p attention origin-ref)))
         (active-shape (eq shape-id
                           (scad-sketch-session-active-shape-id session))))
    (unless suppress-stroke
      (pcase-let ((`(,min-x ,max-x ,min-y ,max-y)
                   (scad-sketch--text-rough-bounds shape)))
        (let* ((p0 (funcall transform (list min-x min-y)))
               (p1 (funcall transform (list max-x max-y)))
               (rx (min (nth 0 p0) (nth 0 p1)))
               (ry (min (nth 1 p0) (nth 1 p1)))
               (rw (abs (- (nth 0 p1) (nth 0 p0))))
               (rh (abs (- (nth 1 p1) (nth 1 p0)))))
          (svg-rectangle svg rx ry rw rh
                         :stroke (cond (shape-sel "#d13f00")
                                       (shape-attn "#0057c2")
                                       (active-shape "#333333")
                                       (t "#777777"))
                         :stroke-width (cond (shape-sel 4)
                                             (shape-attn 3)
                                             (t 1))
                         :stroke-dasharray "4,4"
                         :fill "none"))))
    (apply #'svg-text svg str
           (append
            (list :x (nth 0 screen)
                  :y (nth 1 screen)
                  :font-size font-px
                  :fill "#111111")
            (when font
              (list :font-family font))
            (unless (< (abs angle) 0.000001)
              (list :transform
                    (format "rotate(%s %s %s)"
                            (scad-sketch--fmt-num (- angle))
                            (scad-sketch--fmt-num (nth 0 screen))
                            (scad-sketch--fmt-num (nth 1 screen)))))))
    (when (scad-sketch--hover-attention-p session origin-ref)
      (scad-sketch--draw-attention-halo svg screen 11))
    (svg-circle svg (nth 0 screen) (nth 1 screen)
                (cond (origin-sel 8)
                      (origin-attn 7)
                      (shape-sel 8)
                      (shape-attn 7)
                      (active-shape 6)
                      (t 5))
                :stroke (cond (origin-sel "#d13f00")
                              (origin-attn "#0057c2")
                              (shape-sel "#d13f00")
                              (shape-attn "#0057c2")
                              (active-shape "#333333")
                              (t "#777777"))
                :stroke-width 2
                :fill "#ffffff")
    (scad-sketch--draw-label
     svg
     (format "%s" shape-id)
     (+ (nth 0 screen) 8)
     (- (nth 1 screen) 8)
     (or origin-sel shape-sel)
     (or origin-attn shape-attn)
     active-shape)))

;;;; ── Boolean bounding boxes ─────────────────────────────────────────────────
(defun scad-sketch--shape-bounds (shape)
  "Return (MIN-X MAX-X MIN-Y MAX-Y) for SHAPE."
  (pcase (scad-sketch-shape-kind shape)
    ('circle
     (scad-sketch--circle-bounds shape))
    ('square
     (let ((pts (scad-sketch--square-corner-points shape)))
       (list (apply #'min (mapcar #'car pts))
             (apply #'max (mapcar #'car pts))
             (apply #'min (mapcar #'cadr pts))
             (apply #'max (mapcar #'cadr pts)))))
    ('text
     (scad-sketch--text-rough-bounds shape))
    ('polygon
     (let ((pts (mapcar #'scad-sketch--point-xy
                        (scad-sketch-shape-points shape))))
       (when pts
         (list (apply #'min (mapcar #'car  pts))
               (apply #'max (mapcar #'car  pts))
               (apply #'min (mapcar #'cadr pts))
               (apply #'max (mapcar #'cadr pts))))))
    (_ nil)))

(defun scad-sketch--merge-bounds (bounds-list)
  "Merge BOUNDS-LIST into one (MIN-X MAX-X MIN-Y MAX-Y) tuple."
  (let ((bl (delq nil bounds-list)))
    (when bl
      (list (apply #'min (mapcar #'car    bl))
            (apply #'max (mapcar #'cadr   bl))
            (apply #'min (mapcar #'caddr  bl))
            (apply #'max (mapcar #'cadddr bl))))))

(defun scad-sketch--tree-bounds (session tree)
  "Return model-space bounds for TREE as (MIN-X MAX-X MIN-Y MAX-Y)."
  (let ((pts (scad-sketch--tree-xy-points session tree)))
    (when pts
      (list (apply #'min (mapcar #'car pts))
            (apply #'max (mapcar #'car pts))
            (apply #'min (mapcar #'cadr pts))
            (apply #'max (mapcar #'cadr pts))))))

(defun scad-sketch--draw-boolean-boxes (svg transform session tree)
  "Draw labelled dotted bounding boxes for boolean groups in TREE.

Boolean labels use the same unobtrusive state-aware styling as point/shape
labels."
  (pcase (and tree (plist-get tree :kind))
    ('boolean
     (let* ((op     (plist-get tree :op))
            (bounds (scad-sketch--tree-bounds session tree)))
       (when bounds
         (pcase-let ((`(,min-x ,max-x ,min-y ,max-y) bounds))
           (let* ((p0    (funcall transform (list min-x min-y)))
                  (p1    (funcall transform (list max-x max-y)))
                  (x     (min (nth 0 p0) (nth 0 p1)))
                  (y     (min (nth 1 p0) (nth 1 p1)))
                  (w     (abs (- (nth 0 p1) (nth 0 p0))))
                  (h     (abs (- (nth 1 p1) (nth 1 p0))))
                  (state (scad-sketch--boolean-group-label-state
                          session tree))
                  (selected  (plist-get state :selected))
                  (attention (plist-get state :attention)))
             (svg-rectangle svg x y w h
                            :stroke (cond (attention "#0057c2")
                                          (selected  "#d13f00")
                                          (t         "#6a5acd"))
                            :stroke-width (cond (attention 2.5)
                                                (selected  2)
                                                (t         1))
                            :stroke-dasharray "6,4"
                            :fill "none")
             (scad-sketch--draw-label
              svg
              (format "%s" op)
              (+ x 6)
              (+ y 14)
              selected
              attention
              nil)))))
     (dolist (child (plist-get tree :children))
       (scad-sketch--draw-boolean-boxes svg transform session child)))

    ('mirror
     (scad-sketch--draw-boolean-boxes
      svg transform session (plist-get tree :child)))

    ('sequence
     (dolist (child (plist-get tree :children))
       (scad-sketch--draw-boolean-boxes svg transform session child)))))

;;;; ── Main scene draw pass ───────────────────────────────────────────────────
(defun scad-sketch--draw-path (svg transform session)
  "Draw the full scene: preview, mirror axes, selection, attention, overlays."
  (scad-sketch-session-sync-active-shape-from-points session)
  (let ((boolean-session (scad-sketch--root-is-boolean-p session)))

    ;; Layer 1 – Semantic preview, including mirror output outlines.
    (when (scad-sketch-session-tree session)
      (let ((defs (scad-sketch--ensure-defs svg)))
        (setq scad-sketch--svg-id-counter 0)
        (scad-sketch--render-tree svg defs transform session
                                  (scad-sketch-session-tree session))))

    ;; Layer 2 – Mirror axes and axis handles.
    (scad-sketch--draw-mirror-axes svg transform session)

    ;; Layer 3 – Selection highlight for boolean sessions.
    (when boolean-session
      (scad-sketch--draw-group-highlight svg transform session))

    ;; Layer 4 – Hover-attention geometry halo, if present.
    (when (fboundp 'scad-sketch--hover-attention-ref)
      (let ((hover-attention (scad-sketch--hover-attention-ref session)))
        (unless (eq (and hover-attention
                         (scad-sketch--ref-kind hover-attention))
                    'point)
          (scad-sketch--draw-ref-geometry-halo
           svg transform session hover-attention))))

    ;; Layer 5 – Per-shape source-side interactive overlays.
    (dolist (shape (scad-sketch-session-shapes session))
      (let* ((shape-id    (scad-sketch-shape-id shape))
             (point-attn  (let ((att (scad-sketch--attention-ref session)))
                            (and att
                                 (eq (scad-sketch--ref-kind att) 'point)
                                 (eq (scad-sketch--ref-shape-id att) shape-id))))
             (suppress    (and boolean-session (not point-attn))))
        (pcase (scad-sketch-shape-kind shape)
          ('polygon (scad-sketch--draw-one-polygon-shape
                     svg transform session shape suppress))
          ('circle  (scad-sketch--draw-one-circle-shape
                     svg transform session shape suppress))
          ('square  (scad-sketch--draw-one-square-shape
                     svg transform session shape suppress))
          ('text    (scad-sketch--draw-one-text-shape
                     svg transform session shape suppress)))))

    ;; Layer 6 – Boolean bounding-box labels.
    (when (scad-sketch-session-tree session)
      (scad-sketch--draw-boolean-boxes
       svg transform session (scad-sketch-session-tree session)))))

;;;; ── Marks and cursor overlay ───────────────────────────────────────────────

(defun scad-sketch--draw-point-and-marks (svg transform session)
  "Draw mark dots, mark→cursor dashed lines, and the cursor crosshair."
  (let* ((marks  (scad-sketch-session-marks session))
         (cursor (scad-sketch-session-point session)))
    (let ((ordered (reverse marks)))
      (cl-loop for a on ordered for b = (cadr a) when b do
               (scad-sketch--svg-line svg transform (car a) b
                                      :stroke "#008a2e" :stroke-width 1
                                      :stroke-dasharray "4,4"))
      (when ordered
        (scad-sketch--svg-line svg transform (car (last ordered)) cursor
                               :stroke "#008a2e" :stroke-width 1
                               :stroke-dasharray "4,4")))
    (dolist (m (reverse marks))
      (let* ((screen  (funcall transform m))
             (current (equal m (car marks)))
             (color   (if current "#008a2e" "#50a870")))
        (svg-circle svg (nth 0 screen) (nth 1 screen) 6
                    :stroke color :stroke-width 2 :fill "#e2ffe9")
        (when current
          (svg-text svg "mark"
                    :x (+ (nth 0 screen) 10) :y (+ (nth 1 screen) 4)
                    :font-size 12 :fill color))))
    (let ((p (funcall transform cursor)))
      (svg-circle svg (nth 0 p) (nth 1 p) 5
                  :stroke "#0057c2" :stroke-width 2 :fill "#dfefff")
      (svg-line svg (- (nth 0 p) 10) (nth 1 p) (+ (nth 0 p) 10) (nth 1 p)
                :stroke "#0057c2" :stroke-width 2)
      (svg-line svg (nth 0 p) (- (nth 1 p) 10) (nth 0 p) (+ (nth 1 p) 10)
                :stroke "#0057c2" :stroke-width 2)
      (svg-text svg "point"
                :x (+ (nth 0 p) 12) :y (+ (nth 1 p) 4)
                :font-size 12 :fill "#0057c2"))))

;;;; ── HUD / status bar ───────────────────────────────────────────────────────

(defun scad-sketch--draw-hud (svg session)
  "Draw the status bar at the top of the canvas."
  (let* ((marks    (scad-sketch-session-marks session))
         (mark-str (cond ((null marks) "none")
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
         (text      (format
                     "%s  root=%s  shapes=%d  active=%s  grid=%s%s  point=%s  mark=%s  attn=%s  sel=%s  %s"
                     (scad-sketch-session-name session)
                     root
                     (length (scad-sketch-session-shapes session))
                     (scad-sketch-session-active-shape-id session)
                     (scad-sketch--fmt-num (scad-sketch-session-grid session))
                     (scad-sketch-session-units session)
                     (scad-sketch--fmt-xy (scad-sketch-session-point session))
                     mark-str
                     (scad-sketch--ref-summary attention)
                     (scad-sketch--selection-summary session)
                     (if (scad-sketch-session-dirty session) "*dirty*" "saved"))))
    (svg-rectangle svg 0 0 scad-sketch-canvas-width 28 :fill "#f8f8f8")
    (svg-text svg text :x 10 :y 19 :font-size 13 :fill "#111111")))

;;;; ── Top-level render entry point ───────────────────────────────────────────

(defun scad-sketch--render ()
  "Re-render the editor buffer from current session state."
  (let* ((session   (scad-sketch--assert-session))
         (svg       (svg-create scad-sketch-canvas-width scad-sketch-canvas-height))
         (bounds    (scad-sketch--bounds session))
         (transform (scad-sketch--transform bounds)))
    (svg-rectangle svg 0 0 scad-sketch-canvas-width scad-sketch-canvas-height
                   :fill "#ffffff")
    (scad-sketch--draw-grid            svg bounds transform session)
    (scad-sketch--draw-path            svg transform session)
    (scad-sketch--draw-point-and-marks svg transform session)
    (scad-sketch--draw-hud             svg session)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((beg (point)))
        (insert-image (svg-image svg :ascent 'center))
        (remove-text-properties beg (point) '(keymap nil)))
      (insert "\n\n")
      (insert (scad-sketch--emit-content session))
      (goto-char (point-min)))))

(defun scad-sketch--emit-content (session)
  "Return the live source preview string for SESSION."
  (scad-sketch-session-preview session))

(provide 'scad-sketch-editor--rendering)
;;; scad-sketch-editor--rendering.el ends here
