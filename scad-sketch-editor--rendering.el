;;; scad-sketch-editor--rendering.el --- SVG canvas rendering -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns everything that produces pixels: bounds/transform computation,
;; grid drawing, per-shape drawing (polygon + circle), boolean tree
;; preview, marks/cursor overlay, HUD status bar, and the top-level
;; `scad-sketch--render' entry point called by the change dispatch triad
;; in `scad-sketch-editor-core'.
;;
;; This file is intentionally read-only over session state: it queries
;; but never mutates.

;;; Code:

(require 'cl-lib)
(require 'svg)
(require 'scad-sketch-session)
(require 'scad-sketch-geometry)
(require 'scad-sketch-editor--refs)
(require 'scad-sketch-editor--selection)
(require 'scad-sketch-editor-core)

;;; Canvas geometry constants

(defcustom scad-sketch-canvas-width 900
  "Sketch editor canvas width in pixels."
  :type 'integer :group 'scad-sketch)

(defcustom scad-sketch-canvas-height 650
  "Sketch editor canvas height in pixels."
  :type 'integer :group 'scad-sketch)

(defcustom scad-sketch-margin 48
  "Canvas margin in pixels."
  :type 'integer :group 'scad-sketch)

;;; Numeric formatting

(defun scad-sketch--fmt-num (n)
  "Format number N compactly (drop trailing zeros after decimal)."
  (let ((s (format "%.4f" n)))
    (string-trim-right (string-trim-right s "0") "\\.")))

(defun scad-sketch--fmt-xy (xy)
  "Format XY pair compactly."
  (format "(%s, %s)"
          (scad-sketch--fmt-num (nth 0 xy))
          (scad-sketch--fmt-num (nth 1 xy))))

;;; Bounds and coordinate transform

(defun scad-sketch--circle-bounds (shape)
  "Return (MIN-X MAX-X MIN-Y MAX-Y) for circle SHAPE."
  (let* ((md (scad-sketch-shape-metadata shape))
         (cx (plist-get md :cx))
         (cy (plist-get md :cy))
         (r  (plist-get md :r)))
    (list (- cx r) (+ cx r) (- cy r) (+ cy r))))

(defun scad-sketch--shape-xy-points (shape)
  "Return representative XY points for SHAPE."
  (pcase (scad-sketch-shape-kind shape)
    ('polygon
     (mapcar #'scad-sketch--point-xy (scad-sketch-shape-points shape)))
    ('circle
     (let ((b (scad-sketch--circle-bounds shape)))
       (list (list (nth 0 b) (nth 2 b))
             (list (nth 1 b) (nth 3 b)))))
    (_ nil)))

(defun scad-sketch--bounds (session)
  "Return (MIN-X MAX-X MIN-Y MAX-Y) for all shapes, marks, and cursor."
  (scad-sketch-session-sync-active-shape-from-points session)
  (let* ((shape-points
          (apply #'append
                 (mapcar #'scad-sketch--shape-xy-points
                         (scad-sketch-session-shapes session))))
         (extra (delq nil (cons (scad-sketch-session-point session)
                                (scad-sketch-session-marks session))))
         (all   (append shape-points extra)))
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
          (list (- min-x px) (+ max-x px)
                (- min-y py) (+ max-y py)))))))

(defun scad-sketch--transform (bounds)
  "Return a pixel-coordinate closure for BOUNDS."
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
  "Convert model radius R to pixel radius using TRANSFORM."
  (let* ((o (funcall transform '(0 0)))
         (p (funcall transform (list r 0))))
    (abs (- (nth 0 p) (nth 0 o)))))

;;; SVG primitives

(defun scad-sketch--svg-line (svg transform a b &rest args)
  "Draw a model-space line A→B on SVG using TRANSFORM."
  (let ((pa (funcall transform a))
        (pb (funcall transform b)))
    (apply #'svg-line svg
           (nth 0 pa) (nth 1 pa)
           (nth 0 pb) (nth 1 pb)
           args)))

(defun scad-sketch--draw-shape-path (svg d &rest attrs)
  "Draw SVG path D on SVG with ATTRS (ignored when D is nil)."
  (when d
    (apply #'svg-node svg 'path (append (list :d d) attrs))))

;;; Grid

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

;;; Shape path generation

(defun scad-sketch--shape-path-d (shape transform)
  "Return an SVG path string for SHAPE under TRANSFORM."
  (pcase (scad-sketch-shape-kind shape)
    ('polygon
     (let* ((points (scad-sketch-shape-points shape))
            (closed (scad-sketch-shape-closed shape)))
       (if (scad-sketch--any-radius-p points)
           (scad-sketch--polyround-path-d points closed transform)
         (let* ((xy-points     (mapcar #'scad-sketch--point-xy points))
                (screen-points (mapcar transform xy-points)))
           (when screen-points
             (concat
              (format "M %s %s"
                      (scad-sketch--fmt-num (nth 0 (car screen-points)))
                      (scad-sketch--fmt-num (nth 1 (car screen-points))))
              (mapconcat
               (lambda (p)
                 (format " L %s %s"
                         (scad-sketch--fmt-num (nth 0 p))
                         (scad-sketch--fmt-num (nth 1 p))))
               (cdr screen-points) "")
              (if (and closed (> (length screen-points) 2)) " Z" "")))))))
    ('circle
     (let* ((md     (scad-sketch-shape-metadata shape))
            (center (funcall transform
                             (list (plist-get md :cx) (plist-get md :cy))))
            (r      (scad-sketch--pixel-radius (plist-get md :r) transform))
            (cx     (nth 0 center))
            (cy     (nth 1 center)))
       (format "M %s %s A %s %s 0 1 0 %s %s A %s %s 0 1 0 %s %s"
               (scad-sketch--fmt-num (- cx r)) (scad-sketch--fmt-num cy)
               (scad-sketch--fmt-num r)         (scad-sketch--fmt-num r)
               (scad-sketch--fmt-num (+ cx r)) (scad-sketch--fmt-num cy)
               (scad-sketch--fmt-num r)         (scad-sketch--fmt-num r)
               (scad-sketch--fmt-num (- cx r)) (scad-sketch--fmt-num cy))))
    (_ nil)))

;;; Boolean tree preview

(defun scad-sketch--root-is-boolean-p (session)
  "Return non-nil if SESSION has a boolean root target."
  (let ((root (scad-sketch-session-root-target session)))
    (and root (eq (scad-sketch-target-kind root) 'boolean))))

(defun scad-sketch--draw-tree-preview (svg transform session tree &optional mode)
  "Draw a boolean preview for TREE.

MODE: nil/'solid = positive preview; 'erase = difference child;
      'ghost = dashed outline only."
  (pcase (plist-get tree :kind)
    ('shape
     (let* ((shape (scad-sketch-session-shape-by-id
                    session (plist-get tree :shape-id)))
            (d     (and shape (scad-sketch--shape-path-d shape transform))))
       (when d
         (pcase mode
           ('erase
            (scad-sketch--draw-shape-path
             svg d :fill "#ffffff" :stroke "#999999"
             :stroke-width 2 :stroke-dasharray "6,4"))
           ('ghost
            (scad-sketch--draw-shape-path
             svg d :fill "none" :stroke "#aaaaaa"
             :stroke-width 2 :stroke-dasharray "6,4"))
           (_
            (scad-sketch--draw-shape-path
             svg d :fill "#ffffff" :stroke "#777777" :stroke-width 3))))))
    ('boolean
     (let ((op       (plist-get tree :op))
           (children (plist-get tree :children)))
       (pcase mode
         ((or 'erase 'ghost)
          (dolist (child children)
            (scad-sketch--draw-tree-preview svg transform session child mode)))
         (_
          (pcase op
            ('union
             (dolist (child children)
               (scad-sketch--draw-tree-preview
                svg transform session child 'solid)))
            ('difference
             (when children
               (scad-sketch--draw-tree-preview
                svg transform session (car children) 'solid)
               (dolist (child (cdr children))
                 (scad-sketch--draw-tree-preview
                  svg transform session child 'erase))))
            ('intersection
             (when children
               (scad-sketch--draw-tree-preview
                svg transform session (car children) 'solid)
               (dolist (child (cdr children))
                 (scad-sketch--draw-tree-preview
                  svg transform session child 'ghost))))
            (_
             (dolist (child children)
               (scad-sketch--draw-tree-preview
                svg transform session child 'solid))))))))
    (_ nil)))

;;; Boolean bounding boxes

(defun scad-sketch--shape-bounds (shape)
  "Return (MIN-X MAX-X MIN-Y MAX-Y) for SHAPE in model space."
  (pcase (scad-sketch-shape-kind shape)
    ('circle (scad-sketch--circle-bounds shape))
    ('polygon
     (let ((points (mapcar #'scad-sketch--point-xy
                           (scad-sketch-shape-points shape))))
       (when points
         (list (apply #'min (mapcar #'car   points))
               (apply #'max (mapcar #'car   points))
               (apply #'min (mapcar #'cadr  points))
               (apply #'max (mapcar #'cadr  points))))))
    (_ nil)))

(defun scad-sketch--merge-bounds (bounds-list)
  "Merge BOUNDS-LIST into one (MIN-X MAX-X MIN-Y MAX-Y) tuple."
  (let ((bounds-list (delq nil bounds-list)))
    (when bounds-list
      (list (apply #'min (mapcar #'car    bounds-list))
            (apply #'max (mapcar #'cadr   bounds-list))
            (apply #'min (mapcar #'caddr  bounds-list))
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
      (mapcar (lambda (child) (scad-sketch--tree-bounds session child))
              (plist-get tree :children))))
    (_ nil)))

(defun scad-sketch--draw-boolean-boxes (svg transform session tree)
  "Draw dotted labeled bounding boxes for boolean groups in TREE."
  (when (eq (plist-get tree :kind) 'boolean)
    (let* ((op     (plist-get tree :op))
           (bounds (scad-sketch--tree-bounds session tree)))
      (when bounds
        (pcase-let ((`(,min-x ,max-x ,min-y ,max-y) bounds))
          (let* ((p0 (funcall transform (list min-x min-y)))
                 (p1 (funcall transform (list max-x max-y)))
                 (x  (min (nth 0 p0) (nth 0 p1)))
                 (y  (min (nth 1 p0) (nth 1 p1)))
                 (w  (abs (- (nth 0 p1) (nth 0 p0))))
                 (h  (abs (- (nth 1 p1) (nth 1 p0)))))
            (svg-rectangle svg x y w h
                           :stroke "#6a5acd" :stroke-width 1
                           :stroke-dasharray "6,4" :fill "none")
            (svg-text svg (format "%s" op)
                      :x (+ x 6) :y (+ y 14)
                      :font-size 12 :fill "#6a5acd")))))
    (dolist (child (plist-get tree :children))
      (scad-sketch--draw-boolean-boxes svg transform session child))))

;;; Per-shape drawing

(defun scad-sketch--draw-vertex-dot (svg screen point-ref sel attn
                                         active-shape session pt transform
                                         closed points idx n)
  "Draw a single vertex dot and its radius annotation.

This is a shared helper for polygon shape drawing."
  (let ((radius (scad-sketch--point-radius pt))
        (xy     (scad-sketch--point-xy pt)))
    (svg-circle svg (nth 0 screen) (nth 1 screen)
                (cond (sel 8) (attn 7) (active-shape 6) (t 5))
                :stroke      (cond (sel         "#d13f00")
                                   (attn        "#0057c2")
                                   (active-shape "#333333")
                                   (t           "#777777"))
                :stroke-width (cond (sel 3) (attn 3) (t 2))
                :fill        (cond (sel          "#fff0e8")
                                   (attn         "#dfefff")
                                   (active-shape "#ffffff")
                                   (t            "#f8f8f8")))
    (svg-text svg (format "%s:%d" (scad-sketch--ref-shape-id point-ref) idx)
              :x (+ (nth 0 screen) 8) :y (- (nth 1 screen) 8)
              :font-size 11 :fill "#333333")
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
                    :stroke-width 1 :stroke-dasharray "3,3" :fill "none")
        (svg-text svg (if capped
                          (format "r=%s\u2192%s"
                                  (scad-sketch--fmt-num radius)
                                  (scad-sketch--fmt-num actual-r))
                        (format "r=%s" (scad-sketch--fmt-num actual-r)))
                  :x (+ (nth 0 screen) 8) :y (+ (nth 1 screen) 18)
                  :font-size 11
                  :fill (if capped "#c04000" "#804000"))))))

(defun scad-sketch--draw-one-polygon-shape (svg transform session shape)
  "Draw polygon SHAPE with editor overlays."
  (let* ((points         (scad-sketch-shape-points shape))
         (closed         (scad-sketch-shape-closed shape))
         (shape-id       (scad-sketch-shape-id shape))
         (n              (length points))
         (shape-selected (scad-sketch--shape-selected-p session shape-id))
         (attention      (scad-sketch--attention-ref session))
         (shape-attn     (and attention
                              (eq (scad-sketch--ref-kind attention) 'shape)
                              (eq (scad-sketch--ref-shape-id attention) shape-id)))
         (active-shape   (eq shape-id
                             (scad-sketch-session-active-shape-id session)))
         (ghosted        (and (scad-sketch--root-is-boolean-p session)
                              (not shape-selected)
                              (not shape-attn)
                              (not active-shape)))
         (stroke         (cond (shape-selected "#d13f00")
                               (shape-attn     "#0057c2")
                               (active-shape   "#333333")
                               (ghosted        "#9a9a9a")
                               (t              "#777777")))
         (width          (cond ((or shape-selected shape-attn) 5)
                               (active-shape 4) (ghosted 1.5) (t 3)))
         (dash           (and ghosted "6,4")))
    (when (>= n 2)
      (if (scad-sketch--any-radius-p points)
          (let ((d (scad-sketch--polyround-path-d points closed transform)))
            (when d
              (apply #'svg-node svg 'path
                     (append (list :d d :stroke stroke
                                   :stroke-width width :fill "none")
                             (when dash (list :stroke-dasharray dash))))))
        (let ((xy-points  (mapcar #'scad-sketch--point-xy points))
              (line-attrs (append (list :stroke stroke :stroke-width width)
                                  (when dash (list :stroke-dasharray dash)))))
          (cl-loop for a on xy-points for b = (cadr a) when b do
                   (apply #'scad-sketch--svg-line svg transform (car a) b
                          line-attrs))
          (when (and closed (> n 2))
            (apply #'scad-sketch--svg-line
                   svg transform (car (last xy-points)) (car xy-points)
                   line-attrs)))))
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

(defun scad-sketch--draw-one-circle-shape (svg transform session shape)
  "Draw circle SHAPE with editor overlays."
  (let* ((shape-id       (scad-sketch-shape-id shape))
         (md             (scad-sketch-shape-metadata shape))
         (center         (list (plist-get md :cx) (plist-get md :cy)))
         (r              (plist-get md :r))
         (screen         (funcall transform center))
         (pr             (scad-sketch--pixel-radius r transform))
         (shape-selected (scad-sketch--shape-selected-p session shape-id))
         (attention      (scad-sketch--attention-ref session))
         (shape-attn     (and attention
                              (eq (scad-sketch--ref-kind attention) 'shape)
                              (eq (scad-sketch--ref-shape-id attention) shape-id)))
         (active-shape   (eq shape-id
                             (scad-sketch-session-active-shape-id session)))
         (ghosted        (and (scad-sketch--root-is-boolean-p session)
                              (not shape-selected)
                              (not shape-attn)
                              (not active-shape)))
         (attrs          (append
                          (list :stroke (cond (shape-selected "#d13f00")
                                              (shape-attn     "#0057c2")
                                              (active-shape   "#333333")
                                              (ghosted        "#9a9a9a")
                                              (t              "#777777"))
                                :stroke-width (cond ((or shape-selected
                                                         shape-attn) 5)
                                                    (active-shape 4)
                                                    (ghosted 1.5)
                                                    (t 3))
                                :fill "none")
                          (when ghosted (list :stroke-dasharray "6,4")))))
    (apply #'svg-circle svg (nth 0 screen) (nth 1 screen) pr attrs)
    (svg-circle svg (nth 0 screen) (nth 1 screen)
                (cond (shape-selected 8) (shape-attn 7) (active-shape 6) (t 5))
                :stroke      (cond (shape-selected "#d13f00")
                                   (shape-attn     "#0057c2")
                                   (active-shape   "#333333")
                                   (t              "#777777"))
                :stroke-width 2
                :fill        (cond (shape-selected  "#fff0e8")
                                   (shape-attn      "#dfefff")
                                   (active-shape    "#ffffff")
                                   (t               "#f8f8f8")))
    (svg-text svg (format "%s" shape-id)
              :x (+ (nth 0 screen) 8) :y (- (nth 1 screen) 8)
              :font-size 11 :fill "#333333")))

;;; Path drawing (all shapes + boolean boxes)

(defun scad-sketch--draw-path (svg transform session)
  "Draw all shapes and boolean boxes in SESSION."
  (scad-sketch-session-sync-active-shape-from-points session)
  (when (and (scad-sketch--root-is-boolean-p session)
             (scad-sketch-session-tree session))
    (scad-sketch--draw-tree-preview
     svg transform session (scad-sketch-session-tree session) 'solid))
  (dolist (shape (scad-sketch-session-shapes session))
    (pcase (scad-sketch-shape-kind shape)
      ('polygon (scad-sketch--draw-one-polygon-shape  svg transform session shape))
      ('circle  (scad-sketch--draw-one-circle-shape   svg transform session shape))))
  (when (scad-sketch-session-tree session)
    (scad-sketch--draw-boolean-boxes
     svg transform session (scad-sketch-session-tree session))))

;;; Marks and cursor overlay

(defun scad-sketch--draw-point-and-marks (svg transform session)
  "Draw all marks and the cursor crosshair."
  (let* ((marks  (scad-sketch-session-marks session))
         (cursor (scad-sketch-session-point session)))
    ;; Dashed lines connecting marks to cursor
    (let ((ordered (reverse marks)))
      (cl-loop for a on ordered for b = (cadr a) when b do
               (scad-sketch--svg-line svg transform (car a) b
                                      :stroke "#008a2e" :stroke-width 1
                                      :stroke-dasharray "4,4"))
      (when ordered
        (scad-sketch--svg-line svg transform (car (last ordered)) cursor
                               :stroke "#008a2e" :stroke-width 1
                               :stroke-dasharray "4,4")))
    ;; Mark dots
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
    ;; Cursor crosshair
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

;;; HUD / status bar

(defun scad-sketch--draw-hud (svg session)
  "Draw the status bar at the top of the canvas."
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

;;; Top-level render entry point

(defun scad-sketch--render ()
  "Re-render the editor buffer from current session state."
  (let* ((session   (scad-sketch--assert-session))
         (svg       (svg-create scad-sketch-canvas-width scad-sketch-canvas-height))
         (bounds    (scad-sketch--bounds session))
         (transform (scad-sketch--transform bounds)))
    (svg-rectangle svg 0 0 scad-sketch-canvas-width scad-sketch-canvas-height
                   :fill "#ffffff")
    (scad-sketch--draw-grid         svg bounds transform session)
    (scad-sketch--draw-path         svg transform session)
    (scad-sketch--draw-point-and-marks svg transform session)
    (scad-sketch--draw-hud          svg session)
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
