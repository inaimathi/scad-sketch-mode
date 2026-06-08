;;; scad-sketch-editor--editing.el --- Point and shape mutation commands -*- lexical-binding: t; -*-

;;; Commentary:

;; All commands that mutate source geometry: vertex append/insert/delete,
;; line and rectangle creation, closed toggle, radius setting, selected-
;; vertex movement, TAB/hover cycling, selection toggle, and undo restore.
;;
;; Every geometry mutation goes through `scad-sketch--edit', which pushes
;; undo and marks the session dirty.  Focus/selection-only commands use
;; `scad-sketch--clean-change'.

;;; Code:

(require 'cl-lib)
(require 'scad-sketch-session)
(require 'scad-sketch-geometry)
(require 'scad-sketch-editor-core)
(require 'scad-sketch-editor--refs)
(require 'scad-sketch-editor--selection)
(require 'scad-sketch-editor--cursor)    ; for --grid/--fine/--coarse
(require 'scad-sketch-editor--undo)

;;; Internal helpers
(defun scad-sketch--parse-axis-pair (s)
  "Parse S as \"x,y\" or \"x y\" and return (X Y)."
  (unless (string-match
           "\\`[ \t\n]*\\([-+]?[0-9.]+\\(?:[eE][-+]?[0-9]+\\)?\\)[ \t\n]*[,]?[ \t\n]+\\([-+]?[0-9.]+\\(?:[eE][-+]?[0-9]+\\)?\\)[ \t\n]*\\'"
           s)
    ;; Try comma with no whitespace.
    (unless (string-match
             "\\`[ \t\n]*\\([-+]?[0-9.]+\\(?:[eE][-+]?[0-9]+\\)?\\)[ \t\n]*,[ \t\n]*\\([-+]?[0-9.]+\\(?:[eE][-+]?[0-9]+\\)?\\)[ \t\n]*\\'"
             s)
      (user-error "Axis must be two numbers, like 1,0 or 0, 1")))
  (let ((x (string-to-number (match-string 1 s)))
        (y (string-to-number (match-string 2 s))))
    (when (< (+ (* x x) (* y y)) 0.000001)
      (user-error "Mirror axis vector must be non-zero"))
    (list (float x) (float y))))

(defun scad-sketch--select-new-shape (session shape)
  "Make newly created SHAPE the active selected/focused editor object.

This lives in the editor layer because it uses editor refs."
  (let* ((shape-id (scad-sketch-shape-id shape))
         (ref      (scad-sketch--shape-ref shape-id)))
    (scad-sketch-session-set-active-shape session shape-id)
    (setf (scad-sketch-session-selection session) (list ref))
    (setf (scad-sketch-session-focus-ref session) ref)
    (setf (scad-sketch-session-selected-index session) nil)
    (setf (scad-sketch-session-hover-index session) 0)
    shape))

(defun scad-sketch--selected-point (session)
  "Return the currently selected model point in SESSION, or nil."
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

(defun scad-sketch--append-model-point (session point)
  "Append POINT to SESSION's active shape and focus/select it."
  (let* ((shape    (scad-sketch-session-active-shape session))
         (shape-id (scad-sketch-shape-id shape))
         (points   (append (scad-sketch-shape-points shape) (list point)))
         (idx      (1- (length points))))
    (setf (scad-sketch-shape-points shape) points)
    (scad-sketch-session-set-active-shape session shape-id)
    (setf (scad-sketch-session-selected-index session) idx)
    (setf (scad-sketch-session-focus-ref session)
          (scad-sketch--point-ref idx shape-id))
    (setf (scad-sketch-session-selection session)
          (list (scad-sketch--point-ref idx shape-id)))))

;;; Mirror-related
(defun scad-sketch--current-mirror-node (session)
  "Return the mirror node currently targeted by attention/focus.

If attention is not on a mirror ref and the session has exactly one mirror,
return that mirror."
  (let* ((ref (scad-sketch--attention-ref session))
         (mirror-id (and ref (scad-sketch--ref-mirror-id ref)))
         (tree (scad-sketch-session-tree session)))
    (or (and mirror-id
             (scad-sketch-session--tree-find-mirror tree mirror-id))
        (let ((mirrors (scad-sketch-session--tree-mirrors tree)))
          (cond
           ((= (length mirrors) 1) (car mirrors))
           ((null mirrors) (user-error "No mirror primitive in this session"))
           (t (user-error "Hover or focus a mirror axis first")))))))

(defun scad-sketch--set-mirror-axis-vector (mirror mx my)
  "Set MIRROR normal vector to MX MY."
  (when (< (+ (* mx mx) (* my my)) 0.000001)
    (user-error "Mirror axis vector must be non-zero"))
  (plist-put mirror :mx (float mx))
  (plist-put mirror :my (float my))
  mirror)

(defun scad-sketch--move-mirror-handle-to (session mirror-id idx xy)
  "Move mirror handle IDX for MIRROR-ID to XY.

Handle 0 sets the positive normal.  Handle 1 sets the negative normal."
  (let ((mirror (scad-sketch-session--tree-find-mirror
                 (scad-sketch-session-tree session)
                 mirror-id)))
    (unless mirror
      (user-error "No such mirror: %s" mirror-id))
    (pcase idx
      (0
       (scad-sketch--set-mirror-axis-vector
        mirror (nth 0 xy) (nth 1 xy)))
      (1
       (scad-sketch--set-mirror-axis-vector
        mirror (- (nth 0 xy)) (- (nth 1 xy))))
      (_
       (user-error "No such mirror handle: %s" idx)))))

(defun scad-sketch-set-mirror-axis (axis)
  "Set current mirror axis normal vector from minibuffer.

Input is two numbers, for example:
  1,0
  0,1
  1,1"
  (interactive
   (let* ((session (scad-sketch--assert-session))
          (mirror  (scad-sketch--current-mirror-node session))
          (default (format "%s,%s"
                           (scad-sketch--fmt-num (plist-get mirror :mx))
                           (scad-sketch--fmt-num (plist-get mirror :my)))))
     (list (read-string "Mirror axis normal [x,y]: " default))))
  (let ((pair (scad-sketch--parse-axis-pair axis)))
    (scad-sketch--edit
     (lambda (s)
       (let ((mirror (scad-sketch--current-mirror-node s)))
         (scad-sketch--set-mirror-axis-vector
          mirror (nth 0 pair) (nth 1 pair)))))))

;;; Selected geometry movement
(defun scad-sketch--bounds-center (bounds)
  "Return center point of BOUNDS.

BOUNDS is (MIN-X MAX-X MIN-Y MAX-Y)."
  (list (/ (+ (nth 0 bounds) (nth 1 bounds)) 2.0)
        (/ (+ (nth 2 bounds) (nth 3 bounds)) 2.0)))

(defun scad-sketch--points-bounds-for-centering (points)
  "Return bounds for POINTS as (MIN-X MAX-X MIN-Y MAX-Y)."
  (when points
    (let ((xs (mapcar #'car points))
          (ys (mapcar #'cadr points)))
      (list (apply #'min xs)
            (apply #'max xs)
            (apply #'min ys)
            (apply #'max ys)))))

(defun scad-sketch--bounds-union-for-centering (bounds-list)
  "Return union of BOUNDS-LIST.

Each bounds item is (MIN-X MAX-X MIN-Y MAX-Y)."
  (let ((bounds-list (delq nil bounds-list)))
    (when bounds-list
      (list (apply #'min (mapcar (lambda (b) (nth 0 b)) bounds-list))
            (apply #'max (mapcar (lambda (b) (nth 1 b)) bounds-list))
            (apply #'min (mapcar (lambda (b) (nth 2 b)) bounds-list))
            (apply #'max (mapcar (lambda (b) (nth 3 b)) bounds-list))))))

(defun scad-sketch--square-corners-for-centering (shape)
  "Return approximate corner points for square SHAPE."
  (let* ((md    (scad-sketch-shape-metadata shape))
         (x     (float (or (plist-get md :x) 0.0)))
         (y     (float (or (plist-get md :y) 0.0)))
         (w     (float (or (plist-get md :w) 1.0)))
         (h     (float (or (plist-get md :h) 1.0)))
         (angle (float (or (plist-get md :angle) 0.0)))
         (rad   (* float-pi (/ angle 180.0)))
         (co    (cos rad))
         (si    (sin rad)))
    (mapcar
     (lambda (pt)
       (let ((dx (- (nth 0 pt) x))
             (dy (- (nth 1 pt) y)))
         (list (+ x (- (* dx co) (* dy si)))
               (+ y (+ (* dx si) (* dy co))))))
     (list (list x y)
           (list (+ x w) y)
           (list (+ x w) (+ y h))
           (list x (+ y h))))))

(defun scad-sketch--shape-bounds-for-centering (shape)
  "Return approximate bounds for SHAPE as (MIN-X MAX-X MIN-Y MAX-Y)."
  (pcase (scad-sketch-shape-kind shape)
    ('polygon
     (scad-sketch--points-bounds-for-centering
      (mapcar #'scad-sketch--point-xy
              (scad-sketch-shape-points shape))))

    ('circle
     (let* ((md (scad-sketch-shape-metadata shape))
            (cx (float (or (plist-get md :cx) 0.0)))
            (cy (float (or (plist-get md :cy) 0.0)))
            (r  (float (or (plist-get md :r) 1.0))))
       (list (- cx r) (+ cx r) (- cy r) (+ cy r))))

    ('square
     (scad-sketch--points-bounds-for-centering
      (scad-sketch--square-corners-for-centering shape)))

    ('text
     ;; Approximate text bounds.  This is only for move-to-center utilities; the
     ;; renderer still owns actual visual text rendering.
     (let* ((md   (scad-sketch-shape-metadata shape))
            (str  (or (plist-get md :str) ""))
            (x    (float (or (plist-get md :x) 0.0)))
            (y    (float (or (plist-get md :y) 0.0)))
            (size (float (or (plist-get md :size) 10.0)))
            (w    (* size 0.6 (max 1 (length str))))
            (h    size))
       (list x (+ x w) y (+ y h))))

    (_ nil)))

(defun scad-sketch--ref-bounds-for-centering (session ref)
  "Return approximate bounds for REF in SESSION."
  (pcase (scad-sketch--ref-kind ref)
    ('shape
     (let ((shape (scad-sketch-session-shape-by-id
                   session
                   (scad-sketch--ref-shape-id ref))))
       (and shape
            (scad-sketch--shape-bounds-for-centering shape))))

    ('point
     (let* ((shape-id (scad-sketch--ref-shape-id ref))
            (idx      (scad-sketch--ref-index ref))
            (shape    (and shape-id
                            (scad-sketch-session-shape-by-id
                             session shape-id))))
       (when shape
         (let ((xy
                (if (eq (scad-sketch-shape-kind shape) 'polygon)
                    (scad-sketch--point-xy
                     (nth idx (scad-sketch-shape-points shape)))
                  (when (and (fboundp 'scad-sketch--primitive-handle-count)
                             (fboundp 'scad-sketch--primitive-handle-xy)
                             idx
                             (>= idx 0)
                             (< idx (scad-sketch--primitive-handle-count shape)))
                    (scad-sketch--primitive-handle-xy shape idx)))))
           (when xy
             (list (nth 0 xy) (nth 0 xy)
                   (nth 1 xy) (nth 1 xy)))))))

    (_ nil)))

(defun scad-sketch--refs-bounds-for-centering (session refs)
  "Return approximate union bounds for REFS in SESSION."
  (scad-sketch--bounds-union-for-centering
   (mapcar (lambda (ref)
             (scad-sketch--ref-bounds-for-centering session ref))
           refs)))

(defun scad-sketch--all-shapes-bounds-for-centering (session)
  "Return approximate bounds for all shapes in SESSION."
  (scad-sketch--bounds-union-for-centering
   (mapcar #'scad-sketch--shape-bounds-for-centering
           (scad-sketch-session-shapes session))))

(defun scad-sketch--canvas-center-for-moving (session)
  "Return the model-space center of the current canvas for SESSION.

When rendering has provided `scad-sketch--bounds', use that so mirror preview
bounds and render-time bounds policy are respected.  Otherwise fall back to the
editable shape bounds."
  (let ((bounds
         (if (fboundp 'scad-sketch--bounds)
             (scad-sketch--bounds session)
           (scad-sketch--all-shapes-bounds-for-centering session))))
    (unless bounds
      (user-error "No canvas bounds available"))
    (scad-sketch--bounds-center bounds)))

(defun scad-sketch--zero-delta-p (dx dy)
  "Return non-nil if DX DY is effectively zero."
  (and (< (abs (float dx)) 0.000001)
       (< (abs (float dy)) 0.000001)))

(defun scad-sketch--move-selected-refs-by
    (refs dx dy &optional snap move-cursor description)
  "Move selected REFS by DX DY.

When SNAP is non-nil, snap only the axes being moved, using editor point as the
movement anchor.  This keeps off-grid geometry from being pulled sideways during
vertical moves or vertically during horizontal moves.

When MOVE-CURSOR is non-nil, carry editor point along with the moved geometry.

This uses operation undo entries.  Undo restores the changed shapes and the
previous cursor state."
  (let ((session (scad-sketch--assert-session)))
    (scad-sketch-session-sync-active-shape-from-points session)
    (let* ((records    (and refs
                            (scad-sketch--move-undo-records-for-refs
                             session refs)))
           (old-point  (copy-tree (scad-sketch-session-point session)))
           (old-hover  (scad-sketch-session-hover-index session))
           (old-active (scad-sketch-session-active-shape-id session))
           (delta      (scad-sketch--effective-move-delta
                        old-point dx dy (scad-sketch--grid session) snap))
           (actual-dx  (nth 0 delta))
           (actual-dy  (nth 1 delta)))
      (unless records
        (user-error "No selected point or shape"))

      (if (scad-sketch--zero-delta-p actual-dx actual-dy)
          (message "Selection is already centered")
        (scad-sketch--edit-with-undo-action
         (lambda (s)
           (dolist (record records)
             (scad-sketch-undo-restore-state-record s record))
           (setf (scad-sketch-session-point s) (copy-tree old-point))
           (setf (scad-sketch-session-hover-index s) old-hover)
           (setf (scad-sketch-session-active-shape-id s) old-active)
           (when old-active
             (scad-sketch-session-set-active-shape s old-active)))

         (lambda (s)
           (dolist (ref refs)
             (scad-sketch--move-ref-geometry
              s ref actual-dx actual-dy nil))

           (when move-cursor
             (setf (scad-sketch-session-point s)
                   (scad-sketch--move-xy
                    (scad-sketch-session-point s)
                    actual-dx actual-dy)))

           (setf (scad-sketch-session-hover-index s) 0)

           (let ((active-id (scad-sketch-session-active-shape-id s)))
             (when active-id
               (scad-sketch-session-set-active-shape s active-id))))

         (or description "move selected geometry"))))))

(defun scad-sketch--current-edit-shape (session)
  "Return the shape currently receiving edit attention in SESSION."
  (let* ((ref (scad-sketch--attention-ref session))
         (shape-id (or (and ref (scad-sketch--ref-shape-id ref))
                       (scad-sketch-session-active-shape-id session))))
    (or (and shape-id (scad-sketch-session-shape-by-id session shape-id))
        (scad-sketch-session-active-shape session))))

(defun scad-sketch--move-square-corner-to (shape idx xy)
  "Move square SHAPE corner IDX to XY, preserving the opposite corner."
  (let* ((md    (scad-sketch-shape-metadata shape))
         (x     (float (or (plist-get md :x) 0.0)))
         (y     (float (or (plist-get md :y) 0.0)))
         (w     (float (or (plist-get md :w) 0.0)))
         (h     (float (or (plist-get md :h) 0.0)))
         (angle (float (or (plist-get md :angle) 0.0)))
         (a     (* float-pi (/ angle 180.0)))
         (ux    (cos a))
         (uy    (sin a))
         (vx    (- (sin a)))
         (vy    (cos a))
         (opp   (mod (+ idx 2) 4))
         (opp-local
          (pcase opp
            (0 (list 0.0 0.0))
            (1 (list w 0.0))
            (2 (list w h))
            (3 (list 0.0 h))))
         (dx    (- (nth 0 xy) x))
         (dy    (- (nth 1 xy) y))
         (new-local (list (+ (* dx ux) (* dy uy))
                          (+ (* dx vx) (* dy vy))))
         (min-x (min (nth 0 opp-local) (nth 0 new-local)))
         (max-x (max (nth 0 opp-local) (nth 0 new-local)))
         (min-y (min (nth 1 opp-local) (nth 1 new-local)))
         (max-y (max (nth 1 opp-local) (nth 1 new-local)))
         (new-x (+ x (* min-x ux) (* min-y vx)))
         (new-y (+ y (* min-x uy) (* min-y vy))))
    (setq md (plist-put md :x new-x))
    (setq md (plist-put md :y new-y))
    (setq md (plist-put md :w (max 0.0001 (- max-x min-x))))
    (setq md (plist-put md :h (max 0.0001 (- max-y min-y))))
    (setf (scad-sketch-shape-metadata shape) md)))

(defun scad-sketch--move-primitive-handle-to (shape idx xy)
  "Move primitive SHAPE handle IDX to XY.

Circle:
  0 moves the center.
  1 and 2 set radius from center to handle position.

Square:
  0..3 resize from corner handles.
  4 moves the whole square by center.

Text:
  0 moves the text origin."
  (pcase (scad-sketch-shape-kind shape)
    ('circle
     (let* ((md (scad-sketch-shape-metadata shape))
            (cx (float (or (plist-get md :cx) 0.0)))
            (cy (float (or (plist-get md :cy) 0.0))))
       (if (= idx 0)
           (progn
             (setq md (plist-put md :cx (nth 0 xy)))
             (setq md (plist-put md :cy (nth 1 xy)))
             (setf (scad-sketch-shape-metadata shape) md))
         (let* ((dx (- (nth 0 xy) cx))
                (dy (- (nth 1 xy) cy))
                (r  (sqrt (+ (* dx dx) (* dy dy)))))
           (setq md (plist-put md :r (max 0.0001 r)))
           (setf (scad-sketch-shape-metadata shape) md)))))
    ('square
     (if (= idx 4)
         (let* ((old-center (scad-sketch--square-center shape))
                (dx (- (nth 0 xy) (nth 0 old-center)))
                (dy (- (nth 1 xy) (nth 1 old-center))))
           (scad-sketch--move-shape shape dx dy))
       (scad-sketch--move-square-corner-to shape idx xy)))
    ('text
     (let ((md (scad-sketch-shape-metadata shape)))
       (setq md (plist-put md :x (nth 0 xy)))
       (setq md (plist-put md :y (nth 1 xy)))
       (setf (scad-sketch-shape-metadata shape) md)))
    (_
     (user-error "Selected point is not editable for this shape"))))

(defun scad-sketch--move-shape (shape dx dy &optional snap grid)
  "Move whole SHAPE by DX DY, snapping to GRID when SNAP is non-nil."
  (pcase (scad-sketch-shape-kind shape)
    ('polygon
     (setf (scad-sketch-shape-points shape)
           (mapcar (lambda (pt)
                     (let* ((xy  (scad-sketch--move-xy
                                  (scad-sketch--point-xy pt) dx dy))
                            (xy  (if snap (scad-sketch--snap-xy xy grid) xy)))
                       (scad-sketch--make-model-point xy pt)))
                   (scad-sketch-shape-points shape))))
    ('circle
     (let* ((md (scad-sketch-shape-metadata shape))
            (xy (scad-sketch--move-xy
                 (list (plist-get md :cx) (plist-get md :cy))
                 dx dy))
            (xy (if snap (scad-sketch--snap-xy xy grid) xy)))
       (setq md (plist-put md :cx (nth 0 xy)))
       (setq md (plist-put md :cy (nth 1 xy)))
       (setf (scad-sketch-shape-metadata shape) md)))
    ((or 'square 'text)
     (let* ((md (scad-sketch-shape-metadata shape))
            (xy (scad-sketch--move-xy
                 (list (plist-get md :x) (plist-get md :y))
                 dx dy))
            (xy (if snap (scad-sketch--snap-xy xy grid) xy)))
       (setq md (plist-put md :x (nth 0 xy)))
       (setq md (plist-put md :y (nth 1 xy)))
       (setf (scad-sketch-shape-metadata shape) md)))))

(defun scad-sketch--move-session-point-by (session dx dy &optional snap)
  "Move SESSION cursor point by DX DY, snapping when SNAP is non-nil."
  (let* ((old (scad-sketch-session-point session))
         (new (scad-sketch--move-xy old dx dy))
         (new (if snap
                  (scad-sketch--snap-xy new (scad-sketch--grid session))
                new)))
    (setf (scad-sketch-session-point session) new)))

(defun scad-sketch--move-selected-refs (session)
  "Return selected refs that can be moved in SESSION.

If there is no explicit movable selection, fall back to the active shape."
  (let ((refs
         (cl-remove-if-not
          (lambda (ref)
            (memq (scad-sketch--ref-kind ref) '(shape point)))
          (scad-sketch-session-selection session))))
    (cond
     (refs refs)

     ((scad-sketch-session-active-shape-id session)
      (list (scad-sketch--shape-ref
             (scad-sketch-session-active-shape-id session))))

     (t nil))))

(defun scad-sketch--move-undo-records-for-refs (session refs)
  "Return shape-state undo records for movable REFS in SESSION.

A point ref mutates its parent shape, even when it is a primitive handle, so the
undo record captures the parent shape state."
  (let (shape-ids)
    (dolist (ref refs)
      (pcase (scad-sketch--ref-kind ref)
        ('shape
         (push (scad-sketch--ref-shape-id ref) shape-ids))
        ('point
         (push (scad-sketch--ref-shape-id ref) shape-ids))))
    (setq shape-ids (delete-dups (delq nil (nreverse shape-ids))))
    (delq nil
          (mapcar (lambda (shape-id)
                    (scad-sketch-undo-capture-shape-state
                     session shape-id))
                  shape-ids))))

(defun scad-sketch--move-ref-geometry (session ref dx dy &optional _snap)
  "Move geometry identified by REF in SESSION by DX DY.

Shape refs move whole shapes.  Polygon point refs move one polygon vertex.
Primitive point refs move the corresponding primitive handle.

DX/DY are already the effective movement delta; snapping is handled by the
caller so all selected geometry moves rigidly together."
  (pcase (scad-sketch--ref-kind ref)
    ('shape
     (let ((shape (scad-sketch-session-shape-by-id
                   session
                   (scad-sketch--ref-shape-id ref))))
       (when shape
         (scad-sketch--move-shape shape dx dy nil
                                  (scad-sketch--grid session)))))

    ('point
     (let* ((shape-id (scad-sketch--ref-shape-id ref))
            (idx      (scad-sketch--ref-index ref))
            (shape    (and shape-id
                            (scad-sketch-session-shape-by-id
                             session shape-id))))
       (unless shape
         (user-error "No shape for point ref: %S" ref))
       (cond
        ((eq (scad-sketch-shape-kind shape) 'polygon)
         (let* ((points (scad-sketch-shape-points shape))
                (old    (and points (nth idx points))))
           (unless old
             (user-error "No polygon point for ref: %S" ref))
           (let* ((new-xy (scad-sketch--move-xy
                           (scad-sketch--point-xy old)
                           dx dy))
                  (new    (scad-sketch--make-model-point new-xy old)))
             (setf (scad-sketch-shape-points shape)
                   (scad-sketch--replace-nth idx new points)))))

        (t
         (unless (and (fboundp 'scad-sketch--primitive-handle-count)
                      (fboundp 'scad-sketch--primitive-handle-xy)
                      (fboundp 'scad-sketch--move-primitive-handle-to))
           (user-error "Primitive handle helpers are not loaded"))
         (unless (and idx
                      (>= idx 0)
                      (< idx (scad-sketch--primitive-handle-count shape)))
           (user-error "No primitive handle for ref: %S" ref))
         (let* ((old-xy (scad-sketch--primitive-handle-xy shape idx))
                (new-xy (scad-sketch--move-xy old-xy dx dy)))
           (scad-sketch--move-primitive-handle-to shape idx new-xy))))))))

(defun scad-sketch--move-selected (dx dy &optional snap)
  "Move selected vertices/shapes by DX, DY.  Snap to grid when SNAP is non-nil.

Falls back to the active shape when no explicit selection exists.  Arrow-key
movement carries editor point along with the moved geometry."
  (let* ((session (scad-sketch--assert-session))
         (refs    (scad-sketch--move-selected-refs session)))
    (scad-sketch--move-selected-refs-by
     refs dx dy snap t "move selected geometry")))

(defun scad-sketch-center-selection-on-point ()
  "Move selected geometry so its bounds center lands on editor point.

This does not move editor point.  The operation is undoable."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (refs    (scad-sketch--move-selected-refs session))
         (bounds  (scad-sketch--refs-bounds-for-centering session refs)))
    (unless bounds
      (user-error "No selected point or shape"))
    (let* ((center (scad-sketch--bounds-center bounds))
           (target (scad-sketch-session-point session))
           (dx     (- (nth 0 target) (nth 0 center)))
           (dy     (- (nth 1 target) (nth 1 center))))
      (scad-sketch--move-selected-refs-by
       refs dx dy nil nil "center selection on point"))))

(defun scad-sketch-center-selection-on-canvas ()
  "Move selected geometry so its bounds center lands on the current canvas.

The canvas center is computed in model space from the current render bounds.
This does not move editor point.  The operation is undoable."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (refs    (scad-sketch--move-selected-refs session))
         (bounds  (scad-sketch--refs-bounds-for-centering session refs)))
    (unless bounds
      (user-error "No selected point or shape"))
    (let* ((center (scad-sketch--bounds-center bounds))
           (target (scad-sketch--canvas-center-for-moving session))
           (dx     (- (nth 0 target) (nth 0 center)))
           (dy     (- (nth 1 target) (nth 1 center))))
      (scad-sketch--move-selected-refs-by
       refs dx dy nil nil "center selection on canvas"))))

;;; Selected-vertex movement interactive commands

(defun scad-sketch-move-selected-left ()
  "Move selected one grid step left."
  (interactive)
  (scad-sketch--move-selected (- (scad-sketch--grid (scad-sketch--assert-session))) 0 t))

(defun scad-sketch-move-selected-right ()
  "Move selected one grid step right."
  (interactive)
  (scad-sketch--move-selected (scad-sketch--grid (scad-sketch--assert-session)) 0 t))

(defun scad-sketch-move-selected-up ()
  "Move selected one grid step up."
  (interactive)
  (scad-sketch--move-selected 0 (scad-sketch--grid (scad-sketch--assert-session)) t))

(defun scad-sketch-move-selected-down ()
  "Move selected one grid step down."
  (interactive)
  (scad-sketch--move-selected 0 (- (scad-sketch--grid (scad-sketch--assert-session))) t))

(defun scad-sketch-move-selected-fine-left ()
  "Move selected one fine step left (off-grid)."
  (interactive)
  (scad-sketch--move-selected (- (scad-sketch--fine (scad-sketch--assert-session))) 0))

(defun scad-sketch-move-selected-fine-right ()
  "Move selected one fine step right (off-grid)."
  (interactive)
  (scad-sketch--move-selected (scad-sketch--fine (scad-sketch--assert-session)) 0))

(defun scad-sketch-move-selected-fine-up ()
  "Move selected one fine step up (off-grid)."
  (interactive)
  (scad-sketch--move-selected 0 (scad-sketch--fine (scad-sketch--assert-session))))

(defun scad-sketch-move-selected-fine-down ()
  "Move selected one fine step down (off-grid)."
  (interactive)
  (scad-sketch--move-selected 0 (- (scad-sketch--fine (scad-sketch--assert-session)))))

(defun scad-sketch-move-selected-coarse-left ()
  "Move selected one coarse step left."
  (interactive)
  (scad-sketch--move-selected (- (scad-sketch--coarse (scad-sketch--assert-session))) 0 t))

(defun scad-sketch-move-selected-coarse-right ()
  "Move selected one coarse step right."
  (interactive)
  (scad-sketch--move-selected (scad-sketch--coarse (scad-sketch--assert-session)) 0 t))

(defun scad-sketch-move-selected-coarse-up ()
  "Move selected one coarse step up."
  (interactive)
  (scad-sketch--move-selected 0 (scad-sketch--coarse (scad-sketch--assert-session)) t))

(defun scad-sketch-move-selected-coarse-down ()
  "Move selected one coarse step down."
  (interactive)
  (scad-sketch--move-selected 0 (- (scad-sketch--coarse (scad-sketch--assert-session))) t))

;;; Vertex editing commands

(defun scad-sketch-append-point ()
  "Append the cursor position as a new vertex."
  (interactive)
  (scad-sketch--edit
   (lambda (s)
     (scad-sketch--append-model-point
      s (scad-sketch--make-model-point (scad-sketch-session-point s))))))

(defun scad-sketch-insert-point-after-selected ()
  "Insert point/marks after the selected polygon point.

If marks are set, insert marks oldest-first, then current cursor point.  The new
last inserted point becomes selected.  This works for direct arrays, polygons,
and line-like polygon shapes."
  (interactive)
  (scad-sketch--edit
   (lambda (s)
     (scad-sketch-session-sync-active-shape-from-points s)
     (let* ((target-ref (scad-sketch--insert-after-target-ref s))
            (shape-id   (scad-sketch--ref-shape-id target-ref))
            (idx        (scad-sketch--ref-index target-ref))
            (shape      (scad-sketch-session-shape-by-id s shape-id))
            (points     (scad-sketch-shape-points shape))
            (insert-xy  (append
                         ;; marks are a stack; oldest-first is reverse order
                         (reverse (copy-tree (scad-sketch-session-marks s)))
                         (list (copy-sequence
                                (scad-sketch-session-point s)))))
            (insert-pts (mapcar #'scad-sketch--model-point-from-xy
                                insert-xy))
            (insert-at  (1+ idx))
            (new-index  (+ idx (length insert-pts))))
       (unless shape
         (user-error "No polygon shape for selected point"))
       (unless insert-pts
         (user-error "No point to insert"))

       (setf (scad-sketch-shape-points shape)
             (append (cl-subseq points 0 insert-at)
                     insert-pts
                     (nthcdr insert-at points)))

       (scad-sketch-session-set-active-shape s shape-id)
       (setf (scad-sketch-session-selected-index s) new-index)
       (setf (scad-sketch-session-focus-ref s)
             (scad-sketch--point-ref new-index shape-id))
       (setf (scad-sketch-session-selection s)
             (list (scad-sketch--point-ref new-index shape-id)))))))

(defun scad-sketch-delete-selected ()
  "Delete selected vertices.

A selected shape deletes all vertices in that shape.  Falls back to the
active point when no explicit selection exists."
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
       (unless locs (user-error "No selected point or shape"))
       (dolist (loc locs)
         (let* ((shape-id (car loc))
                (idx      (cdr loc))
                (shape    (scad-sketch-session-shape-by-id s shape-id))
                (points   (and shape (scad-sketch-shape-points shape))))
           (when (and points (>= idx 0) (< idx (length points)))
             (setf (scad-sketch-shape-points shape)
                   (append (cl-subseq points 0 idx)
                           (nthcdr (1+ idx) points))))))

       ;; Drop empty shapes only in multi-shape sessions.
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

;;; Shape-creation commands
(defun scad-sketch--polygon-point-ref-p (session ref)
  "Return non-nil if REF is a polygon point ref in SESSION."
  (and ref
       (eq (scad-sketch--ref-kind ref) 'point)
       (let ((shape (scad-sketch-session-shape-by-id
                     session
                     (scad-sketch--ref-shape-id ref))))
         (and shape
              (eq (scad-sketch-shape-kind shape) 'polygon)
              (let ((idx (scad-sketch--ref-index ref)))
                (and idx
                     (>= idx 0)
                     (< idx (length (scad-sketch-shape-points shape)))))))))

(defun scad-sketch--selected-polygon-point-refs (session)
  "Return explicitly selected polygon point refs in SESSION."
  (cl-remove-if-not
   (lambda (ref)
     (scad-sketch--polygon-point-ref-p session ref))
   (scad-sketch-session-selection session)))

(defun scad-sketch--insert-after-target-ref (session)
  "Return polygon point ref after which insertion should occur.

Priority:
  1. exactly one explicitly selected polygon point
  2. current attention, if it is a polygon point
  3. active polygon's selected-index"
  (let ((selected (scad-sketch--selected-polygon-point-refs session)))
    (cond
     ((= (length selected) 1)
      (car selected))

     ((> (length selected) 1)
      (user-error "Select exactly one polygon point to insert after"))

     ((scad-sketch--polygon-point-ref-p
       session
       (scad-sketch--attention-ref session))
      (scad-sketch--attention-ref session))

     ((and (scad-sketch-session-active-shape-id session)
           (scad-sketch-session-selected-index session))
      (let* ((shape-id (scad-sketch-session-active-shape-id session))
             (idx      (scad-sketch-session-selected-index session))
             (ref      (scad-sketch--point-ref idx shape-id)))
        (if (scad-sketch--polygon-point-ref-p session ref)
            ref
          (user-error "No selected polygon point"))))

     (t
      (user-error "No selected polygon point")))))

(defun scad-sketch--model-point-from-xy (xy)
  "Return model point from XY with default zero radius."
  (list (float (nth 0 xy))
        (float (nth 1 xy))
        0.0))

(defun scad-sketch--marks-oldest-first (session)
  "Return SESSION marks in geometry order, oldest first."
  (reverse (scad-sketch-session-marks session)))

(defun scad-sketch--require-mark-count (session min-count &optional max-count)
  "Return marks when SESSION has between MIN-COUNT and MAX-COUNT marks.

Marks are returned oldest first.  When MAX-COUNT is nil, there is no upper
bound."
  (let ((marks (scad-sketch--marks-oldest-first session)))
    (when (< (length marks) min-count)
      (user-error "Need at least %d mark%s"
                  min-count
                  (if (= min-count 1) "" "s")))
    (when (and max-count (> (length marks) max-count))
      (user-error "Need at most %d mark%s"
                  max-count
                  (if (= max-count 1) "" "s")))
    marks))

(defun scad-sketch--distance-xy (a b)
  "Return Euclidean distance between model-space points A and B."
  (let ((dx (- (nth 0 a) (nth 0 b)))
        (dy (- (nth 1 a) (nth 1 b))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun scad-sketch--drawn-square-from-diagonal (mark point)
  "Return square metadata plist from diagonal MARK and POINT.

This creates an axis-aligned OpenSCAD square/rectangle primitive."
  (let* ((x1 (float (nth 0 mark)))
         (y1 (float (nth 1 mark)))
         (x2 (float (nth 0 point)))
         (y2 (float (nth 1 point)))
         (x  (min x1 x2))
         (y  (min y1 y2))
         (w  (abs (- x2 x1)))
         (h  (abs (- y2 y1))))
    (when (or (< w 0.0001) (< h 0.0001))
      (user-error "Square needs non-zero width and height"))
    (list :x x :y y :w w :h h :angle 0.0)))

(defun scad-sketch--drawn-square-from-three-corners (origin width-corner point)
  "Return square metadata from ORIGIN, WIDTH-CORNER, and POINT.

ORIGIN and WIDTH-CORNER define the local X axis and width.  POINT defines the
height by projection onto the perpendicular local Y axis.  The result is a
possibly rotated OpenSCAD square/rectangle primitive."
  (let* ((ox (float (nth 0 origin)))
         (oy (float (nth 1 origin)))
         (wx (- (float (nth 0 width-corner)) ox))
         (wy (- (float (nth 1 width-corner)) oy))
         (width (sqrt (+ (* wx wx) (* wy wy)))))
    (when (< width 0.0001)
      (user-error "First two square corners must be distinct"))

    (let* ((ux (/ wx width))
           (uy (/ wy width))
           ;; Perpendicular to local X.
           (vx (- uy))
           (vy ux)
           (px (- (float (nth 0 point)) ox))
           (py (- (float (nth 1 point)) oy))
           (raw-height (+ (* px vx) (* py vy)))
           (height (abs raw-height))
           ;; If point is on the negative side of the perpendicular axis,
           ;; flip local Y by moving origin to that side and using positive h.
           (x (if (< raw-height 0)
                  (+ ox (* vx raw-height))
                ox))
           (y (if (< raw-height 0)
                  (+ oy (* vy raw-height))
                oy))
           (angle (* 180.0 (/ (atan uy ux) pi))))
      (when (< height 0.0001)
        (user-error "Third square corner must not be collinear with first edge"))
      (list :x x :y y :w width :h height :angle angle))))

(defun scad-sketch--drawn-square-metadata (session)
  "Return square metadata from SESSION marks and cursor point.

With one mark, the mark and point are opposite diagonal corners.
With two marks, the two marks and point are interpreted as three corners:
oldest mark, newest mark, point."
  (let* ((marks (scad-sketch--require-mark-count session 1 2))
         (point (scad-sketch-session-point session)))
    (pcase (length marks)
      (1
       (scad-sketch--drawn-square-from-diagonal (car marks) point))
      (2
       (scad-sketch--drawn-square-from-three-corners
        (nth 0 marks) (nth 1 marks) point))
      (_
       (user-error "Square drawing expects one or two marks")))))

(defun scad-sketch--polygon-points-from-marks-and-point (session)
  "Return model polygon points from SESSION marks and cursor point.

Marks are used oldest first, followed by the current cursor point.  New points
default to radius 0."
  (let ((marks (scad-sketch--require-mark-count session 1))
        (point (scad-sketch-session-point session)))
    (mapcar #'scad-sketch--make-model-point
            (append marks (list point)))))

(defun scad-sketch-draw-text-at-point (text size font)
  "Draw a text primitive at the current cursor point.

TEXT is the string.  SIZE is the OpenSCAD text size.  FONT may be empty/nil."
  (interactive
   (let* ((text (read-string "Text: "))
          (size (read-number "Text size: " 10.0))
          (font (when (fboundp 'scad-sketch--available-font-families)
                  (completing-read
                   "Font family (empty for default): "
                   (scad-sketch--available-font-families)
                   nil nil ""))))
     (list text size (and font (not (string-empty-p font)) font))))
  (scad-sketch--edit
   (lambda (s)
     (let* ((pt       (scad-sketch-session-point s))
            (shape-id (scad-sketch-session-next-shape-id s))
            (shape
             (scad-sketch-session--make-text-shape
              shape-id
              text
              (nth 0 pt)
              (nth 1 pt)
              size
              0.0
              font
              (list :created-in-session t))))
       (scad-sketch--add-drawn-shape s shape)))))

(defun scad-sketch-draw-square-from-marks ()
  "Draw a square/rectangle primitive from marks and cursor point.

With one mark, the mark and point are opposite diagonal corners.

With two marks, the oldest mark is the origin corner, the newest mark is the
width corner, and point defines the height-side corner."
  (interactive)
  (scad-sketch--edit
   (lambda (s)
     (let* ((md       (scad-sketch--drawn-square-metadata s))
            (shape-id (scad-sketch-session-next-shape-id s))
            (shape
             (scad-sketch-session--make-square-shape
              shape-id
              (plist-get md :x)
              (plist-get md :y)
              (plist-get md :w)
              (plist-get md :h)
              (plist-get md :angle)
              (list :created-in-session t))))
       (scad-sketch--add-drawn-shape s shape)))))

(defun scad-sketch--add-drawn-shape (session shape)
  "Add drawn SHAPE to SESSION and select/focus it.

Model insertion is delegated to `scad-sketch-session-add-shape-object';
editor selection/focus is handled here to avoid a session→refs dependency."
  (scad-sketch-session-add-shape-object session shape)
  (scad-sketch--select-new-shape session shape))

(defun scad-sketch--add-drawn-polygon (session points &optional polyround)
  "Add a drawn polygon with POINTS to SESSION and select/focus it."
  (let* ((shape-id (scad-sketch-session-next-shape-id session))
         (shape
          (scad-sketch-session--make-polygon-shape
           shape-id points polyround nil nil
           (list :created-in-session t))))
    (scad-sketch--add-drawn-shape session shape)))

(defun scad-sketch-draw-circle-from-mark ()
  "Draw a circle primitive using cursor point as center and mark as radius point.

Uses the most recent mark as the point on the radius."
  (interactive)
  (scad-sketch--edit
   (lambda (s)
     (let* ((mark     (or (car (scad-sketch-session-marks s))
                          (user-error "No marks set")))
            (center   (scad-sketch-session-point s))
            (radius   (scad-sketch--distance-xy center mark))
            (shape-id (scad-sketch-session-next-shape-id s))
            (shape
             (scad-sketch-session--make-circle-shape
              shape-id
              (nth 0 center)
              (nth 1 center)
              radius
              (list :created-in-session t))))
       (when (< radius 0.0001)
         (user-error "Circle radius must be non-zero"))
       (scad-sketch--add-drawn-shape s shape)))))

(defun scad-sketch-draw-polygon-from-marks ()
  "Draw a closed polygon from marks and cursor point.

Marks are used oldest first, followed by the current cursor point.  New vertices
default to polyRound radius 0; use `scad-sketch-set-radius' afterward to set
radii normally."
  (interactive)
  (scad-sketch--edit
   (lambda (s)
     (let ((points (scad-sketch--polygon-points-from-marks-and-point s)))
       (when (< (length points) 3)
         (user-error "Polygon needs at least two marks plus point"))
       (scad-sketch--add-drawn-polygon s points)))))

(defun scad-sketch-line-from-mark ()
  "Create a new polygon path from marks, oldest first, and cursor point.

This is retained as the historical `l' command.  For a closed polygon requiring
at least three vertices, use `scad-sketch-draw-polygon-from-marks'."
  (interactive)
  (scad-sketch--edit
   (lambda (s)
     (let ((points (scad-sketch--polygon-points-from-marks-and-point s)))
       (scad-sketch--add-drawn-polygon s points)))))

(defun scad-sketch-rectangle-from-mark ()
  "Create a square/rectangle primitive from most recent mark to cursor.

This is retained as the historical `r' command.  It uses the most recent mark
and cursor point as opposite diagonal corners."
  (interactive)
  (scad-sketch--edit
   (lambda (s)
     (let* ((mark (or (car (scad-sketch-session-marks s))
                      (user-error "No marks set")))
            (pt   (scad-sketch-session-point s))
            (md   (scad-sketch--drawn-square-from-diagonal mark pt))
            (shape-id (scad-sketch-session-next-shape-id s))
            (shape
             (scad-sketch-session--make-square-shape
              shape-id
              (plist-get md :x)
              (plist-get md :y)
              (plist-get md :w)
              (plist-get md :h)
              (plist-get md :angle)
              (list :created-in-session t))))
       (scad-sketch--add-drawn-shape s shape)))))

(defun scad-sketch-toggle-closed ()
  "Toggle the closed flag on the active shape."
  (interactive)
  (scad-sketch--clean-change
   (lambda (s)
     (let ((shape (scad-sketch-session-active-shape s)))
       (setf (scad-sketch-shape-closed shape)
             (not (scad-sketch-shape-closed shape)))
       (setf (scad-sketch-session-closed s)
             (scad-sketch-shape-closed shape))))))

(defun scad-sketch--attention-polygon-shape (session)
  "Return the polygon shape currently under attention in SESSION.

Point refs resolve to their parent shape.  Shape refs resolve directly.  If no
attention ref exists, fall back to the active shape."
  (let* ((ref      (scad-sketch--attention-ref session))
         (shape-id (and ref
                        (pcase (scad-sketch--ref-kind ref)
                          ((or 'shape 'point)
                           (scad-sketch--ref-shape-id ref))
                          (_ nil))))
         (shape    (or (and shape-id
                            (scad-sketch-session-shape-by-id
                             session shape-id))
                       (scad-sketch-session-active-shape session))))
    (unless (and shape
                 (eq (scad-sketch-shape-kind shape) 'polygon))
      (user-error "Attention is not on a polygon-like shape"))
    shape))

(defun scad-sketch-toggle-points-extraction (name)
  "Toggle current polygon points between inline and variable-reference output.

When the current polygon is inline, prompt for NAME and emit:

  NAME = [[...]];
  polygon(NAME);

When the current polygon is already variable-backed, force it back inline."
  (interactive
   (let* ((session (scad-sketch--assert-session))
          (shape   (scad-sketch--attention-polygon-shape session)))
     (if (scad-sketch-session-polygon-extracted-p session shape)
         (list nil)
       (list (read-string "Points variable name: " "pts")))))
  (scad-sketch--edit
   (lambda (s)
     (let* ((shape (scad-sketch--attention-polygon-shape s))
            (style (scad-sketch-session-toggle-polygon-extraction
                    s shape name)))
       (message "Polygon points now emit %s"
                (pcase style
                  ('inline "inline")
                  ('extracted
                   (format "through `%s'"
                           (scad-sketch-session--shape-points-var-name
                            shape)))
                  (_ (format "%S" style))))))))

(defun scad-sketch-set-radius (radius)
  "Set radius.

For circles, sets the circle radius.
For polygons, sets the polyRound radius of selected vertices."
  (interactive (list (read-number "Radius: " 0)))
  (scad-sketch--edit
   (lambda (s)
     (let ((shape (scad-sketch--current-edit-shape s)))
       (pcase (and shape (scad-sketch-shape-kind shape))
         ('circle
          (let ((md (scad-sketch-shape-metadata shape)))
            (setq md (plist-put md :r (max 0.0001 (float radius))))
            (setf (scad-sketch-shape-metadata shape) md)))
         ('polygon
          (let ((locs (scad-sketch--selected-point-locs s t)))
            (unless locs (user-error "No selected point or shape"))
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
                       points))))))
         (_
          (user-error "Radius applies to circles or polygon vertices")))))))

(defun scad-sketch-set-size ()
  "Set the size of the current primitive shape.

Square: prompts for width and height.
Circle: prompts for radius.
Text: prompts for font size."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (shape   (scad-sketch--current-edit-shape session)))
    (unless shape (user-error "No active shape"))
    (pcase (scad-sketch-shape-kind shape)
      ('square
       (let* ((md (scad-sketch-shape-metadata shape))
              (w  (read-number "Width: "  (float (or (plist-get md :w) 0.0))))
              (h  (read-number "Height: " (float (or (plist-get md :h) 0.0)))))
         (scad-sketch--edit
          (lambda (_s)
            (setq md (plist-put md :w (max 0.0001 (float w))))
            (setq md (plist-put md :h (max 0.0001 (float h))))
            (setf (scad-sketch-shape-metadata shape) md)))))
      ('circle
       (call-interactively #'scad-sketch-set-radius))
      ('text
       (let* ((md   (scad-sketch-shape-metadata shape))
              (size (read-number "Text size: "
                                 (float (or (plist-get md :size) 10.0)))))
         (scad-sketch--edit
          (lambda (_s)
            (setq md (plist-put md :size (max 0.0001 (float size))))
            (setf (scad-sketch-shape-metadata shape) md)))))
      (_
       (user-error "Size applies to square, circle, or text shapes")))))

(defun scad-sketch-set-text (text)
  "Set the string of the current text shape to TEXT."
  (interactive
   (let* ((session (scad-sketch--assert-session))
          (shape   (scad-sketch--current-edit-shape session))
          (md      (and shape (scad-sketch-shape-metadata shape))))
     (unless (and shape (eq (scad-sketch-shape-kind shape) 'text))
       (user-error "No active text shape"))
     (list (read-string "Text: " (or (plist-get md :str) "")))))
  (scad-sketch--edit
   (lambda (s)
     (let* ((shape (scad-sketch--current-edit-shape s))
            (md    (scad-sketch-shape-metadata shape)))
       (unless (eq (scad-sketch-shape-kind shape) 'text)
         (user-error "No active text shape"))
       (setq md (plist-put md :str text))
       (setf (scad-sketch-shape-metadata shape) md)))))

(defun scad-sketch--available-font-families ()
  "Return available font family names for completion."
  (sort (delete-dups
         (delq nil
               (mapcar (lambda (font)
                         (cond
                          ((stringp font) font)
                          ((consp font)   (car font))
                          (t nil)))
                       (font-family-list))))
        #'string<))

(defun scad-sketch-set-text-font (font)
  "Set the font family of the current text shape to FONT.

An empty FONT clears the explicit font."
  (interactive
   (let* ((session (scad-sketch--assert-session))
          (shape   (scad-sketch--current-edit-shape session))
          (md      (and shape (scad-sketch-shape-metadata shape))))
     (unless (and shape (eq (scad-sketch-shape-kind shape) 'text))
       (user-error "No active text shape"))
     (list
      (completing-read
       "Font family (empty clears): "
       (scad-sketch--available-font-families)
       nil nil
       (or (plist-get md :font) "")))))
  (scad-sketch--edit
   (lambda (s)
     (let* ((shape (scad-sketch--current-edit-shape s))
            (md    (scad-sketch-shape-metadata shape)))
       (unless (eq (scad-sketch-shape-kind shape) 'text)
         (user-error "No active text shape"))
       (setq md (plist-put md :font
                            (if (string-empty-p font) nil font)))
       (setf (scad-sketch-shape-metadata shape) md)))))

;;; Selection boolean commands
(defun scad-sketch--next-editor-group-id (session op)
  "Return a fresh editor-created group id for OP in SESSION."
  (let* ((prefix (format "%s-edit-" op))
         (used nil))
    (cl-labels
        ((walk
          (tree)
          (when tree
            (pcase (plist-get tree :kind)
              ('boolean
               (push (plist-get tree :group-id) used)
               (dolist (child (plist-get tree :children))
                 (walk child)))
              ('mirror
               (push (plist-get tree :mirror-id) used)
               (walk (plist-get tree :child)))))))
      (walk (scad-sketch-session-tree session)))
    (let ((n 0)
          id)
      (while
          (progn
            (setq id (intern (format "%s%d" prefix n)))
            (memq id used))
        (setq n (1+ n)))
      id)))

(defun scad-sketch--shape-leaf-nodes (shape-ids)
  "Return tree shape leaves for SHAPE-IDS."
  (mapcar #'scad-sketch-session--tree-shape shape-ids))

(defun scad-sketch--tree-insert-node-at-first-selected
    (tree selected-ids replacement-node)
  "Replace first selected shape leaf in TREE with REPLACEMENT-NODE.

Other selected leaves are removed.  SELECTED-IDS should already be in tree
traversal order."
  (let ((inserted nil))
    (cl-labels
        ((walk
          (node)
          (pcase (and node (plist-get node :kind))
            ('shape
             (let ((shape-id (plist-get node :shape-id)))
               (cond
                ((not (memq shape-id selected-ids))
                 node)
                ((not inserted)
                 (setq inserted t)
                 replacement-node)
                (t
                 nil))))

            ('boolean
             (let* ((children
                     (delq nil
                           (mapcar #'walk
                                   (plist-get node :children)))))
               (cond
                ((null children)
                 nil)
                ((null (cdr children))
                 (car children))
                (t
                 (plist-put
                  (copy-sequence node)
                  :children children)))))

            ('mirror
             (let ((child (walk (plist-get node :child))))
               (when child
                 (plist-put (copy-sequence node) :child child))))

            (_ node))))
      (let ((new-tree (walk tree)))
        (unless inserted
          (user-error "Selected shapes were not found in tree"))
        new-tree))))

(defun scad-sketch--wrap-selected-shapes-with-node
    (session ordered-ids replacement-node &optional focus-ref)
  "Wrap ORDERED-IDS in SESSION tree with REPLACEMENT-NODE.

ORDERED-IDS must be selected whole-shape ids in tree traversal order.
FOCUS-REF, when non-nil, becomes the new selection and focus."
  (setf (scad-sketch-session-tree session)
        (scad-sketch--tree-insert-node-at-first-selected
         (scad-sketch-session-tree session)
         ordered-ids
         replacement-node))

  (if focus-ref
      (progn
        (setf (scad-sketch-session-selection session) (list focus-ref))
        (setf (scad-sketch-session-focus-ref session) focus-ref))
    ;; Keep the constituent shapes selected. Existing boolean rendering already
    ;; uses selected shape refs to highlight containing boolean groups.
    (setf (scad-sketch-session-selection session)
          (mapcar #'scad-sketch--shape-ref ordered-ids)))

  (setf (scad-sketch-session-hover-index session) 0)
  replacement-node)

(defun scad-sketch-wrap-selection-as-union ()
  "Wrap selected whole shapes in a union node."
  (interactive)
  (scad-sketch--edit
   (lambda (s)
     (let* ((ordered-ids
             (scad-sketch--selected-shape-ids-in-tree-order
              s 2 "Union"))
            (group-id (scad-sketch--next-editor-group-id s 'union))
            (node
             (scad-sketch-session--tree-boolean
              'union
              group-id
              (scad-sketch--shape-leaf-nodes ordered-ids))))
       (scad-sketch--wrap-selected-shapes-with-node
        s ordered-ids node nil)))))

(defun scad-sketch-wrap-selection-as-intersection ()
  "Wrap selected whole shapes in an intersection node."
  (interactive)
  (scad-sketch--edit
   (lambda (s)
     (let* ((ordered-ids
             (scad-sketch--selected-shape-ids-in-tree-order
              s 2 "Intersection"))
            (group-id (scad-sketch--next-editor-group-id s 'intersection))
            (node
             (scad-sketch-session--tree-boolean
              'intersection
              group-id
              (scad-sketch--shape-leaf-nodes ordered-ids))))
       (scad-sketch--wrap-selected-shapes-with-node
        s ordered-ids node nil)))))

(defun scad-sketch-wrap-selection-as-difference ()
  "Wrap selected whole shapes in a difference node.

The first selected shape in tree order is the positive child.  Remaining
selected shapes become subtractive children."
  (interactive)
  (scad-sketch--edit
   (lambda (s)
     (let* ((ordered-ids
             (scad-sketch--selected-shape-ids-in-tree-order
              s 2 "Difference"))
            (group-id (scad-sketch--next-editor-group-id s 'difference))
            (node
             (scad-sketch-session--tree-boolean
              'difference
              group-id
              (scad-sketch--shape-leaf-nodes ordered-ids))))
       (scad-sketch--wrap-selected-shapes-with-node
        s ordered-ids node nil)))))

(defun scad-sketch-wrap-selection-as-mirror ()
  "Wrap selected whole shapes in a mirror node.

The new mirror uses default normal vector [1, 0].  Use
`scad-sketch-set-mirror-axis' afterward to set the mirror normal."
  (interactive)
  (scad-sketch--edit
   (lambda (s)
     (let* ((ordered-ids
             (scad-sketch--selected-shape-ids-in-tree-order
              s 1 "Mirror"))
            (mirror-id (scad-sketch--next-editor-group-id s 'mirror))
            (child
             (if (= (length ordered-ids) 1)
                 (scad-sketch-session--tree-shape (car ordered-ids))
               (scad-sketch-session--tree-boolean
                'union
                (scad-sketch--next-editor-group-id s 'union)
                (scad-sketch--shape-leaf-nodes ordered-ids))))
            (node
             (scad-sketch-session--tree-mirror
              mirror-id
              1.0
              0.0
              child)))
       (scad-sketch--wrap-selected-shapes-with-node
        s
        ordered-ids
        node
        (scad-sketch--mirror-ref mirror-id))))))

;;; Focus/selection commands
(defun scad-sketch--current-breakable-group-ref (session)
  "Return the currently attentioned group wrapper ref to break apart.

Break works only on direct group refs:
  - :boolean
  - :mirror

It does not work on :boolean-members, shapes, points, or selected child shapes."
  (let* ((ref  (scad-sketch--attention-ref session))
         (kind (and ref (scad-sketch--ref-kind ref))))
    (pcase kind
      ('boolean ref)
      ('mirror  ref)
      (_ nil))))

(defun scad-sketch--tree-break-ref (tree ref)
  "Return TREE with boolean/mirror REF broken apart.

A matched boolean node is replaced by its children.  A matched mirror node is
replaced by its child.  If breaking a root node yields multiple children, the
new root is a sequence node."
  (let ((changed nil))
    (cl-labels
        ((match-p
          (node)
          (pcase (and node (plist-get node :kind))
            ('boolean
             (and (eq (scad-sketch--ref-kind ref) 'boolean)
                  (equal (plist-get node :group-id)
                         (plist-get ref :group-id))))
            ('mirror
             (and (eq (scad-sketch--ref-kind ref) 'mirror)
                  (eq (plist-get node :mirror-id)
                      (scad-sketch--ref-mirror-id ref))))
            (_ nil)))

         (walk-list
          (nodes)
          (apply #'append (mapcar #'walk nodes)))

         (walk
          (node)
          (cond
           ((null node)
            nil)

           ((match-p node)
            (setq changed t)
            (pcase (plist-get node :kind)
              ('boolean (plist-get node :children))
              ('mirror  (list (plist-get node :child)))))

           (t
            (pcase (plist-get node :kind)
              ('boolean
               (list
                (plist-put
                 (copy-sequence node)
                 :children
                 (walk-list (plist-get node :children)))))

              ('mirror
               (let ((children (walk (plist-get node :child))))
                 (cond
                  ((null children) nil)
                  ((null (cdr children))
                   (list (plist-put
                          (copy-sequence node)
                          :child (car children))))
                  (t
                   (list (plist-put
                          (copy-sequence node)
                          :child
                          (scad-sketch-session--tree-sequence children)))))))

              ('sequence
               (list
                (plist-put
                 (copy-sequence node)
                 :children
                 (walk-list (plist-get node :children)))))

              (_
               (list node)))))))
      (let ((children (walk tree)))
        (unless changed
          (user-error "No breakable group found"))
        (cond
         ((null children) nil)
         ((null (cdr children)) (car children))
         (t (scad-sketch-session--tree-sequence children)))))))

(defun scad-sketch-break-apart-group ()
  "Break apart the currently attentioned boolean or mirror group wrapper.

To break a boolean group, put attention on the group wrapper itself, not on
`:boolean-members' and not on a child shape."
  (interactive)
  (scad-sketch--edit
   (lambda (s)
     (let ((ref (scad-sketch--current-breakable-group-ref s)))
       (unless ref
         (user-error "Break needs attention on a boolean or mirror group wrapper"))
       (setf (scad-sketch-session-tree s)
             (scad-sketch--tree-break-ref
              (scad-sketch-session-tree s)
              ref))
       (setf (scad-sketch-session-selection s) nil)
       (setf (scad-sketch-session-focus-ref s) nil)
       (setf (scad-sketch-session-hover-index s) 0)))))

(defun scad-sketch--set-focus-ref (session ref)
  "Set SESSION global focus to REF and move cursor to REF's anchor.

This is used by global selectable cycling, not by hover cycling."
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
  "Cycle global focus by DELTA through all selectable refs.

Unlike hover cycling, this moves the editor cursor to the selected ref's anchor."
  (scad-sketch--clean-change
   (lambda (s)
     (let* ((refs  (scad-sketch--selectable-refs s))
            (n     (length refs))
            (focus (or (scad-sketch-session-focus-ref s) (car refs)))
            (idx   (or (cl-position-if
                        (lambda (ref) (scad-sketch--same-ref-p ref focus))
                        refs)
                       0)))
       (unless (> n 0) (user-error "No selectable objects"))
       (scad-sketch--set-focus-ref s (nth (mod (+ idx delta) n) refs))))))

(defun scad-sketch-next-selectable ()
  "Cycle focus to the next selectable shape/point."
  (interactive)
  (scad-sketch--cycle-selectable 1))

(defun scad-sketch-previous-selectable ()
  "Cycle focus to the previous selectable shape/point."
  (interactive)
  (scad-sketch--cycle-selectable -1))

(defun scad-sketch--cycle-hovered (delta)
  "Cycle hover by DELTA among refs under the cursor point.

This does not move the cursor and does not update global focus."
  (scad-sketch--clean-change
   (lambda (s)
     (let* ((candidates (scad-sketch--hover-candidates s))
            (n          (length candidates)))
       (unless (> n 0) (user-error "No hovered objects under point"))
       (setf (scad-sketch-session-hover-index s)
             (mod (+ (or (scad-sketch-session-hover-index s) 0) delta)
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
  "Toggle the currently attended object in the selection."
  (interactive)
  (scad-sketch--clean-change
   (lambda (s)
     (let ((ref (scad-sketch--attention-ref s)))
       (unless ref (user-error "No focused or hovered object"))
       (scad-sketch--toggle-ref-selection s ref)))))

(defun scad-sketch-clear-selection ()
  "Clear the current selection."
  (interactive)
  (scad-sketch--clean-change
   (lambda (s)
     (setf (scad-sketch-session-selection s) nil))))

(defun scad-sketch-clear-transient-state ()
  "Clear transient editor state.

Clears marks, selection, and hover cycling state.  If marks are present, this
is undoable so accidental `Esc' can be recovered with `u'.  This does not dirty
source geometry."
  (interactive)
  (let ((session (scad-sketch--assert-session)))
    (if (scad-sketch-session-marks session)
        (scad-sketch--undoable-clean-change
         (lambda (s)
           (setf (scad-sketch-session-marks s) nil)
           (setf (scad-sketch-session-selection s) nil)
           (setf (scad-sketch-session-hover-index s) 0)))
      (scad-sketch--clean-change
       (lambda (s)
         (setf (scad-sketch-session-selection s) nil)
         (setf (scad-sketch-session-hover-index s) 0))))))

(provide 'scad-sketch-editor--editing)
;;; scad-sketch-editor--editing.el ends here
