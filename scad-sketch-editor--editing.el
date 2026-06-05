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
(require 'scad-sketch-editor--selection)
(require 'scad-sketch-editor--cursor)    ; for --grid/--fine/--coarse

;;; Internal helpers

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

;;; Selected geometry movement
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
         (a     (* pi (/ angle 180.0)))
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
  "Move primitive SHAPE handle IDX to XY."
  (pcase (scad-sketch-shape-kind shape)
    ('circle
     (let* ((md (scad-sketch-shape-metadata shape))
            (cx (float (or (plist-get md :cx) 0.0)))
            (cy (float (or (plist-get md :cy) 0.0)))
            (dx (- (nth 0 xy) cx))
            (dy (- (nth 1 xy) cy))
            (r  (sqrt (+ (* dx dx) (* dy dy)))))
       (setq md (plist-put md :r (max 0.0001 r)))
       (setf (scad-sketch-shape-metadata shape) md)))
    ('square
     (scad-sketch--move-square-corner-to shape idx xy))
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

(defun scad-sketch--move-selected (dx dy &optional snap)
  "Move selected vertices/shapes by DX, DY.  Snap to grid when SNAP is non-nil.

For circle and square point refs, this moves the primitive edit handle rather
than translating the whole shape."
  (scad-sketch--edit
   (lambda (s)
     (let ((shape-ids (scad-sketch--selected-shape-ids s))
           (locs      (scad-sketch--selected-point-locs s nil)))
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
                  (shape    (scad-sketch-session-shape-by-id s shape-id)))
             (pcase (and shape (scad-sketch-shape-kind shape))
               ('polygon
                (let* ((points   (scad-sketch-shape-points shape))
                       (old      (nth idx points))
                       (new-xy   (scad-sketch--move-xy
                                  (scad-sketch--point-xy old) dx dy))
                       (snapped  (if snap
                                     (scad-sketch--snap-xy new-xy
                                                           (scad-sketch--grid s))
                                   new-xy))
                       (new      (scad-sketch--make-model-point snapped old)))
                  (setf (scad-sketch-shape-points shape)
                        (scad-sketch--replace-nth idx new points))))
               ((or 'circle 'square)
                (let* ((old-xy  (scad-sketch--primitive-handle-xy shape idx))
                       (new-xy  (scad-sketch--move-xy old-xy dx dy))
                       (snapped (if snap
                                    (scad-sketch--snap-xy new-xy
                                                          (scad-sketch--grid s))
                                  new-xy)))
                  (scad-sketch--move-primitive-handle-to shape idx snapped)))))))

        (t
         (let ((shape (scad-sketch-session-active-shape s)))
           (unless shape (user-error "No selected point or shape"))
           (scad-sketch--move-shape shape dx dy snap
                                    (scad-sketch--grid s)))))))))

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
  "Insert points after the selected vertex in the active shape.

With marks set, inserts each mark (oldest first) then the cursor.
Without marks, inserts only the cursor."
  (interactive)
  (scad-sketch--edit
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

(defun scad-sketch-line-from-mark ()
  "Create a new polygon shape from marks (oldest first) and cursor."
  (interactive)
  (scad-sketch--edit
   (lambda (s)
     (unless (scad-sketch-session-marks s) (user-error "No marks set"))
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
  (scad-sketch--edit
   (lambda (s)
     (let ((mark (or (car (scad-sketch-session-marks s))
                     (user-error "No marks set")))
           (pt   (scad-sketch-session-point s)))
       (let* ((x1 (nth 0 mark)) (y1 (nth 1 mark))
              (x2 (nth 0 pt))   (y2 (nth 1 pt))
              (points
               (mapcar #'scad-sketch--make-model-point
                       (list (list x1 y1) (list x2 y1)
                             (list x2 y2) (list x1 y2)))))
         (scad-sketch-session-add-shape s points))))))

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

;;; Focus/selection commands

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
  "Cycle attention by DELTA among hovered refs under point."
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

;;; Undo restore command

(defun scad-sketch-undo ()
  "Undo the last sketch edit."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (entry   (pop (scad-sketch-session-undo-stack session))))
    (unless entry (user-error "No sketch undo available"))
    (setf (scad-sketch-session-points          session) (plist-get entry :points))
    (setf (scad-sketch-session-point           session) (plist-get entry :point))
    (setf (scad-sketch-session-marks           session) (plist-get entry :marks))
    (setf (scad-sketch-session-named-marks     session) (plist-get entry :named-marks))
    (setf (scad-sketch-session-selected-index  session) (plist-get entry :selected-index))
    (setf (scad-sketch-session-closed          session) (plist-get entry :closed))
    (setf (scad-sketch-session-shapes          session) (plist-get entry :shapes))
    (setf (scad-sketch-session-active-shape-id session) (plist-get entry :active-shape-id))
    (setf (scad-sketch-session-targets         session) (plist-get entry :targets))
    (setf (scad-sketch-session-root-target-id  session) (plist-get entry :root-target-id))
    (setf (scad-sketch-session-selection       session) (plist-get entry :selection))
    (setf (scad-sketch-session-focus-ref       session) (plist-get entry :focus-ref))
    (setf (scad-sketch-session-dirty           session) t)
    (scad-sketch--render)))

(provide 'scad-sketch-editor--editing)
;;; scad-sketch-editor--editing.el ends here
