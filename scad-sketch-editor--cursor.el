;;; scad-sketch-editor--cursor.el --- Cursor movement and mark commands -*- lexical-binding: t; -*-

;;; Commentary:

;; All commands that move the editor cursor or manipulate the mark stack,
;; plus coordinate/constraint setters (x, y, X, Y, d, a) and the grid
;; command.  These are all *clean* changes: they do not mutate source
;; geometry and therefore do not push undo or mark the session dirty.
;;
;; The interactive movement commands are defined at the bottom and are
;; wired into the keymap in `scad-sketch-editor-mode'.

;;; Code:

(require 'scad-sketch-session)
(require 'scad-sketch-geometry)
(require 'scad-sketch-editor-core)

;;; Step accessors

(defun scad-sketch--grid   (s) (float (scad-sketch-session-grid       s)))
(defun scad-sketch--fine   (s) (float (scad-sketch-session-fine-step  s)))
(defun scad-sketch--coarse (s) (float (scad-sketch-session-coarse-step s)))

;;; Primitive cursor movement

(defun scad-sketch--move-point (dx dy &optional snap)
  "Move cursor by DX, DY.  When SNAP is non-nil, snap to grid.

This is a clean operation: moving the editor cursor does not dirty source."
  (scad-sketch--clean-change
   (lambda (s)
     (let ((new (scad-sketch--move-xy (scad-sketch-session-point s) dx dy)))
       (setf (scad-sketch-session-point s)
             (if snap (scad-sketch--snap-xy new (scad-sketch--grid s)) new))
       (setf (scad-sketch-session-hover-index s) 0)))))

;;; Cursor movement interactive commands

(defun scad-sketch-move-point-left ()
  "Move cursor one grid step left."
  (interactive)
  (scad-sketch--move-point (- (scad-sketch--grid (scad-sketch--assert-session))) 0 t))

(defun scad-sketch-move-point-right ()
  "Move cursor one grid step right."
  (interactive)
  (scad-sketch--move-point (scad-sketch--grid (scad-sketch--assert-session)) 0 t))

(defun scad-sketch-move-point-up ()
  "Move cursor one grid step up."
  (interactive)
  (scad-sketch--move-point 0 (scad-sketch--grid (scad-sketch--assert-session)) t))

(defun scad-sketch-move-point-down ()
  "Move cursor one grid step down."
  (interactive)
  (scad-sketch--move-point 0 (- (scad-sketch--grid (scad-sketch--assert-session))) t))

(defun scad-sketch-move-point-fine-left ()
  "Move cursor one fine step left (off-grid)."
  (interactive)
  (scad-sketch--move-point (- (scad-sketch--fine (scad-sketch--assert-session))) 0))

(defun scad-sketch-move-point-fine-right ()
  "Move cursor one fine step right (off-grid)."
  (interactive)
  (scad-sketch--move-point (scad-sketch--fine (scad-sketch--assert-session)) 0))

(defun scad-sketch-move-point-fine-up ()
  "Move cursor one fine step up (off-grid)."
  (interactive)
  (scad-sketch--move-point 0 (scad-sketch--fine (scad-sketch--assert-session))))

(defun scad-sketch-move-point-fine-down ()
  "Move cursor one fine step down (off-grid)."
  (interactive)
  (scad-sketch--move-point 0 (- (scad-sketch--fine (scad-sketch--assert-session)))))

(defun scad-sketch-move-point-coarse-left ()
  "Move cursor one coarse step left."
  (interactive)
  (scad-sketch--move-point (- (scad-sketch--coarse (scad-sketch--assert-session))) 0 t))

(defun scad-sketch-move-point-coarse-right ()
  "Move cursor one coarse step right."
  (interactive)
  (scad-sketch--move-point (scad-sketch--coarse (scad-sketch--assert-session)) 0 t))

(defun scad-sketch-move-point-coarse-up ()
  "Move cursor one coarse step up."
  (interactive)
  (scad-sketch--move-point 0 (scad-sketch--coarse (scad-sketch--assert-session)) t))

(defun scad-sketch-move-point-coarse-down ()
  "Move cursor one coarse step down."
  (interactive)
  (scad-sketch--move-point 0 (- (scad-sketch--coarse (scad-sketch--assert-session))) t))

;;; Mark commands
(defun scad-sketch-set-mark ()
  "Replace all marks with the current cursor position.

This is undoable but does not dirty source geometry."
  (interactive)
  (scad-sketch--undoable-clean-change
   (lambda (s)
     (setf (scad-sketch-session-marks s)
           (list (copy-sequence (scad-sketch-session-point s)))))))

(defun scad-sketch-push-mark ()
  "Push the current cursor position onto the mark stack.

This is undoable but does not dirty source geometry."
  (interactive)
  (scad-sketch--undoable-clean-change
   (lambda (s)
     (push (copy-sequence (scad-sketch-session-point s))
           (scad-sketch-session-marks s)))))

(defun scad-sketch-pop-mark ()
  "Pop the most recent mark and jump cursor to it.

This is undoable but does not dirty source geometry."
  (interactive)
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-marks session)
      (user-error "No marks set")))
  (scad-sketch--undoable-clean-change
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
  "Clear all marks.

This is undoable when marks are actually present, but does not dirty source
geometry."
  (interactive)
  (let ((session (scad-sketch--assert-session)))
    (if (scad-sketch-session-marks session)
        (scad-sketch--undoable-clean-change
         (lambda (s)
           (setf (scad-sketch-session-marks s) nil)))
      ;; No-op clears should not clutter the undo stack.
      (scad-sketch--clean-change
       (lambda (_s) nil)))))

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
  "Set cursor X coordinate."
  (interactive
   (list (read-number "X: "
                      (nth 0 (scad-sketch-session-point
                               (scad-sketch--assert-session))))))
  (scad-sketch--set-point-axis 0 x))

(defun scad-sketch-set-y (y)
  "Set cursor Y coordinate."
  (interactive
   (list (read-number "Y: "
                      (nth 1 (scad-sketch-session-point
                               (scad-sketch--assert-session))))))
  (scad-sketch--set-point-axis 1 y))

(defun scad-sketch--set-delta-axis (axis value)
  "Set cursor AXIS to (most recent mark AXIS) + VALUE."
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-marks session) (user-error "No marks set"))
    (scad-sketch--set-point-axis
     axis (+ (nth axis (car (scad-sketch-session-marks session)))
             (float value)))))

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
     (let* ((m     (car (scad-sketch-session-marks s)))
            (p     (scad-sketch-session-point s))
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

;;; Grid

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

(provide 'scad-sketch-editor--cursor)
;;; scad-sketch-editor--cursor.el ends here
