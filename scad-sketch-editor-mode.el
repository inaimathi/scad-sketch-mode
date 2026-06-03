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
    (define-key map (kbd "TAB")       #'scad-sketch-next-point)
    (define-key map (kbd "<backtab>") #'scad-sketch-previous-point)
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
  (push (list :points         (copy-tree (scad-sketch-session-points session))
              :point          (copy-tree (scad-sketch-session-point session))
              :marks          (copy-tree (scad-sketch-session-marks session))
              :named-marks    (copy-tree (scad-sketch-session-named-marks session))
              :selected-index (scad-sketch-session-selected-index session)
              :closed         (scad-sketch-session-closed session))
        (scad-sketch-session-undo-stack session)))

(defun scad-sketch--mark-dirty (session)
  "Mark SESSION as having unsaved edits."
  (setf (scad-sketch-session-dirty session) t))

(defun scad-sketch--mutate (fn)
  "Push undo, call FN with session, mark dirty, re-render."
  (let ((session (scad-sketch--assert-session)))
    (scad-sketch--push-undo session)
    (funcall fn session)
    (scad-sketch--mark-dirty session)
    (scad-sketch--render)))

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

;;; Movement


(defun scad-sketch--move-point (dx dy &optional snap)
  "Move cursor by DX, DY; snap to grid when SNAP is non-nil."
  (scad-sketch--mutate
   (lambda (s)
     (let ((new (scad-sketch--move-xy (scad-sketch-session-point s) dx dy)))
       (setf (scad-sketch-session-point s)
             (if snap (scad-sketch--snap-xy new (scad-sketch--grid s)) new))))))

(defun scad-sketch--move-selected (dx dy &optional snap)
  "Move selected vertex by DX, DY; snap to grid when SNAP is non-nil."
  (scad-sketch--mutate
   (lambda (s)
     (let* ((old    (or (scad-sketch--selected-point s)
                        (user-error "No selected point")))
            (new-xy (scad-sketch--move-xy (scad-sketch--point-xy old) dx dy))
            (snapped (if snap (scad-sketch--snap-xy new-xy (scad-sketch--grid s)) new-xy))
            (new    (scad-sketch--make-model-point snapped old)))
       (scad-sketch--set-selected-point s new)
       (setf (scad-sketch-session-point s) snapped)))))

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
  (scad-sketch--mutate
   (lambda (s)
     (setf (scad-sketch-session-marks s)
           (list (copy-sequence (scad-sketch-session-point s)))))))

(defun scad-sketch-push-mark ()
  "Push the current cursor position onto the mark stack."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (push (copy-sequence (scad-sketch-session-point s))
           (scad-sketch-session-marks s)))))

(defun scad-sketch-pop-mark ()
  "Pop the most recent mark and jump cursor to it."
  (interactive)
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-marks session) (user-error "No marks set")))
  (scad-sketch--mutate
   (lambda (s)
     (setf (scad-sketch-session-point s)
           (copy-sequence (pop (scad-sketch-session-marks s)))))))

(defun scad-sketch-jump-to-mark ()
  "Move cursor to the most recent mark without consuming it."
  (interactive)
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-marks session) (user-error "No marks set")))
  (scad-sketch--mutate
   (lambda (s)
     (setf (scad-sketch-session-point s)
           (copy-sequence (car (scad-sketch-session-marks s)))))))

(defun scad-sketch-clear-marks ()
  "Clear all marks."
  (interactive)
  (scad-sketch--mutate
   (lambda (s) (setf (scad-sketch-session-marks s) nil))))

;;; Vertex editing

(defun scad-sketch--append-model-point (session point)
  "Append POINT to SESSION and select it."
  (setf (scad-sketch-session-points session)
        (append (scad-sketch-session-points session) (list point)))
  (setf (scad-sketch-session-selected-index session)
        (1- (length (scad-sketch-session-points session)))))

(defun scad-sketch-append-point ()
  "Append the cursor position as a new vertex."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (scad-sketch--append-model-point
      s (scad-sketch--make-model-point (scad-sketch-session-point s))))))

(defun scad-sketch-insert-point-after-selected ()
  "Insert points after the selected vertex.
With marks set, inserts each mark (oldest first) then the cursor.
Without marks, inserts only the cursor."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (let* ((idx       (or (scad-sketch-session-selected-index s) -1))
            (points    (scad-sketch-session-points s))
            (insert-at (min (1+ idx) (length points)))
            (mark-pts  (mapcar (lambda (m) (scad-sketch--make-model-point m))
                               (reverse (scad-sketch-session-marks s))))
            (cursor-pt (scad-sketch--make-model-point (scad-sketch-session-point s)))
            (new-pts   (append mark-pts (list cursor-pt)))
            (new-idx   (+ insert-at (length new-pts) -1)))
       (setf (scad-sketch-session-points s)
             (append (cl-subseq points 0 insert-at)
                     new-pts
                     (nthcdr insert-at points)))
       (setf (scad-sketch-session-selected-index s) new-idx)))))

(defun scad-sketch-delete-selected ()
  "Delete the selected vertex."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (let ((idx    (or (scad-sketch-session-selected-index s)
                       (user-error "No selected point")))
           (points (scad-sketch-session-points s)))
       (unless (< idx (length points)) (user-error "Selected point out of range"))
       (setf (scad-sketch-session-points s)
             (append (cl-subseq points 0 idx) (nthcdr (1+ idx) points)))
       (setf (scad-sketch-session-selected-index s)
             (cond ((null (scad-sketch-session-points s)) nil)
                   ((>= idx (length (scad-sketch-session-points s)))
                    (1- (length (scad-sketch-session-points s))))
                   (t idx)))))))

(defun scad-sketch-line-from-mark ()
  "Append marks (oldest first) then cursor as new vertices."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (dolist (p (scad-sketch--geometry-line-points
                 (scad-sketch-session-marks s)
                 (scad-sketch-session-point s)))
       (scad-sketch--append-model-point s p)))))

(defun scad-sketch-rectangle-from-mark ()
  "Append rectangle corners from most recent mark to cursor."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (dolist (p (scad-sketch--geometry-rectangle-points
                 (car (scad-sketch-session-marks s))
                 (scad-sketch-session-point s)))
       (scad-sketch--append-model-point s p)))))

(defun scad-sketch-toggle-closed ()
  "Toggle the closed flag."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (setf (scad-sketch-session-closed s) (not (scad-sketch-session-closed s))))))

(defun scad-sketch-set-radius (radius)
  "Set the polyRound radius of the selected vertex."
  (interactive (list (read-number "Radius: " 0)))
  (scad-sketch--mutate
   (lambda (s)
     (let ((pt (or (scad-sketch--selected-point s) (user-error "No selected point"))))
       (scad-sketch--set-selected-point
        s (list (nth 0 pt) (nth 1 pt) (float radius)))))))

(defun scad-sketch-next-point ()
  "Select the next vertex, moving cursor to it."
  (interactive)
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-points session) (user-error "No points")))
  (scad-sketch--mutate
   (lambda (s)
     (let* ((n   (length (scad-sketch-session-points s)))
            (idx (mod (1+ (or (scad-sketch-session-selected-index s) -1)) n)))
       (setf (scad-sketch-session-selected-index s) idx)
       (setf (scad-sketch-session-point s)
             (scad-sketch--point-xy (nth idx (scad-sketch-session-points s))))))))

(defun scad-sketch-previous-point ()
  "Select the previous vertex, moving cursor to it."
  (interactive)
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-points session) (user-error "No points")))
  (scad-sketch--mutate
   (lambda (s)
     (let* ((n   (length (scad-sketch-session-points s)))
            (idx (mod (1- (or (scad-sketch-session-selected-index s) 0)) n)))
       (setf (scad-sketch-session-selected-index s) idx)
       (setf (scad-sketch-session-point s)
             (scad-sketch--point-xy (nth idx (scad-sketch-session-points s))))))))

;;; Coordinate commands

(defun scad-sketch--set-point-axis (axis value)
  "Set cursor coordinate AXIS (0=x, 1=y) to VALUE."
  (scad-sketch--mutate
   (lambda (s)
     (let ((pt (copy-sequence (scad-sketch-session-point s))))
       (setf (nth axis pt) (float value))
       (setf (scad-sketch-session-point s) pt)))))

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
  (scad-sketch--mutate
   (lambda (s)
     (setf (scad-sketch-session-point s)
           (scad-sketch--geometry-point-at-distance
            (car (scad-sketch-session-marks s))
            (scad-sketch-session-point s)
            distance)))))

(defun scad-sketch-set-angle-from-mark (degrees)
  "Set angle from most recent mark to cursor in DEGREES, preserving distance."
  (interactive (list (read-number "Angle degrees from mark: " 0)))
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-marks session) (user-error "No marks set")))
  (scad-sketch--mutate
   (lambda (s)
     (setf (scad-sketch-session-point s)
           (scad-sketch--geometry-point-at-angle
            (car (scad-sketch-session-marks s))
            (scad-sketch-session-point s)
            degrees)))))

(defun scad-sketch-set-grid (grid)
  "Set the grid step."
  (interactive (list (read-number "Grid step: " (scad-sketch-session-grid (scad-sketch--assert-session)))))
  (scad-sketch--mutate
   (lambda (s) (setf (scad-sketch-session-grid s) (float grid)))))

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
    (setf (scad-sketch-session-dirty session) t)
    (scad-sketch--render)))


;;; Rendering

(defun scad-sketch--bounds (session)
  "Return (min-x max-x min-y max-y) for all points, marks, and cursor."
  (let* ((pts   (mapcar #'scad-sketch--point-xy (scad-sketch-session-points session)))
         (extra (delq nil (cons (scad-sketch-session-point session)
                                (scad-sketch-session-marks session))))
         (all   (append pts extra)))
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

(defun scad-sketch--draw-path (svg transform session)
  "Draw the polygon path and vertex circles."
  (let* ((points  (scad-sketch-session-points session))
         (closed  (scad-sketch-session-closed session))
         (n       (length points))
         (idx     0))
    (when (>= n 2)
      (if (scad-sketch--any-radius-p points)
          (let ((d (scad-sketch--polyround-path-d points closed transform)))
            (when d
              (svg-node svg 'path :d d :stroke "#111111" :stroke-width 3 :fill "none")))
        (let ((xy-points (mapcar #'scad-sketch--point-xy points)))
          (cl-loop for a on xy-points for b = (cadr a) when b do
                   (scad-sketch--svg-line svg transform (car a) b
                                          :stroke "#111111" :stroke-width 3))
          (when (and closed (> n 2))
            (scad-sketch--svg-line svg transform (car (last xy-points)) (car xy-points)
                                   :stroke "#111111" :stroke-width 3)))))
    (let* ((n      (length points))
           (closed (scad-sketch-session-closed session)))
      (dolist (pt points)
        (let* ((xy     (scad-sketch--point-xy pt))
               (screen (funcall transform xy))
               (sel    (= idx (or (scad-sketch-session-selected-index session) -1)))
               (radius (scad-sketch--point-radius pt)))
          (svg-circle svg (nth 0 screen) (nth 1 screen) (if sel 7 5)
                      :stroke (if sel "#d13f00" "#111111") :stroke-width (if sel 3 2)
                      :fill   (if sel "#fff0e8" "#ffffff"))
          (svg-text svg (number-to-string idx)
                    :x (+ (nth 0 screen) 8) :y (- (nth 1 screen) 8)
                    :font-size 12 :fill "#333333")
          (when (> radius 0)
            (let* ((prev    (cond ((> idx 0)      (nth (1- idx) points))
                                  (closed         (nth (1- n)   points))))
                   (next    (cond ((< idx (1- n)) (nth (1+ idx) points))
                                  (closed         (nth 0        points))))
                   (corner  (when (and prev next)
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
                        :font-size 11 :fill (if capped "#c04000" "#804000")))))
        (setq idx (1+ idx))))))

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
  (let* ((marks    (scad-sketch-session-marks session))
         (sel      (scad-sketch-session-selected-index session))
         (mark-str (cond ((null marks) "none")
                         ((= 1 (length marks)) (scad-sketch--fmt-xy (car marks)))
                         (t (format "%s (+%d)" (scad-sketch--fmt-xy (car marks))
                                    (1- (length marks))))))
         (text (format "%s  grid=%s%s  point=%s  mark=%s  sel=%s  %s"
                       (scad-sketch-session-name session)
                       (scad-sketch--fmt-num (scad-sketch-session-grid session))
                       (scad-sketch-session-units session)
                       (scad-sketch--fmt-xy (scad-sketch-session-point session))
                       mark-str
                       (if sel (number-to-string sel) "none")
                       (if (scad-sketch-session-dirty session) "*dirty*" "saved"))))
    (svg-rectangle svg 0 0 scad-sketch-canvas-width 28 :fill "#f8f8f8")
    (svg-text svg text :x 10 :y 19 :font-size 13 :fill "#111111")))

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
  (message (concat "arrows=move  C-arrows=coarse  M-arrows=fine(off-grid)  S-arrows=move-vertex | "
                   "TAB/S-TAB=select  p=append  i=insert  k=delete | "
                   "m=set-mark  M=push  `=pop  '=jump  C=clear | "
                   "R=radius  c=closed  l=line  r=rect | "
                   "x/y=coord  X/Y=delta  d=dist  a=angle  g=grid | "
                   "w=write  u=undo  q=quit  C-h m=full help")))


(provide 'scad-sketch-editor-mode)
;;; scad-sketch-editor-mode.el ends here
