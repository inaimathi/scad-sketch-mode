;;; scad-sketch-editor-mode.el --- Major mode for the scad-sketch visual editor -*- lexical-binding: t; -*-

;;; Commentary:

;; Top-level assembly file.  Owns:
;;   - the keymap
;;   - `define-derived-mode' for `scad-sketch-editor-mode'
;;   - write-back and quit commands
;;   - the help summary command
;;
;; All substantive logic lives in the subsystem files required below.
;; Load order matters: rendering must be loaded after core so that
;; `scad-sketch--render' (called by core's dispatch triad) is defined
;; by the time any command fires.

;;; Code:

(require 'scad-sketch-session)
(require 'scad-sketch-geometry)
(require 'scad-sketch-editor--refs)
(require 'scad-sketch-editor--selection)
(require 'scad-sketch-editor-core)
(require 'scad-sketch-editor--cursor)
(require 'scad-sketch-editor--editing)
(require 'scad-sketch-editor--rendering)

;;; Keymap
(defvar scad-sketch-editor-insert-map
  (let ((map (make-sparse-keymap)))
    ;; Existing polygon/array point editing.
    (define-key map (kbd "a") #'scad-sketch-append-point)
    (define-key map (kbd "i") #'scad-sketch-insert-point-after-selected)

    ;; Drawing from marks + point.
    (define-key map (kbd "l") #'scad-sketch-line-from-mark)
    (define-key map (kbd "r") #'scad-sketch-rectangle-from-mark)
    (define-key map (kbd "p") #'scad-sketch-draw-polygon-from-marks)
    (define-key map (kbd "b") #'scad-sketch-draw-square-from-marks)
    (define-key map (kbd "s") #'scad-sketch-draw-square-from-marks)
    (define-key map (kbd "c") #'scad-sketch-draw-circle-from-mark)
    (define-key map (kbd "o") #'scad-sketch-draw-circle-from-mark)
    (define-key map (kbd "t") #'scad-sketch-draw-text-at-point)
    map)
  "Prefix map for scad-sketch insertion/drawing commands.")

(defvar scad-sketch-editor-group-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "u") #'scad-sketch-wrap-selection-as-union)
    (define-key map (kbd "d") #'scad-sketch-wrap-selection-as-difference)
    (define-key map (kbd "i") #'scad-sketch-wrap-selection-as-intersection)
    (define-key map (kbd "m") #'scad-sketch-wrap-selection-as-mirror)
    (define-key map (kbd "v") #'scad-sketch-wrap-selection-as-mirror)
    (define-key map (kbd "b") #'scad-sketch-break-apart-group)
    (define-key map (kbd "x") #'scad-sketch-break-apart-group)
    map)
  "Prefix map for scad-sketch grouping/boolean commands.")

(defvar scad-sketch-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)

    ;; ── Cursor movement ──────────────────────────────────────────────
    (define-key map (kbd "<left>")      #'scad-sketch-move-point-left)
    (define-key map (kbd "<right>")     #'scad-sketch-move-point-right)
    (define-key map (kbd "<up>")        #'scad-sketch-move-point-up)
    (define-key map (kbd "<down>")      #'scad-sketch-move-point-down)
    (define-key map (kbd "M-<left>")    #'scad-sketch-move-point-fine-left)
    (define-key map (kbd "M-<right>")   #'scad-sketch-move-point-fine-right)
    (define-key map (kbd "M-<up>")      #'scad-sketch-move-point-fine-up)
    (define-key map (kbd "M-<down>")    #'scad-sketch-move-point-fine-down)
    (define-key map (kbd "C-<left>")    #'scad-sketch-move-point-coarse-left)
    (define-key map (kbd "C-<right>")   #'scad-sketch-move-point-coarse-right)
    (define-key map (kbd "C-<up>")      #'scad-sketch-move-point-coarse-up)
    (define-key map (kbd "C-<down>")    #'scad-sketch-move-point-coarse-down)

    ;; ── Selected geometry movement ──────────────────────────────────
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

    ;; ── Marks and transient clears ──────────────────────────────────
    (define-key map (kbd "m")           #'scad-sketch-set-mark)
    (define-key map (kbd "M")           #'scad-sketch-push-mark)
    (define-key map (kbd "`")           #'scad-sketch-pop-mark)
    (define-key map (kbd "'")           #'scad-sketch-jump-to-mark)
    (define-key map (kbd "C")           #'scad-sketch-clear-marks)
    (define-key map (kbd "s")           #'scad-sketch-clear-selection)
    (define-key map (kbd "<escape>")    #'scad-sketch-clear-transient-state)

    ;; ── Prefix maps ─────────────────────────────────────────────────
    (define-key map (kbd "i")           scad-sketch-editor-insert-map)
    (define-key map (kbd "b")           scad-sketch-editor-group-map)

    ;; ── Shape/parameter editing ─────────────────────────────────────
    (define-key map (kbd "k")           #'scad-sketch-delete-selected)
    (define-key map (kbd "c")           #'scad-sketch-toggle-closed)
    (define-key map (kbd "R")           #'scad-sketch-set-radius)
    (define-key map (kbd "A")           #'scad-sketch-set-mirror-axis)

    ;; ── Hover / focus / selection ───────────────────────────────────
    (define-key map (kbd "TAB")         #'scad-sketch-next-hovered)
    (define-key map (kbd "<backtab>")   #'scad-sketch-previous-hovered)
    (define-key map (kbd ".")           #'scad-sketch-next-hovered)
    (define-key map (kbd ",")           #'scad-sketch-previous-hovered)
    (define-key map (kbd "M-TAB")       #'scad-sketch-next-selectable)
    (define-key map (kbd "C-M-i")       #'scad-sketch-next-selectable)
    (define-key map (kbd "M-<backtab>") #'scad-sketch-previous-selectable)
    (define-key map (kbd "SPC")         #'scad-sketch-toggle-attention-selection)

    ;; ── Coordinate / constraint commands ────────────────────────────
    (define-key map (kbd "x")           #'scad-sketch-set-x)
    (define-key map (kbd "y")           #'scad-sketch-set-y)
    (define-key map (kbd "X")           #'scad-sketch-set-delta-x)
    (define-key map (kbd "Y")           #'scad-sketch-set-delta-y)
    (define-key map (kbd "d")           #'scad-sketch-set-distance-from-mark)
    (define-key map (kbd "a")           #'scad-sketch-set-angle-from-mark)
    (define-key map (kbd "g")           #'scad-sketch-set-grid)

    ;; ── Session/help ────────────────────────────────────────────────
    (define-key map (kbd "u")           #'scad-sketch-undo)
    (define-key map (kbd "w")           #'scad-sketch-write-back)
    (define-key map (kbd "q")           #'scad-sketch-quit)
    (define-key map (kbd "?")           #'describe-mode)
    (define-key map (kbd "S-SPC") #'scad-sketch-preview-until-next-input)
    map)
  "Keymap for `scad-sketch-editor-mode'.")

;;; Mode definition

(define-derived-mode scad-sketch-editor-mode special-mode "SCAD-Sketch"
  "Major mode for the scad-sketch visual editor.

The buffer shows an SVG canvas followed by a live OpenSCAD preview.
The preview may represent an array, a single 2D primitive, a polygon,
a transformed shape, a boolean tree, a mirror tree, or a newly inserted
generic block.

The editor is intended as a small FreeCAD-sketch-like interface for
OpenSCAD source.  It edits a supported 2D subset directly and writes the
result back to the source buffer.

Visual model:
  - The blue cursor crosshair is the editor point.
  - Green markers are construction marks.
  - Orange objects are explicitly selected.
  - A blue halo marks the currently hovered attention target.
  - Polygon vertices and primitive handles are point-like refs.
  - Circle, square, text, mirror-axis, and polygon handles can be hovered,
    selected, and edited.
  - Boolean and mirror previews are rendered from the session tree.
  - Mirror output is shown as a dashed secondary outline; the source-side
    geometry remains directly editable.

Core concepts:
  selection
    Explicit multi-object set toggled by selection commands.

  hover
    Stack of refs under or near the editor point.  Hover cycling chooses
    which one receives attention without moving point.

  focus
    Global fallback ref used when nothing is hovered.  Global focus cycling
    moves point to the focused ref.

  attention
    The effective current ref: hovered ref if any exists, otherwise focus.

Editing model:
  - Plain cursor movement is clean UI state.
  - Shape, point, primitive-handle, mirror-axis, and tree mutations are dirty
    source edits and are undoable.
  - Moving selected geometry also moves point by the same delta.
  - Drawing commands create inline shape calls.
  - Existing inline polygons remain inline.
  - Existing variable-reference polygons continue to reference their source
    arrays when safe.
  - Direct array sessions remain array sessions; generic blank sessions emit
    shape/tree source instead of an array assignment.

Major command groups:
  point and mark operations
    Move point, set/push/pop/jump marks, clear transient state.

  hover/focus/selection operations
    Cycle hovered refs, cycle global focus, toggle selection, clear selection.

  insertion prefix
    Add polygon points, draw polygons, boxes, circles, text, and legacy
    mark-based paths.

  group prefix
    Wrap selected whole shapes as union, difference, intersection, or mirror;
    break apart boolean/mirror groups.

  parameter editing
    Set coordinates, grid, radius, mirror axis, text content, text size,
    text font, and primitive sizes.

  session operations
    Undo, write back, quit, and native Emacs help.

Use `describe-mode' or `describe-bindings' for the generated key list.

\\{scad-sketch-editor-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t))

;;; Write-back and quit

(defun scad-sketch-write-back ()
  "Write the edited sketch back to the source buffer."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (source  (scad-sketch-session-source-buffer session)))
    (scad-sketch-session-write-back session)
    (scad-sketch--render)
    (message "Wrote scad-sketch `%s' back to %s"
             (scad-sketch-session-name session)
             (if (buffer-live-p source)
                 (buffer-name source)
               "<dead buffer>"))))

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

;;; Help
(defun scad-sketch-help ()
  "Show native Emacs mode help for `scad-sketch-editor-mode'."
  (interactive)
  (call-interactively #'describe-mode))

(provide 'scad-sketch-editor-mode)
;;; scad-sketch-editor-mode.el ends here
