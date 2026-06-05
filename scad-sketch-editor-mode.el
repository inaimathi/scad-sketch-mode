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

    ;; ── Selected vertex / primitive handle movement ──────────────────
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

    ;; ── Marks / transient clears ─────────────────────────────────────
    (define-key map (kbd "m")           #'scad-sketch-set-mark)
    (define-key map (kbd "M")           #'scad-sketch-push-mark)
    (define-key map (kbd "`")           #'scad-sketch-pop-mark)
    (define-key map (kbd "'")           #'scad-sketch-jump-to-mark)
    (define-key map (kbd "C")           #'scad-sketch-clear-marks)
    (define-key map (kbd "s")           #'scad-sketch-clear-selection)
    (define-key map (kbd "<escape>")    #'scad-sketch-clear-transient-state)

    ;; ── Vertex / shape editing ────────────────────────────────────────
    (define-key map (kbd "p")           #'scad-sketch-append-point)
    (define-key map (kbd "i")           #'scad-sketch-insert-point-after-selected)
    (define-key map (kbd "k")           #'scad-sketch-delete-selected)
    (define-key map (kbd "c")           #'scad-sketch-toggle-closed)
    (define-key map (kbd "R")           #'scad-sketch-set-radius)
    (define-key map (kbd "A")           #'scad-sketch-set-mirror-axis)

    ;; ── Drawing from marks + point ────────────────────────────────────
    (define-key map (kbd "l")           #'scad-sketch-line-from-mark)
    (define-key map (kbd "r")           #'scad-sketch-rectangle-from-mark)
    (define-key map (kbd "B")           #'scad-sketch-draw-square-from-marks)
    (define-key map (kbd "O")           #'scad-sketch-draw-circle-from-mark)
    (define-key map (kbd "P")           #'scad-sketch-draw-polygon-from-marks)

    ;; ── Hover / focus / selection ─────────────────────────────────────
    (define-key map (kbd "TAB")         #'scad-sketch-next-hovered)
    (define-key map (kbd "<backtab>")   #'scad-sketch-previous-hovered)
    (define-key map (kbd ".")           #'scad-sketch-next-hovered)
    (define-key map (kbd ",")           #'scad-sketch-previous-hovered)
    (define-key map (kbd "M-TAB")       #'scad-sketch-next-selectable)
    (define-key map (kbd "C-M-i")       #'scad-sketch-next-selectable)
    (define-key map (kbd "M-<backtab>") #'scad-sketch-previous-selectable)
    (define-key map (kbd "SPC")         #'scad-sketch-toggle-attention-selection)

    ;; ── Coordinate / constraint commands ─────────────────────────────
    (define-key map (kbd "x")           #'scad-sketch-set-x)
    (define-key map (kbd "y")           #'scad-sketch-set-y)
    (define-key map (kbd "X")           #'scad-sketch-set-delta-x)
    (define-key map (kbd "Y")           #'scad-sketch-set-delta-y)
    (define-key map (kbd "d")           #'scad-sketch-set-distance-from-mark)
    (define-key map (kbd "a")           #'scad-sketch-set-angle-from-mark)
    (define-key map (kbd "g")           #'scad-sketch-set-grid)

    ;; ── Session ───────────────────────────────────────────────────────
    (define-key map (kbd "u")           #'scad-sketch-undo)
    (define-key map (kbd "w")           #'scad-sketch-write-back)
    (define-key map (kbd "q")           #'scad-sketch-quit)
    (define-key map (kbd "?")           #'scad-sketch-help)
    map)
  "Keymap for `scad-sketch-editor-mode'.")

;;; Mode definition

(define-derived-mode scad-sketch-editor-mode special-mode "SCAD-Sketch"
  "Major mode for the scad-sketch visual editor.

The buffer shows an SVG canvas followed by a live OpenSCAD array preview.

The canvas displays:
  - a grid (step set with `g')
  - the polygon path with arcs where polyRound radii are set
  - vertex dots labelled SHAPE:INDEX; selected vertex highlighted in orange
  - dashed radius circles on rounded vertices (orange = capped by edge length)
  - the cursor crosshair in blue, marks in green
  - a status bar: name, grid size, cursor coords, dirty flag

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
  "Display a key binding summary in the echo area.
For full documentation use \\[describe-mode]."
  (interactive)
  (scad-sketch--assert-session)
  (message
   (concat
    "arrows=move cursor(clean)  C-arrows=coarse  M-arrows=fine | "
    "TAB/S-TAB=focus shape/point  ./,=cycle hovered  "
    "SPC=toggle selection  s=clear selection | "
    "S-arrows=move selected geometry(dirty) | "
    "p=append  i=insert  k=delete | "
    "m=set-mark  M=push  `=pop  '=jump  C=clear marks | "
    "R=radius  c=closed  l=line  r=rect | "
    "x/y=coord  X/Y=delta  d=dist  a=angle  g=grid | "
    "w=write  u=undo  q=quit  C-h m=full help")))

(provide 'scad-sketch-editor-mode)
;;; scad-sketch-editor-mode.el ends here
