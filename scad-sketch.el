;;; scad-sketch.el --- Keyboard sketch editor entry point for OpenSCAD -*- lexical-binding: t; -*-

;; Author: inaimathi, Claude Sonnet
;; Version: 0.5.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: cad, openscad, svg, tools

;;; Commentary:

;; Top-level entry point and minor mode for scad-sketch.
;;
;; In any `.scad' buffer, enable `scad-sketch-mode' and use:
;;
;;   C-c C-.  -> `scad-sketch-or-insert-at-point'
;;   C-c C-a  -> `scad-sketch-at-point'
;;
;; The parser/session layer lives in `scad-sketch-session.el'.  The editor
;; major mode lives in `scad-sketch-editor-mode.el'.  Pure geometry helpers
;; live in `scad-sketch-geometry.el'.

;;; Code:

(require 'scad-sketch-session)
(require 'scad-sketch-editor-mode)

(defvar scad-sketch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'scad-sketch-at-point)
    (define-key map (kbd "C-c C-.") #'scad-sketch-or-insert-at-point)
    map)
  "Keymap for `scad-sketch-mode'.
C-c C-s and C-c C-o are intentionally left free for `scad-mode'.")

;;;###autoload
(define-minor-mode scad-sketch-mode
  "Minor mode for opening scad-sketch blocks from OpenSCAD buffers."
  :lighter " Sketch"
  :keymap scad-sketch-mode-map)

(defun scad-sketch--ensure-svg ()
  "Signal unless SVG image support is available."
  (unless (image-type-available-p 'svg)
    (user-error "This Emacs was not built with SVG image support")))

;;;###autoload
(defun scad-sketch-at-point ()
  "Open the sketch editor for the array at point."
  (interactive)
  (scad-sketch--ensure-svg)
  (scad-sketch--open-session (scad-sketch-session-at-point)))

;;;###autoload
(defun scad-sketch-insert-array-at-point (name)
  "Insert a new empty named array at point and open the sketch editor."
  (interactive "sArray name: ")
  (scad-sketch--ensure-svg)
  (scad-sketch--open-session (scad-sketch-session-insert-array-at-point name)))

;;;###autoload
(defun scad-sketch-or-insert-at-point ()
  "Edit the array at point, or insert and open a new one if none is found."
  (interactive)
  (scad-sketch--ensure-svg)
  (condition-case nil
      (scad-sketch-at-point)
    (user-error
     (call-interactively #'scad-sketch-insert-array-at-point))))

(provide 'scad-sketch)
;;; scad-sketch.el ends here
