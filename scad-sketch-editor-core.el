;;; scad-sketch-editor-core.el --- Editor dispatch and undo infrastructure -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared infrastructure used by every editor subsystem.  Nothing in this
;; file should depend on navigation, editing, selection, or rendering — it
;; sits below all of them and is required by each.
;;
;; Responsibilities:
;;   - `scad-sketch--assert-session'     session guard
;;   - `scad-sketch--change' / `--edit' / `--clean-change'
;;                                        unified render-dispatch triad
;;   - `scad-sketch--push-undo' / `--mark-dirty'
;;                                        undo stack primitives
;;   - `scad-sketch--open-session'        editor buffer lifecycle
;;
;; `scad-sketch--render' is declared as a forward reference here and
;; defined in `scad-sketch-editor--rendering'.  Core calls it by name
;; so the subsystem load order is: core → {selection,navigation,editing}
;; → rendering → mode (top-level).

;;; Code:

(require 'scad-sketch-session)
(require 'scad-sketch-editor--selection)   ; for --normalize-attention

(defvar scad-sketch--editor-buffer-prefix "*scad-sketch: ")

;;; Session guard
(defun scad-sketch--assert-session ()
  "Return the buffer-local session or signal an error."
  (unless (boundp 'scad-sketch--session)
    (error "No scad-sketch session in this buffer"))
  (or scad-sketch--session
      (error "No scad-sketch session in this buffer")))

;;; Change dispatch

(defun scad-sketch--change (fn &optional source-mutation-p)
  "Call FN with session, normalize attention, then re-render.

When SOURCE-MUTATION-P is non-nil, push undo and mark the session dirty
before calling FN.  Cursor movement, mark changes, hover, focus, and
selection changes are clean; edits to source geometry are dirty."
  (let ((session (scad-sketch--assert-session)))
    (when source-mutation-p
      (scad-sketch--push-undo session))
    (funcall fn session)
    (scad-sketch--normalize-attention session)
    (when source-mutation-p
      (scad-sketch--mark-dirty session))
    ;; Forward reference: defined in scad-sketch-editor--rendering.
    (scad-sketch--render)))

(defun scad-sketch--edit (fn)
  "Apply FN as a source-geometry mutation (dirty, undo-able)."
  (scad-sketch--change fn t))

(defun scad-sketch--undoable-clean-change (fn)
  "Apply FN as a clean but undoable UI/session change.

This is for editor-state changes that are not source-geometry mutations but
should still be recoverable with `u', such as adding, popping, or clearing
marks.  The session dirty flag is preserved exactly."
  (let* ((session   (scad-sketch--assert-session))
         (was-dirty (scad-sketch-session-dirty session)))
    (scad-sketch--push-undo session)
    (funcall fn session)
    (scad-sketch--normalize-attention session)
    (setf (scad-sketch-session-dirty session) was-dirty)
    (scad-sketch--render)))

(defun scad-sketch--clean-change (fn)
  "Apply FN as a clean UI/session change (no undo, no dirty flag)."
  (scad-sketch--change fn nil))

;; Backwards-compatible alias kept for callers written before --edit existed.
(defun scad-sketch--mutate (fn)
  "Apply FN as a source-geometry mutation.
Prefer `scad-sketch--edit'; this alias is retained for compatibility."
  (scad-sketch--edit fn))

;;; Editor buffer lifecycle

(defvar-local scad-sketch--session nil
  "The `scad-sketch-session' associated with the current editor buffer.")

(defvar-local scad-sketch--window-config nil
  "Window configuration recorded just before the editor buffer was opened.")

(defun scad-sketch--open-session (session)
  "Open an editor buffer for SESSION, saving the current window configuration."
  (let ((wconf (current-window-configuration))
        (buf   (get-buffer-create
                (format "%s%s*" scad-sketch--editor-buffer-prefix
                        (scad-sketch-session-name session)))))
    (with-current-buffer buf
      (scad-sketch-editor-mode)          ; defined in scad-sketch-editor-mode.el
      (setq-local scad-sketch--session session)
      (setq-local scad-sketch--window-config wconf))
    (pop-to-buffer buf)
    ;; Render after display so canvas sizing can use the actual editor window.
    (with-current-buffer buf
      (scad-sketch--render))))

(provide 'scad-sketch-editor-core)
;;; scad-sketch-editor-core.el ends here
