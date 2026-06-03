;;; scad-sketch-session.el --- Session construction for scad-sketch -*- lexical-binding: t; -*-

;;; Commentary:

;; Session and source-buffer discovery layer for scad-sketch.
;;
;; This file intentionally still uses the pre-parser array-assignment scanner.
;; The parser-backed target/region resolver belongs here next, but this split
;; preserves the existing behavior first.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup scad-sketch nil
  "Keyboard sketch editor for OpenSCAD array literals."
  :group 'tools
  :prefix "scad-sketch-")

(defcustom scad-sketch-default-grid 1.0
  "Default grid step in sketch units."
  :type 'number :group 'scad-sketch)

(defcustom scad-sketch-default-fine-step 0.1
  "Default fine movement step in sketch units."
  :type 'number :group 'scad-sketch)

(defcustom scad-sketch-default-coarse-step 5.0
  "Default coarse movement step in sketch units."
  :type 'number :group 'scad-sketch)



(cl-defstruct scad-sketch-session
  name units grid fine-step coarse-step closed
  points point
  marks          ; list of [x y], newest first; (car marks) is the current mark
  named-marks selected-index
  source-buffer content-beg content-end
  dirty undo-stack)

(defvar-local scad-sketch--session nil)

(defun scad-sketch--assert-session ()
  "Return the current sketch session or signal an error."
  (unless (and (boundp 'scad-sketch--session) scad-sketch--session)
    (user-error "No active scad-sketch session"))
  scad-sketch--session)

;;; Array-assignment finding

(defconst scad-sketch--number-re
  "[-+]?[0-9]*\\.?[0-9]+\\(?:[eE][-+]?[0-9]+\\)?")

(defun scad-sketch--strip-line-comments (s)
  "Remove // line comments from each line of S."
  (mapconcat (lambda (line)
               (if (string-match "//" line)
                   (substring line 0 (match-beginning 0))
                 line))
             (split-string s "\n") "\n"))

(defun scad-sketch--forward-balanced-bracket (pos)
  "Return position just after the `[...]' form starting at POS."
  (save-excursion
    (goto-char pos)
    (unless (= (char-after) ?\[)
      (user-error "Expected `[' at array start"))
    (let ((depth 0) done in-string escape line-comment block-comment)
      (while (and (not done) (< (point) (point-max)))
        (let ((ch   (char-after))
              (next (char-after (1+ (point)))))
          (cond
           (line-comment  (when (= ch ?\n) (setq line-comment nil)) (forward-char 1))
           (block-comment (if (and next (= ch ?*) (= next ?/))
                              (progn (setq block-comment nil) (forward-char 2))
                            (forward-char 1)))
           (in-string (cond (escape (setq escape nil) (forward-char 1))
                            ((= ch ?\\) (setq escape t) (forward-char 1))
                            ((= ch ?\") (setq in-string nil) (forward-char 1))
                            (t (forward-char 1))))
           ((and next (= ch ?/) (= next ?/)) (setq line-comment t)  (forward-char 2))
           ((and next (= ch ?/) (= next ?*)) (setq block-comment t) (forward-char 2))
           ((= ch ?\") (setq in-string t) (forward-char 1))
           ((= ch ?\[) (setq depth (1+ depth)) (forward-char 1))
           ((= ch ?\]) (setq depth (1- depth)) (forward-char 1)
            (when (= depth 0) (setq done t)))
           (t (forward-char 1)))))
      (unless done (user-error "Could not find the end of this array literal"))
      (point))))

(defun scad-sketch--find-array-assignment-at-point ()
  "Find a literal SCAD array assignment at or surrounding point.
Returns plist (:name :beg :end :text)."
  (let ((origin (point)) name beg open end)
    (save-excursion
      (goto-char (line-end-position))
      (unless (re-search-backward
               (rx (group (+ (any "A-Za-z0-9_$"))) (* space) "=" (* space) "[")
               nil t)
        (user-error "No literal array assignment before point"))
      (setq name (match-string-no-properties 1)
            beg  (match-beginning 0)
            open (1- (match-end 0))
            end  (scad-sketch--forward-balanced-bracket open))
      (goto-char end)
      (skip-chars-forward " \t\r\n")
      (unless (= (char-after) ?\;)
        (user-error "Array assignment must end with a semicolon"))
      (forward-char 1)
      (setq end (point))
      (unless (<= origin end)
        (user-error "Point is not inside the nearest literal array assignment"))
      (list :name name :beg beg :end end
            :text (buffer-substring-no-properties beg end)))))

;;; Point parsing — always produces [x y r] triples

(defun scad-sketch--parse-points (text)
  "Parse all [x, y] or [x, y, r] literals from TEXT.
Always returns a list of [x y r] triples; r defaults to 0."
  (let* ((clean (scad-sketch--strip-line-comments text))
         (n     scad-sketch--number-re)
         (re    (concat "\\[\\s-*\\(" n "\\)\\s-*,\\s-*\\(" n "\\)"
                        "\\(?:\\s-*,\\s-*\\(" n "\\)\\)?" "\\s-*\\]"))
         points)
    (with-temp-buffer
      (insert clean)
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (push (list (string-to-number (match-string 1))
                    (string-to-number (match-string 2))
                    (if (match-string 3) (string-to-number (match-string 3)) 0))
              points)))
    (nreverse points)))

;;; Session construction

(defun scad-sketch--make-session (name points beg-marker end-marker)
  "Create a session for array NAME with POINTS between BEG-MARKER and END-MARKER."
  (let ((init-pt (if points
                     (list (float (nth 0 (car points)))
                           (float (nth 1 (car points))))
                   (list 0.0 0.0))))
    (make-scad-sketch-session
     :name name
     :units "mm"
     :grid (float scad-sketch-default-grid)
     :fine-step (float scad-sketch-default-fine-step)
     :coarse-step (float scad-sketch-default-coarse-step)
     :closed t
     :points points
     :point init-pt
     :marks nil
     :named-marks nil
     :selected-index (if points 0 nil)
     :source-buffer (current-buffer)
     :content-beg beg-marker
     :content-end end-marker
     :dirty nil
     :undo-stack nil)))


(defun scad-sketch-session-at-point ()
  "Build a sketch session for the literal array assignment at point.
This is the current non-parser implementation.  The parser-backed resolver
should replace the internals of this function while keeping the return value
as a `scad-sketch-session'."
  (let* ((assignment (scad-sketch--find-array-assignment-at-point))
         (name   (plist-get assignment :name))
         (points (scad-sketch--parse-points (plist-get assignment :text)))
         (beg    (copy-marker (plist-get assignment :beg)))
         (end    (copy-marker (plist-get assignment :end) t)))
    (scad-sketch--make-session name points beg end)))

(defun scad-sketch-session-insert-array-at-point (name)
  "Insert a new empty array named NAME at point and return its session."
  (let (beg end)
    (setq beg (point-marker))
    (insert (format "%s = [
];
" name))
    (setq end (copy-marker (point) t))
    (scad-sketch--make-session name nil beg end)))

(provide 'scad-sketch-session)
;;; scad-sketch-session.el ends here
