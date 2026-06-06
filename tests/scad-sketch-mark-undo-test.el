;;; scad-sketch-mark-undo-test.el --- ERT tests for mark undo behavior -*- lexical-binding: t; -*-

;;; Commentary:

;; Run from the repository root with:
;;
;;   emacs --batch -Q \
;;     --load tests/scad-sketch-mark-undo-test.el \
;;     --eval "(ert-run-tests-batch-and-exit)"
;;
;; Or run all tests with:
;;
;;   bash unittest.sh

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar scad-sketch-mark-undo-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

(defvar scad-sketch-mark-undo-test--root
  (expand-file-name ".." scad-sketch-mark-undo-test--dir)
  "Repository root directory.")

(add-to-list 'load-path scad-sketch-mark-undo-test--root)

(defun smark-test--load (file feature)
  "Load FILE from the repository root unless FEATURE is already provided."
  (unless (featurep feature)
    (load-file (expand-file-name file scad-sketch-mark-undo-test--root))))

(smark-test--load "scad-sketch-parse.el"              'scad-sketch-parse)
(smark-test--load "scad-sketch-geometry.el"           'scad-sketch-geometry)
(smark-test--load "scad-sketch-session.el"            'scad-sketch-session)
(smark-test--load "scad-sketch-editor--refs.el"       'scad-sketch-editor--refs)
(smark-test--load "scad-sketch-editor--selection.el"  'scad-sketch-editor--selection)
(smark-test--load "scad-sketch-editor-core.el"        'scad-sketch-editor-core)
(smark-test--load "scad-sketch-editor--cursor.el"     'scad-sketch-editor--cursor)
(smark-test--load "scad-sketch-editor--editing.el"    'scad-sketch-editor--editing)

;; These tests exercise command dispatch, not SVG rendering.
(unless (fboundp 'scad-sketch--render)
  (defun scad-sketch--render ()
    "No-op render stub for mark undo tests."
    nil))

(defun smark-test--goto-substring (needle &optional offset)
  "Move point to NEEDLE's beginning plus OFFSET."
  (goto-char (point-min))
  (unless (search-forward needle nil t)
    (error "Could not find test substring: %S" needle))
  (goto-char (+ (match-beginning 0) (or offset 0))))

(defmacro smark-test--with-editor-session (&rest body)
  "Create a source session and run BODY in a fake editor buffer.

Within BODY, `session' is bound and `scad-sketch--session' is buffer-local."
  (declare (indent 0))
  `(with-temp-buffer
     (insert "polygon([[0,0], [30,0], [15,26]]);\n")
     (smark-test--goto-substring "polygon")
     (let ((session (scad-sketch-session-at-point)))
       (with-temp-buffer
         (setq-local scad-sketch--session session)
         ,@body))))

(defun smark-test--set-point (session point)
  "Set SESSION cursor POINT."
  (setf (scad-sketch-session-point session) (copy-sequence point))
  session)

(defun smark-test--undo-stack-length (session)
  "Return SESSION undo stack length."
  (length (scad-sketch-session-undo-stack session)))


;;;; =========================================================================
;;;; Mark commands are undoable but clean
;;;; =========================================================================

(ert-deftest smark-set-mark-is-undoable-clean ()
  "Replacing marks with `m' can be undone and does not dirty the session."
  (smark-test--with-editor-session
    (smark-test--set-point session '(1.0 2.0))
    (let ((before (smark-test--undo-stack-length session)))
      (scad-sketch-set-mark)
      (should (equal (scad-sketch-session-marks session)
                     '((1.0 2.0))))
      (should (= (1+ before)
                 (smark-test--undo-stack-length session)))
      (should-not (scad-sketch-session-dirty session))

      (scad-sketch-undo)
      (should (null (scad-sketch-session-marks session)))
      (should-not (scad-sketch-session-dirty session)))))

(ert-deftest smark-push-mark-is-undoable-clean ()
  "Pushing a mark with `M' can be undone and does not dirty the session."
  (smark-test--with-editor-session
    (setf (scad-sketch-session-marks session) '((0.0 0.0)))
    (smark-test--set-point session '(3.0 4.0))
    (scad-sketch-push-mark)
    (should (equal (scad-sketch-session-marks session)
                   '((3.0 4.0) (0.0 0.0))))
    (should-not (scad-sketch-session-dirty session))

    (scad-sketch-undo)
    (should (equal (scad-sketch-session-marks session)
                   '((0.0 0.0))))
    (should-not (scad-sketch-session-dirty session))))

(ert-deftest smark-pop-mark-is-undoable-clean ()
  "Popping a mark with backtick can be undone and does not dirty the session."
  (smark-test--with-editor-session
    (setf (scad-sketch-session-marks session)
          '((1.0 1.0) (0.0 0.0)))
    (smark-test--set-point session '(9.0 9.0))

    (scad-sketch-pop-mark)
    (should (equal (scad-sketch-session-point session)
                   '(1.0 1.0)))
    (should (equal (scad-sketch-session-marks session)
                   '((0.0 0.0))))
    (should-not (scad-sketch-session-dirty session))

    (scad-sketch-undo)
    (should (equal (scad-sketch-session-point session)
                   '(9.0 9.0)))
    (should (equal (scad-sketch-session-marks session)
                   '((1.0 1.0) (0.0 0.0))))
    (should-not (scad-sketch-session-dirty session))))

(ert-deftest smark-clear-marks-is-undoable-clean ()
  "Clearing marks with `C' can be undone and does not dirty the session."
  (smark-test--with-editor-session
    (setf (scad-sketch-session-marks session)
          '((2.0 2.0) (1.0 1.0)))
    (scad-sketch-clear-marks)
    (should (null (scad-sketch-session-marks session)))
    (should-not (scad-sketch-session-dirty session))

    (scad-sketch-undo)
    (should (equal (scad-sketch-session-marks session)
                   '((2.0 2.0) (1.0 1.0))))
    (should-not (scad-sketch-session-dirty session))))

(ert-deftest smark-clear-empty-marks-does-not-push-undo ()
  "Clearing marks when none exist should not clutter undo."
  (smark-test--with-editor-session
    (setf (scad-sketch-session-marks session) nil)
    (let ((before (smark-test--undo-stack-length session)))
      (scad-sketch-clear-marks)
      (should (= before (smark-test--undo-stack-length session)))
      (should-not (scad-sketch-session-dirty session)))))

(ert-deftest smark-jump-to-mark-is-not-undoable ()
  "Jumping to a mark does not mutate marks and should stay non-undoable."
  (smark-test--with-editor-session
    (setf (scad-sketch-session-marks session) '((7.0 8.0)))
    (smark-test--set-point session '(1.0 2.0))
    (let ((before (smark-test--undo-stack-length session)))
      (scad-sketch-jump-to-mark)
      (should (equal (scad-sketch-session-point session)
                     '(7.0 8.0)))
      (should (= before (smark-test--undo-stack-length session))))))

(ert-deftest smark-clear-transient-state-clears-marks-undoably ()
  "Esc-style transient clear can restore accidentally cleared marks."
  (smark-test--with-editor-session
    (setf (scad-sketch-session-marks session)
          '((5.0 5.0) (1.0 1.0)))
    (setf (scad-sketch-session-selection session)
          (list (scad-sketch--shape-ref
                 (scad-sketch-shape-id
                  (car (scad-sketch-session-shapes session))))))
    (scad-sketch-clear-transient-state)
    (should (null (scad-sketch-session-marks session)))
    (should (null (scad-sketch-session-selection session)))
    (should-not (scad-sketch-session-dirty session))

    (scad-sketch-undo)
    (should (equal (scad-sketch-session-marks session)
                   '((5.0 5.0) (1.0 1.0))))
    ;; Since the undo snapshot includes selection too, Esc is fully recoverable
    ;; when marks were present.
    (should (scad-sketch-session-selection session))
    (should-not (scad-sketch-session-dirty session))))

(ert-deftest smark-push-mark-preserves-existing-dirty-state ()
  "Undoable clean mark changes preserve an already-dirty session."
  (smark-test--with-editor-session
    (setf (scad-sketch-session-dirty session) t)
    (setf (scad-sketch-session-marks session) '((0.0 0.0)))
    (smark-test--set-point session '(3.0 4.0))

    (scad-sketch-push-mark)
    (should (equal (scad-sketch-session-marks session)
                   '((3.0 4.0) (0.0 0.0))))
    (should (scad-sketch-session-dirty session))

    (scad-sketch-undo)
    (should (equal (scad-sketch-session-marks session)
                   '((0.0 0.0))))
    (should (scad-sketch-session-dirty session))))

(provide 'scad-sketch-mark-undo-test)
;;; scad-sketch-mark-undo-test.el ends here
