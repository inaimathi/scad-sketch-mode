;;; scad-sketch-insert-test.el --- ERT tests for polygon point insertion -*- lexical-binding: t; -*-

;;; Commentary:

;; Run all tests with:
;;
;;   bash unittest.sh

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar scad-sketch-insert-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

(defvar scad-sketch-insert-test--root
  (expand-file-name ".." scad-sketch-insert-test--dir)
  "Repository root directory.")

(add-to-list 'load-path scad-sketch-insert-test--root)

(defun sinsert-test--load (file feature)
  "Load FILE from repository root unless FEATURE is already provided."
  (unless (featurep feature)
    (load-file (expand-file-name file scad-sketch-insert-test--root))))

(sinsert-test--load "scad-sketch-parse.el"              'scad-sketch-parse)
(sinsert-test--load "scad-sketch-geometry.el"           'scad-sketch-geometry)
(sinsert-test--load "scad-sketch-session.el"            'scad-sketch-session)
(sinsert-test--load "scad-sketch-editor--refs.el"       'scad-sketch-editor--refs)
(sinsert-test--load "scad-sketch-editor--selection.el"  'scad-sketch-editor--selection)
(sinsert-test--load "scad-sketch-editor-core.el"        'scad-sketch-editor-core)
(sinsert-test--load "scad-sketch-editor--undo.el"       'scad-sketch-editor--undo)
(sinsert-test--load "scad-sketch-editor--cursor.el"     'scad-sketch-editor--cursor)
(sinsert-test--load "scad-sketch-editor--editing.el"    'scad-sketch-editor--editing)

(unless (fboundp 'scad-sketch--render)
  (defun scad-sketch--render ()
    "No-op render stub for insert tests."
    nil))

(defun sinsert-test--goto-substring (needle &optional offset)
  "Move point to NEEDLE's beginning plus OFFSET."
  (goto-char (point-min))
  (unless (search-forward needle nil t)
    (error "Could not find test substring: %S" needle))
  (goto-char (+ (match-beginning 0) (or offset 0))))

(defmacro sinsert-test--with-editor-session (source needle &rest body)
  "Create a session from SOURCE at NEEDLE and run BODY in fake editor buffer.

Within BODY, `session' is bound and `scad-sketch--session' is buffer-local.
Rendering is stubbed because these tests exercise insertion/editing behavior,
not SVG image display support."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,source)
     (sinsert-test--goto-substring ,needle)
     (let ((session (scad-sketch-session-at-point)))
       (with-temp-buffer
         (setq-local scad-sketch--session session)
         (cl-letf (((symbol-function 'scad-sketch--render)
                    (lambda () nil)))
           ,@body)))))

(defun sinsert-test--active-shape (session)
  "Return SESSION's active shape."
  (or (scad-sketch-session-active-shape session)
      (car (scad-sketch-session-shapes session))))

(defun sinsert-test--active-points (session)
  "Return active polygon points."
  (copy-tree (scad-sketch-shape-points
              (sinsert-test--active-shape session))))

(defun sinsert-test--select-point (session idx)
  "Select active polygon point IDX."
  (let* ((shape    (sinsert-test--active-shape session))
         (shape-id (scad-sketch-shape-id shape))
         (ref      (scad-sketch--point-ref idx shape-id)))
    (setf (scad-sketch-session-selection session) (list ref))
    (setf (scad-sketch-session-focus-ref session) ref)
    (setf (scad-sketch-session-selected-index session) idx)
    ref))

(defun sinsert-test--set-point (session point)
  "Set SESSION editor point."
  (setf (scad-sketch-session-point session) (copy-sequence point)))

(ert-deftest sinsert-inserts-after-selected-non-last-point ()
  "i i inserts after the explicitly selected point, not at the end."
  (sinsert-test--with-editor-session
      "polygon([[0,0], [10,0], [10,10], [0,10]]);\n"
      "polygon"
    (sinsert-test--select-point session 1)
    (sinsert-test--set-point session '(99.0 88.0))

    (scad-sketch-insert-point-after-selected)

    (should (equal (sinsert-test--active-points session)
                   '((0.0 0.0 0.0)
                     (10.0 0.0 0.0)
                     (99.0 88.0 0.0)
                     (10.0 10.0 0.0)
                     (0.0 10.0 0.0))))))

(ert-deftest sinsert-selection-moves-to-inserted-point ()
  "After insertion, the newly inserted point is selected."
  (sinsert-test--with-editor-session
      "polygon([[0,0], [10,0], [10,10], [0,10]]);\n"
      "polygon"
    (let* ((old-ref (sinsert-test--select-point session 1))
           (_       (sinsert-test--set-point session '(99.0 88.0))))
      (scad-sketch-insert-point-after-selected)
      (let ((selection (scad-sketch-session-selection session)))
        (should (= (length selection) 1))
        (should (eq (scad-sketch--ref-kind (car selection)) 'point))
        (should (eq (scad-sketch--ref-shape-id (car selection))
                    (scad-sketch--ref-shape-id old-ref)))
        (should (= (scad-sketch--ref-index (car selection)) 2))))))

(ert-deftest sinsert-inserts-marks-oldest-first-then-point-after-selection ()
  "i i inserts marks oldest-first, then point, after selected point."
  (sinsert-test--with-editor-session
      "polygon([[0,0], [10,0], [10,10], [0,10]]);\n"
      "polygon"
    (sinsert-test--select-point session 1)
    ;; Mark stack is newest-first.
    (setf (scad-sketch-session-marks session)
          '((2.0 2.0) (1.0 1.0)))
    (sinsert-test--set-point session '(3.0 3.0))

    (scad-sketch-insert-point-after-selected)

    (should (equal (sinsert-test--active-points session)
                   '((0.0 0.0 0.0)
                     (10.0 0.0 0.0)
                     (1.0 1.0 0.0)
                     (2.0 2.0 0.0)
                     (3.0 3.0 0.0)
                     (10.0 10.0 0.0)
                     (0.0 10.0 0.0))))))

(provide 'scad-sketch-insert-test)
;;; scad-sketch-insert-test.el ends here
