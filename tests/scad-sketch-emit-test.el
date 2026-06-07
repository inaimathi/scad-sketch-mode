;;; scad-sketch-emit-test.el --- ERT tests for source emission formatting -*- lexical-binding: t; -*-

;;; Commentary:

;; Run all tests with:
;;
;;   bash unittest.sh

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(defvar scad-sketch-emit-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

(defvar scad-sketch-emit-test--root
  (expand-file-name ".." scad-sketch-emit-test--dir)
  "Repository root directory.")

(add-to-list 'load-path scad-sketch-emit-test--root)

(defun semit-test--load (file feature)
  "Load FILE from the repository root unless FEATURE is already provided."
  (unless (featurep feature)
    (load-file (expand-file-name file scad-sketch-emit-test--root))))

(semit-test--load "scad-sketch-parse.el"          'scad-sketch-parse)
(semit-test--load "scad-sketch-geometry.el"       'scad-sketch-geometry)
(semit-test--load "scad-sketch-session.el"        'scad-sketch-session)
(semit-test--load "scad-sketch-session--emit.el"  'scad-sketch-session--emit)

(defun semit-test--goto-substring (needle &optional offset)
  "Move point to NEEDLE's beginning plus OFFSET."
  (goto-char (point-min))
  (unless (search-forward needle nil t)
    (error "Could not find test substring: %S" needle))
  (goto-char (+ (match-beginning 0) (or offset 0))))

(defmacro semit-test--with-session (source needle &rest body)
  "Create a session from SOURCE at NEEDLE and run BODY with `session' bound."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,source)
     (semit-test--goto-substring ,needle)
     (let ((session (scad-sketch-session-at-point)))
       ,@body)))

(defun semit-test--write-back-string (session)
  "Write SESSION back and return source contents."
  (scad-sketch-session-write-back session)
  (with-current-buffer (scad-sketch-session-source-buffer session)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun semit-test--set-active-points (session points)
  "Set SESSION active polygon POINTS consistently."
  (setf (scad-sketch-session-points session) (copy-tree points))
  (let ((shape (or (scad-sketch-session-active-shape session)
                   (car (scad-sketch-session-shapes session)))))
    (setf (scad-sketch-shape-points shape) (copy-tree points)))
  session)

(defun semit-test--active-shape (session)
  "Return active shape in SESSION."
  (or (scad-sketch-session-active-shape session)
      (car (scad-sketch-session-shapes session))))

(defun semit-test--assert-contains (needle haystack)
  "Assert HAYSTACK contains literal NEEDLE."
  (should (string-match-p (regexp-quote needle) haystack)))

(defun semit-test--assert-not-contains (needle haystack)
  "Assert HAYSTACK does not contain literal NEEDLE."
  (should-not (string-match-p (regexp-quote needle) haystack)))

(ert-deftest semit-inline-array-is-lisp-style ()
  "Point arrays emit Lisp-style with closing bracket on the last element line."
  (should
   (string=
    (scad-sketch-session--fmt-points-array-lisp
     '((0 0 0) (40 0 0) (50 20 0))
     "pts = "
     nil
     ";")
    "pts = [[0, 0],
       [40, 0],
       [50, 20]];")))

(ert-deftest semit-inline-polygon-preview-is-lisp-style ()
  "Inline polygon preview uses Lisp-style multiline point arrays."
  (semit-test--with-session
      "polygon([[0,0], [30,0], [15,26]]);\n"
      "polygon"
    (let ((preview (scad-sketch-session-preview session)))
      (should
       (string=
        preview
        "polygon([[0, 0],
         [30, 0],
         [15, 26]]);")))))

(ert-deftest semit-polyround-preview-is-lisp-style ()
  "Inline polyRound preview uses Lisp-style multiline point arrays."
  (semit-test--with-session
      (concat "polygon([[0,0], [40,0], [0,40]]);\n")
      "polygon"
    (semit-test--set-active-points
     session
     '((0.0 0.0 0.0)
       (40.0 0.0 10.0)
       (0.0 40.0 0.0)))
    (let ((preview (scad-sketch-session-preview session)))
      (should
       (string=
        preview
        "polygon(polyRound([[0, 0, 0],
                   [40, 0, 10],
                   [0, 40, 0]], 16));")))))

(ert-deftest semit-writeback-does-not-add-extra-newline ()
  "Write-back replacement should not create extra blank lines around target."
  (semit-test--with-session
      (concat "// before\n"
              "polygon([[0,0], [30,0], [15,26]]);\n"
              "// after\n")
      "polygon"
    (semit-test--set-active-points
     session
     '((1.0 2.0 0.0)
       (31.0 2.0 0.0)
       (16.0 28.0 0.0)))
    (let ((out (semit-test--write-back-string session)))
      (should
       (string=
        out
        (concat "// before\n"
                "polygon([[1, 2],\n"
                "         [31, 2],\n"
                "         [16, 28]]);\n"
                "// after\n"))))))

(ert-deftest semit-writeback-preserves-target-indentation ()
  "Root replacement is emitted at the target's existing indentation."
  (semit-test--with-session
      (concat "module foo() {\n"
              "  polygon([[0,0], [30,0], [15,26]]);\n"
              "}\n")
      "polygon"
    (semit-test--set-active-points
     session
     '((1.0 2.0 0.0)
       (31.0 2.0 0.0)
       (16.0 28.0 0.0)))
    (let ((out (semit-test--write-back-string session)))
      (should
       (string=
        out
        (concat "module foo() {\n"
                "  polygon([[1, 2],\n"
                "           [31, 2],\n"
                "           [16, 28]]);\n"
                "}\n"))))))

(ert-deftest semit-extracted-points-assignment-is-lisp-style ()
  "Extracted point arrays use Lisp-style assignment indentation."
  (semit-test--with-session
      "polygon([[0,0], [30,0], [15,26]]);\n"
      "polygon"
    (let ((shape (semit-test--active-shape session)))
      (scad-sketch-session-toggle-polygon-extraction
       session shape "pts")
      (let ((preview (scad-sketch-session-preview session)))
        (should
         (string=
          preview
          (concat "pts = [[0, 0],\n"
                  "       [30, 0],\n"
                  "       [15, 26]];\n"
                  "polygon(pts);")))))))

(ert-deftest semit-extracted-rounded-points-assignment-is-lisp-style ()
  "Extracted rounded point arrays preserve radii and emit polyRound source."
  (semit-test--with-session
      "polygon([[0,0], [30,0], [15,26]]);\n"
      "polygon"
    (semit-test--set-active-points
     session
     '((0.0 0.0 0.0)
       (30.0 0.0 4.0)
       (15.0 26.0 0.0)))
    (let ((shape (semit-test--active-shape session)))
      (scad-sketch-session-toggle-polygon-extraction
       session shape "pts")
      (let ((preview (scad-sketch-session-preview session)))
        (should
         (string=
          preview
          (concat "pts = [[0, 0, 0],\n"
                  "       [30, 0, 4],\n"
                  "       [15, 26, 0]];\n"
                  "polygon(polyRound(pts, 16));")))))))

(provide 'scad-sketch-emit-test)
;;; scad-sketch-emit-test.el ends here
