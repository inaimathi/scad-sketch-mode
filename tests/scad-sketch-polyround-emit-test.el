;;; scad-sketch-polyround-emit-test.el --- ERT tests for automatic polyRound emission -*- lexical-binding: t; -*-

;;; Commentary:

;; Run from the repository root with:
;;
;;   emacs --batch -Q \
;;     --load tests/scad-sketch-polyround-emit-test.el \
;;     --eval "(ert-run-tests-batch-and-exit)"
;;
;; Or run all tests with:
;;
;;   bash unittest.sh

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(defvar scad-sketch-polyround-emit-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

(defvar scad-sketch-polyround-emit-test--root
  (expand-file-name ".." scad-sketch-polyround-emit-test--dir)
  "Repository root directory.")

(add-to-list 'load-path scad-sketch-polyround-emit-test--root)

(unless (featurep 'scad-sketch-parse)
  (load-file (expand-file-name "scad-sketch-parse.el"
                               scad-sketch-polyround-emit-test--root)))

(unless (featurep 'scad-sketch-session)
  (load-file (expand-file-name "scad-sketch-session.el"
                               scad-sketch-polyround-emit-test--root)))

(defun spround-test--goto-substring (needle &optional offset)
  "Move point to NEEDLE's beginning plus OFFSET."
  (goto-char (point-min))
  (unless (search-forward needle nil t)
    (error "Could not find test substring: %S" needle))
  (goto-char (+ (match-beginning 0) (or offset 0))))

(defmacro spround-test--with-session (source needle &rest body)
  "Create a session from SOURCE at NEEDLE and run BODY with `session' bound."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,source)
     (spround-test--goto-substring ,needle)
     (let ((session (scad-sketch-session-at-point)))
       ,@body)))

(defun spround-test--set-active-points (session points)
  "Set SESSION active polygon POINTS consistently."
  (setf (scad-sketch-session-points session) (copy-tree points))
  (let ((shape (spround-test--active-shape session)))
    (setf (scad-sketch-shape-points shape) (copy-tree points)))
  session)

(defun spround-test--active-shape (session)
  "Return SESSION's active shape."
  (or (scad-sketch-session-active-shape session)
      (car (scad-sketch-session-shapes session))))

(defun spround-test--write-back-string (session)
  "Write SESSION back and return the source buffer contents."
  (scad-sketch-session-write-back session)
  (with-current-buffer (scad-sketch-session-source-buffer session)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun spround-test--assert-contains (needle haystack)
  "Assert HAYSTACK contains literal NEEDLE."
  (should (string-match-p (regexp-quote needle) haystack)))

(defun spround-test--assert-not-contains (needle haystack)
  "Assert HAYSTACK does not contain literal NEEDLE."
  (should-not (string-match-p (regexp-quote needle) haystack)))


;;;; =========================================================================
;;;; Parser/unparser behavior
;;;; =========================================================================

(ert-deftest spround-unparse-inline-polygon-with-radius-emits-polyround-16 ()
  "Parser unparse emits polyRound(..., 16) for positive point radii."
  (let* ((node '(:type polygon
                 :points ((0.0 0.0 0.0)
                          (40.0 0.0 5.0)
                          (0.0 40.0 0.0))))
         (out  (scad-sketch-unparse node)))
    (spround-test--assert-contains
     "polygon(polyRound([[0, 0, 0], [40, 0, 5], [0, 40, 0]], 16));"
     out)
    (spround-test--assert-not-contains "polygon([[0, 0, 0]" out)))

(ert-deftest spround-unparse-inline-polygon-all-zero-radii-stays-plain-2d ()
  "All-zero radii do not trigger polyRound emission."
  (let* ((node '(:type polygon
                 :points ((0.0 0.0 0.0)
                          (40.0 0.0 0.0)
                          (0.0 40.0 0.0))))
         (out  (scad-sketch-unparse node)))
    (spround-test--assert-contains
     "polygon([[0, 0], [40, 0], [0, 40]]);"
     out)
    (spround-test--assert-not-contains "polyRound" out)))

(ert-deftest spround-unparse-varref-polygon-with-radius-emits-polyround-16 ()
  "Variable-ref polygon with rounded resolved points emits polyRound(source, 16)."
  (let* ((node '(:type polygon
                 :source "points"
                 :points ((0.0 0.0 0.0)
                          (40.0 0.0 5.0)
                          (0.0 40.0 0.0))))
         (out  (scad-sketch-unparse node)))
    (spround-test--assert-contains
     "polygon(polyRound(points, 16));"
     out)))


;;;; =========================================================================
;;;; Session/write-back behavior
;;;; =========================================================================
(ert-deftest spround-session-inline-polygon-radius-writeback-emits-polyround-16 ()
  "Editing an inline polygon point radius emits inline polyRound(..., 16)."
  (spround-test--with-session
      "polygon([[0,0], [40,0], [0,40]]);\n"
      "polygon"
    (spround-test--set-active-points
     session
     '((0.0 0.0 0.0)
       (40.0 0.0 10.0)
       (0.0 40.0 0.0)))
    (let ((out (spround-test--write-back-string session)))
      (spround-test--assert-contains
       "polygon(polyRound([[0, 0, 0],"
       out)
      (spround-test--assert-contains
       "[40, 0, 10],"
       out)
      (spround-test--assert-contains
       "[0, 40, 0]], 16));"
       out)
      (spround-test--assert-not-contains
       "polygon([[0, 0, 0]"
       out))))

(ert-deftest spround-session-inline-polygon-zero-radii-writeback-stays-plain ()
  "All-zero radii still emit a plain 2D polygon."
  (spround-test--with-session
      "polygon([[0,0], [40,0], [0,40]]);\n"
      "polygon"
    (spround-test--set-active-points
     session
     '((0.0 0.0 0.0)
       (40.0 0.0 0.0)
       (0.0 40.0 0.0)))
    (let ((out (spround-test--write-back-string session)))
      (spround-test--assert-contains
       "polygon([[0, 0],"
       out)
      (spround-test--assert-contains
       "[40, 0],"
       out)
      (spround-test--assert-contains
       "[0, 40]]);"
       out)
      (spround-test--assert-not-contains "polyRound" out))))

(ert-deftest spround-session-generic-block-radius-polygon-emits-polyround-16 ()
  "A newly drawn/generic polygon with radii emits polyRound(..., 16)."
  (with-temp-buffer
    (insert "// slot\n")
    (goto-char (point-max))
    (let ((session (scad-sketch-session-insert-block-at-point)))
      (scad-sketch-session-add-shape
       session
       '((4.0 11.0 10.0)
         (40.0 0.0 0.0)
         (50.0 20.0 2.0)))
      (let ((out (spround-test--write-back-string session)))
        (spround-test--assert-contains
         "polygon(polyRound([[4, 11, 10],"
         out)
        (spround-test--assert-contains
         "[40, 0, 0],"
         out)
        (spround-test--assert-contains
         "[50, 20, 2]], 16));"
         out)
        (spround-test--assert-not-contains
         "polygon([[4, 11, 10]"
         out)))))

(ert-deftest spround-session-extracted-rounded-polygon-emits-polyround-source-16 ()
  "Extracted rounded polygon emits NAME assignment and polygon(polyRound(NAME, 16))."
  (spround-test--with-session
      "polygon([[0,0], [40,0], [0,40]]);\n"
      "polygon"
    (spround-test--set-active-points
     session
     '((0.0 0.0 0.0)
       (40.0 0.0 10.0)
       (0.0 40.0 0.0)))
    (let ((shape (spround-test--active-shape session)))
      (scad-sketch-session-toggle-polygon-extraction
       session shape "pts")
      (let ((out (spround-test--write-back-string session)))
        (spround-test--assert-contains "pts =" out)
        (spround-test--assert-contains "[40, 0, 10]" out)
        (spround-test--assert-contains
         "polygon(polyRound(pts, 16));"
         out)
        (spround-test--assert-not-contains "polygon(pts);" out)))))

(ert-deftest spround-session-varref-all-zero-radii-stays-plain-source ()
  "polygon(points) stays plain when the referenced points have no positive radii."
  (spround-test--with-session
      (concat "points = [[0, 0, 0], [40, 0, 0], [50, 20, 0]];\n"
              "polygon(points);\n")
      "polygon"
    (let ((out (spround-test--write-back-string session)))
      (spround-test--assert-contains "polygon(points);" out)
      (spround-test--assert-not-contains "polyRound(points" out))))

(provide 'scad-sketch-polyround-emit-test)
;;; scad-sketch-polyround-emit-test.el ends here
