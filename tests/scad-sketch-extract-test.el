;;; scad-sketch-extract-test.el --- ERT tests for point extraction toggle -*- lexical-binding: t; -*-

;;; Commentary:

;; Run from the repository root with:
;;
;;   emacs --batch -Q \
;;     --load tests/scad-sketch-extract-test.el \
;;     --eval "(ert-run-tests-batch-and-exit)"
;;
;; Or run all tests with:
;;
;;   bash unittest.sh

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(defvar scad-sketch-extract-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

(defvar scad-sketch-extract-test--root
  (expand-file-name ".." scad-sketch-extract-test--dir)
  "Repository root directory.")

(add-to-list 'load-path scad-sketch-extract-test--root)

(unless (featurep 'scad-sketch-parse)
  (load-file (expand-file-name "scad-sketch-parse.el"
                               scad-sketch-extract-test--root)))

(unless (featurep 'scad-sketch-session)
  (load-file (expand-file-name "scad-sketch-session.el"
                               scad-sketch-extract-test--root)))

(defun sextract-test--goto-substring (needle &optional offset)
  "Move point to NEEDLE's beginning plus OFFSET."
  (goto-char (point-min))
  (unless (search-forward needle nil t)
    (error "Could not find test substring: %S" needle))
  (goto-char (+ (match-beginning 0) (or offset 0))))

(defmacro sextract-test--with-session (source needle &rest body)
  "Create a session from SOURCE at NEEDLE and run BODY with `session' bound."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,source)
     (sextract-test--goto-substring ,needle)
     (let ((session (scad-sketch-session-at-point)))
       ,@body)))

(defun sextract-test--active-shape (session)
  "Return SESSION's active shape."
  (or (scad-sketch-session-active-shape session)
      (car (scad-sketch-session-shapes session))))

(defun sextract-test--write-back-string (session)
  "Write SESSION back and return the source buffer contents."
  (scad-sketch-session-write-back session)
  (with-current-buffer (scad-sketch-session-source-buffer session)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun sextract-test--assert-contains (needle haystack)
  "Assert HAYSTACK contains literal NEEDLE."
  (should (string-match-p (regexp-quote needle) haystack)))

(defun sextract-test--assert-not-contains (needle haystack)
  "Assert HAYSTACK does not contain literal NEEDLE."
  (should-not (string-match-p (regexp-quote needle) haystack)))


;;;; =========================================================================
;;;; Inline polygon extraction / inlining
;;;; =========================================================================

(ert-deftest sextract-inline-polygon-toggle-extracts-to-default-name ()
  "Inline polygon can be toggled to emit through pts."
  (sextract-test--with-session
      "polygon([[0,0], [30,0], [15,26]]);\n"
      "polygon"
    (let ((shape (sextract-test--active-shape session)))
      (should (eq 'extracted
                  (scad-sketch-session-toggle-polygon-extraction
                   session shape "pts")))
      (let ((preview (scad-sketch-session-preview session)))
        (sextract-test--assert-contains "pts =" preview)
        (sextract-test--assert-contains "polygon(pts);" preview)
        (sextract-test--assert-not-contains "polygon([[" preview)))))

(ert-deftest sextract-inline-polygon-toggle-round-trips-to-inline ()
  "Extracted inline polygon can be toggled back inline."
  (sextract-test--with-session
      "polygon([[0,0], [30,0], [15,26]]);\n"
      "polygon"
    (let ((shape (sextract-test--active-shape session)))
      (scad-sketch-session-toggle-polygon-extraction session shape "pts")
      (should (eq 'inline
                  (scad-sketch-session-toggle-polygon-extraction
                   session shape)))
      (let ((preview (scad-sketch-session-preview session)))
        (sextract-test--assert-contains
         "polygon([[0, 0], [30, 0], [15, 26]]);"
         preview)
        (sextract-test--assert-not-contains "pts =" preview)
        (sextract-test--assert-not-contains "polygon(pts);" preview)))))

(ert-deftest sextract-inline-polygon-writeback-inserts-assignment-and-call ()
  "Extracting an inline polygon writes assignment plus polygon call."
  (sextract-test--with-session
      "// before\npolygon([[0,0], [30,0], [15,26]]);\n// after\n"
      "polygon"
    (let ((shape (sextract-test--active-shape session)))
      (scad-sketch-session-toggle-polygon-extraction session shape "pts")
      (let ((out (sextract-test--write-back-string session)))
        (sextract-test--assert-contains "// before" out)
        (sextract-test--assert-contains "pts =" out)
        (sextract-test--assert-contains "polygon(pts);" out)
        (sextract-test--assert-contains "// after" out)))))


;;;; =========================================================================
;;;; Custom names and polyRound
;;;; =========================================================================

(ert-deftest sextract-inline-polygon-uses-custom-variable-name ()
  "Extraction uses the requested variable name."
  (sextract-test--with-session
      "polygon([[0,0], [30,0], [15,26]]);\n"
      "polygon"
    (let ((shape (sextract-test--active-shape session)))
      (scad-sketch-session-toggle-polygon-extraction
       session shape "profile_pts")
      (let ((preview (scad-sketch-session-preview session)))
        (sextract-test--assert-contains "profile_pts =" preview)
        (sextract-test--assert-contains "polygon(profile_pts);" preview)
        ;; Make sure the default standalone name was not emitted.  Do not use
        ;; a plain substring check for \"pts =\" because \"profile_pts =\"
        ;; contains that substring.
        (should-not
         (string-match-p "\\_<pts\\_>[[:space:]]*=" preview))))))

(ert-deftest sextract-inline-polyround-extracts-as-polyround-ref ()
  "Inline polyRound polygon extracts to NAME plus polygon(polyRound(NAME, fn))."
  (sextract-test--with-session
      (concat "polygon(polyRound([\n"
              "  [0, 0, 3],\n"
              "  [80, 0, 3],\n"
              "  [80, 50, 3],\n"
              "  [0, 50, 3]\n"
              "], 32));\n")
      "polygon"
    (let ((shape (sextract-test--active-shape session)))
      (scad-sketch-session-toggle-polygon-extraction session shape "pts")
      (let ((preview (scad-sketch-session-preview session)))
        (sextract-test--assert-contains "pts =" preview)
        (sextract-test--assert-contains "[0, 0, 3]" preview)
        (sextract-test--assert-contains
         "polygon(polyRound(pts, 32));"
         preview)))))

(ert-deftest sextract-inline-polyround-round-trips-to-inline-polyround ()
  "Extracted polyRound polygon can be toggled back inline."
  (sextract-test--with-session
      (concat "polygon(polyRound([\n"
              "  [0, 0, 3],\n"
              "  [80, 0, 3],\n"
              "  [80, 50, 3],\n"
              "  [0, 50, 3]\n"
              "], 32));\n")
      "polygon"
    (let ((shape (sextract-test--active-shape session)))
      (scad-sketch-session-toggle-polygon-extraction session shape "pts")
      (scad-sketch-session-toggle-polygon-extraction session shape)
      (let ((preview (scad-sketch-session-preview session)))
        (sextract-test--assert-contains "polygon(polyRound([" preview)
        (sextract-test--assert-contains "], 32));" preview)
        (sextract-test--assert-not-contains "pts =" preview)
        (sextract-test--assert-not-contains "polyRound(pts" preview)))))


;;;; =========================================================================
;;;; Existing variable refs
;;;; =========================================================================

(ert-deftest sextract-existing-varref-toggle-forces-inline-call ()
  "Existing polygon(pts) toggles back to inline polygon output."
  (sextract-test--with-session
      (concat "pts = [\n"
              "  [0, 0],\n"
              "  [30, 0],\n"
              "  [15, 26]\n"
              "];\n\n"
              "polygon(pts);\n")
      "polygon"
    (let ((shape (sextract-test--active-shape session)))
      (should (scad-sketch-session-polygon-extracted-p session shape))
      (should (eq 'inline
                  (scad-sketch-session-toggle-polygon-extraction
                   session shape)))
      (let ((preview (scad-sketch-session-preview session)))
        (sextract-test--assert-contains
         "polygon([[0, 0], [30, 0], [15, 26]]);"
         preview)
        (sextract-test--assert-not-contains "polygon(pts);" preview)))))

(ert-deftest sextract-existing-varref-inline-writeback-deletes-source-array ()
  "Inlining polygon(points) rewrites the call and removes the source array."
  (sextract-test--with-session
      (concat "points = [[0, 0], [40, 0], [50, 20], [40, 40], [0, 40]];\n"
              "polygon(points);\n")
      "polygon"
    (let ((shape (sextract-test--active-shape session)))
      (should (scad-sketch-session-polygon-extracted-p session shape))
      (should (eq 'inline
                  (scad-sketch-session-toggle-polygon-extraction
                   session shape)))
      (let ((out (sextract-test--write-back-string session)))
        (sextract-test--assert-not-contains "points =" out)
        (sextract-test--assert-not-contains "polygon(points);" out)
        (sextract-test--assert-contains
         "polygon([[0, 0], [40, 0], [50, 20], [40, 40], [0, 40]]);"
         out)))))

(ert-deftest sextract-existing-polyround-varref-inline-writeback-deletes-source-array ()
  "Inlining polygon(polyRound(points, fn)) removes the source array."
  (sextract-test--with-session
      (concat "points = [\n"
              "  [0, 0, 3],\n"
              "  [40, 0, 3],\n"
              "  [50, 20, 3],\n"
              "  [40, 40, 3],\n"
              "  [0, 40, 3]\n"
              "];\n"
              "polygon(polyRound(points, 32));\n")
      "polygon"
    (let ((shape (sextract-test--active-shape session)))
      (should (scad-sketch-session-polygon-extracted-p session shape))
      (should (eq 'inline
                  (scad-sketch-session-toggle-polygon-extraction
                   session shape)))
      (let ((out (sextract-test--write-back-string session)))
        (sextract-test--assert-not-contains "points =" out)
        (sextract-test--assert-not-contains "polyRound(points" out)
        (sextract-test--assert-contains "polygon(polyRound([" out)
        (sextract-test--assert-contains "[50, 20, 3]" out)
        (sextract-test--assert-contains "], 32));" out)))))


(provide 'scad-sketch-extract-test)
;;; scad-sketch-extract-test.el ends here
