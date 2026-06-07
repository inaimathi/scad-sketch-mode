;;; scad-sketch-session-test.el --- ERT tests for scad-sketch-session.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run from the repository root with:
;;
;;   emacs --batch -Q \
;;     --load tests/scad-sketch-session-test.el \
;;     --eval "(ert-run-tests-batch-and-exit)"
;;
;; Or interactively:
;;
;;   M-x ert RET sss- RET
;;
;; These tests exercise session construction and source write-back behavior:
;;
;;   1. Direct array sessions stay array sessions.
;;   2. Inline polygons stay inline, including large inline polygons.
;;   3. Inline polyRound polygons stay inline polyRound polygons.
;;   4. Variable-ref polygons update the referenced array, not the polygon call.
;;   5. Variable-ref polyRound polygons preserve the polyRound call.
;;   6. Primitive root sessions write primitives back as primitives.
;;   7. Generic blank blocks emit inline shape/tree source, not array blocks.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

;; ---------------------------------------------------------------------------
;; Locate and load the project files.
;; Supports running from the repo root or from tests/.
;; ---------------------------------------------------------------------------

(defvar scad-sketch-session-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

(defvar scad-sketch-session-test--root
  (expand-file-name ".." scad-sketch-session-test--dir)
  "Repository root directory.")

(add-to-list 'load-path scad-sketch-session-test--root)

(let ((parser  (expand-file-name "scad-sketch-parse.el"
                                  scad-sketch-session-test--root))
      (session (expand-file-name "scad-sketch-session.el"
                                  scad-sketch-session-test--root)))
  (unless (featurep 'scad-sketch-parse)
    (load-file parser))
  (unless (featurep 'scad-sketch-session)
    (load-file session)))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defun sss-test--buffer-string ()
  "Return current buffer contents as a string."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun sss-test--goto-substring (needle &optional offset)
  "Move point to NEEDLE's beginning plus OFFSET.

Signals if NEEDLE is not found."
  (goto-char (point-min))
  (unless (search-forward needle nil t)
    (error "Could not find test substring: %S" needle))
  (goto-char (+ (match-beginning 0) (or offset 0))))

(defun sss-test--session-at (needle source &optional offset)
  "Create a session in SOURCE with point at NEEDLE plus OFFSET."
  (with-temp-buffer
    (insert source)
    (sss-test--goto-substring needle offset)
    (scad-sketch-session-at-point)))

(defmacro sss-test--with-source-at (source needle &rest body)
  "In a temp buffer containing SOURCE, move point to NEEDLE and run BODY.

Within BODY, `session' is bound to the result of
`scad-sketch-session-at-point'."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,source)
     (sss-test--goto-substring ,needle)
     (let ((session (scad-sketch-session-at-point)))
       ,@body)))

(defmacro sss-test--with-source-at-offset (source needle offset &rest body)
  "Like `sss-test--with-source-at', but adds OFFSET to NEEDLE's beginning."
  (declare (indent 3))
  `(with-temp-buffer
     (insert ,source)
     (sss-test--goto-substring ,needle ,offset)
     (let ((session (scad-sketch-session-at-point)))
       ,@body)))

(defun sss-test--write-back-string (session)
  "Write SESSION back to its source buffer and return the source contents."
  (scad-sketch-session-write-back session)
  (with-current-buffer (scad-sketch-session-source-buffer session)
    (sss-test--buffer-string)))

(defun sss-test--set-session-points (session points)
  "Set SESSION's editable active polygon points to POINTS."
  (setf (scad-sketch-session-points session) (copy-tree points))
  session)

(defun sss-test--shape-of-kind (session kind)
  "Return first shape in SESSION whose kind is KIND."
  (cl-find-if (lambda (shape)
                (eq (scad-sketch-shape-kind shape) kind))
              (scad-sketch-session-shapes session)))

(defun sss-test--set-circle-radius (session radius)
  "Set the first circle shape in SESSION to RADIUS."
  (let* ((shape (or (sss-test--shape-of-kind session 'circle)
                    (error "No circle shape in session")))
         (md    (copy-sequence (scad-sketch-shape-metadata shape))))
    (setf (scad-sketch-shape-metadata shape)
          (plist-put md :r (float radius))))
  session)

(defun sss-test--assert-no-generated-sketch-arrays (source)
  "Assert SOURCE contains no generated _sketch_N extraction arrays."
  (should-not (string-match-p "_sketch_[0-9]+" source)))

(defun sss-test--assert-contains (needle haystack)
  "Assert HAYSTACK contains literal NEEDLE."
  (should (string-match-p (regexp-quote needle) haystack)))

(defun sss-test--assert-not-contains (needle haystack)
  "Assert HAYSTACK does not contain literal NEEDLE."
  (should-not (string-match-p (regexp-quote needle) haystack)))


;;;; =========================================================================
;;;; 1. Direct array sessions
;;;; =========================================================================

(ert-deftest sss-array-writeback-updates-array-only ()
  "Opening inside an array writes the array assignment back.

The polygon call that refers to the array is untouched."
  (sss-test--with-source-at
      (concat "pts = [\n"
              "  [0, 0],\n"
              "  [10, 0],\n"
              "  [0, 10]\n"
              "];\n\n"
              "polygon(pts);\n")
      "pts ="
    (sss-test--set-session-points
     session
     '((1.0 2.0 0.0)
       (11.0 2.0 0.0)
       (1.0 12.0 0.0)))
    (let ((out (sss-test--write-back-string session)))
      (sss-test--assert-contains "pts = [" out)
      (sss-test--assert-contains "[1, 2]" out)
      (sss-test--assert-contains "[11, 2]" out)
      (sss-test--assert-contains "[1, 12]" out)
      (sss-test--assert-contains "polygon(pts);" out)
      (sss-test--assert-no-generated-sketch-arrays out))))

(ert-deftest sss-array-writeback-preserves-polyround-radii-when-present ()
  "Direct array write-back emits radii when any point has a non-zero radius."
  (sss-test--with-source-at
      (concat "rounded_box = [\n"
              "  [0, 0, 5],\n"
              "  [10, 0, 5],\n"
              "  [10, 10, 5]\n"
              "];\n")
      "rounded_box ="
    (sss-test--set-session-points
     session
     '((0.0 0.0 2.0)
       (20.0 0.0 3.0)
       (20.0 20.0 4.0)))
    (let ((out (sss-test--write-back-string session)))
      (sss-test--assert-contains "[0, 0, 2]" out)
      (sss-test--assert-contains "[20, 0, 3]" out)
      (sss-test--assert-contains "[20, 20, 4]" out))))


;;;; =========================================================================
;;;; 2. Inline polygon root sessions
;;;; =========================================================================

(ert-deftest sss-inline-polygon-writeback-keeps-inline-small ()
  "Opening an inline polygon writes back an inline polygon."
  (sss-test--with-source-at
      "polygon([[0,0], [10,0], [5,8]]);\n"
      "polygon"
    (sss-test--set-session-points
     session
     '((0.0 0.0 0.0)
       (20.0 0.0 0.0)
       (10.0 17.0 0.0)))
    (let ((out (sss-test--write-back-string session)))
      (sss-test--assert-contains "polygon([[0, 0]," out)
      (sss-test--assert-contains "[20, 0]," out)
      (sss-test--assert-contains "[10, 17]]);" out)
      (sss-test--assert-no-generated-sketch-arrays out))))

(ert-deftest sss-inline-polygon-writeback-keeps-inline-large ()
  "A large inline polygon remains inline; it is not extracted to _sketch_N."
  (sss-test--with-source-at
      (concat "polygon([\n"
              "  [0, 0],\n"
              "  [40, 0],\n"
              "  [50, 20],\n"
              "  [40, 40],\n"
              "  [0, 40]\n"
              "]);\n")
      "polygon"
    (sss-test--set-session-points
     session
     '((0.0 0.0 0.0)
       (40.0 0.0 0.0)
       (50.0 20.0 0.0)
       (40.0 40.0 0.0)
       (0.0 40.0 0.0)))
    (let ((out (sss-test--write-back-string session)))
      (sss-test--assert-contains "polygon([" out)
      (sss-test--assert-contains "[50, 20]" out)
      (sss-test--assert-no-generated-sketch-arrays out)
      (sss-test--assert-not-contains "polygon(_sketch" out))))

(ert-deftest sss-inline-polyround-writeback-keeps-polyround-inline ()
  "polygon(polyRound([...], fn)) stays inline polyRound on write-back."
  (sss-test--with-source-at
      (concat "polygon(polyRound([\n"
              "  [0, 0, 3],\n"
              "  [80, 0, 3],\n"
              "  [80, 50, 3],\n"
              "  [0, 50, 3]\n"
              "], 32));\n")
      "polygon"
    (sss-test--set-session-points
     session
     '((0.0 0.0 4.0)
       (80.0 0.0 4.0)
       (80.0 50.0 4.0)
       (0.0 50.0 4.0)))
    (let ((out (sss-test--write-back-string session)))
      (sss-test--assert-contains "polygon(polyRound([" out)
      (sss-test--assert-contains "[0, 0, 4]" out)
      (sss-test--assert-contains "[80, 50, 4]" out)
      (sss-test--assert-contains "], 32));" out)
      (sss-test--assert-no-generated-sketch-arrays out))))


;;;; =========================================================================
;;;; 3. Variable-ref polygon sessions
;;;; =========================================================================

(ert-deftest sss-varref-polygon-writeback-updates-array-not-call ()
  "Opening polygon(pts) edits pts = [...] and leaves polygon(pts) intact."
  (sss-test--with-source-at
      (concat "pts = [\n"
              "  [0, 0],\n"
              "  [20, 0],\n"
              "  [10, 17]\n"
              "];\n\n"
              "polygon(pts);\n")
      "polygon"
    (sss-test--set-session-points
     session
     '((1.0 1.0 0.0)
       (21.0 1.0 0.0)
       (11.0 18.0 0.0)))
    (let ((out (sss-test--write-back-string session)))
      (sss-test--assert-contains "pts = [" out)
      (sss-test--assert-contains "[1, 1]" out)
      (sss-test--assert-contains "[21, 1]" out)
      (sss-test--assert-contains "[11, 18]" out)
      (sss-test--assert-contains "polygon(pts);" out)
      (sss-test--assert-not-contains "polygon([[1, 1]" out)
      (sss-test--assert-no-generated-sketch-arrays out))))

(ert-deftest sss-varref-polyround-writeback-updates-array-keeps-call ()
  "polygon(polyRound(name, fn)) updates name and preserves the polyRound call."
  (sss-test--with-source-at
      (concat "rounded_box = [\n"
              "  [0, 0, 5],\n"
              "  [100, 0, 5],\n"
              "  [100, 60, 5],\n"
              "  [0, 60, 5]\n"
              "];\n\n"
              "polygon(polyRound(rounded_box, 64));\n")
      "polygon"
    (sss-test--set-session-points
     session
     '((0.0 0.0 7.0)
       (100.0 0.0 7.0)
       (100.0 60.0 7.0)
       (0.0 60.0 7.0)))
    (let ((out (sss-test--write-back-string session)))
      (sss-test--assert-contains "rounded_box = [" out)
      (sss-test--assert-contains "[0, 0, 7]" out)
      (sss-test--assert-contains "[100, 60, 7]" out)
      (sss-test--assert-contains "polygon(polyRound(rounded_box, 64));" out)
      (sss-test--assert-not-contains "polygon(polyRound([[" out)
      (sss-test--assert-no-generated-sketch-arrays out))))

(ert-deftest sss-varref-polygon-uses-nearest-prior-array ()
  "Variable-ref write-back updates the in-scope/prior array assignment."
  (sss-test--with-source-at
      (concat "pts = [[0,0]];\n"
              "polygon(pts);\n"
              "pts = [[99,99]];\n")
      "polygon"
    (sss-test--set-session-points
     session
     '((2.0 3.0 0.0)
       (4.0 5.0 0.0)))
    (let ((out (sss-test--write-back-string session)))
      (sss-test--assert-contains
       "pts = [[2, 3],\n       [4, 5]];"
       out)
      (sss-test--assert-contains "polygon(pts);" out)
      (sss-test--assert-contains "pts = [[99,99]];" out)
      ;; Make sure the rewritten source assignment still appears before the
      ;; polygon call.  This catches insertion-at-current-point regressions.
      (should (< (string-match (regexp-quote "pts = [[2, 3]") out)
                 (string-match (regexp-quote "polygon(pts);") out))))))


;;;; =========================================================================
;;;; 4. Primitive root sessions
;;;; =========================================================================

(ert-deftest sss-circle-root-writeback-stays-circle ()
  "Opening a circle root writes back a circle, not an array or polygon."
  (sss-test--with-source-at
      "circle(r=5);\n"
      "circle"
    (sss-test--set-circle-radius session 12.5)
    (let ((out (sss-test--write-back-string session)))
      (sss-test--assert-contains "circle(r=12.5);" out)
      (sss-test--assert-not-contains "polygon(" out)
      (sss-test--assert-not-contains " = [" out))))

(ert-deftest sss-square-root-preview-is-square ()
  "Opening a square root previews as a square primitive."
  (sss-test--with-source-at
      "square([80, 40]);\n"
      "square"
    (let ((preview (scad-sketch-session-preview session)))
      (sss-test--assert-contains "square([80, 40]);" preview)
      (sss-test--assert-not-contains "polygon(" preview))))

(ert-deftest sss-text-root-preview-is-text ()
  "Opening a text root previews as a text primitive, preserving supported params."
  (sss-test--with-source-at
      "text(\"hi\", size=8, font=\"Liberation Sans\");\n"
      "text"
    (let ((preview (scad-sketch-session-preview session)))
      (sss-test--assert-contains
       "text(\"hi\", size=8, font=\"Liberation Sans\");"
       preview)
      (sss-test--assert-not-contains "polygon(" preview))))


;;;; =========================================================================
;;;; 5. Generic blank block sessions
;;;; =========================================================================
(ert-deftest sss-generic-block-writeback-inserts-inline-polygon ()
  "A generic blank block emits inline shape source, not an array assignment."
  (unless (fboundp 'scad-sketch-session-insert-block-at-point)
    (ert-fail "Expected `scad-sketch-session-insert-block-at-point' to exist"))
  (with-temp-buffer
    (insert "// before\n\n// after\n")
    (goto-char (point-min))
    (search-forward "\n\n")
    (let ((session (scad-sketch-session-insert-block-at-point)))
      (scad-sketch-session-add-shape
       session
       '((0.0 0.0 0.0)
         (10.0 0.0 0.0)
         (0.0 10.0 0.0)))
      (let ((out (sss-test--write-back-string session)))
        (sss-test--assert-contains "// before" out)
        (sss-test--assert-contains "polygon([[0, 0]," out)
        (sss-test--assert-contains "[10, 0]," out)
        (sss-test--assert-contains "[0, 10]]);" out)
        (sss-test--assert-contains "// after" out)
        (sss-test--assert-not-contains " = [" out)
        (sss-test--assert-no-generated-sketch-arrays out)))))

(ert-deftest sss-generic-block-preview-empty-is-empty ()
  "A newly inserted generic block with no shapes previews as empty source."
  (unless (fboundp 'scad-sketch-session-insert-block-at-point)
    (ert-fail "Expected `scad-sketch-session-insert-block-at-point' to exist"))
  (with-temp-buffer
    (insert "// before\n// after\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((session (scad-sketch-session-insert-block-at-point)))
      (should (string= (scad-sketch-session-preview session) "")))))

(ert-deftest sss-generic-block-polyround-polygon-stays-inline ()
  "A generic block polygon with radii emits inline polyRound source."
  (unless (fboundp 'scad-sketch-session-insert-block-at-point)
    (ert-fail "Expected `scad-sketch-session-insert-block-at-point' to exist"))
  (with-temp-buffer
    (insert "// slot\n")
    (goto-char (point-max))
    (let ((session (scad-sketch-session-insert-block-at-point)))
      (scad-sketch-session-add-shape
       session
       '((0.0 0.0 3.0)
         (20.0 0.0 3.0)
         (20.0 20.0 3.0)
         (0.0 20.0 3.0))
       32)
      (let ((out (sss-test--write-back-string session)))
        (sss-test--assert-contains "polygon(polyRound([" out)
        (sss-test--assert-contains "[0, 0, 3]" out)
        (sss-test--assert-contains "], 32));" out)
        (sss-test--assert-not-contains " = [" out)
        (sss-test--assert-no-generated-sketch-arrays out)))))


;;;; =========================================================================
;;;; 6. Unsupported source-style guardrails
;;;; =========================================================================

(ert-deftest sss-array-only-session-rejects-multiple-shapes ()
  "Array-only sessions cannot serialize multiple independent shapes."
  (sss-test--with-source-at
      "pts = [[0,0],[10,0],[0,10]];\n"
      "pts ="
    (let ((extra
           (scad-sketch-session--make-polygon-shape
            'shape-extra
            '((100.0 100.0 0.0)
              (110.0 100.0 0.0)
              (100.0 110.0 0.0))
            nil nil nil
            (list :created-in-test t))))
      (setf (scad-sketch-session-shapes session)
            (append (scad-sketch-session-shapes session) (list extra)))
      (should-error
       (scad-sketch-session-write-back session)
       :type 'scad-sketch-unsupported-edit-target))))

(provide 'scad-sketch-session-test)
;;; scad-sketch-session-test.el ends here
