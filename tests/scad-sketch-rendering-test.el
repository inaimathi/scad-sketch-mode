;;; scad-sketch-rendering-test.el --- ERT tests for rendering invariants -*- lexical-binding: t; -*-

;;; Commentary:

;; Run from the repository root with:
;;
;;   emacs --batch -Q \
;;     --load tests/scad-sketch-rendering-test.el \
;;     --eval "(ert-run-tests-batch-and-exit)"
;;
;; Or run all tests:
;;
;;   emacs --batch -Q \
;;     --eval "(progn
;;               (dolist (file (directory-files \"tests\" t \"-test\\\\.el\\\\'\"))
;;                 (load-file file))
;;               (ert-run-tests-batch-and-exit))"
;;
;; These tests intentionally avoid pixel/image diffs.  They render to SVG and
;; assert structural invariants:
;;
;;   - preview mode omits editor affordances
;;   - preview mode renders mirrors as solid geometry, not dashed ghosts
;;   - normal mirror rendering still uses dashed mirror/axis styling
;;   - text used as a difference subtractor is rendered into the mask as text,
;;     not as a rough rectangular helper path
;;   - visible text uses the white-fill/dark-stroke renderer

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'svg)

;; ---------------------------------------------------------------------------
;; Locate and load project files.
;; Supports running from repo root or from tests/.
;; ---------------------------------------------------------------------------

(defvar scad-sketch-rendering-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

(defvar scad-sketch-rendering-test--root
  (expand-file-name ".." scad-sketch-rendering-test--dir)
  "Repository root directory.")

(add-to-list 'load-path scad-sketch-rendering-test--root)

(defun srend-test--load (file feature)
  "Load FILE from the repository root unless FEATURE is already provided."
  (unless (featurep feature)
    (load-file (expand-file-name file scad-sketch-rendering-test--root))))

;; Keep load order explicit; editor modules use a few forward references.
(srend-test--load "scad-sketch-parse.el"              'scad-sketch-parse)
(srend-test--load "scad-sketch-geometry.el"           'scad-sketch-geometry)
(srend-test--load "scad-sketch-session.el"            'scad-sketch-session)
(srend-test--load "scad-sketch-editor--refs.el"       'scad-sketch-editor--refs)
(srend-test--load "scad-sketch-editor--selection.el"  'scad-sketch-editor--selection)
(srend-test--load "scad-sketch-editor-core.el"        'scad-sketch-editor-core)
(srend-test--load "scad-sketch-editor--cursor.el"     'scad-sketch-editor--cursor)
(srend-test--load "scad-sketch-editor--editing.el"    'scad-sketch-editor--editing)
(srend-test--load "scad-sketch-editor--rendering.el"  'scad-sketch-editor--rendering)

;; ---------------------------------------------------------------------------
;; Fixtures
;; ---------------------------------------------------------------------------

(defconst srend-test--difference-source
  (concat "difference() {\n"
          "  union() {\n"
          "    square([80, 40]);\n"
          "    translate([80, 0])\n"
          "      circle(r=20);\n"
          "  }\n"
          "  circle(r=10);\n"
          "  translate([40, 20])\n"
          "    circle(r=5);\n"
          "}\n")
  "Difference fixture with unioned positive geometry and two subtractors.")

(defconst srend-test--mirror-source
  "mirror([1, 0])\n  polygon([[0,0],[20,0],[10,17]]);\n"
  "Simple mirror fixture.")

(defconst srend-test--text-difference-source
  (concat "difference() {\n"
          "  square([80, 40]);\n"
          "  text(\"CUT\", size=10);\n"
          "}\n")
  "Text subtractor fixture.")

(defconst srend-test--standalone-text-source
  "text(\"hi\", size=8, font=\"Liberation Sans\");\n"
  "Standalone visible text fixture.")

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defun srend-test--goto-substring (needle &optional offset)
  "Move point to NEEDLE's beginning plus OFFSET."
  (goto-char (point-min))
  (unless (search-forward needle nil t)
    (error "Could not find test substring: %S" needle))
  (goto-char (+ (match-beginning 0) (or offset 0))))

(defmacro srend-test--with-session (source needle &rest body)
  "Create a session from SOURCE at NEEDLE and run BODY with `session' bound."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,source)
     (srend-test--goto-substring ,needle)
     (let ((session (scad-sketch-session-at-point)))
       ,@body)))

(defun srend-test--svg-string (svg)
  "Return SVG as a string."
  (with-temp-buffer
    (svg-print svg)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun srend-test--render-session-svg-string (session &optional preview)
  "Render SESSION to raw SVG string.

When PREVIEW is non-nil, render with `scad-sketch--preview-mode' enabled.
This mirrors the canvas-rendering portions of `scad-sketch--render' while
returning inspectable SVG text instead of inserting an image into a buffer."
  (let* ((scad-sketch--preview-mode preview)
         (svg       (svg-create scad-sketch-canvas-width
                                scad-sketch-canvas-height))
         (bounds    (scad-sketch--bounds session))
         (transform (scad-sketch--transform bounds)))
    (svg-rectangle svg
                   0 0
                   scad-sketch-canvas-width
                   scad-sketch-canvas-height
                   :fill (if preview
                             scad-sketch-preview-background-color
                           "#ffffff"))

    (unless preview
      (scad-sketch--draw-grid svg bounds transform session))

    (scad-sketch--draw-path svg transform session)

    (unless preview
      (scad-sketch--draw-point-and-marks svg transform session)
      ;; Deliberately skip HUD here.  These tests focus on canvas geometry and
      ;; editor overlays, not status text.
      )

    (srend-test--svg-string svg)))

(defun srend-test--assert-contains (needle haystack)
  "Assert HAYSTACK contains literal NEEDLE."
  (should (stringp haystack))
  (should (string-match-p (regexp-quote needle) haystack)))

(defun srend-test--assert-not-contains (needle haystack)
  "Assert HAYSTACK does not contain literal NEEDLE."
  (should (stringp haystack))
  (should-not (string-match-p (regexp-quote needle) haystack)))

(defun srend-test--assert-matches (regexp haystack)
  "Assert HAYSTACK matches REGEXP."
  (should (stringp haystack))
  (should (string-match-p regexp haystack)))

(defun srend-test--assert-not-matches (regexp haystack)
  "Assert HAYSTACK does not match REGEXP."
  (should (stringp haystack))
  (should-not (string-match-p regexp haystack)))

(defun srend-test--assert-svg-text (text haystack)
  "Assert HAYSTACK contains an SVG text element whose content is TEXT.

`svg-print' may serialize text content with leading whitespace, so this helper
allows whitespace around the content."
  (srend-test--assert-matches
   (format ">[[:space:]]*%s[[:space:]]*</text>"
           (regexp-quote text))
   haystack))


;;;; =========================================================================
;;;; 1. Preview affordance suppression
;;;; =========================================================================

(ert-deftest srend-preview-omits-editor-labels-and-handles ()
  "Preview mode omits labels, points, handles, group labels, and attention UI."
  (srend-test--with-session srend-test--difference-source "difference"
			    (let ((svg (srend-test--render-session-svg-string session t)))
			      ;; Preview has the solid preview background.
			      (srend-test--assert-contains
			       scad-sketch-preview-background-color
			       svg)

			      ;; No boolean group labels.
			      (srend-test--assert-not-contains ">difference<" svg)
			      (srend-test--assert-not-contains ">union<" svg)

			      ;; No point/shape labels.
			      (srend-test--assert-not-contains "shape-0" svg)
			      (srend-test--assert-not-contains "shape-1" svg)
			      (srend-test--assert-not-contains "center" svg)
			      (srend-test--assert-not-contains "r-east" svg)
			      (srend-test--assert-not-contains "r-north" svg)

			      ;; No editor handle circles.  The fixture's real circles are rendered as
			      ;; paths, so circle tags here would almost certainly be editor affordances.
			      (srend-test--assert-not-contains "<circle" svg)

			      ;; No dashed helper/group/attention styling.
			      (srend-test--assert-not-contains "stroke-dasharray" svg))))

(ert-deftest srend-normal-render-does-include-editor-labels ()
  "Normal rendering still includes labels/overlays that preview omits."
  (srend-test--with-session srend-test--difference-source "difference"
    (let ((svg (srend-test--render-session-svg-string session nil)))
      ;; Boolean boxes/labels should be present in normal rendering.
      (srend-test--assert-svg-text "difference" svg)
      (srend-test--assert-svg-text "union" svg)

      ;; Normal rendering contains editor affordance circles and dashed lines.
      (srend-test--assert-contains "<circle" svg)
      (srend-test--assert-contains "stroke-dasharray" svg))))


;;;; =========================================================================
;;;; 2. Mirror preview vs normal mirror rendering
;;;; =========================================================================

(ert-deftest srend-preview-mirror-has-no-dashed-ghosts-or-axis-labels ()
  "Mirror preview renders source+mirror as solid geometry, not dashed helpers."
  (srend-test--with-session srend-test--mirror-source "mirror"
    (let ((svg (srend-test--render-session-svg-string session t)))
      (srend-test--assert-not-contains "14,8" svg)
      (srend-test--assert-not-contains "axis0" svg)
      (srend-test--assert-not-contains "axis1" svg)
      (srend-test--assert-not-contains "mirror" svg)
      (srend-test--assert-not-contains "shape-0" svg))))

(ert-deftest srend-normal-mirror-has-dashed-output-or-axis ()
  "Normal mirror rendering exposes mirror helper styling."
  (srend-test--with-session srend-test--mirror-source "mirror"
    (let ((svg (srend-test--render-session-svg-string session nil)))
      ;; Mirror output and/or mirror axis use the long-dash pattern.
      (srend-test--assert-contains "14,8" svg)
      ;; Axis handles are labelled in normal mode.
      (srend-test--assert-contains "axis0" svg)
      (srend-test--assert-contains "axis1" svg))))


;;;; =========================================================================
;;;; 3. Text in difference/intersection masks
;;;; =========================================================================

(ert-deftest srend-normal-text-difference-renders-mask-text-not-helper-box ()
  "A text subtractor is represented as text in the mask, not as a bbox helper."
  (srend-test--with-session srend-test--text-difference-source "difference"
    (let ((svg (srend-test--render-session-svg-string session nil)))
      ;; The text glyphs should appear in SVG because they are drawn into the
      ;; difference mask as actual text.
      (srend-test--assert-svg-text "CUT" svg)

      ;; The text subtractor should not create a dashed helper outline.  With
      ;; only a text subtractor, this catches the old bbox-as-helper-path bug.
      (srend-test--assert-not-contains "5,3" svg)

      ;; The text subtractor should not draw as a normal visible text overlay
      ;; on top of the boolean unless it has attention/selection.  In this test,
      ;; it should only be mask text, not visible white-outlined text.
      (srend-test--assert-not-contains "stroke=\"#111111\"" svg))))

(ert-deftest srend-preview-text-difference-keeps-text-as-cutout-paint ()
  "Preview text difference uses text content as the subtractive paint shape."
  (srend-test--with-session srend-test--text-difference-source "difference"
    (let ((svg (srend-test--render-session-svg-string session t)))
      ;; Painter-style preview draws subtractive text using the background fill.
      (srend-test--assert-svg-text "CUT" svg)
      (srend-test--assert-contains
       scad-sketch-preview-background-color
       svg)

      ;; Preview should not show the editor's visible text outline.
      (srend-test--assert-not-contains "stroke=\"#111111\"" svg)
      (srend-test--assert-not-contains "stroke-dasharray" svg))))


;;;; =========================================================================
;;;; 4. Visible text style
;;;; =========================================================================

(ert-deftest srend-visible-text-uses-white-fill-dark-outline ()
  "Standalone visible text renders white with a dark outline."
  (srend-test--with-session srend-test--standalone-text-source "text"
    (let ((svg (srend-test--render-session-svg-string session nil)))
      (srend-test--assert-svg-text "hi" svg)
      (srend-test--assert-contains "fill=\"#ffffff\"" svg)
      (srend-test--assert-contains "stroke=\"#111111\"" svg))))

(ert-deftest srend-text-attention-halo-does-not-fill-bounding-box ()
  "Text attention halo uses no-fill rough bounds, not a solid white bbox."
  (srend-test--with-session srend-test--standalone-text-source "text"
    (let* ((shape-id (scad-sketch-shape-id
                      (car (scad-sketch-session-shapes session))))
           (ref      (scad-sketch--shape-ref shape-id)))
      ;; Put focus on the text shape and point far away so attention comes from
      ;; focus, not accidental hover.
      (setf (scad-sketch-session-point session) '(9999.0 9999.0))
      (setf (scad-sketch-session-focus-ref session) ref)
      (let ((svg (srend-test--render-session-svg-string session nil)))
        ;; The rough-bounds attention halo should be no-fill.
        (srend-test--assert-matches
         "<rect[^>]+fill=\"none\"[^>]+stroke=\"#0057c2\""
         svg)

        ;; The old bad behavior looked like a filled white selected bbox.
        ;; Visible text itself can still use fill=\"#ffffff\", so we only reject
        ;; rectangle tags that combine a white fill with attention stroke.
        (srend-test--assert-not-matches
         "<rect[^>]+fill=\"#ffffff\"[^>]+stroke=\"#0057c2\""
         svg)))))

(ert-deftest srend-canvas-size-from-pixels-uses-window-body-size ()
  "Canvas sizing uses available window body pixels minus padding."
  (let ((scad-sketch-canvas-window-padding 12)
        (scad-sketch-canvas-width 900)
        (scad-sketch-canvas-height 650))
    (should (equal (scad-sketch--canvas-size-from-pixels 500 300)
                   '(488 288)))))

(ert-deftest srend-canvas-size-from-pixels-falls-back-for-invalid-size ()
  "Canvas sizing falls back to configured values when no valid window size exists."
  (let ((scad-sketch-canvas-window-padding 12)
        (scad-sketch-canvas-width 900)
        (scad-sketch-canvas-height 650))
    (should (equal (scad-sketch--canvas-size-from-pixels nil nil)
                   '(900 650)))
    (should (equal (scad-sketch--canvas-size-from-pixels 0 -1)
                   '(900 650)))))

(ert-deftest srend-transform-respects-dynamically-bound-canvas-size ()
  "The coordinate transform should use dynamically bound canvas dimensions."
  (let ((scad-sketch-canvas-width 400)
        (scad-sketch-canvas-height 300)
        (scad-sketch-margin 50))
    (let* ((tf (scad-sketch--transform '(0 100 0 100)))
           (p0 (funcall tf '(0 0)))
           (p1 (funcall tf '(100 100))))
      (should (equal p0 '(50 250)))
      (should (equal p1 '(250 50))))))

(provide 'scad-sketch-rendering-test)
;;; scad-sketch-rendering-test.el ends here
