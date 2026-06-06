;;; scad-sketch-complex-test.el --- Integration tests for test-complex.scad -*- lexical-binding: t; -*-

;;; Commentary:

;; Run from the repository root with:
;;
;;   emacs --batch -Q \
;;     --load tests/scad-sketch-complex-test.el \
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
;; These tests exercise the dense parser fixture in tests/test-complex.scad
;; without overfitting to exact total node counts.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; ---------------------------------------------------------------------------
;; Locate and load project files.
;; Supports running from repo root or from tests/.
;; ---------------------------------------------------------------------------

(defvar scad-sketch-complex-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

(defvar scad-sketch-complex-test--root
  (expand-file-name ".." scad-sketch-complex-test--dir)
  "Repository root directory.")

(defvar scad-sketch-complex-test--fixture
  (expand-file-name "test-complex.scad" scad-sketch-complex-test--dir)
  "Path to the dense complex OpenSCAD parser fixture.")

(add-to-list 'load-path scad-sketch-complex-test--root)

(unless (featurep 'scad-sketch-parse)
  (load-file (expand-file-name "scad-sketch-parse.el"
                               scad-sketch-complex-test--root)))


;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defun scx-test--source ()
  "Return contents of `tests/test-complex.scad'."
  (with-temp-buffer
    (insert-file-contents scad-sketch-complex-test--fixture)
    (buffer-string)))

(defun scx-test--nodes ()
  "Parse and return nodes from `tests/test-complex.scad'."
  (scad-sketch-parse (scx-test--source)))

(defun scx-test--nodes-of-type (nodes type)
  "Return all nodes of TYPE in NODES."
  (let (found)
    (dolist (node nodes)
      (scad-sketch-parse--walk
       node
       (lambda (n)
         (when (eq (plist-get n :type) type)
           (push n found)))))
    (nreverse found)))

(defun scx-test--node-contains-type-p (node type)
  "Return non-nil if NODE or one of its descendants has TYPE."
  (let ((found nil))
    (scad-sketch-parse--walk
     node
     (lambda (n)
       (when (eq (plist-get n :type) type)
         (setq found t))))
    found))

(defun scx-test--top-level-shape-nodes (nodes)
  "Return non-array top-level NODES."
  (cl-remove-if (lambda (node)
                  (eq (plist-get node :type) 'array))
                nodes))

(defun scx-test--first-shape-root ()
  "Return the single top-level shape root from the complex fixture."
  (car (scx-test--top-level-shape-nodes (scx-test--nodes))))

(defun scx-test--root-difference ()
  "Return the root difference node under translate/rotate."
  (let* ((root   (scx-test--first-shape-root))
         (rotate (plist-get root :child)))
    (plist-get rotate :child)))


;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(ert-deftest scx-complex-parses-without-error ()
  "The dense complex fixture parses without error."
  (let ((nodes (scx-test--nodes)))
    (should nodes)
    (should (>= (length nodes) 4))))

(ert-deftest scx-complex-has-three-top-level-arrays-and-one-shape-root ()
  "The complex fixture has three point arrays and one top-level shape tree."
  (let* ((nodes       (scx-test--nodes))
         (arrays      (scx-test--nodes-of-type nodes 'array))
         (shape-roots (scx-test--top-level-shape-nodes nodes))
         (array-names (mapcar (lambda (node)
                                (plist-get node :name))
                              arrays)))
    (should (= 3 (length arrays)))
    (should (= 1 (length shape-roots)))
    (should (member "badge_outline" array-names))
    (should (member "badge_chevron" array-names))
    (should (member "badge_starish" array-names))))

(ert-deftest scx-complex-root-transform-chain-descends-to-difference ()
  "The complex root is translate -> rotate -> difference."
  (let* ((root   (scx-test--first-shape-root))
         (rotate (plist-get root :child))
         (diff   (plist-get rotate :child)))
    (should (eq (plist-get root :type) 'translate))
    (should (eq (plist-get rotate :type) 'rotate))
    (should (eq (plist-get diff :type) 'difference))
    (should (>= (length (plist-get diff :children)) 4))))

(ert-deftest scx-complex-has-positive-and-subtractive-text ()
  "The complex fixture contains text in positive and subtractive positions."
  (let* ((diff        (scx-test--root-difference))
         (kids        (plist-get diff :children))
         (positive    (car kids))
         (subtractors (cdr kids)))
    (should (scx-test--node-contains-type-p positive 'text))
    (should (cl-some (lambda (node)
                       (scx-test--node-contains-type-p node 'text))
                     subtractors))))

(ert-deftest scx-complex-has-inline-and-varref-polygons ()
  "The complex fixture includes inline polygons and variable-reference polygons."
  (let* ((nodes   (scx-test--nodes))
         (polys   (scx-test--nodes-of-type nodes 'polygon))
         (inline  (cl-remove-if-not
                   (lambda (node)
                     (plist-get node :points))
                   polys))
         (varrefs (cl-remove-if-not
                   (lambda (node)
                     (plist-get node :source))
                   polys))
         (sources (mapcar (lambda (node)
                            (plist-get node :source))
                          varrefs)))
    (should inline)
    (should varrefs)
    (should (member "badge_outline" sources))
    (should (member "badge_chevron" sources))
    (should (member "badge_starish" sources))))

(ert-deftest scx-complex-has-polyround-varrefs ()
  "The complex fixture includes variable-reference polyRound polygons."
  (let* ((nodes   (scx-test--nodes))
         (polys   (scx-test--nodes-of-type nodes 'polygon))
         (pr-polys
          (cl-remove-if-not
           (lambda (node)
             (plist-get node :polyround))
           polys))
         (sources (mapcar (lambda (node)
                            (plist-get node :source))
                          pr-polys)))
    (should (>= (length pr-polys) 2))
    (should (member "badge_outline" sources))
    (should (member "badge_starish" sources))))

(ert-deftest scx-complex-polyround-fn-values-preserved ()
  "polyRound fn values are parsed from the complex fixture."
  (let* ((nodes (scx-test--nodes))
         (polys (scx-test--nodes-of-type nodes 'polygon))
         (fns   (delq nil
                      (mapcar (lambda (node)
                                (plist-get node :polyround))
                              polys))))
    (should (member 48 fns))
    (should (member 32 fns))))

(ert-deftest scx-complex-has-mirror-inside-intersection ()
  "The complex fixture contains a mirror node below an intersection node."
  (let* ((nodes         (scx-test--nodes))
         (intersections (scx-test--nodes-of-type nodes 'intersection)))
    (should intersections)
    (should (cl-some (lambda (node)
                       (scx-test--node-contains-type-p node 'mirror))
                     intersections))))

(ert-deftest scx-complex-node-at-deep-starish-polygon ()
  "Node-at can find the deep starish polygon reference inside the fixture."
  (let* ((source (scx-test--source))
         (nodes  (scad-sketch-parse source))
         (pos    (string-match "polygon(polyRound(badge_starish, 32))"
                               source))
         (node   (and pos (scad-sketch-parse-node-at nodes pos))))
    (should pos)
    (should node)
    (should (eq (plist-get node :type) 'polygon))
    (should (string= (plist-get node :source) "badge_starish"))
    (should (= (plist-get node :polyround) 32))))

(provide 'scad-sketch-complex-test)
;;; scad-sketch-complex-test.el ends here
