;;; scad-sketch-parse-test.el --- ERT tests for scad-sketch-parse.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run from the repository root with:
;;
;;   emacs --batch -Q \
;;     --load tests/scad-sketch-parse-test.el \
;;     --eval "(ert-run-tests-batch-and-exit)"
;;
;; Or interactively: M-x ert RET t RET
;;
;; The suite is structured in sections that mirror the parser's own sections:
;;
;;   1. Tokenizer
;;   2. Array assignments
;;   3. 2D primitives  (circle / square / text / polygon)
;;   4. Transforms     (translate / rotate / scale / mirror)
;;   5. Booleans       (union / difference / intersection)
;;   6. Skipped forms  (include, use, 3D ops, scalar assignments)
;;   7. Module-body harvesting
;;   8. Positions      (:beg / :end are plausible offsets)
;;   9. scad-sketch-parse-node-at
;;  10. scad-sketch-parse--path-to
;;  11. scad-sketch-parse--walk
;;  12. scad-sketch-parse--lookup-variable
;;  13. scad-sketch-parse--fmt-num
;;  14. scad-sketch-unparse
;;  15. scad-sketch-unparse-top-level
;;  16. Integration: parse test.scad end-to-end

;;; Code:

(require 'ert)
(require 'cl-lib)

;; ---------------------------------------------------------------------------
;; Locate and load the parser.
;; Supports running from the repo root or from the tests/ sub-directory.
;; ---------------------------------------------------------------------------

(defvar scad-sketch-parse-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

(defvar scad-sketch-parse-test--scad-file
  (expand-file-name "test.scad" scad-sketch-parse-test--dir)
  "Path to the integration test fixture.")

(let ((parser (expand-file-name "../scad-sketch-parse.el"
                                scad-sketch-parse-test--dir)))
  (unless (featurep 'scad-sketch-parse)
    (load-file parser)))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defun ssp-test--parse (src)
  "Parse SRC string and return the flat node list."
  (scad-sketch-parse src))

(defun ssp-test--nodes-of-type (nodes type)
  "Return all nodes in NODES whose :type is TYPE."
  (cl-remove-if-not (lambda (n) (eq (plist-get n :type) type)) nodes))

(defun ssp-test--first-of-type (nodes type)
  "Return the first node in NODES whose :type is TYPE, or nil."
  (cl-find-if (lambda (n) (eq (plist-get n :type) type)) nodes))

(defun ssp-test--approx= (a b)
  "Return non-nil when A and B are within 1e-9 of each other."
  (< (abs (- a b)) 1e-9))

(defmacro ssp-test--with-scad-file (&rest body)
  "Bind `nodes' to the parse result of test.scad and execute BODY."
  `(let* ((src (with-temp-buffer
                 (insert-file-contents scad-sketch-parse-test--scad-file)
                 (buffer-string)))
          (nodes (scad-sketch-parse src)))
     ,@body))


;;;; =========================================================================
;;;; 1. Tokenizer
;;;; =========================================================================

(ert-deftest ssp-tokenizer-numbers ()
  "Integer, float, and sign variants tokenize to `num'."
  (let* ((tokens (scad-sketch-parse--tokenize "0 1 -1 3.14 .5 1e2 1.0e-3"))
         (vals   (mapcar (lambda (t) (nth 1 t)) (append tokens nil)))
         (types  (mapcar (lambda (t) (nth 0 t)) (append tokens nil))))
    (should (equal types '(num num num num num num num)))
    (should (member "3.14" vals))
    (should (member "-1"   vals))))

(ert-deftest ssp-tokenizer-strings ()
  "String tokens include surrounding quotes and handle escapes."
  (let* ((tokens (scad-sketch-parse--tokenize "\"hello\" \"a\\\"b\""))
         (first  (aref tokens 0))
         (second (aref tokens 1)))
    (should (eq (nth 0 first)  'str))
    (should (equal (nth 1 first) "\"hello\""))
    (should (eq (nth 0 second) 'str))
    (should (string-match "\\\\\"" (nth 1 second)))))

(ert-deftest ssp-tokenizer-identifiers ()
  "Identifier tokens are tagged `id'."
  (let* ((tokens (scad-sketch-parse--tokenize "circle polygon _x $special"))
         (types  (mapcar (lambda (t) (nth 0 t)) (append tokens nil))))
    (should (cl-every (lambda (ty) (eq ty 'id)) types))))

(ert-deftest ssp-tokenizer-punctuation ()
  "Each punctuation character becomes a separate `punct' token."
  (let* ((tokens (scad-sketch-parse--tokenize "(){}[],;="))
         (vals   (mapcar (lambda (t) (nth 1 t)) (append tokens nil))))
    (should (equal vals '("(" ")" "{" "}" "[" "]" "," ";" "=")))))

(ert-deftest ssp-tokenizer-angle-path ()
  "Angle-bracket include/use paths tokenize as one `path' token."
  (let* ((tokens (scad-sketch-parse--tokenize "include <MCAD/shapes.scad>"))
         (types  (mapcar (lambda (tok) (nth 0 tok)) (append tokens nil)))
         (vals   (mapcar (lambda (tok) (nth 1 tok)) (append tokens nil))))
    (should (equal types '(id path)))
    (should (equal vals '("include" "<MCAD/shapes.scad>")))))

(ert-deftest ssp-tokenizer-whitespace-skipped ()
  "Whitespace and newlines produce no tokens."
  (let* ((tokens (scad-sketch-parse--tokenize "  \t\n  42  \r\n  ")))
    (should (= 1 (length tokens)))
    (should (equal (nth 1 (aref tokens 0)) "42"))))

(ert-deftest ssp-tokenizer-line-comments-stripped ()
  "// comments are replaced by spaces so token positions remain valid."
  (let* ((tokens (scad-sketch-parse--tokenize "circle // this is a comment\n(r=5)"))
         (vals   (mapcar (lambda (t) (nth 1 t)) (append tokens nil))))
    (should (member "circle" vals))
    (should (member "r"      vals))
    (should (member "5"      vals))
    (should-not (member "this" vals))))

(ert-deftest ssp-tokenizer-block-comments-stripped ()
  "/* */ block comments are replaced so surrounding tokens survive."
  (let* ((tokens (scad-sketch-parse--tokenize "circle /* radius */ (r=5)"))
         (vals   (mapcar (lambda (t) (nth 1 t)) (append tokens nil))))
    (should (member "circle" vals))
    (should (member "5"      vals))
    (should-not (member "radius" vals))))

(ert-deftest ssp-tokenizer-positions ()
  "Token start/end positions are 0-based and non-overlapping."
  (let* ((tokens (scad-sketch-parse--tokenize "abc 123")))
    (should (= (nth 2 (aref tokens 0)) 0))   ; "abc" starts at 0
    (should (= (nth 3 (aref tokens 0)) 3))   ; "abc" ends at 3
    (should (= (nth 2 (aref tokens 1)) 4))   ; "123" starts at 4
    (should (= (nth 3 (aref tokens 1)) 7)))) ; "123" ends at 7


;;;; =========================================================================
;;;; 2. Array assignments
;;;; =========================================================================

(ert-deftest ssp-array-basic ()
  "A simple [[x,y], ...] assignment parses to an array node."
  (let* ((nodes (ssp-test--parse "pts = [[0,0],[10,5]];"))
         (n     (ssp-test--first-of-type nodes 'array)))
    (should n)
    (should (string= (plist-get n :name) "pts"))
    (should (= (length (plist-get n :points)) 2))
    (let ((p0 (nth 0 (plist-get n :points)))
          (p1 (nth 1 (plist-get n :points))))
      (should (ssp-test--approx= (nth 0 p0) 0.0))
      (should (ssp-test--approx= (nth 1 p0) 0.0))
      (should (ssp-test--approx= (nth 0 p1) 10.0))
      (should (ssp-test--approx= (nth 1 p1) 5.0)))))

(ert-deftest ssp-array-with-radii ()
  "[[x,y,r], ...] stores the radius as the third element."
  (let* ((nodes (ssp-test--parse "pts = [[0,0,5],[10,0,3]];"))
         (n     (ssp-test--first-of-type nodes 'array))
         (p0    (nth 0 (plist-get n :points)))
         (p1    (nth 1 (plist-get n :points))))
    (should (ssp-test--approx= (nth 2 p0) 5.0))
    (should (ssp-test--approx= (nth 2 p1) 3.0))))

(ert-deftest ssp-array-zero-radius-default ()
  "[x,y] points receive a default radius of 0.0."
  (let* ((nodes (ssp-test--parse "pts = [[1,2]];"))
         (n     (ssp-test--first-of-type nodes 'array))
         (p0    (nth 0 (plist-get n :points))))
    (should (ssp-test--approx= (nth 2 p0) 0.0))))

(ert-deftest ssp-array-single-point ()
  "A single-element array is valid."
  (let* ((nodes (ssp-test--parse "dot = [[10,20]];"))
         (n     (ssp-test--first-of-type nodes 'array)))
    (should (= (length (plist-get n :points)) 1))))

(ert-deftest ssp-array-floats ()
  "Points with floating-point coordinates are preserved."
  (let* ((nodes (ssp-test--parse "fp = [[1.5, 2.75]];"))
         (n     (ssp-test--first-of-type nodes 'array))
         (p0    (nth 0 (plist-get n :points))))
    (should (ssp-test--approx= (nth 0 p0) 1.5))
    (should (ssp-test--approx= (nth 1 p0) 2.75))))

(ert-deftest ssp-array-name-preserved ()
  "The variable name is stored verbatim on the node."
  (let* ((nodes (ssp-test--parse "my_shape_pts = [[0,0]];"))
         (n     (ssp-test--first-of-type nodes 'array)))
    (should (string= (plist-get n :name) "my_shape_pts"))))

(ert-deftest ssp-array-multiple-in-file ()
  "Multiple array assignments all appear in the result list."
  (let* ((nodes (ssp-test--parse "a=[[0,0]]; b=[[1,1]]; c=[[2,2]];"))
         (arrs  (ssp-test--nodes-of-type nodes 'array)))
    (should (= (length arrs) 3))
    (should (equal (mapcar (lambda (n) (plist-get n :name)) arrs)
                   '("a" "b" "c")))))

(ert-deftest ssp-array-node-type ()
  "Array nodes carry :type array."
  (let* ((nodes (ssp-test--parse "x = [[0,0]];"))
         (n     (car nodes)))
    (should (eq (plist-get n :type) 'array))))


;;;; =========================================================================
;;;; 3. 2D Primitives
;;;; =========================================================================

;;; 3a. circle

(ert-deftest ssp-circle-bare-radius ()
  "circle(N) stores the bare number as the radius."
  (let* ((nodes (ssp-test--parse "circle(15);"))
         (n     (ssp-test--first-of-type nodes 'circle)))
    (should n)
    (should (ssp-test--approx= (plist-get n :r) 15.0))))

(ert-deftest ssp-circle-r-keyword ()
  "circle(r=N) stores N as the radius."
  (let* ((nodes (ssp-test--parse "circle(r=20);"))
         (n     (ssp-test--first-of-type nodes 'circle)))
    (should (ssp-test--approx= (plist-get n :r) 20.0))))

(ert-deftest ssp-circle-d-keyword-halved ()
  "circle(d=N) stores N/2 as the radius."
  (let* ((nodes (ssp-test--parse "circle(d=25);"))
         (n     (ssp-test--first-of-type nodes 'circle)))
    (should (ssp-test--approx= (plist-get n :r) 12.5))))

(ert-deftest ssp-circle-default-center ()
  "circle nodes have :cx 0 and :cy 0 by default."
  (let* ((nodes (ssp-test--parse "circle(r=5);"))
         (n     (ssp-test--first-of-type nodes 'circle)))
    (should (ssp-test--approx= (plist-get n :cx) 0.0))
    (should (ssp-test--approx= (plist-get n :cy) 0.0))))

(ert-deftest ssp-circle-float-radius ()
  "Floating-point radii are stored precisely."
  (let* ((nodes (ssp-test--parse "circle(r=3.5);"))
         (n     (ssp-test--first-of-type nodes 'circle)))
    (should (ssp-test--approx= (plist-get n :r) 3.5))))

(ert-deftest ssp-circle-extra-keyword-params-ignored ()
  "circle(r=N, $fn=K) keeps the radius and ignores unsupported params."
  (let* ((nodes (ssp-test--parse "circle(r=5, $fn=64);"))
         (n     (ssp-test--first-of-type nodes 'circle)))
    (should n)
    (should (ssp-test--approx= (plist-get n :r) 5.0))))

;;; 3b. square

(ert-deftest ssp-square-plain ()
  "square([W,H]) stores width and height; origin at (0,0)."
  (let* ((nodes (ssp-test--parse "square([80, 40]);"))
         (n     (ssp-test--first-of-type nodes 'square)))
    (should n)
    (should (ssp-test--approx= (plist-get n :w) 80.0))
    (should (ssp-test--approx= (plist-get n :h) 40.0))
    (should (ssp-test--approx= (plist-get n :x) 0.0))
    (should (ssp-test--approx= (plist-get n :y) 0.0))))

(ert-deftest ssp-square-centered ()
  "square([W,H], center=true) sets :x to -W/2 and :y to -H/2."
  (let* ((nodes (ssp-test--parse "square([60, 30], center=true);"))
         (n     (ssp-test--first-of-type nodes 'square)))
    (should (ssp-test--approx= (plist-get n :x) -30.0))
    (should (ssp-test--approx= (plist-get n :y) -15.0))
    (should (ssp-test--approx= (plist-get n :w)  60.0))
    (should (ssp-test--approx= (plist-get n :h)  30.0))))

(ert-deftest ssp-square-angle-zero ()
  "Freshly parsed squares always carry :angle 0.0."
  (let* ((nodes (ssp-test--parse "square([10, 10]);"))
         (n     (ssp-test--first-of-type nodes 'square)))
    (should (ssp-test--approx= (plist-get n :angle) 0.0))))

(ert-deftest ssp-square-center-false ()
  "square([W,H], center=false) behaves the same as the plain form."
  (let* ((nodes (ssp-test--parse "square([20, 10], center=false);"))
         (n     (ssp-test--first-of-type nodes 'square)))
    (should (ssp-test--approx= (plist-get n :x) 0.0))
    (should (ssp-test--approx= (plist-get n :y) 0.0))))

;;; 3c. text

(ert-deftest ssp-text-bare ()
  "text(\"str\") stores the string and uses a default size of 10."
  (let* ((nodes (ssp-test--parse "text(\"hello\");"))
         (n     (ssp-test--first-of-type nodes 'text)))
    (should n)
    (should (string= (plist-get n :str) "hello"))
    (should (ssp-test--approx= (plist-get n :size) 10.0))))

(ert-deftest ssp-text-with-size ()
  "text(\"str\", size=N) stores the explicit size."
  (let* ((nodes (ssp-test--parse "text(\"OpenSCAD\", size=14);"))
         (n     (ssp-test--first-of-type nodes 'text)))
    (should (string= (plist-get n :str) "OpenSCAD"))
    (should (ssp-test--approx= (plist-get n :size) 14.0))))

(ert-deftest ssp-text-unknown-params-ignored ()
  "Unknown keyword params (e.g. font=) are silently ignored."
  (let* ((nodes (ssp-test--parse "text(\"hi\", size=8, font=\"Liberation Sans\");"))
         (n     (ssp-test--first-of-type nodes 'text)))
    (should (string= (plist-get n :str) "hi"))
    (should (ssp-test--approx= (plist-get n :size) 8.0))))

(ert-deftest ssp-text-default-position ()
  "text nodes default to :x 0 and :y 0."
  (let* ((nodes (ssp-test--parse "text(\"x\");"))
         (n     (ssp-test--first-of-type nodes 'text)))
    (should (ssp-test--approx= (plist-get n :x) 0.0))
    (should (ssp-test--approx= (plist-get n :y) 0.0))))

;;; 3d. polygon

(ert-deftest ssp-polygon-inline-points ()
  "polygon([[x,y], ...]) stores the points list and nil source."
  (let* ((nodes (ssp-test--parse "polygon([[0,0],[30,0],[15,26]]);"))
         (n     (ssp-test--first-of-type nodes 'polygon)))
    (should n)
    (should (= (length (plist-get n :points)) 3))
    (should (null (plist-get n :source)))
    (should (null (plist-get n :polyround)))))

(ert-deftest ssp-polygon-inline-point-values ()
  "Inline polygon point coordinates are stored as floats."
  (let* ((nodes (ssp-test--parse "polygon([[0,0],[10,0],[5,8]]);"))
         (n     (ssp-test--first-of-type nodes 'polygon))
         (pts   (plist-get n :points)))
    (should (ssp-test--approx= (nth 0 (nth 0 pts)) 0.0))
    (should (ssp-test--approx= (nth 0 (nth 1 pts)) 10.0))
    (should (ssp-test--approx= (nth 1 (nth 2 pts)) 8.0))))

(ert-deftest ssp-polygon-variable-ref ()
  "polygon(name) stores the variable name in :source and nil :points."
  (let* ((nodes (ssp-test--parse "pts=[[0,0]]; polygon(pts);"))
         (poly  (ssp-test--first-of-type nodes 'polygon)))
    (should (string= (plist-get poly :source) "pts"))
    (should (null (plist-get poly :points)))
    (should (null (plist-get poly :polyround)))))

(ert-deftest ssp-polygon-points-keyword-varref ()
  "polygon(points=name) stores the variable name in :source."
  (let* ((nodes (ssp-test--parse "pts=[[0,0]]; polygon(points=pts);"))
         (poly  (ssp-test--first-of-type nodes 'polygon)))
    (should (string= (plist-get poly :source) "pts"))
    (should (null (plist-get poly :points)))))

(ert-deftest ssp-polygon-points-keyword-inline-with-extra-params ()
  "polygon(points=[...], paths=..., convexity=...) keeps points and ignores the rest."
  (let* ((nodes (ssp-test--parse
                 "polygon(points=[[0,0],[10,0],[0,10]], paths=[[0,1,2]], convexity=2);"))
         (poly  (ssp-test--first-of-type nodes 'polygon)))
    (should poly)
    (should (= (length (plist-get poly :points)) 3))))

(ert-deftest ssp-polygon-polyround-inline ()
  "polygon(polyRound([...], fn)) stores the points and fn."
  (let* ((nodes (ssp-test--parse
                 "polygon(polyRound([[0,0,3],[80,0,3],[80,50,3],[0,50,3]], 32));"))
         (n     (ssp-test--first-of-type nodes 'polygon)))
    (should (= (plist-get n :polyround) 32))
    (should (= (length (plist-get n :points)) 4))
    (should (null (plist-get n :source)))))

(ert-deftest ssp-polygon-polyround-varref ()
  "polygon(polyRound(name, fn)) stores nil :points, the name, and fn."
  (let* ((nodes (ssp-test--parse "polygon(polyRound(rounded_box, 64));"))
         (n     (ssp-test--first-of-type nodes 'polygon)))
    (should (string= (plist-get n :source) "rounded_box"))
    (should (= (plist-get n :polyround) 64))
    (should (null (plist-get n :points)))))

(ert-deftest ssp-polygon-five-points ()
  "A five-point inline polygon stores all five points."
  (let* ((nodes (ssp-test--parse
                 "polygon([[0,0],[40,0],[50,20],[40,40],[0,40]]);"))
         (n     (ssp-test--first-of-type nodes 'polygon)))
    (should (= (length (plist-get n :points)) 5))))


;;;; =========================================================================
;;;; 4. Transforms
;;;; =========================================================================

(ert-deftest ssp-translate-values ()
  "translate([tx,ty]) stores tx and ty on the node."
  (let* ((nodes (ssp-test--parse "translate([10, 20]) circle(r=5);"))
         (n     (ssp-test--first-of-type nodes 'translate)))
    (should n)
    (should (ssp-test--approx= (plist-get n :tx) 10.0))
    (should (ssp-test--approx= (plist-get n :ty) 20.0))))

(ert-deftest ssp-translate-child-type ()
  "translate wraps its child; child :type is circle."
  (let* ((nodes (ssp-test--parse "translate([10, 20]) circle(r=5);"))
         (n     (ssp-test--first-of-type nodes 'translate))
         (child (plist-get n :child)))
    (should child)
    (should (eq (plist-get child :type) 'circle))
    (should (ssp-test--approx= (plist-get child :r) 5.0))))

(ert-deftest ssp-translate-negative-coords ()
  "Negative translation coordinates are accepted."
  (let* ((nodes (ssp-test--parse "translate([5, -10]) square([40,20]);"))
         (n     (ssp-test--first-of-type nodes 'translate)))
    (should (ssp-test--approx= (plist-get n :ty) -10.0))))

(ert-deftest ssp-rotate-angle ()
  "rotate(angle) stores the angle as a float."
  (let* ((nodes (ssp-test--parse "rotate(45) square([20, 20]);"))
         (n     (ssp-test--first-of-type nodes 'rotate)))
    (should n)
    (should (ssp-test--approx= (plist-get n :angle) 45.0))))

(ert-deftest ssp-rotate-child-type ()
  "rotate's child is the following shape."
  (let* ((nodes (ssp-test--parse "rotate(45) square([20, 20]);"))
         (n     (ssp-test--first-of-type nodes 'rotate))
         (child (plist-get n :child)))
    (should (eq (plist-get child :type) 'square))))

(ert-deftest ssp-scale-values ()
  "scale([sx, sy]) stores both scale factors."
  (let* ((nodes (ssp-test--parse "scale([2, 0.5]) circle(r=10);"))
         (n     (ssp-test--first-of-type nodes 'scale)))
    (should n)
    (should (ssp-test--approx= (plist-get n :sx) 2.0))
    (should (ssp-test--approx= (plist-get n :sy) 0.5))))

(ert-deftest ssp-mirror-x-axis ()
  "mirror([1, 0]) stores :mx 1 :my 0."
  (let* ((nodes (ssp-test--parse "mirror([1, 0]) polygon([[0,0],[20,0],[10,17]]);"))
         (n     (ssp-test--first-of-type nodes 'mirror)))
    (should n)
    (should (ssp-test--approx= (plist-get n :mx) 1.0))
    (should (ssp-test--approx= (plist-get n :my) 0.0))))

(ert-deftest ssp-mirror-y-axis ()
  "mirror([0, 1]) stores :mx 0 :my 1."
  (let* ((nodes (ssp-test--parse "mirror([0, 1]) square([30, 15]);"))
         (n     (ssp-test--first-of-type nodes 'mirror)))
    (should (ssp-test--approx= (plist-get n :mx) 0.0))
    (should (ssp-test--approx= (plist-get n :my) 1.0))))

(ert-deftest ssp-nested-transforms ()
  "translate → rotate → scale → circle nesting is represented correctly."
  (let* ((nodes (ssp-test--parse
                 "translate([100, 50]) rotate(30) scale([1.5, 1.5]) circle(r=8);"))
         (xl  (ssp-test--first-of-type nodes 'translate))
         (rot (plist-get xl :child))
         (scl (plist-get rot :child))
         (cir (plist-get scl :child)))
    (should (eq (plist-get xl  :type) 'translate))
    (should (eq (plist-get rot :type) 'rotate))
    (should (eq (plist-get scl :type) 'scale))
    (should (eq (plist-get cir :type) 'circle))
    (should (ssp-test--approx= (plist-get xl  :tx)    100.0))
    (should (ssp-test--approx= (plist-get xl  :ty)     50.0))
    (should (ssp-test--approx= (plist-get rot :angle)  30.0))
    (should (ssp-test--approx= (plist-get scl :sx)      1.5))
    (should (ssp-test--approx= (plist-get scl :sy)      1.5))
    (should (ssp-test--approx= (plist-get cir :r)       8.0))))

(ert-deftest ssp-transform-child-is-composition ()
  "A transform may wrap a boolean composition."
  (let* ((nodes (ssp-test--parse
                 "translate([200, 0]) difference() { square([50,50]); circle(r=15); }"))
         (xl   (ssp-test--first-of-type nodes 'translate))
         (diff (plist-get xl :child)))
    (should (eq (plist-get diff :type) 'difference))
    (should (= (length (plist-get diff :children)) 2))))


;;;; =========================================================================
;;;; 5. Boolean compositions
;;;; =========================================================================

(ert-deftest ssp-difference-two-children ()
  "difference() { A; B; } produces a difference node with two children."
  (let* ((nodes (ssp-test--parse
                 "difference() { square([60,60], center=true); circle(r=20); }"))
         (n     (ssp-test--first-of-type nodes 'difference)))
    (should n)
    (should (= (length (plist-get n :children)) 2))
    (should (eq (plist-get (nth 0 (plist-get n :children)) :type) 'square))
    (should (eq (plist-get (nth 1 (plist-get n :children)) :type) 'circle))))

(ert-deftest ssp-union-two-children ()
  "union() { A; B; } produces a union node."
  (let* ((nodes (ssp-test--parse
                 "union() { circle(r=15); translate([25,0]) circle(r=15); }"))
         (n     (ssp-test--first-of-type nodes 'union)))
    (should n)
    (should (eq (plist-get n :type) 'union))
    (should (= (length (plist-get n :children)) 2))))

(ert-deftest ssp-union-second-child-is-translate ()
  "The translate inside union() is stored as a child node."
  (let* ((nodes (ssp-test--parse
                 "union() { circle(r=15); translate([25,0]) circle(r=15); }"))
         (n     (ssp-test--first-of-type nodes 'union))
         (c1    (nth 1 (plist-get n :children))))
    (should (eq (plist-get c1 :type) 'translate))
    (should (ssp-test--approx= (plist-get c1 :tx) 25.0))))

(ert-deftest ssp-intersection-children ()
  "intersection() parses analogously to difference and union."
  (let* ((nodes (ssp-test--parse
                 "intersection() { square([40,40], center=true); circle(r=25); }"))
         (n     (ssp-test--first-of-type nodes 'intersection)))
    (should n)
    (should (= (length (plist-get n :children)) 2))))

(ert-deftest ssp-difference-three-children ()
  "A difference with three children stores all three in order."
  (let* ((nodes (ssp-test--parse
                 (concat "difference() {"
                         "  union() { square([80,40]); translate([80,0]) circle(r=20); }"
                         "  circle(r=10);"
                         "  translate([40,20]) circle(r=5);"
                         "}")))
         (n  (ssp-test--first-of-type nodes 'difference))
         (ch (plist-get n :children)))
    (should (= (length ch) 3))
    (should (eq (plist-get (nth 0 ch) :type) 'union))
    (should (eq (plist-get (nth 1 ch) :type) 'circle))
    (should (eq (plist-get (nth 2 ch) :type) 'translate))))

(ert-deftest ssp-nested-boolean-union-inside-difference ()
  "union() nested as the first child of difference() is accessible."
  (let* ((nodes (ssp-test--parse
                 (concat "difference() {"
                         "  union() { square([80,40]); translate([80,0]) circle(r=20); }"
                         "  circle(r=10);"
                         "}")))
         (diff  (ssp-test--first-of-type nodes 'difference))
         (un    (nth 0 (plist-get diff :children))))
    (should (eq (plist-get un :type) 'union))
    (should (= (length (plist-get un :children)) 2))))


;;;; =========================================================================
;;;; 6. Skipped forms
;;;; =========================================================================

(ert-deftest ssp-skip-include ()
  "include <...> produces no node and does not eat the following shape."
  (let* ((nodes (ssp-test--parse "include <foo.scad>
circle(r=5);")))
    (should (= (length nodes) 1))
    (should (eq (plist-get (car nodes) :type) 'circle))))

(ert-deftest ssp-skip-include-with-semicolon ()
  "include <...>; also remains safe with an explicit semicolon."
  (let* ((nodes (ssp-test--parse "include <foo.scad>;
circle(r=5);")))
    (should (= (length nodes) 1))
    (should (eq (plist-get (car nodes) :type) 'circle))))

(ert-deftest ssp-skip-use ()
  "use <...> produces no node and does not eat the following shape."
  (let* ((nodes (ssp-test--parse "use <utils/helpers.scad>
circle(r=5);")))
    (should (= (length nodes) 1))
    (should (eq (plist-get (car nodes) :type) 'circle))))

(ert-deftest ssp-skip-use-with-semicolon ()
  "use <...>; also remains safe with an explicit semicolon."
  (let* ((nodes (ssp-test--parse "use <utils/helpers.scad>;
circle(r=5);")))
    (should (= (length nodes) 1))
    (should (eq (plist-get (car nodes) :type) 'circle))))

(ert-deftest ssp-skip-scalar-assignment ()
  "name = scalar_value; (non-array) produces no node."
  (let* ((nodes (ssp-test--parse "fn = 32;\ncircle(r=5);")))
    (should (= (length nodes) 1))
    (should (eq (plist-get (car nodes) :type) 'circle))))

(ert-deftest ssp-skip-string-assignment ()
  "name = \"string\"; produces no node."
  (let* ((nodes (ssp-test--parse "quality = \"high\";\ncircle(r=3);")))
    (should (= (length nodes) 1))))

(ert-deftest ssp-skip-linear-extrude ()
  "linear_extrude() itself is skipped, but a supported unbraced child is harvested."
  (let* ((nodes (ssp-test--parse
                 "linear_extrude(height=10) circle(r=30);\ncircle(r=5);")))
    (should (= (length nodes) 2))
    (should (eq (plist-get (nth 0 nodes) :type) 'circle))
    (should (eq (plist-get (nth 1 nodes) :type) 'circle))
    (should (= (plist-get (nth 0 nodes) :r) 30.0))
    (should (= (plist-get (nth 1 nodes) :r) 5.0))))

(ert-deftest ssp-skip-linear-extrude-braced ()
  "A braced linear_extrude wrapper is skipped, but supported children are harvested."
  (let* ((nodes (ssp-test--parse
                 "linear_extrude(height=10) { circle(r=30); }
circle(r=5);")))
    (should (= (length nodes) 2))
    (should (eq (plist-get (nth 0 nodes) :type) 'circle))
    (should (eq (plist-get (nth 1 nodes) :type) 'circle))
    (should (ssp-test--approx= (plist-get (nth 0 nodes) :r) 30.0))
    (should (ssp-test--approx= (plist-get (nth 1 nodes) :r) 5.0))))

(ert-deftest ssp-skip-3d-translate-wrapper ()
  "A 3D translate wrapper is skipped, but its supported child is harvested."
  (let* ((nodes (ssp-test--parse
                 "translate([1,2,0]) circle(r=30);
circle(r=5);")))
    (should (= (length nodes) 2))
    (should (eq (plist-get (nth 0 nodes) :type) 'circle))
    (should (eq (plist-get (nth 1 nodes) :type) 'circle))
    (should (ssp-test--approx= (plist-get (nth 0 nodes) :r) 30.0))
    (should (ssp-test--approx= (plist-get (nth 1 nodes) :r) 5.0))))

(ert-deftest ssp-skip-does-not-corrupt-subsequent-nodes ()
  "Nodes following a skipped form are still parsed correctly."
  (let* ((nodes (ssp-test--parse
                 "fn = 32;\ntriangle = [[0,0],[10,0],[5,8]];\ncircle(r=5);"))
         (arr   (ssp-test--first-of-type nodes 'array))
         (circ  (ssp-test--first-of-type nodes 'circle)))
    (should arr)
    (should circ)
    (should (string= (plist-get arr :name) "triangle"))))

(ert-deftest ssp-unsupported-wrapper-harvests-circle-child ()
  "An unsupported unbraced wrapper exposes its circle child as editable."
  (let* ((src   "linear_extrude(5) circle(d=10);")
         (nodes (ssp-test--parse src))
         (circ  (car nodes)))
    (should (= (length nodes) 1))
    (should (eq (plist-get circ :type) 'circle))
    (should (= (plist-get circ :r) 5.0))
    (should (= (plist-get circ :beg)
               (string-match "circle" src)))))

(ert-deftest ssp-unsupported-wrapper-harvests-polygon-child-inner-point ()
  "node-at inside polygon points under linear_extrude returns the polygon child."
  (let* ((src (concat "linear_extrude(5)\n"
                      "polygon(polyRound([\n"
                      "  [0, 0, 3],\n"
                      "  [80, 0, 3],\n"
                      "  [80, 50, 3],\n"
                      "  [0, 50, 3]\n"
                      "], 32));\n"))
         (nodes (ssp-test--parse src))
         (pos   (string-match "\\[80, 0, 3\\]" src))
         (node  (scad-sketch-parse-node-at nodes pos))
         (path  (scad-sketch-parse--path-to nodes pos)))
    (should pos)
    (should node)
    (should (eq (plist-get node :type) 'polygon))
    (should (= (plist-get node :polyround) 32))
    ;; The unsupported wrapper is intentionally not in the path.
    (should (= (length path) 1))
    (should (eq (plist-get (car path) :type) 'polygon))))

(ert-deftest ssp-nested-unsupported-wrappers-harvest-square-child ()
  "Nested unsupported wrappers expose their supported square child."
  (let* ((src   "rotate([0, 10, 45]) linear_extrude(5) square(25);")
         (nodes (ssp-test--parse src))
         (sq    (car nodes)))
    (should (= (length nodes) 1))
    (should (eq (plist-get sq :type) 'square))
    (should (= (plist-get sq :w) 25.0))
    (should (= (plist-get sq :h) 25.0))
    (should (= (plist-get sq :beg)
               (string-match "square" src)))))

(ert-deftest ssp-mixed-unsupported-block-harvests-descendant-circle-only ()
  "Mixed unsupported blocks are not editable as groups, but child shapes are."
  (let* ((src (concat "union() {\n"
                      "  linear_extrude(5) circle(10);\n"
                      "  translate([0, 10, 20]) cube(15);\n"
                      "}\n"))
         (nodes (ssp-test--parse src))
         (circle-pos (string-match "circle" src))
         (union-pos  (string-match "union" src))
         (circle-node (scad-sketch-parse-node-at nodes circle-pos))
         (union-node  (scad-sketch-parse-node-at nodes union-pos)))
    (should (= (length nodes) 1))
    (should circle-node)
    (should (eq (plist-get circle-node :type) 'circle))
    ;; The mixed/unsupported union wrapper itself is not exposed as editable.
    (should-not union-node)))

(ert-deftest sss-linear-extrude-circle-at-child-writeback-keeps-wrapper ()
  "Editing a circle child under linear_extrude rewrites only the circle call."
  (sss-test--with-source-at
      "linear_extrude(5) circle(d=10);\n"
      "circle"
    (sss-test--set-circle-radius session 7.0)
    (let ((out (sss-test--write-back-string session)))
      (sss-test--assert-contains "linear_extrude(5)" out)
      (sss-test--assert-contains "circle(r=7);" out)
      (sss-test--assert-not-contains "circle(d=10)" out))))

(ert-deftest sss-nested-unsupported-wrapper-square-writeback-keeps-wrapper ()
  "Editing a square child under nested unsupported wrappers keeps the wrappers."
  (sss-test--with-source-at
      "rotate([0, 10, 45]) linear_extrude(5) square(25);\n"
      "square"
    (let ((out (sss-test--write-back-string session)))
      (sss-test--assert-contains "rotate([0, 10, 45])" out)
      (sss-test--assert-contains "linear_extrude(5)" out)
      ;; square(25) canonicalizes to square([25, 25]) on write-back.
      (sss-test--assert-contains "square([25, 25]);" out))))

(ert-deftest sss-mixed-unsupported-block-point-on-circle-edits-circle ()
  "Point on a supported child inside a mixed unsupported block opens that child."
  (sss-test--with-source-at
      (concat "union() {\n"
              "  linear_extrude(5) circle(10);\n"
              "  translate([0, 10, 20]) cube(15);\n"
              "}\n")
      "circle"
    (let ((preview (scad-sketch-session-preview session)))
      (sss-test--assert-contains "circle(r=10);" preview)
      (sss-test--assert-not-contains "union()" preview))))

(ert-deftest sss-mixed-unsupported-block-point-on-union-is-not-target ()
  "Point on a mixed unsupported union wrapper does not open the wrapper."
  (with-temp-buffer
    (insert (concat "union() {\n"
                    "  linear_extrude(5) circle(10);\n"
                    "  translate([0, 10, 20]) cube(15);\n"
                    "}\n"))
    (goto-char (point-min))
    (search-forward "union")
    (goto-char (match-beginning 0))
    (should-error
     (scad-sketch-session-at-point)
     :type 'scad-sketch-no-edit-target)))


;;;; =========================================================================
;;;; 7. Module-body harvesting
;;;; =========================================================================

(ert-deftest ssp-module-array-harvested ()
  "Array assignments inside module bodies appear in the flat result."
  (let* ((nodes (ssp-test--parse
                 "module profile() { body_pts = [[0,0],[60,0],[60,40]]; }"))
         (arr   (ssp-test--first-of-type nodes 'array)))
    (should arr)
    (should (string= (plist-get arr :name) "body_pts"))
    (should (= (length (plist-get arr :points)) 3))))

(ert-deftest ssp-module-shapes-harvested ()
  "Shapes inside a module body are included in the flat result."
  (let* ((nodes (ssp-test--parse
                 "module m() { circle(r=5); square([10,10]); }"))
         (circles (ssp-test--nodes-of-type nodes 'circle))
         (squares (ssp-test--nodes-of-type nodes 'square)))
    (should (= (length circles) 1))
    (should (= (length squares) 1))))

(ert-deftest ssp-module-and-top-level-both-collected ()
  "Nodes from both module bodies and top-level are in the result."
  (let* ((nodes (ssp-test--parse
                 "circle(r=3);\nmodule m() { circle(r=7); }"))
         (circles (ssp-test--nodes-of-type nodes 'circle)))
    (should (= (length circles) 2))))


(ert-deftest ssp-module-nodes-carry-scope ()
  "Harvested module-body nodes carry a non-nil :scope."
  (let* ((nodes (ssp-test--parse
                 "module m() { pts = [[1,1]]; polygon(pts); }"))
         (arr   (ssp-test--first-of-type nodes 'array))
         (poly  (ssp-test--first-of-type nodes 'polygon)))
    (should (plist-get arr :scope))
    (should (equal (plist-get arr :scope) (plist-get poly :scope)))))

;;;; =========================================================================
;;;; 8. Positions (:beg / :end)
;;;; =========================================================================

(ert-deftest ssp-positions-beg-le-end ()
  "Every node satisfies :beg <= :end."
  (let* ((nodes (ssp-test--parse
                 "pts=[[0,0]]; circle(r=5); translate([1,2]) square([3,4]);")))
    (dolist (n nodes)
      (should (<= (plist-get n :beg) (plist-get n :end))))))

(ert-deftest ssp-positions-beg-nonneg ()
  "Every node has a non-negative :beg."
  (let* ((nodes (ssp-test--parse "circle(r=5); square([10,10]);")))
    (dolist (n nodes)
      (should (>= (plist-get n :beg) 0)))))

(ert-deftest ssp-positions-within-source ()
  "Every node's :end is within the source string length."
  (let* ((src "circle(r=5);")
         (nodes (ssp-test--parse src)))
    (dolist (n nodes)
      (should (<= (plist-get n :end) (length src))))))

(ert-deftest ssp-positions-ordering ()
  "Nodes at the top level appear in source order (beg is non-decreasing)."
  (let* ((nodes (ssp-test--parse "circle(r=1); circle(r=2); circle(r=3);"))
         (begs  (mapcar (lambda (n) (plist-get n :beg)) nodes)))
    (should (equal begs (sort (copy-sequence begs) #'<)))))

(ert-deftest ssp-positions-child-within-parent ()
  "A transform child's range is contained within the parent's range."
  (let* ((nodes (ssp-test--parse "translate([10,20]) circle(r=5);"))
         (xl    (ssp-test--first-of-type nodes 'translate))
         (child (plist-get xl :child)))
    (should (<= (plist-get xl    :beg) (plist-get child :beg)))
    (should (>= (plist-get xl    :end) (plist-get child :end)))))


;;;; =========================================================================
;;;; 9. scad-sketch-parse-node-at
;;;; =========================================================================

(ert-deftest ssp-node-at-finds-top-level ()
  "node-at returns the array node when given a position inside it."
  (let* ((src   "pts = [[0,0],[10,5]];")
         (nodes (ssp-test--parse src))
         (n     (ssp-test--first-of-type nodes 'array))
         (found (scad-sketch-parse-node-at nodes (plist-get n :beg))))
    (should found)
    (should (eq (plist-get found :type) 'array))))

(ert-deftest ssp-node-at-returns-nil-outside ()
  "node-at returns nil when the position is strictly before all nodes.
Because the first token of `circle(r=5);' starts at offset 0, passing
position -1 (or any pos < :beg) returns nil.  Passing 0 returns the
node because the check is inclusive: (<= :beg pos) && (pos <= :end)."
  ;; Use a source string that begins after position 0 by prefixing whitespace.
  (let* ((nodes (ssp-test--parse "  circle(r=5);")))
    ;; :beg should now be 2; pos 0 is outside.
    (should (null (scad-sketch-parse-node-at nodes 0)))))

(ert-deftest ssp-node-at-finds-deepest ()
  "node-at descends into transforms and returns the deepest match."
  (let* ((src   "translate([10,20]) circle(r=5);")
         (nodes (ssp-test--parse src))
         (xl    (ssp-test--first-of-type nodes 'translate))
         (child (plist-get xl :child))
         ;; Use a position well inside the child
         (pos   (+ (plist-get child :beg) 1))
         (found (scad-sketch-parse-node-at nodes pos)))
    (should found)
    (should (eq (plist-get found :type) 'circle))))

(ert-deftest ssp-node-at-returns-parent-when-between-children ()
  "node-at returns the composition node itself when pos is in the braces,
not inside any child."
  ;; We test this by pointing at the difference node's own :beg.
  (let* ((src   "difference() { square([10,10]); circle(r=4); }")
         (nodes (ssp-test--parse src))
         (diff  (ssp-test--first-of-type nodes 'difference))
         (found (scad-sketch-parse-node-at nodes (plist-get diff :beg))))
    (should found)
    ;; Should be difference itself or a child — but not nil.
    (should (memq (plist-get found :type) '(difference square circle)))))

(ert-deftest ssp-node-at-single-node-plist ()
  "node-at accepts a single node plist (not a list of nodes)."
  (let* ((src   "circle(r=7);")
         (nodes (ssp-test--parse src))
         (n     (car nodes))
         (found (scad-sketch-parse-node-at n (plist-get n :beg))))
    (should found)
    (should (eq (plist-get found :type) 'circle))))

(ert-deftest ssp-node-at-union-child ()
  "node-at can find a circle nested inside a union."
  (let* ((src   "union() { circle(r=5); circle(r=10); }")
         (nodes (ssp-test--parse src))
         (un    (ssp-test--first-of-type nodes 'union))
         (c0    (nth 0 (plist-get un :children)))
         (pos   (+ (plist-get c0 :beg) 1))
         (found (scad-sketch-parse-node-at nodes pos)))
    (should (eq (plist-get found :type) 'circle))
    (should (ssp-test--approx= (plist-get found :r) 5.0))))


;;;; =========================================================================
;;;; 10. scad-sketch-parse--path-to
;;;; =========================================================================

(ert-deftest ssp-path-to-top-level ()
  "path-to returns a one-element list for a top-level node."
  (let* ((src   "circle(r=5);")
         (nodes (ssp-test--parse src))
         (n     (ssp-test--first-of-type nodes 'circle))
         (path  (scad-sketch-parse--path-to nodes (plist-get n :beg))))
    (should path)
    (should (= (length path) 1))
    (should (eq (plist-get (car path) :type) 'circle))))

(ert-deftest ssp-path-to-returns-nil-outside ()
  "path-to returns nil when pos is strictly outside all nodes.
The range check is inclusive, so prefix whitespace to push :beg above 0."
  (let* ((nodes (ssp-test--parse "  circle(r=5);")))
    (should (null (scad-sketch-parse--path-to nodes 0)))))

(ert-deftest ssp-path-to-nested-four-levels ()
  "path-to through translate→rotate→scale→circle returns all four nodes."
  (let* ((src   "translate([100,50]) rotate(30) scale([1.5,1.5]) circle(r=8);")
         (nodes (ssp-test--parse src))
         (xl    (ssp-test--first-of-type nodes 'translate))
         (circ  (plist-get (plist-get (plist-get xl :child) :child) :child))
         (pos   (+ (plist-get circ :beg) 1))
         (path  (scad-sketch-parse--path-to nodes pos)))
    (should (= (length path) 4))
    (should (eq (plist-get (nth 0 path) :type) 'translate))
    (should (eq (plist-get (nth 1 path) :type) 'rotate))
    (should (eq (plist-get (nth 2 path) :type) 'scale))
    (should (eq (plist-get (nth 3 path) :type) 'circle))))

(ert-deftest ssp-path-to-outermost-first ()
  "path-to returns path outermost node first."
  (let* ((src   "translate([1,2]) circle(r=3);")
         (nodes (ssp-test--parse src))
         (path  (scad-sketch-parse--path-to
                 nodes
                 (+ (plist-get (ssp-test--first-of-type nodes 'translate) :beg) 1))))
    (should (eq (plist-get (car path) :type) 'translate))))


;;;; =========================================================================
;;;; 11. scad-sketch-parse--walk
;;;; =========================================================================

(ert-deftest ssp-walk-visits-all ()
  "walk visits every node in the subtree exactly once."
  (let* ((src   "translate([1,2]) circle(r=3);")
         (nodes (ssp-test--parse src))
         (n     (car nodes))
         count)
    (setq count 0)
    (scad-sketch-parse--walk n (lambda (_) (setq count (1+ count))))
    ;; translate + circle = 2
    (should (= count 2))))

(ert-deftest ssp-walk-visits-composition-children ()
  "walk on a difference node visits the difference plus all children."
  (let* ((src   "difference() { square([10,10]); circle(r=4); circle(r=2); }")
         (nodes (ssp-test--parse src))
         (diff  (car nodes))
         types)
    (scad-sketch-parse--walk diff (lambda (n) (push (plist-get n :type) types)))
    (setq types (nreverse types))
    (should (equal (car types) 'difference))
    (should (= (length types) 4))
    (should (= (length (cl-remove-if-not (lambda (t) (eq t 'circle)) types)) 2))))

(ert-deftest ssp-walk-deep-nesting ()
  "walk handles deeply nested transforms without stack overflow."
  (let* ((src   (concat "translate([1,1])"
                        " translate([2,2])"
                        " translate([3,3])"
                        " translate([4,4])"
                        " circle(r=1);"))
         (nodes (ssp-test--parse src))
         count)
    (setq count 0)
    (scad-sketch-parse--walk (car nodes) (lambda (_) (setq count (1+ count))))
    ;; 4 translates + 1 circle = 5
    (should (= count 5))))

(ert-deftest ssp-walk-leaf-visited-once ()
  "walk on a leaf node (circle) visits exactly one node."
  (let* ((src   "circle(r=1);")
         (nodes (ssp-test--parse src))
         count)
    (setq count 0)
    (scad-sketch-parse--walk (car nodes) (lambda (_) (setq count (1+ count))))
    (should (= count 1))))


;;;; =========================================================================
;;;; 12. scad-sketch-parse--lookup-variable
;;;; =========================================================================

(ert-deftest ssp-lookup-finds-top-level-array ()
  "lookup-variable returns the parsed points for a top-level array."
  (let* ((src "triangle = [[0,0],[50,0],[25,43]];\ncircle(r=1);")
         (pts (scad-sketch-parse--lookup-variable "triangle" src 9999)))
    (should pts)
    (should (= (length pts) 3))
    (should (ssp-test--approx= (nth 0 (nth 0 pts)) 0.0))
    (should (ssp-test--approx= (nth 0 (nth 1 pts)) 50.0))))

(ert-deftest ssp-lookup-respects-before-pos ()
  "lookup-variable ignores assignments that come AFTER before-pos."
  (let* ((src "triangle = [[0,0],[50,0],[25,43]];")
         ;; before-pos = 0 means we haven't seen anything yet
         (pts (scad-sketch-parse--lookup-variable "triangle" src 0)))
    (should (null pts))))

(ert-deftest ssp-lookup-unknown-name ()
  "lookup-variable returns nil for a name that does not exist."
  (let* ((src "pts = [[0,0]];")
         (pts (scad-sketch-parse--lookup-variable "no_such_var" src 9999)))
    (should (null pts))))

(ert-deftest ssp-lookup-radii-preserved ()
  "lookup-variable returns radii when the array uses [x,y,r] form."
  (let* ((src "rb = [[0,0,5],[100,0,5]];\ncircle(r=1);")
         (pts (scad-sketch-parse--lookup-variable "rb" src 9999)))
    (should (ssp-test--approx= (nth 2 (nth 0 pts)) 5.0))
    (should (ssp-test--approx= (nth 2 (nth 1 pts)) 5.0))))


(ert-deftest ssp-lookup-prefers-same-module-scope ()
  "lookup-variable prefers an array in the polygon's module scope over top-level."
  (let* ((src (concat "pts = [[0,0]];
"
                      "module a() {
"
                      "  pts = [[1,1],[2,2]];
"
                      "  polygon(pts);
"
                      "}
"))
         (pos (string-match "polygon" src))
         (pts (scad-sketch-parse--lookup-variable "pts" src pos)))
    (should (= (length pts) 2))
    (should (ssp-test--approx= (nth 0 (nth 0 pts)) 1.0))))

(ert-deftest ssp-lookup-falls-back-to-parent-scope ()
  "lookup-variable falls back to top-level when no module-local binding exists."
  (let* ((src (concat "pts = [[0,0],[10,0]];
"
                      "module a() { polygon(pts); }
"))
         (pos (string-match "polygon" src))
         (pts (scad-sketch-parse--lookup-variable "pts" src pos)))
    (should (= (length pts) 2))
    (should (ssp-test--approx= (nth 0 (nth 1 pts)) 10.0))))

(ert-deftest ssp-lookup-separates-sibling-module-scopes ()
  "lookup-variable does not confuse same-named arrays in sibling modules."
  (let* ((src (concat "module a() { pts = [[1,1]]; polygon(pts); }
"
                      "module b() { pts = [[2,2]]; polygon(pts); }
"))
         (pos (string-match "polygon" src (string-match "module b" src)))
         (pts (scad-sketch-parse--lookup-variable "pts" src pos)))
    (should (= (length pts) 1))
    (should (ssp-test--approx= (nth 0 (car pts)) 2.0))))

;;;; =========================================================================
;;;; 13. scad-sketch-parse--fmt-num
;;;; =========================================================================

(ert-deftest ssp-fmt-num-integers ()
  "Integers are formatted without decimal points."
  (should (string= (scad-sketch-parse--fmt-num 0)   "0"))
  (should (string= (scad-sketch-parse--fmt-num 1)   "1"))
  (should (string= (scad-sketch-parse--fmt-num -1)  "-1"))
  (should (string= (scad-sketch-parse--fmt-num 100) "100")))

(ert-deftest ssp-fmt-num-negative-zero ()
  "-0.0 formats as \"0\"."
  (should (string= (scad-sketch-parse--fmt-num -0.0) "0")))

(ert-deftest ssp-fmt-num-trailing-zeros-stripped ()
  "Trailing zeros after the decimal point are stripped."
  (should (string= (scad-sketch-parse--fmt-num 1.5)  "1.5"))
  (should (string= (scad-sketch-parse--fmt-num 0.1)  "0.1"))
  (should (string= (scad-sketch-parse--fmt-num 2.50) "2.5")))

(ert-deftest ssp-fmt-num-four-decimal-places ()
  "Non-integer floats use at most 4 significant decimal places."
  (let ((s (scad-sketch-parse--fmt-num 3.14159)))
    ;; Should be "3.1416" after rounding and stripping
    (should (string= s "3.1416"))))

(ert-deftest ssp-fmt-num-half ()
  "12.5 (from d=25) formats without trailing zero."
  (should (string= (scad-sketch-parse--fmt-num 12.5) "12.5")))


;;;; =========================================================================
;;;; 14. scad-sketch-unparse
;;;; =========================================================================

(ert-deftest ssp-unparse-circle ()
  "circle node unparses to circle(r=...) form."
  (let* ((n (list :type 'circle :r 7.5 :cx 0.0 :cy 0.0 :beg 0 :end 0)))
    (should (string= (scad-sketch-unparse n) "circle(r=7.5);\n"))))

(ert-deftest ssp-unparse-circle-integer-radius ()
  "Integer radii unparse without decimal point."
  (let* ((n (list :type 'circle :r 10.0 :cx 0.0 :cy 0.0 :beg 0 :end 0)))
    (should (string= (scad-sketch-unparse n) "circle(r=10);\n"))))

(ert-deftest ssp-unparse-square-plain ()
  "Plain square unparses to square([W, H])."
  (let* ((n (list :type 'square :w 80.0 :h 40.0 :x 0.0 :y 0.0 :angle 0.0
                  :beg 0 :end 0)))
    (should (string= (scad-sketch-unparse n) "square([80, 40]);\n"))))

(ert-deftest ssp-square-scalar-size ()
  "square(N) parses as a square with width and height N."
  (let* ((nodes (ssp-test--parse "square(25);"))
         (sq    (car nodes)))
    (should (eq (plist-get sq :type) 'square))
    (should (= (plist-get sq :w) 25.0))
    (should (= (plist-get sq :h) 25.0))
    (should (= (plist-get sq :x) 0.0))
    (should (= (plist-get sq :y) 0.0))))

(ert-deftest ssp-unparse-square-centered ()
  "Centered square unparses with center=true."
  (let* ((n (list :type 'square :w 60.0 :h 30.0 :x -30.0 :y -15.0 :angle 0.0
                  :beg 0 :end 0)))
    (should (string= (scad-sketch-unparse n) "square([60, 30], center=true);\n"))))

(ert-deftest ssp-unparse-square-rotated ()
  "A square with non-zero :angle unparses wrapped in rotate(...)."
  (let* ((n (list :type 'square :w 10.0 :h 10.0 :x 0.0 :y 0.0 :angle 45.0
                  :beg 0 :end 0)))
    (let ((s (scad-sketch-unparse n)))
      (should (string-match "^rotate(45)" s))
      (should (string-match "square" s)))))

(ert-deftest ssp-unparse-text ()
  "text node unparses with quoted string and size."
  (let* ((n (list :type 'text :str "hi" :x 0.0 :y 0.0 :size 8.0 :beg 0 :end 0)))
    (should (string= (scad-sketch-unparse n) "text(\"hi\", size=8);\n"))))

(ert-deftest ssp-unparse-text-default-size ()
  "text with size 10 unparses as size=10."
  (let* ((n (list :type 'text :str "x" :x 0.0 :y 0.0 :size 10.0 :beg 0 :end 0)))
    (should (string-match "size=10" (scad-sketch-unparse n)))))

(ert-deftest ssp-unparse-polygon-inline ()
  "Short polygon (≤4 pts) unparses as inline polygon([...])."
  (let* ((n (list :type 'polygon
                  :points '((0.0 0.0 0.0) (10.0 0.0 0.0) (5.0 8.0 0.0))
                  :source nil :polyround nil :beg 0 :end 0)))
    (should (string= (scad-sketch-unparse n)
                     "polygon([[0, 0], [10, 0], [5, 8]]);\n"))))

(ert-deftest ssp-unparse-polygon-variable-ref ()
  "Variable-ref polygon unparses as polygon(name)."
  (let* ((n (list :type 'polygon :points nil :source "my_pts"
                  :polyround nil :beg 0 :end 0)))
    (should (string= (scad-sketch-unparse n) "polygon(my_pts);\n"))))

(ert-deftest ssp-unparse-polygon-polyround-ref ()
  "polyRound variable-ref polygon unparses as polygon(polyRound(name, fn))."
  (let* ((n (list :type 'polygon :points nil :source "my_pts"
                  :polyround 32 :beg 0 :end 0)))
    (should (string= (scad-sketch-unparse n)
                     "polygon(polyRound(my_pts, 32));\n"))))

(ert-deftest ssp-unparse-array ()
  "Array node unparses as name = [\\n  [x, y],\\n  ...\\n];."
  (let* ((n (list :type 'array :name "foo" :beg 0 :end 0
                  :points '((0.0 0.0 0.0) (10.0 5.0 0.0)))))
    (let ((s (scad-sketch-unparse n)))
      (should (string-match "^foo = " s))
      (should (string-match "\\[0, 0\\]" s))
      (should (string-match "\\[10, 5\\]" s)))))

(ert-deftest ssp-unparse-array-with-radii ()
  "Array with non-zero radii includes the third component in each point."
  (let* ((n (list :type 'array :name "rb" :beg 0 :end 0
                  :points '((0.0 0.0 5.0) (10.0 0.0 3.0)))))
    (let ((s (scad-sketch-unparse n)))
      (should (string-match "\\[0, 0, 5\\]" s))
      (should (string-match "\\[10, 0, 3\\]" s)))))

(ert-deftest ssp-unparse-translate ()
  "translate node unparses with correct vector and indented child."
  (let* ((child (list :type 'circle :r 2.0 :cx 0.0 :cy 0.0 :beg 0 :end 0))
         (n     (list :type 'translate :tx 5.0 :ty -3.0 :child child :beg 0 :end 0)))
    (should (string= (scad-sketch-unparse n)
                     "translate([5, -3])\n  circle(r=2);\n"))))

(ert-deftest ssp-unparse-rotate ()
  "rotate node unparses with angle and indented child."
  (let* ((child (list :type 'square :w 5.0 :h 5.0 :x 0.0 :y 0.0 :angle 0.0
                      :beg 0 :end 0))
         (n     (list :type 'rotate :angle 90.0 :child child :beg 0 :end 0)))
    (should (string= (scad-sketch-unparse n)
                     "rotate(90)\n  square([5, 5]);\n"))))

(ert-deftest ssp-unparse-scale ()
  "scale node unparses with both factors."
  (let* ((child (list :type 'circle :r 1.0 :cx 0.0 :cy 0.0 :beg 0 :end 0))
         (n     (list :type 'scale :sx 2.0 :sy 0.5 :child child :beg 0 :end 0)))
    (let ((s (scad-sketch-unparse n)))
      (should (string-match "scale(\\[2, 0\\.5\\])" s)))))

(ert-deftest ssp-unparse-mirror ()
  "mirror node unparses with both components."
  (let* ((child (list :type 'circle :r 1.0 :cx 0.0 :cy 0.0 :beg 0 :end 0))
         (n     (list :type 'mirror :mx 1.0 :my 0.0 :child child :beg 0 :end 0)))
    (let ((s (scad-sketch-unparse n)))
      (should (string-match "mirror(\\[1, 0\\])" s)))))

(ert-deftest ssp-unparse-difference ()
  "difference node unparses with braces and indented children."
  (let* ((sq (list :type 'square :w 10.0 :h 10.0 :x 0.0 :y 0.0 :angle 0.0
                   :beg 0 :end 0))
         (ci (list :type 'circle :r 4.0 :cx 0.0 :cy 0.0 :beg 0 :end 0))
         (n  (list :type 'difference :children (list sq ci) :beg 0 :end 0)))
    (should (string= (scad-sketch-unparse n 0)
                     "difference() {\n  square([10, 10]);\n  circle(r=4);\n}\n"))))

(ert-deftest ssp-unparse-union ()
  "union node unparses with `union()' keyword."
  (let* ((ci (list :type 'circle :r 1.0 :cx 0.0 :cy 0.0 :beg 0 :end 0))
         (n  (list :type 'union :children (list ci) :beg 0 :end 0)))
    (should (string-match "^union()" (scad-sketch-unparse n 0)))))

(ert-deftest ssp-unparse-intersection ()
  "intersection node unparses with `intersection()' keyword."
  (let* ((ci (list :type 'circle :r 1.0 :cx 0.0 :cy 0.0 :beg 0 :end 0))
         (n  (list :type 'intersection :children (list ci) :beg 0 :end 0)))
    (should (string-match "^intersection()" (scad-sketch-unparse n 0)))))

(ert-deftest ssp-unparse-indent ()
  "indent parameter shifts output by 2 spaces per level."
  (let* ((n (list :type 'circle :r 1.0 :cx 0.0 :cy 0.0 :beg 0 :end 0)))
    (should (string= (scad-sketch-unparse n 0) "circle(r=1);\n"))
    (should (string= (scad-sketch-unparse n 1) "  circle(r=1);\n"))
    (should (string= (scad-sketch-unparse n 2) "    circle(r=1);\n"))))


;;;; =========================================================================
;;;; 15. scad-sketch-unparse-top-level
;;;; =========================================================================

(ert-deftest ssp-unparse-top-large-inline-polygon-stays-inline ()
  "A polygon with >4 points stays inline; no extracted assignment is generated."
  (let* ((poly (list :type 'polygon
                     :points '((0.0 0.0 0.0)(40.0 0.0 0.0)(50.0 20.0 0.0)
                               (40.0 40.0 0.0)(0.0 40.0 0.0))
                     :source nil :polyround nil :beg 0 :end 0))
         (s    (scad-sketch-unparse-top-level (list poly))))
    (should-not (string-match "_sketch" s))
    (should (string-match "polygon(\\[" s))
    (should (string-match "\\[50, 20\\]" s))))

(ert-deftest ssp-unparse-top-small-inline-polygon-stays-inline ()
  "A polygon with ≤4 points stays inline; no extracted assignment is generated."
  (let* ((poly (list :type 'polygon
                     :points '((0.0 0.0 0.0)(10.0 0.0 0.0)(5.0 8.0 0.0))
                     :source nil :polyround nil :beg 0 :end 0))
         (s    (scad-sketch-unparse-top-level (list poly))))
    (should-not (string-match "_sketch" s))
    (should (string-match "polygon(\\[" s))))

(ert-deftest ssp-unparse-top-multiple-large-polygons-stay-inline ()
  "Multiple large inline polygons stay inline and do not get generated names."
  (let* ((mk-poly (lambda (x)
                    (list :type 'polygon
                          :points (list (list x 0.0 0.0)(list (+ x 10) 0.0 0.0)
                                        (list (+ x 20) 10.0 0.0)(list (+ x 10) 20.0 0.0)
                                        (list x 20.0 0.0))
                          :source nil :polyround nil :beg 0 :end 0)))
         (p1 (funcall mk-poly 0.0))
         (p2 (funcall mk-poly 100.0))
         (s  (scad-sketch-unparse-top-level (list p1 p2))))
    (should-not (string-match "_sketch" s))
    (let* ((p1 (string-match "polygon(\\[" s))
           (p2 (and p1 (string-match "polygon(\\[" s (match-end 0)))))
      (should p1)
      (should p2))))

(ert-deftest ssp-unparse-top-existing-sketch-name-does-not-matter ()
  "Existing _sketch_N arrays do not trigger generated names for inline polygons."
  (let* ((arr  (list :type 'array :name "_sketch_1" :beg 0 :end 0
                     :points '((0.0 0.0 0.0))))
         (poly (list :type 'polygon
                     :points '((0.0 0.0 0.0)(40.0 0.0 0.0)(50.0 20.0 0.0)
                               (40.0 40.0 0.0)(0.0 40.0 0.0))
                     :source nil :polyround nil :beg 0 :end 0))
         (s    (scad-sketch-unparse-top-level (list arr poly))))
    (should (string-match "_sketch_1 = " s))
    (should-not (string-match "_sketch_2" s))
    (should (string-match "polygon(\\[" s))))

(ert-deftest ssp-unparse-top-polyround-inline-stays-inline ()
  "Inline polyRound polygons stay inline and keep their polyRound call."
  (let* ((poly (list :type 'polygon
                     :points '((0.0 0.0 3.0)(80.0 0.0 3.0)
                               (80.0 50.0 3.0)(0.0 50.0 3.0))
                     :source nil :polyround 32 :beg 0 :end 0))
         (s    (scad-sketch-unparse-top-level (list poly))))
    (should-not (string-match "_sketch" s))
    (should (string-match "polygon(polyRound(\\[" s))
    (should (string-match ", 32" s))))

(ert-deftest ssp-unparse-top-variable-ref-polygon-kept ()
  "A variable-ref polygon is never extracted; it just emits polygon(name)."
  (let* ((poly (list :type 'polygon :points nil :source "my_pts"
                     :polyround nil :beg 0 :end 0))
         (s    (scad-sketch-unparse-top-level (list poly))))
    (should (string= s "polygon(my_pts);\n"))
    (should-not (string-match "_sketch" s))))

(ert-deftest ssp-unparse-top-array-node-emitted ()
  "An array node in the list unparses to name = [...]."
  (let* ((arr (list :type 'array :name "pts" :beg 0 :end 0
                    :points '((0.0 0.0 0.0)(5.0 5.0 0.0))))
         (s   (scad-sketch-unparse-top-level (list arr))))
    (should (string-match "^pts = " s))))

(ert-deftest ssp-unparse-top-mixed-nodes ()
  "Mixed array + circle + polygon all appear in output."
  (let* ((arr  (list :type 'array :name "pts" :beg 0 :end 0
                     :points '((0.0 0.0 0.0)(1.0 0.0 0.0))))
         (circ (list :type 'circle :r 3.0 :cx 0.0 :cy 0.0 :beg 0 :end 0))
         (poly (list :type 'polygon
                     :points '((0.0 0.0 0.0))
                     :source nil :polyround nil :beg 0 :end 0))
         (s    (scad-sketch-unparse-top-level (list arr circ poly))))
    (should (string-match "pts = " s))
    (should (string-match "circle" s))
    (should (string-match "polygon" s))))


;;;; =========================================================================
;;;; 16. Integration: parse test.scad end-to-end
;;;; =========================================================================
(ert-deftest ssp-integration-node-count ()
  "Parsing test.scad yields exactly 33 top-level nodes.

The extra node comes from harvesting the supported child circle under the
unsupported linear_extrude wrapper."
  (ssp-test--with-scad-file
   (should (= (length nodes) 33))))

(ert-deftest ssp-integration-array-assignments ()
  "test.scad contains the expected array assignments."
  (ssp-test--with-scad-file
   (let ((arrs (ssp-test--nodes-of-type nodes 'array)))
     ;; triangle, rounded_box, dot, pts, body_pts
     (should (= (length arrs) 5))
     (let ((names (mapcar (lambda (n) (plist-get n :name)) arrs)))
       (should (member "triangle"   names))
       (should (member "rounded_box" names))
       (should (member "dot"        names))
       (should (member "pts"        names))
       (should (member "body_pts"   names))))))

(ert-deftest ssp-integration-triangle-points ()
  "The triangle array has 3 points with the correct coordinates."
  (ssp-test--with-scad-file
   (let* ((tri (cl-find-if (lambda (n)
                             (and (eq (plist-get n :type) 'array)
                                  (string= (plist-get n :name) "triangle")))
                           nodes))
          (pts (plist-get tri :points)))
     (should (= (length pts) 3))
     (should (ssp-test--approx= (nth 0 (nth 0 pts)) 0.0))
     (should (ssp-test--approx= (nth 0 (nth 1 pts)) 50.0))
     (should (ssp-test--approx= (nth 0 (nth 2 pts)) 25.0))
     (should (ssp-test--approx= (nth 1 (nth 2 pts)) 43.0)))))

(ert-deftest ssp-integration-rounded-box-radii ()
  "rounded_box stores all four radii as 5.0."
  (ssp-test--with-scad-file
   (let* ((rb  (cl-find-if (lambda (n)
                             (and (eq (plist-get n :type) 'array)
                                  (string= (plist-get n :name) "rounded_box")))
                           nodes))
          (pts (plist-get rb :points)))
     (should (= (length pts) 4))
     (dolist (p pts)
       (should (ssp-test--approx= (nth 2 p) 5.0))))))

(ert-deftest ssp-integration-circles ()
  "test.scad contains circles with radii 15, 20, 12.5, 5, 10, 5, 20, 15, 15, 25, 20, 10, 5, 15, 5."
  (ssp-test--with-scad-file
   (let* ((circs  (ssp-test--nodes-of-type nodes 'circle))
          (radii  (mapcar (lambda (n) (plist-get n :r)) circs)))
     ;; At least the three top-level ones without going into children
     (should (member 15.0 radii))
     (should (member 20.0 radii))
     (should (member 12.5 radii)))))

(ert-deftest ssp-integration-squares ()
  "test.scad contains square nodes."
  (ssp-test--with-scad-file
   (let ((squares (ssp-test--nodes-of-type nodes 'square)))
     (should (>= (length squares) 2)))))

(ert-deftest ssp-integration-texts ()
  "test.scad contains three text nodes."
  (ssp-test--with-scad-file
   (let ((texts (ssp-test--nodes-of-type nodes 'text)))
     (should (= (length texts) 3))
     (let ((strs (mapcar (lambda (n) (plist-get n :str)) texts)))
       (should (member "hello"    strs))
       (should (member "OpenSCAD" strs))
       (should (member "hi"       strs))))))

(ert-deftest ssp-integration-text-sizes ()
  "test.scad text nodes have sizes 10, 14, 8."
  (ssp-test--with-scad-file
   (let* ((texts (ssp-test--nodes-of-type nodes 'text))
          (sizes (mapcar (lambda (n) (plist-get n :size)) texts)))
     (should (member 10.0 sizes))
     (should (member 14.0 sizes))
     (should (member  8.0 sizes)))))

(ert-deftest ssp-integration-polygons ()
  "test.scad contains the expected polygon variants as top-level nodes.
The mirror([1,0]) child polygon is nested inside a mirror node, so the
flat top-level list has 6 polygon nodes (not 7)."
  (ssp-test--with-scad-file
   (let ((polys (ssp-test--nodes-of-type nodes 'polygon)))
     (should (= (length polys) 6))
     ;; variable refs present
     (let ((srcs (cl-remove nil (mapcar (lambda (n) (plist-get n :source)) polys))))
       (should (member "pts"         srcs))
       (should (member "rounded_box" srcs))
       (should (member "body_pts"    srcs)))
     ;; polyRound entries
     (let ((pr (cl-remove-if-not (lambda (n) (plist-get n :polyround)) polys)))
       (should (= (length pr) 2))))))

(ert-deftest ssp-integration-polyround-inline-fn ()
  "The inline polyRound polygon has fn=32 and 4 points with r=3."
  (ssp-test--with-scad-file
   (let* ((pr (cl-find-if (lambda (n)
                            (and (eq (plist-get n :type) 'polygon)
                                 (eql (plist-get n :polyround) 32)
                                 (null (plist-get n :source))))
                          nodes))
          (pts (plist-get pr :points)))
     (should (= (length pts) 4))
     (dolist (p pts)
       (should (ssp-test--approx= (nth 2 p) 3.0))))))

(ert-deftest ssp-integration-transforms ()
  "test.scad produces the correct set of TOP-LEVEL transform nodes.
Transforms nested inside boolean bodies (e.g. translate inside union)
or as children of other transforms are not in the flat top-level list."
  (ssp-test--with-scad-file
   (let ((xlates  (ssp-test--nodes-of-type nodes 'translate))
         (rots    (ssp-test--nodes-of-type nodes 'rotate))
         (scales  (ssp-test--nodes-of-type nodes 'scale))
         (mirrors (ssp-test--nodes-of-type nodes 'mirror)))
     ;; translate([10,20]), translate([100,50]), translate([5,-10]),
     ;; translate([200,0]) — the union-internal translate([25,0]) is nested
     (should (= (length xlates)  4))
     ;; rotate(45) and scale([2,0.5]) are top-level
     (should (= (length rots)    1))
     (should (= (length scales)  1))
     (should (= (length mirrors) 2)))))

(ert-deftest ssp-integration-translate-values ()
  "translate([10,20]) and translate([5,-10]) appear in test.scad."
  (ssp-test--with-scad-file
   (let* ((xlates (ssp-test--nodes-of-type nodes 'translate))
          (txs    (mapcar (lambda (n) (plist-get n :tx)) xlates))
          (tys    (mapcar (lambda (n) (plist-get n :ty)) xlates)))
     (should (member 10.0  txs))
     (should (member 20.0  tys))
     (should (member -10.0 tys)))))

(ert-deftest ssp-integration-nested-translate ()
  "The translate([100,50])→rotate(30)→scale([1.5,1.5])→circle(r=8) chain is intact."
  (ssp-test--with-scad-file
   (let* ((xl  (cl-find-if (lambda (n)
                             (and (eq (plist-get n :type) 'translate)
                                  (ssp-test--approx= (plist-get n :tx) 100.0)))
                           nodes))
          (rot (plist-get xl  :child))
          (scl (plist-get rot :child))
          (cir (plist-get scl :child)))
     (should (eq (plist-get rot :type) 'rotate))
     (should (ssp-test--approx= (plist-get rot :angle) 30.0))
     (should (eq (plist-get scl :type) 'scale))
     (should (ssp-test--approx= (plist-get scl :sx) 1.5))
     (should (eq (plist-get cir :type) 'circle))
     (should (ssp-test--approx= (plist-get cir :r) 8.0)))))

(ert-deftest ssp-integration-booleans ()
  "test.scad produces 4 boolean nodes: 3 difference, 1 union, 1 intersection."
  (ssp-test--with-scad-file
   (let ((diffs  (ssp-test--nodes-of-type nodes 'difference))
         (unions (ssp-test--nodes-of-type nodes 'union))
         (ints   (ssp-test--nodes-of-type nodes 'intersection)))
     (should (= (length diffs)  2))
     (should (= (length unions) 1))
     (should (= (length ints)   1)))))

(ert-deftest ssp-integration-difference-children ()
  "The 2-child difference has square and circle children."
  (ssp-test--with-scad-file
   (let* ((diff2 (cl-find-if (lambda (n)
                               (and (eq (plist-get n :type) 'difference)
                                    (= (length (plist-get n :children)) 2)))
                             nodes))
          (ch    (plist-get diff2 :children)))
     (should diff2)
     (should (eq (plist-get (nth 0 ch) :type) 'square))
     (should (eq (plist-get (nth 1 ch) :type) 'circle)))))

(ert-deftest ssp-integration-difference-three-children ()
  "The 3-child difference has union, circle, translate children."
  (ssp-test--with-scad-file
   (let* ((diff3 (cl-find-if (lambda (n)
                               (and (eq (plist-get n :type) 'difference)
                                    (= (length (plist-get n :children)) 3)))
                             nodes))
          (ch    (plist-get diff3 :children)))
     (should diff3)
     (should (eq (plist-get (nth 0 ch) :type) 'union))
     (should (eq (plist-get (nth 1 ch) :type) 'circle))
     (should (eq (plist-get (nth 2 ch) :type) 'translate)))))

(ert-deftest ssp-integration-translate-wraps-difference ()
  "translate([200,0]) wraps a difference node in test.scad."
  (ssp-test--with-scad-file
   (let* ((xl (cl-find-if (lambda (n)
                            (and (eq (plist-get n :type) 'translate)
                                 (ssp-test--approx= (plist-get n :tx) 200.0)))
                          nodes))
          (child (plist-get xl :child)))
     (should xl)
     (should (eq (plist-get child :type) 'difference))
     (should (= (length (plist-get child :children)) 2)))))

(ert-deftest ssp-integration-module-harvested ()
  "body_pts from the module body and its polygon/circle siblings are harvested."
  (ssp-test--with-scad-file
   (let* ((body (cl-find-if (lambda (n)
                              (and (eq (plist-get n :type) 'array)
                                   (string= (plist-get n :name) "body_pts")))
                            nodes)))
     (should body)
     (should (= (length (plist-get body :points)) 4)))))

(ert-deftest ssp-integration-node-at-on-file ()
  "scad-sketch-parse-node-at can locate the triangle array by its :beg offset."
  (ssp-test--with-scad-file
   (let* ((tri (cl-find-if (lambda (n)
                             (and (eq (plist-get n :type) 'array)
                                  (string= (plist-get n :name) "triangle")))
                           nodes))
          (found (scad-sketch-parse-node-at nodes (plist-get tri :beg))))
     (should found)
     (should (eq (plist-get found :type) 'array))
     (should (string= (plist-get found :name) "triangle")))))

(ert-deftest ssp-integration-walk-total-count ()
  "Walking all top-level nodes from test.scad visits 59 nodes total."
  (ssp-test--with-scad-file
   (let ((count 0))
     (dolist (n nodes)
       (scad-sketch-parse--walk n (lambda (_) (setq count (1+ count)))))
     (should (= count 59)))))

(ert-deftest ssp-integration-lookup-triangle ()
  "lookup-variable finds triangle's 3 points when called with a large before-pos."
  (ssp-test--with-scad-file
   (let* ((pts (scad-sketch-parse--lookup-variable "triangle" src 999999)))
     (should pts)
     (should (= (length pts) 3)))))

(ert-deftest ssp-integration-all-beg-le-end ()
  "Every node in the file satisfies :beg <= :end."
  (ssp-test--with-scad-file
   (dolist (n nodes)
     (scad-sketch-parse--walk
      n
      (lambda (node)
        (should (<= (plist-get node :beg) (plist-get node :end))))))))

(provide 'scad-sketch-parse-test)
;;; scad-sketch-parse-test.el ends here
