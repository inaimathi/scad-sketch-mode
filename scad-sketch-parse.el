;;; scad-sketch-parse.el --- Parser for the 2D subset of OpenSCAD -*- lexical-binding: t; -*-

;;; Commentary:

;; Recursive-descent parser for the 2D subset of OpenSCAD used by scad-sketch.
;;
;; SUPPORTED FORMS
;; ---------------
;; Top-level:
;;   name = [[x,y], ...];                   array assignment
;;   name = [[x,y,r], ...];                 array assignment with radii
;;   <shape>                                bare shape
;;
;; Shapes:
;;   polygon([[x,y], ...])                  inline polygon
;;   polygon(name)                          variable-ref polygon
;;   polygon(polyRound(pts, fn))            polyRound polygon
;;   circle(r=N) | circle(d=N) | circle(N)
;;   square([W, H]) | square([W,H], center=true)
;;   text("str") | text("str", size=N, ...)
;;   difference() { <shape>+ }
;;   union()      { <shape>+ }
;;   intersection(){ <shape>+ }
;;   translate([tx, ty]) <shape>
;;   rotate(angle)       <shape>
;;   scale([sx, sy])     <shape>
;;   mirror([mx, my])    <shape>
;;
;; PARSE TREE NODES
;; ----------------
;; Every node is a plist with at least :type :beg :end.
;; Positions are offsets into the original source string (0-based).
;;
;; (:type array       :name STR :points LIST :beg N :end N)
;; (:type polygon     :points LIST-OR-NIL :source NAME-OR-NIL
;;                    :polyround FN-OR-NIL :beg N :end N)
;; (:type circle      :r NUM :cx 0 :cy 0 :beg N :end N)
;; (:type square      :x NUM :y NUM :w NUM :h NUM :angle 0 :beg N :end N)
;; (:type text        :str STR :x 0 :y 0 :size NUM :beg N :end N)
;; (:type difference  :children LIST :beg N :end N)
;; (:type union       :children LIST :beg N :end N)
;; (:type intersection :children LIST :beg N :end N)
;; (:type translate   :tx NUM :ty NUM :child NODE :beg N :end N)
;; (:type rotate      :angle NUM :child NODE :beg N :end N)
;; (:type scale       :sx NUM :sy NUM :child NODE :beg N :end N)
;; (:type mirror      :mx NUM :my NUM :child NODE :beg N :end N)
;;
;; CURSOR DISPATCH
;; ---------------
;; Use `scad-sketch-parse-node-at' to find the deepest node whose
;; [beg, end] range contains a buffer position.  The result drives
;; which sub-tree scad-sketch-at-point edits.

;;; Code:

(require 'cl-lib)

;;;; Token types

(defconst scad-sketch-parse--token-re
  (rx (or
       ;; Numbers — must come before identifiers to catch leading sign
       (group-n 1 (seq (? (any "+-"))
                       (or (seq (+ digit) (? ".") (* digit))
                           (seq (* digit) "." (+ digit)))
                       (? (any "eE") (? (any "+-")) (+ digit))))
       ;; Strings
       (group-n 2 (seq ?\" (* (or (seq ?\\ anything) (not (any "\"\\")))) ?\"))
       ;; Angle-bracket include/use paths.  Keep these as a single token so
       ;; include/use without a trailing semicolon doesn't eat the next form.
       (group-n 3 (seq ?< (* (not (any ">"))) ?>))
       ;; Identifiers
       (group-n 4 (seq (any "A-Za-z_$") (* (any "A-Za-z0-9_$"))))
       ;; Punctuation
       (group-n 5 (any "(){}[],;="))
       ;; Whitespace (to skip)
       (group-n 6 (+ (any " \t\r\n")))))
  "Regexp matching one SCAD token.  Groups: 1=num 2=str 3=path 4=id 5=punct 6=ws.")

(defun scad-sketch-parse--strip-comments (s)
  "Return S with // line comments and /* */ block comments removed."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (re-search-forward "\\(//.*$\\)\\|\\(/\\*\\(?:[^*]\\|\\*[^/]\\)*\\*/\\)" nil t)
      (replace-match (make-string (- (match-end 0) (match-beginning 0)) ?\s)))
    (buffer-string)))

(defun scad-sketch-parse--tokenize (source)
  "Tokenize SOURCE string.
Returns a vector of (TYPE VALUE START END) where TYPE is a symbol
`num', `str', `path', `id', or `punct', and positions are 0-based."
  (let ((clean (scad-sketch-parse--strip-comments source))
        tokens)
    (let ((pos 0)
          (len (length source)))
      (while (< pos len)
        (if (string-match scad-sketch-parse--token-re clean pos)
            (let ((start (match-beginning 0))
                  (end   (match-end 0)))
              (cond
               ((match-beginning 1)
                (push (list 'num (match-string 1 clean) start end) tokens))
               ((match-beginning 2)
                (push (list 'str (match-string 2 clean) start end) tokens))
               ((match-beginning 3)
                (push (list 'path (match-string 3 clean) start end) tokens))
               ((match-beginning 4)
                (push (list 'id  (match-string 4 clean) start end) tokens))
               ((match-beginning 5)
                (push (list 'punct (match-string 5 clean) start end) tokens))
               ;; group 6 = whitespace: skip
               )
              (setq pos end))
          (setq pos (1+ pos)))))
    (vconcat (nreverse tokens))))

;;;; Parser state

;; We pass parser state as a vector [tokens pos] for efficiency.
;; Accessors below hide the representation.

(defun scad-sketch-parse--make (tokens)
  "Create parser state for TOKENS vector."
  (vector tokens 0))

(defmacro scad-sketch-parse--tokens (ps)  `(aref ,ps 0))
(defmacro scad-sketch-parse--pos    (ps)  `(aref ,ps 1))
(defmacro scad-sketch-parse--set-pos (ps n) `(aset ,ps 1 ,n))

(defun scad-sketch-parse--peek (ps)
  "Return current token (TYPE VAL START END) or nil at end."
  (let ((tokens (scad-sketch-parse--tokens ps))
        (pos    (scad-sketch-parse--pos ps)))
    (when (< pos (length tokens))
      (aref tokens pos))))

(defun scad-sketch-parse--peek-val (ps)
  "Return current token value string, or nil."
  (let ((tok (scad-sketch-parse--peek ps)))
    (when tok (nth 1 tok))))

(defun scad-sketch-parse--peek-type (ps)
  "Return current token type symbol, or nil."
  (let ((tok (scad-sketch-parse--peek ps)))
    (when tok (nth 0 tok))))

(defun scad-sketch-parse--cur-start (ps)
  "Return start position of current token, or end-of-tokens."
  (let ((tok (scad-sketch-parse--peek ps)))
    (if tok (nth 2 tok)
      (let ((tokens (scad-sketch-parse--tokens ps)))
        (if (> (length tokens) 0)
            (nth 3 (aref tokens (1- (length tokens))))
          0)))))

(defun scad-sketch-parse--prev-end (ps)
  "Return end position of the most recently consumed token."
  (let ((pos (scad-sketch-parse--pos ps))
        (tokens (scad-sketch-parse--tokens ps)))
    (if (> pos 0)
        (nth 3 (aref tokens (1- pos)))
      0)))

(defun scad-sketch-parse--consume (ps &optional expected-val expected-type)
  "Consume and return the current token value.
Signal an error if EXPECTED-VAL or EXPECTED-TYPE don't match."
  (let ((tok (scad-sketch-parse--peek ps)))
    (unless tok
      (user-error "scad-sketch parser: unexpected end of input"))
    (when (and expected-val (not (string= (nth 1 tok) expected-val)))
      (user-error "scad-sketch parser: expected %S, got %S" expected-val (nth 1 tok)))
    (when (and expected-type (not (eq (nth 0 tok) expected-type)))
      (user-error "scad-sketch parser: expected type %s, got %S (%S)"
                  expected-type (nth 0 tok) (nth 1 tok)))
    (scad-sketch-parse--set-pos ps (1+ (scad-sketch-parse--pos ps)))
    (nth 1 tok)))

(defun scad-sketch-parse--try-consume (ps val)
  "Consume and return t if current token value equals VAL, else return nil."
  (when (equal (scad-sketch-parse--peek-val ps) val)
    (scad-sketch-parse--consume ps val)
    t))

(defun scad-sketch-parse--nth-token (ps n)
  "Return the token N positions after the current one, or nil."
  (let* ((tokens (scad-sketch-parse--tokens ps))
         (idx (+ (scad-sketch-parse--pos ps) n)))
    (when (< idx (length tokens))
      (aref tokens idx))))

(defun scad-sketch-parse--nth-val (ps n)
  "Return the value of the token N positions after the current one, or nil."
  (let ((tok (scad-sketch-parse--nth-token ps n)))
    (when tok (nth 1 tok))))

(defun scad-sketch-parse--skip-balanced (ps open close)
  "Skip a balanced expression that starts with OPEN and ends with CLOSE."
  (scad-sketch-parse--consume ps open)
  (let ((depth 1))
    (while (and (> depth 0) (scad-sketch-parse--peek ps))
      (let ((v (scad-sketch-parse--peek-val ps)))
        (cond ((equal v open)  (setq depth (1+ depth)))
              ((equal v close) (setq depth (1- depth))))
        (scad-sketch-parse--consume ps)))))

(defun scad-sketch-parse--skip-value (ps)
  "Skip one generic SCAD value/expression in an argument list."
  (let ((v (scad-sketch-parse--peek-val ps)))
    (cond
     ((equal v "(") (scad-sketch-parse--skip-balanced ps "(" ")"))
     ((equal v "[") (scad-sketch-parse--skip-balanced ps "[" "]"))
     ((equal v "{") (scad-sketch-parse--skip-balanced ps "{" "}"))
     ((scad-sketch-parse--peek ps) (scad-sketch-parse--consume ps)))))

(defun scad-sketch-parse--skip-keyword-or-value (ps)
  "Skip KEY=VALUE or a single positional value."
  (if (and (eq (scad-sketch-parse--peek-type ps) 'id)
           (equal (scad-sketch-parse--nth-val ps 1) "="))
      (progn
        (scad-sketch-parse--consume ps nil 'id)
        (scad-sketch-parse--consume ps "=")
        (scad-sketch-parse--skip-value ps))
    (scad-sketch-parse--skip-value ps)))

(defun scad-sketch-parse--skip-rest-args (ps)
  "Skip comma-separated arguments until the current closing parenthesis."
  (while (and (scad-sketch-parse--peek ps)
              (not (equal (scad-sketch-parse--peek-val ps) ")")))
    (scad-sketch-parse--try-consume ps ",")
    (unless (equal (scad-sketch-parse--peek-val ps) ")")
      (scad-sketch-parse--skip-keyword-or-value ps))))

;;;; Grammar rules

(defun scad-sketch-parse--num (ps)
  "Parse and return a number."
  (string-to-number (scad-sketch-parse--consume ps nil 'num)))

(defun scad-sketch-parse--str (ps)
  "Parse and return a string literal (strips surrounding quotes)."
  (let ((s (scad-sketch-parse--consume ps nil 'str)))
    (substring s 1 (1- (length s)))))

(defun scad-sketch-parse--bool (ps)
  "Parse and return a boolean (true/false identifier)."
  (string= "true" (scad-sketch-parse--consume ps nil 'id)))

(defun scad-sketch-parse--point (ps)
  "Parse [x, y] or [x, y, r] returning a (x y r) list."
  (scad-sketch-parse--consume ps "[")
  (let ((x (scad-sketch-parse--num ps)))
    (scad-sketch-parse--consume ps ",")
    (let ((y (scad-sketch-parse--num ps)))
      (let ((r 0.0))
        (when (equal (scad-sketch-parse--peek-val ps) ",")
          (scad-sketch-parse--consume ps ",")
          (setq r (scad-sketch-parse--num ps)))
        (scad-sketch-parse--consume ps "]")
        (list (float x) (float y) (float r))))))

(defun scad-sketch-parse--array (ps)
  "Parse [[x,y,r?], ...] returning a list of (x y r) triples."
  (scad-sketch-parse--consume ps "[")
  (let (points)
    (while (not (equal (scad-sketch-parse--peek-val ps) "]"))
      (push (scad-sketch-parse--point ps) points)
      (scad-sketch-parse--try-consume ps ","))
    (scad-sketch-parse--consume ps "]")
    (nreverse points)))

(defun scad-sketch-parse--poly-arg (ps)
  "Parse the point argument to polygon().
Returns (points source-name polyround-fn).  Supports bare positional points,
variable references, polyRound(...), and the common points=... keyword form."
  (cond
   ;; polygon(points=...)
   ((and (eq (scad-sketch-parse--peek-type ps) 'id)
         (string= (scad-sketch-parse--peek-val ps) "points")
         (equal (scad-sketch-parse--nth-val ps 1) "="))
    (scad-sketch-parse--consume ps "points")
    (scad-sketch-parse--consume ps "=")
    (scad-sketch-parse--poly-arg ps))
   ((equal (scad-sketch-parse--peek-val ps) "[")
    (list (scad-sketch-parse--array ps) nil nil))
   ((and (eq (scad-sketch-parse--peek-type ps) 'id)
         (string= (scad-sketch-parse--peek-val ps) "polyRound"))
    (scad-sketch-parse--consume ps "polyRound")
    (scad-sketch-parse--consume ps "(")
    (let* ((pts (if (equal (scad-sketch-parse--peek-val ps) "[")
                    (scad-sketch-parse--array ps)
                  nil))
           (name (when (null pts)
                   (scad-sketch-parse--consume ps nil 'id))))
      (scad-sketch-parse--consume ps ",")
      (let ((fn (scad-sketch-parse--num ps)))
        (scad-sketch-parse--consume ps ")")
        (list pts name fn))))
   ((eq (scad-sketch-parse--peek-type ps) 'id)
    (list nil (scad-sketch-parse--consume ps nil 'id) nil))
   (t
    (user-error "scad-sketch parser: unexpected polygon argument: %S"
                (scad-sketch-parse--peek-val ps)))))

(defun scad-sketch-parse--circle-args (ps)
  "Parse circle(...) args and return a radius.
Handles circle(N), circle(r=N), circle(d=N), and ignores extra keyword
arguments such as `$fn'.  Leaves point just before the closing paren."
  (let ((r nil))
    (while (and (scad-sketch-parse--peek ps)
                (not (equal (scad-sketch-parse--peek-val ps) ")")))
      (cond
       ;; Bare radius: circle(15)
       ((and (null r) (eq (scad-sketch-parse--peek-type ps) 'num))
        (setq r (float (scad-sketch-parse--num ps))))
       ;; r= or d=
       ((and (eq (scad-sketch-parse--peek-type ps) 'id)
             (member (scad-sketch-parse--peek-val ps) '("r" "d"))
             (equal (scad-sketch-parse--nth-val ps 1) "="))
        (let ((kw (scad-sketch-parse--consume ps nil 'id)))
          (scad-sketch-parse--consume ps "=")
          (let ((v (scad-sketch-parse--num ps)))
            (setq r (if (string= kw "d") (/ v 2.0) (float v))))))
       (t
        (scad-sketch-parse--skip-keyword-or-value ps)))
      (scad-sketch-parse--try-consume ps ","))
    (unless r
      (user-error "scad-sketch parser: circle() requires r, d, or bare radius"))
    r))

(defun scad-sketch-parse--text-params (ps)
  "Parse optional keyword parameters for text().  Returns alist."
  (let (params)
    (while (equal (scad-sketch-parse--peek-val ps) ",")
      (scad-sketch-parse--consume ps ",")
      (when (not (equal (scad-sketch-parse--peek-val ps) ")"))
        (if (eq (scad-sketch-parse--peek-type ps) 'id)
            (let ((key (scad-sketch-parse--consume ps nil 'id)))
              (when (equal (scad-sketch-parse--peek-val ps) "=")
                (scad-sketch-parse--consume ps "=")
                (let ((val (cond
                            ((eq (scad-sketch-parse--peek-type ps) 'num)
                             (scad-sketch-parse--num ps))
                            ((eq (scad-sketch-parse--peek-type ps) 'str)
                             (scad-sketch-parse--str ps))
                            ((eq (scad-sketch-parse--peek-type ps) 'id)
                             (scad-sketch-parse--consume ps nil 'id)))))
                  (push (cons (intern key) val) params))))
          ;; positional arg — skip
          (cond
           ((eq (scad-sketch-parse--peek-type ps) 'num) (scad-sketch-parse--num ps))
           ((eq (scad-sketch-parse--peek-type ps) 'str) (scad-sketch-parse--str ps))))))
    (nreverse params)))

(defun scad-sketch-parse--2d-vec (ps)
  "Parse [x, y] returning (x . y)."
  (scad-sketch-parse--consume ps "[")
  (let ((x (scad-sketch-parse--num ps)))
    (scad-sketch-parse--consume ps ",")
    (let ((y (scad-sketch-parse--num ps)))
      (scad-sketch-parse--consume ps "]")
      (cons (float x) (float y)))))

(defun scad-sketch-parse--shape (ps)
  "Parse one shape node."
  (let ((beg (scad-sketch-parse--cur-start ps))
        (v   (scad-sketch-parse--peek-val ps)))
    (cond
     ;; Compositions
     ((member v '("difference" "union" "intersection"))
      (scad-sketch-parse--composition ps beg))

     ;; Transforms
     ((member v '("translate" "rotate" "scale" "mirror"))
      (scad-sketch-parse--transform ps beg))

     ;; polygon
     ((string= v "polygon")
      (scad-sketch-parse--consume ps "polygon")
      (scad-sketch-parse--consume ps "(")
      (let* ((arg (scad-sketch-parse--poly-arg ps)))
        ;; Ignore optional polygon parameters we don't model, such as paths=...
        ;; and convexity=..., while keeping the editable point source.
        (scad-sketch-parse--skip-rest-args ps)
        (scad-sketch-parse--consume ps ")")
        (scad-sketch-parse--try-consume ps ";")
        (list :type 'polygon :beg beg :end (scad-sketch-parse--prev-end ps)
              :points (nth 0 arg) :source (nth 1 arg) :polyround (nth 2 arg))))

     ;; circle
     ((string= v "circle")
      (scad-sketch-parse--consume ps "circle")
      (scad-sketch-parse--consume ps "(")
      (let ((r (scad-sketch-parse--circle-args ps)))
        (scad-sketch-parse--consume ps ")")
        (scad-sketch-parse--try-consume ps ";")
        (list :type 'circle :beg beg :end (scad-sketch-parse--prev-end ps)
              :r r :cx 0.0 :cy 0.0)))

     ;; square
     ((string= v "square")
      (scad-sketch-parse--consume ps "square")
      (scad-sketch-parse--consume ps "(")
      (let ((dims (scad-sketch-parse--2d-vec ps)))
        (let ((w (car dims))
              (h (cdr dims))
              (center nil))
          (when (equal (scad-sketch-parse--peek-val ps) ",")
            (scad-sketch-parse--consume ps ",")
            (when (and (eq (scad-sketch-parse--peek-type ps) 'id)
                       (string= (scad-sketch-parse--peek-val ps) "center")
                       (equal (scad-sketch-parse--nth-val ps 1) "="))
              (scad-sketch-parse--consume ps "center")
              (scad-sketch-parse--consume ps "=")
              (setq center (scad-sketch-parse--bool ps)))
            (scad-sketch-parse--skip-rest-args ps))
          (scad-sketch-parse--consume ps ")")
          (scad-sketch-parse--try-consume ps ";")
          (list :type 'square
                :beg beg
                :end (scad-sketch-parse--prev-end ps)
                :x (if center (/ (- w) 2.0) 0.0)
                :y (if center (/ (- h) 2.0) 0.0)
                :w (float w)
                :h (float h)
                :angle 0.0
                :center center))))

     ;; text
     ((string= v "text")
      (scad-sketch-parse--consume ps "text")
      (scad-sketch-parse--consume ps "(")
      (let* ((str    (scad-sketch-parse--str ps))
             (params (scad-sketch-parse--text-params ps))
             (size   (or (cdr (assq 'size params)) 10.0))
             (font   (cdr (assq 'font params))))
        (scad-sketch-parse--consume ps ")")
        (scad-sketch-parse--try-consume ps ";")
        (list :type 'text
              :beg beg
              :end (scad-sketch-parse--prev-end ps)
              :str str
              :x 0.0
              :y 0.0
              :size (float size)
              :font font
              :angle 0.0)))

     (t
      (user-error "scad-sketch parser: unsupported shape form: %S" v)))))

(defun scad-sketch-parse--composition (ps beg)
  "Parse a composition (difference/union/intersection) node."
  (let ((op (intern (scad-sketch-parse--consume ps nil 'id))))
    (scad-sketch-parse--consume ps "(")
    (scad-sketch-parse--consume ps ")")
    (scad-sketch-parse--consume ps "{")
    (let (children)
      (while (not (equal (scad-sketch-parse--peek-val ps) "}"))
        (push (scad-sketch-parse--shape ps) children))
      (scad-sketch-parse--consume ps "}")
      (scad-sketch-parse--try-consume ps ";")
      (list :type op :beg beg :end (scad-sketch-parse--prev-end ps)
            :children (nreverse children)))))

(defun scad-sketch-parse--transform (ps beg)
  "Parse a transform (translate/rotate/scale/mirror) node."
  (let ((op (scad-sketch-parse--consume ps nil 'id)))
    (scad-sketch-parse--consume ps "(")
    (let ((node
           (cond
            ((string= op "translate")
             (let ((v (scad-sketch-parse--2d-vec ps)))
               (scad-sketch-parse--consume ps ")")
               (let ((child (scad-sketch-parse--shape ps)))
                 (list :type 'translate :beg beg :end (scad-sketch-parse--prev-end ps)
                       :tx (car v) :ty (cdr v) :child child))))
            ((string= op "rotate")
             (let ((angle (scad-sketch-parse--num ps)))
               (scad-sketch-parse--consume ps ")")
               (let ((child (scad-sketch-parse--shape ps)))
                 (list :type 'rotate :beg beg :end (scad-sketch-parse--prev-end ps)
                       :angle (float angle) :child child))))
            ((string= op "scale")
             (let ((v (scad-sketch-parse--2d-vec ps)))
               (scad-sketch-parse--consume ps ")")
               (let ((child (scad-sketch-parse--shape ps)))
                 (list :type 'scale :beg beg :end (scad-sketch-parse--prev-end ps)
                       :sx (car v) :sy (cdr v) :child child))))
            ((string= op "mirror")
             (let ((v (scad-sketch-parse--2d-vec ps)))
               (scad-sketch-parse--consume ps ")")
               (let ((child (scad-sketch-parse--shape ps)))
                 (list :type 'mirror :beg beg :end (scad-sketch-parse--prev-end ps)
                       :mx (car v) :my (cdr v) :child child)))))))
      node)))

(defun scad-sketch-parse--skip-to-brace-or-semi (ps)
  "Skip tokens up to and including the next `;\' or `{\', leaving us positioned
just after the semicolon (statement boundary) or just before the `{\' body.
Returns :semi or :brace to indicate which was found."
  (let ((done nil) (result nil))
    (while (and (not done) (scad-sketch-parse--peek ps))
      (let ((v (scad-sketch-parse--peek-val ps)))
        (cond
         ((equal v ";")
          (scad-sketch-parse--consume ps)
          (setq result :semi done t))
         ((equal v "{")
          (setq result :brace done t))  ; leave { unconsumed
         ((equal v "(")
          (scad-sketch-parse--skip-balanced ps "(" ")"))
         ((equal v "[")
          (scad-sketch-parse--skip-balanced ps "[" "]"))
         (t (scad-sketch-parse--consume ps)))))
    result))

;;;; Scope bookkeeping

(defvar scad-sketch-parse--collector nil
  "Dynamic variable: a function called with each harvested node, or nil.
Used by parser helpers to collect nodes from module/function bodies into the
flat parse result.")

(defvar scad-sketch-parse--scope nil
  "Dynamic variable holding the current lexical-ish SCAD scope.
The list is stored innermost first while parsing; nodes receive :scope in
outermost-first order.")

(defun scad-sketch-parse--current-scope ()
  "Return the current node scope in outermost-first order."
  (reverse scad-sketch-parse--scope))

(defun scad-sketch-parse--stamp-scope (node)
  "Attach the current :scope to NODE and all descendants."
  (when node
    (setq node (plist-put node :scope (scad-sketch-parse--current-scope)))
    (cond
     ((plist-get node :children)
      (setq node (plist-put node :children
                            (mapcar #'scad-sketch-parse--stamp-scope
                                    (plist-get node :children)))))
     ((plist-get node :child)
      (setq node (plist-put node :child
                            (scad-sketch-parse--stamp-scope
                             (plist-get node :child))))))
    node))

(defun scad-sketch-parse--scope-name-for-form (ps beg)
  "Return a stable-ish scope name for the current module/function form."
  (let* ((kind (or (scad-sketch-parse--peek-val ps) "scope"))
         (next (scad-sketch-parse--nth-token ps 1))
         (name (when (and next (eq (nth 0 next) 'id))
                 (nth 1 next))))
    (format "%s:%s@%d" kind (or name "anonymous") beg)))

(defun scad-sketch-parse--skip-brace-block (ps)
  "Skip the balanced brace block at point."
  (when (equal (scad-sketch-parse--peek-val ps) "{")
    (scad-sketch-parse--skip-balanced ps "{" "}")))

(defun scad-sketch-parse--skip-scoped-form (ps scope-name &optional collector)
  "Skip a module/function form, harvesting its body under SCOPE-NAME."
  (let ((boundary (scad-sketch-parse--skip-to-brace-or-semi ps)))
    (when (eq boundary :brace)
      (if collector
          (let ((scad-sketch-parse--scope
                 (cons scope-name scad-sketch-parse--scope)))
            (scad-sketch-parse--descend-block ps collector))
        (scad-sketch-parse--skip-brace-block ps)))))

(defun scad-sketch-parse--skip-include-or-use (ps)
  "Skip an include/use directive, with or without a trailing semicolon."
  (scad-sketch-parse--consume ps nil 'id)
  (when (eq (scad-sketch-parse--peek-type ps) 'path)
    (scad-sketch-parse--consume ps nil 'path))
  (scad-sketch-parse--try-consume ps ";"))

(defun scad-sketch-parse--descend-block (ps collector)
  "Parse the `{ ... }\' block at current position, calling COLLECTOR on each
recognized top-level form found inside.  Unknown forms are skipped.
Used to extract array assignments and 2D shapes from module/function bodies."
  (scad-sketch-parse--consume ps "{")
  (while (and (scad-sketch-parse--peek ps)
              (not (equal (scad-sketch-parse--peek-val ps) "}")))
    (let ((node (scad-sketch-parse--top-level-form ps)))
      (when node (funcall collector node))))
  (when (equal (scad-sketch-parse--peek-val ps) "}")
    (scad-sketch-parse--consume ps "}")))

(defun scad-sketch-parse--skip-unknown-form (ps &optional collector)
  "Skip an unknown form.
If COLLECTOR is non-nil, descend into a brace block and collect recognized
forms.  Callers should pass COLLECTOR only for module/function bodies; 3D
wrappers and other unknown constructs should normally be skipped whole."
  (let ((boundary (scad-sketch-parse--skip-to-brace-or-semi ps)))
    (when (eq boundary :brace)
      (if collector
          (scad-sketch-parse--descend-block ps collector)
        (scad-sketch-parse--skip-brace-block ps)))))

(defun scad-sketch-parse--top-level-form (ps)
  "Parse one top-level form: array assignment, known 2D shape, or skip unknown.
Returns a node plist or nil (for skipped forms like include/use/module)."
  (let ((beg (scad-sketch-parse--cur-start ps))
        (v   (scad-sketch-parse--peek-val ps))
        (typ (scad-sketch-parse--peek-type ps)))
    (cond
     ;; name = [ ... ] ;  — array assignment (look-ahead: id = [)
     ((and (eq typ 'id)
           (let ((tokens  (scad-sketch-parse--tokens ps))
                 (cur-pos (scad-sketch-parse--pos ps)))
             (and (< (1+ cur-pos) (length tokens))
                  (equal (nth 1 (aref tokens (1+ cur-pos))) "=")
                  (< (+ 2 cur-pos) (length tokens))
                  (equal (nth 1 (aref tokens (+ 2 cur-pos))) "["))))
      (let ((name (scad-sketch-parse--consume ps nil 'id)))
        (scad-sketch-parse--consume ps "=")
        (let ((points (scad-sketch-parse--array ps)))
          (scad-sketch-parse--try-consume ps ";")
          (scad-sketch-parse--stamp-scope
           (list :type 'array :beg beg :end (scad-sketch-parse--prev-end ps)
                 :name name :points points)))))
     ;; include/use <...> may omit the semicolon in typical OpenSCAD style.
     ((and (eq typ 'id) (member v '("include" "use")))
      (scad-sketch-parse--skip-include-or-use ps)
      nil)
     ;; Module/function bodies are the only unknown brace forms we descend into.
     ((and (eq typ 'id) (member v '("module" "function")))
      (let ((scope-name (scad-sketch-parse--scope-name-for-form ps beg)))
        (scad-sketch-parse--skip-scoped-form ps scope-name scad-sketch-parse--collector))
      nil)
     ;; Known 2D shape or composition keyword.
     ((and (eq typ 'id)
           (member v '("polygon" "circle" "square" "text"
                        "difference" "union" "intersection"
                        "translate" "rotate" "scale" "mirror")))
      (condition-case nil
          (scad-sketch-parse--stamp-scope (scad-sketch-parse--shape ps))
        (user-error
         ;; If a nominally 2D transform has unsupported arguments, skip that
         ;; wrapper rather than interpreting it as a 2D edit operation.
         (scad-sketch-parse--skip-unknown-form ps)
         nil)))
     ;; Everything else: 3D primitives, scalar assignments, unsupported control
     ;; flow, etc.  Skip whole; do not harvest children from 3D wrappers.
     (t
      (scad-sketch-parse--skip-unknown-form ps)
      nil))))

;;;; Public API

(defun scad-sketch-parse (source)
  "Parse SOURCE string into a flat list of AST nodes.
Each node is a plist with at minimum :type :beg :end.
Positions are 0-based offsets into SOURCE.

Unknown top-level forms (include, use, 3D primitives) are silently skipped.
Module and function bodies are DESCENDED INTO: any array assignments or
known 2D shapes inside a module body are included in the result list as if
they were top-level.  This allows editing arrays defined inside modules."
  (let* ((tokens (scad-sketch-parse--tokenize source))
         (ps     (scad-sketch-parse--make tokens))
         nodes)
    (let ((scad-sketch-parse--collector (lambda (n) (push n nodes)))
          (scad-sketch-parse--scope nil))
      (while (> (length (scad-sketch-parse--tokens ps))
                (scad-sketch-parse--pos ps))
        (let ((node (scad-sketch-parse--top-level-form ps)))
          (when node (push node nodes)))))
    (nreverse nodes)))

(defun scad-sketch-parse-node-at (nodes pos)
  "Find the deepest node in NODES whose [:beg, :end] range contains POS.
NODES may be a single node plist or a list of them.
Returns the node plist, or nil if POS is not inside any node."
  (when (and nodes (listp nodes))
    ;; If called with a list of nodes, check each; if with a single node plist
    ;; (i.e. first element is a keyword), treat it as one node.
    (if (keywordp (car nodes))
        ;; Single node
        (let ((node nodes))
          (when (and (<= (plist-get node :beg) pos)
                     (<= pos (plist-get node :end)))
            ;; Check children for a deeper match
            (let ((deeper
                   (cond
                    ((plist-get node :children)
                     (cl-some (lambda (c) (scad-sketch-parse-node-at c pos))
                              (plist-get node :children)))
                    ((plist-get node :child)
                     (scad-sketch-parse-node-at (plist-get node :child) pos)))))
              (or deeper node))))
      ;; List of nodes
      (cl-some (lambda (n) (scad-sketch-parse-node-at n pos)) nodes))))

(defun scad-sketch-parse--node-children (node)
  "Return the direct child nodes of NODE as a list."
  (cond
   ((plist-get node :children) (plist-get node :children))
   ((plist-get node :child)    (list (plist-get node :child)))
   (t nil)))

(defun scad-sketch-parse--walk (node fn)
  "Call FN on NODE and every descendant, depth-first."
  (funcall fn node)
  (dolist (child (scad-sketch-parse--node-children node))
    (scad-sketch-parse--walk child fn)))

(defun scad-sketch-parse--path-to (nodes pos)
  "Return the path (list of nodes, outermost first) from NODES to the deepest
node containing POS, or nil if POS is outside all nodes."
  (when (and nodes (listp nodes))
    (if (keywordp (car nodes))
        ;; Single node
        (let ((node nodes))
          (when (and (<= (plist-get node :beg) pos)
                     (<= pos (plist-get node :end)))
            (let* ((children (scad-sketch-parse--node-children node))
                   (child-path (cl-some (lambda (c)
                                          (scad-sketch-parse--path-to c pos))
                                        children)))
              (cons node (or child-path nil)))))
      ;; List of nodes
      (cl-some (lambda (n) (scad-sketch-parse--path-to n pos)) nodes))))

;;;; Scope-aware variable lookup

(defun scad-sketch-parse--scope-prefix-p (prefix scope)
  "Return non-nil if PREFIX is a scope prefix of SCOPE."
  (let ((ok t))
    (while (and ok prefix scope)
      (unless (equal (car prefix) (car scope))
        (setq ok nil))
      (setq prefix (cdr prefix)
            scope  (cdr scope)))
    (and ok (null prefix))))

(defun scad-sketch-parse--scope-ancestry (scope)
  "Return SCOPE, its parents, and nil, in nearest-to-farthest order."
  (let ((acc nil)
        (cur scope))
    (while cur
      (push cur acc)
      (setq cur (butlast cur)))
    (nreverse (cons nil acc))))

(defun scad-sketch-parse--context-scope-at (nodes pos)
  "Return the scope of the deepest node at POS, or the nearest prior node."
  (let ((node (scad-sketch-parse-node-at nodes pos))
        prior)
    (unless node
      (dolist (n nodes)
        (when (and (plist-get n :end)
                   (<= (plist-get n :end) pos)
                   (or (null prior)
                       (> (plist-get n :end) (plist-get prior :end))))
          (setq prior n)))
      (setq node prior))
    (plist-get node :scope)))

(defun scad-sketch-parse--lookup-variable (name source before-pos)
  "Find NAME as an in-scope array assignment in SOURCE before BEFORE-POS.
Returns a list of [x y r] points, or nil if not found.

Scope rule: first search the current module/function scope, then parents,
then top level.  Within a scope, the closest preceding assignment wins."
  (let* ((nodes (scad-sketch-parse source))
         (scope (scad-sketch-parse--context-scope-at nodes before-pos))
         (ancestry (scad-sketch-parse--scope-ancestry scope))
         result)
    (while (and ancestry (null result))
      (let* ((target-scope (car ancestry))
             (candidates
              (cl-remove-if-not
               (lambda (n)
                 (and (eq (plist-get n :type) 'array)
                      (string= (plist-get n :name) name)
                      (equal (plist-get n :scope) target-scope)
                      (< (plist-get n :beg) before-pos)))
               nodes)))
        (when candidates
          (setq result
                (car (last (sort candidates
                                 (lambda (a b)
                                   (< (plist-get a :beg) (plist-get b :beg)))))))))
      (setq ancestry (cdr ancestry)))
    (when result
      (plist-get result :points))))

(defun scad-sketch-parse--parse-array-text (text)
  "Parse TEXT as a bare array literal `[[x,y,...],...]'.
Returns list of [x y r] triples."
  (condition-case nil
      (let* ((tokens (scad-sketch-parse--tokenize text))
             (ps     (scad-sketch-parse--make tokens)))
        (scad-sketch-parse--array ps))
    (error nil)))

;;;; Unparsing (AST → SCAD source)

(defconst scad-sketch-inline-threshold 4
  "Polygons with this many points or fewer are inlined in emitted source.
Polygons with more points are extracted to named array assignments.

This is deliberately fixed so sketch output canonicalizes predictably.")

(defun scad-sketch-parse--fmt-num (n)
  "Format number N compactly for OpenSCAD output."
  (let ((x (float n)))
    (if (< (abs (- x (round x))) 0.000001)
        (number-to-string (round x))
      (let ((s (format "%.4f" x)))
        (setq s (replace-regexp-in-string "0+\\'" "" s))
        (setq s (replace-regexp-in-string "\\.\\'" "" s))
        (if (or (string= s "-0") (string= s "")) "0" s)))))

(defun scad-sketch-parse--fmt-point (p use-r)
  "Format point P as [x, y] or [x, y, r]."
  (if use-r
      (format "[%s, %s, %s]"
              (scad-sketch-parse--fmt-num (nth 0 p))
              (scad-sketch-parse--fmt-num (nth 1 p))
              (scad-sketch-parse--fmt-num (nth 2 p)))
    (format "[%s, %s]"
            (scad-sketch-parse--fmt-num (nth 0 p))
            (scad-sketch-parse--fmt-num (nth 1 p)))))

(defun scad-sketch-parse--fmt-array (points)
  "Format POINTS as a multi-line array literal."
  (let* ((use-r (cl-some (lambda (p) (and (nth 2 p) (> (nth 2 p) 0))) points))
         (lines (mapcar (lambda (p) (concat "  " (scad-sketch-parse--fmt-point p use-r)))
                        points)))
    (concat "[\n" (mapconcat #'identity lines ",\n") (if lines "\n" "") "]")))

(defun scad-sketch-parse--fmt-inline-array (points)
  "Format POINTS as a compact single-line array literal."
  (let* ((use-r (cl-some (lambda (p) (and (nth 2 p) (> (nth 2 p) 0))) points)))
    (concat "[" (mapconcat (lambda (p) (scad-sketch-parse--fmt-point p use-r))
                           points ", ") "]")))


(defun scad-sketch-unparse (node &optional indent extracted-names)
  "Convert AST NODE back to an OpenSCAD source string.
INDENT is the current indentation level (default 0).
EXTRACTED-NAMES is a hash-table mapping polygon nodes to their assigned names
\(populated during the extraction pass).
Returns the source string."
  (let ((ind (make-string (* (or indent 0) 2) ?\s))
        (ind1 (make-string (* (1+ (or indent 0)) 2) ?\s))
        (type (plist-get node :type)))
    (cond
     ((eq type 'array)
      (format "%s%s = %s;\n"
              ind
              (plist-get node :name)
              (scad-sketch-parse--fmt-array (plist-get node :points))))
     ((eq type 'polygon)
      (let* ((pts     (plist-get node :points))
             (src     (plist-get node :source))
             (pr-fn   (plist-get node :polyround))
             (name    (when extracted-names
                        (gethash node extracted-names)))
             (n-pts   (length (or pts '())))
             (inline-p (or src (<= n-pts scad-sketch-inline-threshold))))
        (cond
         (src
          ;; Was a variable reference — keep it, or use polyRound form.
          (if pr-fn
              (format "%spolygon(polyRound(%s, %s));\n"
                      ind src (scad-sketch-parse--fmt-num pr-fn))
            (format "%spolygon(%s);\n" ind src)))
         (name
          ;; Extracted to a named assignment (caller handles the assignment line).
          (if pr-fn
              (format "%spolygon(polyRound(%s, %s));\n"
                      ind name (scad-sketch-parse--fmt-num pr-fn))
            (format "%spolygon(%s);\n" ind name)))
         (inline-p
          (format "%spolygon(%s);\n"
                  ind (scad-sketch-parse--fmt-inline-array (or pts '()))))
         (t
          (format "%spolygon(%s);\n"
                  ind (scad-sketch-parse--fmt-array (or pts '())))))))
     ((eq type 'circle)
      (format "%scircle(r=%s);\n" ind
              (scad-sketch-parse--fmt-num (plist-get node :r))))
     ((eq type 'square)
      (let* ((x (plist-get node :x))
             (y (plist-get node :y))
             (w (plist-get node :w))
             (h (plist-get node :h))
             (angle (plist-get node :angle))
             (centered (and (< (abs (+ x (/ w 2.0))) 0.0001)
                            (< (abs (+ y (/ h 2.0))) 0.0001)))
             (rotated  (and angle (> (abs angle) 0.0001))))
        (if rotated
            (format "%srotate(%s) square([%s, %s]%s);\n"
                    ind
                    (scad-sketch-parse--fmt-num angle)
                    (scad-sketch-parse--fmt-num w)
                    (scad-sketch-parse--fmt-num h)
                    (if centered ", center=true" ""))
          (format "%ssquare([%s, %s]%s);\n"
                  ind
                  (scad-sketch-parse--fmt-num w)
                  (scad-sketch-parse--fmt-num h)
                  (if centered ", center=true" "")))))
     ((eq type 'text)
      (format "%stext(%S, size=%s);\n"
              ind
              (plist-get node :str)
              (scad-sketch-parse--fmt-num (plist-get node :size))))
     ((memq type '(difference union intersection))
      (let ((op (symbol-name type)))
        (concat (format "%s%s() {\n" ind op)
                (mapconcat (lambda (c)
                             (scad-sketch-unparse c (1+ (or indent 0)) extracted-names))
                           (plist-get node :children) "")
                (format "%s}\n" ind))))
     ((eq type 'translate)
      (let ((child (plist-get node :child)))
        (concat (format "%stranslate([%s, %s])\n"
                        ind
                        (scad-sketch-parse--fmt-num (plist-get node :tx))
                        (scad-sketch-parse--fmt-num (plist-get node :ty)))
                (scad-sketch-unparse child (1+ (or indent 0)) extracted-names))))
     ((eq type 'rotate)
      (concat (format "%srotate(%s)\n" ind
                      (scad-sketch-parse--fmt-num (plist-get node :angle)))
              (scad-sketch-unparse (plist-get node :child)
                                   (1+ (or indent 0)) extracted-names)))
     ((eq type 'scale)
      (concat (format "%sscale([%s, %s])\n"
                      ind
                      (scad-sketch-parse--fmt-num (plist-get node :sx))
                      (scad-sketch-parse--fmt-num (plist-get node :sy)))
              (scad-sketch-unparse (plist-get node :child)
                                   (1+ (or indent 0)) extracted-names)))
     ((eq type 'mirror)
      (concat (format "%smirror([%s, %s])\n"
                      ind
                      (scad-sketch-parse--fmt-num (plist-get node :mx))
                      (scad-sketch-parse--fmt-num (plist-get node :my)))
              (scad-sketch-unparse (plist-get node :child)
                                   (1+ (or indent 0)) extracted-names)))
     (t (format "%s/* unknown node type %S */\n" ind type)))))

(defun scad-sketch-parse--collect-array-names (nodes)
  "Return a hash table of array names already used in NODES."
  (let ((names (make-hash-table :test 'equal)))
    (dolist (node nodes)
      (scad-sketch-parse--walk
       node
       (lambda (n)
         (when (and (eq (plist-get n :type) 'array)
                    (plist-get n :name))
           (puthash (plist-get n :name) t names)))))
    names))

(defun scad-sketch-parse--fresh-extracted-name (used-names counters)
  "Return a fresh canonical _sketch_N name."
  (let ((base "_sketch")
        name)
    (while (or (null name) (gethash name used-names))
      (let ((idx (1+ (or (gethash base counters) 0))))
        (puthash base idx counters)
        (setq name (format "%s_%d" base idx))))
    (puthash name t used-names)
    name))

(defun scad-sketch-unparse-top-level (nodes)
  "Unparse a list of top-level NODES to an OpenSCAD source string.
Handles extraction of large polygons to named assignments."
  ;; First pass: collect polygons that need extraction and assign names.
  (let ((extracted (make-hash-table :test 'eq))
        (counters  (make-hash-table :test 'equal))
        (used-names (scad-sketch-parse--collect-array-names nodes))
        result)
    (dolist (node nodes)
      (scad-sketch-parse--walk
       node
       (lambda (n)
         (when (eq (plist-get n :type) 'polygon)
           (let* ((pts  (plist-get n :points))
                  (src  (plist-get n :source))
                  (n-pts (length (or pts '()))))
             (when (and (null src) (> n-pts scad-sketch-inline-threshold))
               (unless (gethash n extracted)
                 (puthash n (scad-sketch-parse--fresh-extracted-name
                             used-names counters)
                          extracted))))))))
    ;; Second pass: emit extracted assignments first, then the shapes.
    ;; Build a list of (name . points) for all extracted polygons.
    (let (extractions)
      (maphash (lambda (node name)
                 (push (cons name (plist-get node :points)) extractions))
               extracted)
      ;; Emit array assignments for extracted polygons.
      (dolist (ext (sort extractions (lambda (a b) (string< (car a) (car b)))))
        (push (format "%s = %s;
" (car ext)
                      (scad-sketch-parse--fmt-array (cdr ext)))
              result))
      ;; Emit the shapes, with extracted polygons using their names.
      (dolist (node nodes)
        (push (scad-sketch-unparse node 0 extracted) result))
      (mapconcat #'identity (nreverse result) ""))))

(provide 'scad-sketch-parse)
;;; scad-sketch-parse.el ends here
