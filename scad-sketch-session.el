;;; scad-sketch-session.el --- Session construction for scad-sketch -*- lexical-binding: t; -*-

;;; Commentary:

;; Session and source-buffer discovery layer for scad-sketch.
;;
;; This layer owns the parser dependency.  The editor mode should stay mostly
;; parser-agnostic: it receives a `scad-sketch-session' and mutates the current
;; editable points/marks/selection.  This file is responsible for resolving the
;; buffer position into source regions and constructing that session.
;;
;; Current parser-backed support:
;;   - Direct array assignment nodes.
;;   - Inline polygon([...]) nodes.
;;   - Inline polygon(polyRound([...], fn)) nodes.
;;   - Variable-ref polygon(name) nodes, by resolving NAME to an array node.
;;   - Variable-ref polygon(polyRound(name, fn)) nodes, by resolving NAME.
;;
;; A polygon variable reference records two regions:
;;   - the polygon call itself as a read-only target
;;   - the resolved source array as a write target
;;
;; For this pass, variable-ref polygons are written conservatively by updating
;; the resolved array assignment and leaving the polygon call intact.  Inline
;; polygons are canonicalized on write-back: <=4 points stay inline, >4 points
;; are extracted to a generated `_sketch_N' assignment plus polygon call.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'scad-sketch-parse)

(defgroup scad-sketch nil
  "Keyboard sketch editor for OpenSCAD array literals."
  :group 'tools
  :prefix "scad-sketch-")

(defcustom scad-sketch-default-grid 1.0
  "Default grid step in sketch units."
  :type 'number :group 'scad-sketch)

(defcustom scad-sketch-default-fine-step 0.1
  "Default fine movement step in sketch units."
  :type 'number :group 'scad-sketch)

(defcustom scad-sketch-default-coarse-step 5.0
  "Default coarse movement step in sketch units."
  :type 'number :group 'scad-sketch)

(defconst scad-sketch-session-inline-polygon-threshold 4
  "Maximum number of polygon points to keep inline on session write-back.")

(define-error 'scad-sketch-no-edit-target
  "No scad-sketch edit target"
  'user-error)

(define-error 'scad-sketch-unsupported-edit-target
  "Unsupported scad-sketch edit target"
  'user-error)

(cl-defstruct scad-sketch-target
  id
  kind             ; 'array, 'shape-root, 'polygon-ref, 'boolean, 'transform-root
  role             ; 'source or 'root
  node
  source-node
  beg-marker
  end-marker
  name
  polyround
  write-p
  metadata)

(cl-defstruct scad-sketch-shape
  id
  kind             ; 'polygon, 'circle, or 'text
  points           ; polygon points; nil for primitive shapes
  closed
  polyround
  source-target-id
  call-target-id
  metadata)        ; circle: :cx :cy :r; text: :str :x :y :size :angle

(cl-defstruct scad-sketch-session
  name units grid fine-step coarse-step closed
  points point
  marks
  named-marks selected-index

  shapes
  active-shape-id
  tree             ; plist tree: (:kind shape :shape-id X) or (:kind boolean ...)
  targets
  root-target-id

  selection
  focus-ref
  hover-index

  source-buffer content-beg content-end
  ast path root-node
  dirty undo-stack)

(defvar-local scad-sketch--session nil)

(defun scad-sketch--assert-session ()
  "Return the current sketch session or signal an error."
  (unless (and (boundp 'scad-sketch--session) scad-sketch--session)
    (user-error "No active scad-sketch session"))
  scad-sketch--session)

;;;; 2D transform helpers

(defun scad-sketch-session--mat-identity ()
  "Return the identity affine matrix.

Matrix representation is (A B C D E F), meaning:
  x' = A*x + C*y + E
  y' = B*x + D*y + F"
  '(1.0 0.0 0.0 1.0 0.0 0.0))

(defun scad-sketch-session--mat-mul (m n)
  "Return M*N for two affine matrices."
  (pcase-let ((`(,a ,b ,c ,d ,e ,f) m)
              (`(,g ,h ,i ,j ,k ,l) n))
    (list (+ (* a g) (* c h))
          (+ (* b g) (* d h))
          (+ (* a i) (* c j))
          (+ (* b i) (* d j))
          (+ (* a k) (* c l) e)
          (+ (* b k) (* d l) f))))

(defun scad-sketch-session--mat-translate (tx ty)
  "Return a translation matrix."
  (list 1.0 0.0 0.0 1.0 (float tx) (float ty)))

(defun scad-sketch-session--mat-scale (sx sy)
  "Return a scale matrix."
  (list (float sx) 0.0 0.0 (float sy) 0.0 0.0))

(defun scad-sketch-session--mat-rotate (degrees)
  "Return a rotation matrix for DEGREES."
  (let* ((r (* pi (/ (float degrees) 180.0)))
         (co (cos r))
         (si (sin r)))
    (list co si (- si) co 0.0 0.0)))

(defun scad-sketch-session--mat-mirror (mx my)
  "Return a simple 2D OpenSCAD-style mirror matrix.

This handles the common axes used by the parser:
  mirror([1,0]) -> x negated
  mirror([0,1]) -> y negated"
  (cond ((and (not (= mx 0)) (= my 0))
         '(-1.0 0.0 0.0 1.0 0.0 0.0))
        ((and (= mx 0) (not (= my 0)))
         '(1.0 0.0 0.0 -1.0 0.0 0.0))
        (t
         ;; General reflection across the line through origin perpendicular to
         ;; vector (mx,my).  Good enough for future non-axis mirror inputs.
         (let* ((len (sqrt (+ (* mx mx) (* my my))))
                (nx (/ mx len))
                (ny (/ my len)))
           (list (- 1 (* 2 nx nx))
                 (* -2 nx ny)
                 (* -2 nx ny)
                 (- 1 (* 2 ny ny))
                 0.0
                 0.0)))))

(defun scad-sketch-session--mat-apply (m xy)
  "Apply affine matrix M to XY."
  (pcase-let ((`(,a ,b ,c ,d ,e ,f) m))
    (let ((x (nth 0 xy))
          (y (nth 1 xy)))
      (list (+ (* a x) (* c y) e)
            (+ (* b x) (* d y) f)))))

(defun scad-sketch-session--mat-uniform-scale (m)
  "Return an approximate scale factor for M.

Used for circles.  Non-uniformly scaled circles are canonicalized as circles
using the average basis-vector length for this first pass."
  (pcase-let ((`(,a ,b ,c ,d ,_e ,_f) m))
    (/ (+ (sqrt (+ (* a a) (* b b)))
          (sqrt (+ (* c c) (* d d))))
       2.0)))

;;;; Tree helpers

(defun scad-sketch-session--tree-shape (shape-id)
  "Return a tree node for SHAPE-ID."
  (list :kind 'shape :shape-id shape-id))

(defun scad-sketch-session--tree-boolean (op group-id children)
  "Return a boolean tree node."
  (list :kind 'boolean :op op :group-id group-id :children children))

(defun scad-sketch-session--tree-shape-ids (tree)
  "Return all shape ids in TREE."
  (pcase (plist-get tree :kind)
    ('shape
     (list (plist-get tree :shape-id)))
    ('boolean
     (apply #'append
            (mapcar #'scad-sketch-session--tree-shape-ids
                    (plist-get tree :children))))
    (_ nil)))

(defun scad-sketch-session--tree-groups (tree)
  "Return boolean tree nodes in TREE, outermost first."
  (pcase (plist-get tree :kind)
    ('boolean
     (cons tree
           (apply #'append
                  (mapcar #'scad-sketch-session--tree-groups
                          (plist-get tree :children)))))
    (_ nil)))

(defun scad-sketch-session--replace-tree-shape (tree old-id replacement-tree)
  "Return TREE with shape OLD-ID replaced by REPLACEMENT-TREE."
  (pcase (plist-get tree :kind)
    ('shape
     (if (eq (plist-get tree :shape-id) old-id)
         replacement-tree
       tree))
    ('boolean
     (plist-put
      (copy-sequence tree)
      :children
      (mapcar (lambda (child)
                (scad-sketch-session--replace-tree-shape
                 child old-id replacement-tree))
              (plist-get tree :children))))
    (_ tree)))

(defun scad-sketch-session--append-shape-to-tree (tree shape-id)
  "Return TREE with SHAPE-ID added in a union context."
  (cond
   ((null tree)
    (scad-sketch-session--tree-shape shape-id))
   ((and (eq (plist-get tree :kind) 'boolean)
         (eq (plist-get tree :op) 'union))
    (plist-put
     (copy-sequence tree)
     :children
     (append (plist-get tree :children)
             (list (scad-sketch-session--tree-shape shape-id)))))
   (t
    (scad-sketch-session--tree-boolean
     'union
     'group-added-union
     (list tree (scad-sketch-session--tree-shape shape-id))))))

;;;; Parser node -> editable tree conversion

(defun scad-sketch-session--conversion-shape-id (state)
  "Return the next shape id and advance STATE."
  (let ((n (plist-get state :shape-index)))
    (plist-put state :shape-index (1+ n))
    (scad-sketch-session--shape-id n)))

(defun scad-sketch-session--conversion-group-id (state op)
  "Return the next group id for OP and advance STATE."
  (let ((n (plist-get state :group-index)))
    (plist-put state :group-index (1+ n))
    (intern (format "%s-%d" op n))))

(defun scad-sketch-session--conversion-target-id (state prefix)
  "Return the next target id with PREFIX and advance STATE."
  (let ((n (plist-get state :target-index)))
    (plist-put state :target-index (1+ n))
    (intern (format "%s-%d" prefix n))))

(defun scad-sketch-session--conversion-push-shape (state shape)
  "Push SHAPE into STATE."
  (plist-put state :shapes (cons shape (plist-get state :shapes))))

(defun scad-sketch-session--conversion-push-target (state target)
  "Push TARGET into STATE."
  (plist-put state :targets (cons target (plist-get state :targets))))

(defun scad-sketch-session--resolve-array-node-for-conversion (ast name node matrix)
  "Resolve NAME for NODE when MATRIX allows reference preservation.

If MATRIX is not identity-ish, return nil so the converted shape is flattened
inline rather than rewriting a source array through a transformed call."
  (when (equal matrix (scad-sketch-session--mat-identity))
    (scad-sketch-session--resolve-array-node ast name node)))

(defun scad-sketch-session--convert-polygon-node (ast node matrix state)
  "Convert polygon NODE under MATRIX into a tree shape node."
  (let* ((shape-id         (scad-sketch-session--conversion-shape-id state))
         (polyround        (plist-get node :polyround))
         (source-name      (plist-get node :source))
         (source-node      nil)
         (source-target    nil)
         (source-target-id nil)
         (polygon-pts      nil))

    (if source-name
        (progn
          ;; Preserve variable-reference polygons only when there is no
          ;; transform to flatten.  This lets write-back update the original
          ;; array assignment instead of replacing polygon(name).
          (setq source-node
                (scad-sketch-session--resolve-array-node-for-conversion
                 ast source-name node matrix))

          (if source-node
              (progn
                (setq source-target-id
                      (scad-sketch-session--conversion-target-id state "source"))
                (setq source-target
                      (scad-sketch-session--make-array-target
                       source-node source-target-id 'source polyround))
                (scad-sketch-session--conversion-push-target state source-target)
                (setq polygon-pts
                      (copy-tree (plist-get source-node :points))))

            ;; A transformed polygon(name) cannot safely rewrite `name',
            ;; because the transform has already been baked into editor space.
            ;; Flatten the resolved array into inline editable points instead.
            (let ((resolved
                   (scad-sketch-session--resolve-array-node
                    ast source-name node)))
              (unless resolved
                (signal 'scad-sketch-unsupported-edit-target
                        (list (format "Could not resolve polygon source `%s'"
                                      source-name))))
              (setq polygon-pts
                    (mapcar
                     (lambda (pt)
                       (append
                        (scad-sketch-session--mat-apply
                         matrix
                         (list (nth 0 pt) (nth 1 pt)))
                        (list (or (nth 2 pt) 0))))
                     (plist-get resolved :points))))))

      ;; Inline polygon([...]) case.
      (setq polygon-pts
            (mapcar
             (lambda (pt)
               (append
                (scad-sketch-session--mat-apply
                 matrix
                 (list (nth 0 pt) (nth 1 pt)))
                (list (or (nth 2 pt) 0))))
             (plist-get node :points))))

    (unless polygon-pts
      (signal 'scad-sketch-unsupported-edit-target
              (list "Polygon has no editable points")))

    (scad-sketch-session--conversion-push-shape
     state
     (scad-sketch-session--make-polygon-shape
      shape-id
      polygon-pts
      polyround
      source-target-id
      nil
      (list :source-name source-name
            :node node)))

    (scad-sketch-session--tree-shape shape-id)))

(defun scad-sketch-session--convert-square-node (_ast node matrix state)
  "Convert square NODE under MATRIX into a polygon shape."
  (let* ((shape-id (scad-sketch-session--conversion-shape-id state))
         (x (plist-get node :x))
         (y (plist-get node :y))
         (w (plist-get node :w))
         (h (plist-get node :h))
         (raw (list (list x y 0.0)
                    (list (+ x w) y 0.0)
                    (list (+ x w) (+ y h) 0.0)
                    (list x (+ y h) 0.0)))
         (points
          (mapcar (lambda (p)
                    (append
                     (scad-sketch-session--mat-apply
                      matrix (list (nth 0 p) (nth 1 p)))
                     '(0.0)))
                  raw)))
    (scad-sketch-session--conversion-push-shape
     state
     (scad-sketch-session--make-polygon-shape
      shape-id points nil nil nil
      (list :from 'square :node node)))
    (scad-sketch-session--tree-shape shape-id)))

(defun scad-sketch-session--convert-circle-node (_ast node matrix state)
  "Convert circle NODE under MATRIX into a circle shape."
  (let* ((shape-id (scad-sketch-session--conversion-shape-id state))
         (center (scad-sketch-session--mat-apply matrix '(0.0 0.0)))
         (scale (scad-sketch-session--mat-uniform-scale matrix))
         (r (* (plist-get node :r) scale)))
    (scad-sketch-session--conversion-push-shape
     state
     (scad-sketch-session--make-circle-shape
      shape-id
      (nth 0 center)
      (nth 1 center)
      r
      (list :from 'circle :node node)))
    (scad-sketch-session--tree-shape shape-id)))

(defun scad-sketch-session--convert-text-node (_ast node matrix state)
  "Convert text NODE under MATRIX into a text shape."
  (let* ((shape-id (scad-sketch-session--conversion-shape-id state))
         (origin   (scad-sketch-session--mat-apply matrix '(0.0 0.0)))
         (basis-0  (scad-sketch-session--mat-apply matrix '(0.0 0.0)))
         (basis-x  (scad-sketch-session--mat-apply matrix '(1.0 0.0)))
         (dx       (- (nth 0 basis-x) (nth 0 basis-0)))
         (dy       (- (nth 1 basis-x) (nth 1 basis-0)))
         (angle    (* 180.0 (/ (atan dy dx) pi)))
         (scale    (scad-sketch-session--mat-uniform-scale matrix))
         (size     (* (float (or (plist-get node :size) 10.0)) scale)))
    (scad-sketch-session--conversion-push-shape
     state
     (scad-sketch-session--make-text-shape
      shape-id
      (or (plist-get node :str) "")
      (nth 0 origin)
      (nth 1 origin)
      size
      angle
      (list :from 'text :node node)))
    (scad-sketch-session--tree-shape shape-id)))

(defun scad-sketch-session--convert-boolean-node (ast node matrix state)
  "Convert boolean NODE under MATRIX into a boolean tree node."
  (let* ((op (plist-get node :type))
         (group-id (scad-sketch-session--conversion-group-id state op))
         children)
    (dolist (child (plist-get node :children))
      (push (scad-sketch-session--convert-node ast child matrix state)
            children))
    (scad-sketch-session--tree-boolean op group-id (nreverse children))))

(defun scad-sketch-session--convert-transform-node (ast node matrix state)
  "Convert transform NODE by composing it into MATRIX."
  (let* ((type (plist-get node :type))
         (local
          (pcase type
            ('translate
             (scad-sketch-session--mat-translate
              (plist-get node :tx) (plist-get node :ty)))
            ('rotate
             (scad-sketch-session--mat-rotate
              (plist-get node :angle)))
            ('scale
             (scad-sketch-session--mat-scale
              (plist-get node :sx) (plist-get node :sy)))
            ('mirror
             (scad-sketch-session--mat-mirror
              (plist-get node :mx) (plist-get node :my)))
            (_
             (scad-sketch-session--mat-identity)))))
    (scad-sketch-session--convert-node
     ast
     (plist-get node :child)
     (scad-sketch-session--mat-mul matrix local)
     state)))

(defun scad-sketch-session--convert-node (ast node matrix state)
  "Convert parser NODE under MATRIX into editable shapes and a tree."
  (let ((type (plist-get node :type)))
    (cond
     ((eq type 'polygon)
      (scad-sketch-session--convert-polygon-node ast node matrix state))

     ((eq type 'square)
      (scad-sketch-session--convert-square-node ast node matrix state))

     ((eq type 'circle)
      (scad-sketch-session--convert-circle-node ast node matrix state))

     ((eq type 'text)
      (scad-sketch-session--convert-text-node ast node matrix state))

     ((memq type '(union difference intersection))
      (scad-sketch-session--convert-boolean-node ast node matrix state))

     ((memq type '(translate rotate scale mirror))
      (scad-sketch-session--convert-transform-node ast node matrix state))

     (t
      (signal 'scad-sketch-unsupported-edit-target
              (list (format "Unsupported scad-sketch form: %S" type)))))))

;;;; Parser-backed source resolution

(defun scad-sketch-session--buffer-source ()
  "Return the current buffer contents without text properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun scad-sketch-session--buffer-offset (&optional pos)
  "Return 0-based source offset for buffer POS.
When POS is nil, use current point."
  (1- (or pos (point))))

(defun scad-sketch-session--offset-marker (offset &optional insertion-type)
  "Return a marker in the current buffer for 0-based parser OFFSET."
  (copy-marker (1+ offset) insertion-type))

(defun scad-sketch-session--node-at-point (source pos)
  "Return parser information for the node at POS in SOURCE.
POS is a 0-based source offset.  The return value is a plist:

  (:ast AST :path PATH :node NODE)

Signals `scad-sketch-no-edit-target' if POS is not in any parsed node."
  (let* ((ast  (scad-sketch-parse source))
         (path (scad-sketch-parse--path-to ast pos))
         (node (car (last path))))
    (unless node
      (signal 'scad-sketch-no-edit-target
              (list "No supported scad-sketch form at point")))
    (list :ast ast :path path :node node)))

(defun scad-sketch-session--node-scope (node)
  "Return NODE's parser scope if present."
  (plist-get node :scope))

(defun scad-sketch-session--array-nodes (ast)
  "Return all array nodes in AST."
  (cl-remove-if-not
   (lambda (n) (eq (plist-get n :type) 'array))
   ast))

(defun scad-sketch-session--array-names (ast)
  "Return all array assignment names in AST."
  (delq nil
        (mapcar (lambda (n) (plist-get n :name))
                (scad-sketch-session--array-nodes ast))))

(defun scad-sketch-session--resolve-array-node (ast name before-node)
  "Resolve NAME to an array node in AST visible before BEFORE-NODE.

This uses parser scope metadata when present, but degrades to nearest previous
same-name array assignment if scope tags are absent."
  (let* ((before-pos (plist-get before-node :beg))
         (scope      (scad-sketch-session--node-scope before-node))
         (candidates
          (cl-remove-if-not
           (lambda (n)
             (and (eq (plist-get n :type) 'array)
                  (string= (plist-get n :name) name)
                  (< (plist-get n :beg) before-pos)))
           ast))
         (same-scope
          (when scope
            (cl-remove-if-not
             (lambda (n)
               (equal (scad-sketch-session--node-scope n) scope))
             candidates))))
    (car (sort (copy-sequence (or same-scope candidates))
               (lambda (a b) (> (plist-get a :beg)
                                (plist-get b :beg)))))))

(defun scad-sketch-session--used-identifiers-in-source (source)
  "Return all identifier-looking names in SOURCE.
This intentionally over-approximates: identifiers in comments/strings may count
as used, which is fine for avoiding generated-name collisions."
  (let (names)
    (with-temp-buffer
      (insert source)
      (goto-char (point-min))
      (while (re-search-forward "\\_<[A-Za-z_$][A-Za-z0-9_$]*\\_>" nil t)
        (push (match-string-no-properties 0) names)))
    (delete-dups names)))

(defun scad-sketch-session--unique-sketch-name (session)
  "Return a `_sketch_N' name not already used in SESSION's source buffer."
  (let* ((source-buffer (scad-sketch-session-source-buffer session))
         (source
          (if (buffer-live-p source-buffer)
              (with-current-buffer source-buffer
                (buffer-substring-no-properties (point-min) (point-max)))
            ""))
         (existing (scad-sketch-session--used-identifiers-in-source source))
         (i 1)
         name)
    (while (progn
             (setq name (format "_sketch_%d" i))
             (member name existing))
      (setq i (1+ i)))
    name))

;;;; Shapes

(defun scad-sketch-session--shape-id (n)
  "Return the canonical shape id for index N."
  (intern (format "shape-%d" n)))

(defun scad-sketch-session-shape-by-id (session shape-id)
  "Return the shape in SESSION with SHAPE-ID, or nil."
  (cl-find-if (lambda (shape)
                (eq (scad-sketch-shape-id shape) shape-id))
              (scad-sketch-session-shapes session)))

(defun scad-sketch-session-active-shape (session)
  "Return SESSION's active shape."
  (or (scad-sketch-session-shape-by-id
       session (scad-sketch-session-active-shape-id session))
      (car (scad-sketch-session-shapes session))))

(defun scad-sketch-session-shape-ids (session)
  "Return all shape ids in SESSION."
  (mapcar #'scad-sketch-shape-id
          (scad-sketch-session-shapes session)))

(defun scad-sketch-session-next-shape-id (session)
  "Return a fresh shape id for SESSION."
  (let ((i 0)
        id)
    (while (progn
             (setq id (scad-sketch-session--shape-id i))
             (memq id (scad-sketch-session-shape-ids session)))
      (setq i (1+ i)))
    id))

(defun scad-sketch-session-set-active-shape (session shape-id)
  "Set SESSION's active shape to SHAPE-ID and mirror polygon points into SESSION."
  (let ((shape (scad-sketch-session-shape-by-id session shape-id)))
    (unless shape
      (user-error "No such shape: %S" shape-id))
    (setf (scad-sketch-session-active-shape-id session) shape-id)
    (setf (scad-sketch-session-points session)
          (copy-tree (or (scad-sketch-shape-points shape) nil)))
    (setf (scad-sketch-session-closed session)
          (scad-sketch-shape-closed shape))
    session))

(defun scad-sketch-session-sync-active-shape-from-points (session)
  "Copy SESSION's compatibility `points' mirror into its active polygon shape."
  (let ((shape (scad-sketch-session-active-shape session)))
    (when (and shape (eq (scad-sketch-shape-kind shape) 'polygon))
      (setf (scad-sketch-shape-points shape)
            (copy-tree (scad-sketch-session-points session)))
      (setf (scad-sketch-shape-closed shape)
            (scad-sketch-session-closed session))))
  session)

(defun scad-sketch-session--make-polygon-shape
    (id points &optional polyround source-target-id call-target-id metadata)
  "Build a polygon shape."
  (make-scad-sketch-shape
   :id id
   :kind 'polygon
   :points (copy-tree points)
   :closed t
   :polyround polyround
   :source-target-id source-target-id
   :call-target-id call-target-id
   :metadata metadata))

(defun scad-sketch-session--make-circle-shape
    (id cx cy r &optional metadata)
  "Build a circle shape."
  (make-scad-sketch-shape
   :id id
   :kind 'circle
   :points nil
   :closed t
   :polyround nil
   :source-target-id nil
   :call-target-id nil
   :metadata (append (list :cx (float cx)
                           :cy (float cy)
                           :r  (float r))
                     metadata)))

(defun scad-sketch-session--make-text-shape
    (id str x y size &optional angle metadata)
  "Build a text shape."
  (make-scad-sketch-shape
   :id id
   :kind 'text
   :points nil
   :closed t
   :polyround nil
   :source-target-id nil
   :call-target-id nil
   :metadata (append
              (list :str str
                    :x (float x)
                    :y (float y)
                    :size (float size)
                    :angle (float (or angle 0.0)))
              metadata)))

(defun scad-sketch-session-add-shape (session points &optional polyround)
  "Add a new polygon shape with POINTS to SESSION and make it active.

A session can add shapes only when it owns a root target.  Array-only sessions
have no semantically safe place to serialize multiple shapes."
  (unless (scad-sketch-session-root-target session)
    (signal 'scad-sketch-unsupported-edit-target
            (list "This session only owns an array; open a polygon, shape, or boolean to add shapes")))
  (scad-sketch-session-sync-active-shape-from-points session)
  (let* ((shape-id (scad-sketch-session-next-shape-id session))
         (shape
          (scad-sketch-session--make-polygon-shape
           shape-id points polyround nil nil
           (list :created-in-session t))))
    (setf (scad-sketch-session-shapes session)
          (append (scad-sketch-session-shapes session) (list shape)))
    (setf (scad-sketch-session-tree session)
          (scad-sketch-session--append-shape-to-tree
           (scad-sketch-session-tree session) shape-id))
    (scad-sketch-session-set-active-shape session shape-id)
    shape))

;;;; Formatting / emission

(defun scad-sketch-session--any-radius-p (points)
  "Return non-nil if POINTS contains any non-zero radius."
  (cl-some (lambda (p) (and (nth 2 p) (> (nth 2 p) 0))) points))

(defun scad-sketch-session--fmt-num (n)
  "Format N compactly for OpenSCAD."
  (let ((x (float n)))
    (if (< (abs (- x (round x))) 0.000001)
        (number-to-string (round x))
      (let ((s (format "%.4f" x)))
        (setq s (replace-regexp-in-string "0+\\'" "" s))
        (setq s (replace-regexp-in-string "\\.\\'" "" s))
        (if (or (string= s "-0") (string= s "")) "0" s)))))

(defun scad-sketch-session--fmt-point (point use-radii)
  "Format one model POINT.  When USE-RADII is non-nil emit [x, y, r]."
  (if use-radii
      (format "[%s, %s, %s]"
              (scad-sketch-session--fmt-num (nth 0 point))
              (scad-sketch-session--fmt-num (nth 1 point))
              (scad-sketch-session--fmt-num (or (nth 2 point) 0)))
    (format "[%s, %s]"
            (scad-sketch-session--fmt-num (nth 0 point))
            (scad-sketch-session--fmt-num (nth 1 point)))))

(defun scad-sketch-session--fmt-inline-array (points &optional force-radii)
  "Format POINTS as a single-line SCAD array."
  (let ((use-radii (or force-radii
                       (scad-sketch-session--any-radius-p points))))
    (concat "["
            (mapconcat (lambda (p)
                         (scad-sketch-session--fmt-point p use-radii))
                       points
                       ", ")
            "]")))

(defun scad-sketch-session--fmt-array (points indent &optional force-radii)
  "Format POINTS as a multi-line SCAD array at INDENT."
  (let* ((use-radii (or force-radii
                        (scad-sketch-session--any-radius-p points)))
         (child-indent (concat indent "  "))
         (lines
          (mapcar (lambda (p)
                    (concat child-indent
                            (scad-sketch-session--fmt-point p use-radii)))
                  points)))
    (concat "[\n"
            (mapconcat #'identity lines ",\n")
            (if lines "\n" "")
            indent
            "]")))

(defun scad-sketch-session--emit-array-assignment
    (name points indent &optional force-radii)
  "Emit NAME = POINTS; at INDENT."
  (format "%s%s = %s;\n"
          indent
          name
          (scad-sketch-session--fmt-array points indent force-radii)))

(defun scad-sketch-session--emit-polygon-call
    (points indent polyround &optional source-name extracted-name)
  "Emit a polygon call at INDENT.

POINTS are used for inline emission.  POLYROUND is the optional polyRound
fragment count.  SOURCE-NAME emits polygon(SOURCE-NAME).  EXTRACTED-NAME emits
polygon(EXTRACTED-NAME) and is used when a preceding assignment was generated."
  (let ((ref (or extracted-name source-name)))
    (cond
     ((and ref polyround)
      (format "%spolygon(polyRound(%s, %s));\n"
              indent ref (scad-sketch-session--fmt-num polyround)))
     (ref
      (format "%spolygon(%s);\n" indent ref))
     (polyround
      (format "%spolygon(polyRound(%s, %s));\n"
              indent
              (scad-sketch-session--fmt-inline-array points t)
              (scad-sketch-session--fmt-num polyround)))
     (t
      (format "%spolygon(%s);\n"
              indent
              (scad-sketch-session--fmt-inline-array points nil))))))

;;;; Session construction

(defun scad-sketch-session--initial-point (points)
  "Return the initial cursor point for POINTS."
  (if points
      (list (float (nth 0 (car points)))
            (float (nth 1 (car points))))
    (list 0.0 0.0)))

(defun scad-sketch-session-target-by-id (session target-id)
  "Return the target in SESSION with TARGET-ID, or nil."
  (cl-find-if (lambda (target)
                (eq (scad-sketch-target-id target) target-id))
              (scad-sketch-session-targets session)))

(defun scad-sketch-session-root-target (session)
  "Return SESSION's root target, or nil."
  (when (scad-sketch-session-root-target-id session)
    (scad-sketch-session-target-by-id
     session (scad-sketch-session-root-target-id session))))

(defun scad-sketch-session-source-targets (session)
  "Return writable source targets in SESSION."
  (cl-remove-if-not
   (lambda (target)
     (and (eq (scad-sketch-target-role target) 'source)
          (scad-sketch-target-write-p target)))
   (scad-sketch-session-targets session)))

(defun scad-sketch-session--make-marker-pair (node)
  "Return (BEG . END) markers for parser NODE in the current buffer."
  (cons (scad-sketch-session--offset-marker (plist-get node :beg))
        (scad-sketch-session--offset-marker (plist-get node :end) t)))

(defun scad-sketch-session--make-root-target (node kind &optional id)
  "Build a writable root target for parser NODE of KIND."
  (let ((markers (scad-sketch-session--make-marker-pair node)))
    (make-scad-sketch-target
     :id (or id 'root-0)
     :kind kind
     :role 'root
     :node node
     :source-node nil
     :beg-marker (car markers)
     :end-marker (cdr markers)
     :name (symbol-name kind)
     :polyround (plist-get node :polyround)
     :write-p t
     :metadata nil)))

(defun scad-sketch-session--make-array-target (node &optional id role polyround)
  "Build a writable array target for array NODE."
  (let* ((markers (scad-sketch-session--make-marker-pair node))
         (name    (plist-get node :name)))
    (make-scad-sketch-target
     :id (or id 'array-0)
     :kind 'array
     :role (or role 'source)
     :node node
     :source-node node
     :beg-marker (car markers)
     :end-marker (cdr markers)
     :name name
     :polyround polyround
     :write-p t
     :metadata nil)))

(defun scad-sketch-session--make-session
    (name shapes active-shape-id tree targets root-target-id
          beg-marker end-marker
          &optional ast path root-node)
  "Create a sketch session.

NAME is the display name.  SHAPES are editor objects.  TREE is the boolean/shape
tree used for write-back.  TARGETS and ROOT-TARGET-ID define the write plan."
  (let* ((active-shape
          (or (cl-find-if (lambda (shape)
                            (eq (scad-sketch-shape-id shape) active-shape-id))
                          shapes)
              (car shapes)))
         (shape-id (and active-shape (scad-sketch-shape-id active-shape)))
         (points   (and active-shape
                        (copy-tree (or (scad-sketch-shape-points active-shape)
                                       nil)))))
    (make-scad-sketch-session
     :name name
     :units "mm"
     :grid (float scad-sketch-default-grid)
     :fine-step (float scad-sketch-default-fine-step)
     :coarse-step (float scad-sketch-default-coarse-step)
     :closed (if active-shape (scad-sketch-shape-closed active-shape) t)
     :points points
     :point (scad-sketch-session--initial-point points)
     :marks nil
     :named-marks nil
     :selected-index (if points 0 nil)
     :shapes shapes
     :active-shape-id shape-id
     :tree tree
     :targets targets
     :root-target-id root-target-id
     :selection nil
     :focus-ref (if points
                    (list :kind 'point :shape-id shape-id :index 0)
                  (list :kind 'shape :shape-id shape-id))
     :hover-index 0
     :source-buffer (current-buffer)
     :content-beg beg-marker
     :content-end end-marker
     :ast ast
     :path path
     :root-node root-node
     :dirty nil
     :undo-stack nil)))

(defun scad-sketch-session--shape-from-array-target (target shape-id &optional polyround)
  "Build a polygon shape from array TARGET."
  (let ((node (scad-sketch-target-node target)))
    (scad-sketch-session--make-polygon-shape
     shape-id
     (copy-tree (plist-get node :points))
     polyround
     (scad-sketch-target-id target)
     nil
     (list :source-name (scad-sketch-target-name target)
           :source-node node))))

(defun scad-sketch-session--session-from-array (ast path node)
  "Build an array-only session for direct array NODE."
  (let* ((target
          (scad-sketch-session--make-array-target node 'array-0 'source nil))
         (shape
          (scad-sketch-session--shape-from-array-target target 'shape-0))
         (tree (scad-sketch-session--tree-shape 'shape-0)))
    (scad-sketch-session--make-session
     (plist-get node :name)
     (list shape)
     'shape-0
     tree
     (list target)
     nil
     (scad-sketch-target-beg-marker target)
     (scad-sketch-target-end-marker target)
     ast path node)))

(defun scad-sketch-session--session-from-edit-root (ast path node root-node)
  "Build a normal shape/boolean session from ROOT-NODE."
  (let* ((root-kind (plist-get root-node :type))
         (root-target
          (scad-sketch-session--make-root-target
           root-node
           (if (memq root-kind '(union difference intersection))
               'boolean
             'shape-root)
           'root-0))
         (state (list :shape-index 0
                      :group-index 0
                      :target-index 0
                      :shapes nil
                      :targets nil))
         (tree (scad-sketch-session--convert-node
                ast root-node
                (scad-sketch-session--mat-identity)
                state))
         (shapes (nreverse (plist-get state :shapes)))
         (targets (cons root-target
                        (nreverse (plist-get state :targets)))))
    (unless shapes
      (signal 'scad-sketch-unsupported-edit-target
              (list "No editable shapes in this form")))
    (scad-sketch-session--make-session
     (symbol-name root-kind)
     shapes
     (scad-sketch-shape-id (car shapes))
     tree
     targets
     (scad-sketch-target-id root-target)
     (scad-sketch-target-beg-marker root-target)
     (scad-sketch-target-end-marker root-target)
     ast path node)))

(defun scad-sketch-session--supported-root-node-in-path (path)
  "Return the outermost supported edit root in PATH.

If a transform wraps a boolean/shape, the transform becomes the root so its
source region is replaced and its matrix can be flattened into editable shapes."
  (cl-find-if
   (lambda (node)
     (memq (plist-get node :type)
           '(polygon circle square text
             union difference intersection
             translate rotate scale mirror)))
   path))

(defun scad-sketch-session-at-point ()
  "Build a parser-backed sketch session for the supported form at point."
  (let* ((source (scad-sketch-session--buffer-source))
         (pos    (scad-sketch-session--buffer-offset))
         (info   (scad-sketch-session--node-at-point source pos))
         (ast    (plist-get info :ast))
         (path   (plist-get info :path))
         (node   (plist-get info :node))
         (root   (scad-sketch-session--supported-root-node-in-path path)))
    (cond
     ((and node (eq (plist-get node :type) 'array))
      (scad-sketch-session--session-from-array ast path node))
     (root
      (scad-sketch-session--session-from-edit-root ast path node root))
     (t
      (signal 'scad-sketch-unsupported-edit-target
              (list (format "Unsupported scad-sketch form: %S"
                            (plist-get node :type))))))))

(defun scad-sketch-session-insert-array-at-point (name)
  "Insert a new empty array named NAME at point and return its session."
  (let (beg end node target shape tree)
    (setq beg (point-marker))
    (insert (format "%s = [\n];\n" name))
    (setq end (copy-marker (point) t))
    (setq node (list :type 'array
                     :name name
                     :points nil
                     :beg (1- (marker-position beg))
                     :end (1- (marker-position end))))
    (setq target
          (make-scad-sketch-target
           :id 'array-0
           :kind 'array
           :role 'source
           :node node
           :source-node node
           :beg-marker beg
           :end-marker end
           :name name
           :polyround nil
           :write-p t
           :metadata nil))
    (setq shape
          (scad-sketch-session--make-polygon-shape
           'shape-0 nil nil
           (scad-sketch-target-id target)
           nil
           (list :source-name name :inserted-array t)))
    (setq tree (scad-sketch-session--tree-shape 'shape-0))
    (scad-sketch-session--make-session
     name
     (list shape)
     'shape-0
     tree
     (list target)
     nil
     beg end
     nil (list node) node)))

;;;; Emission / write-back

(defun scad-sketch-session--shape-source-target (session shape)
  "Return SHAPE's source target in SESSION, or nil."
  (when (scad-sketch-shape-source-target-id shape)
    (scad-sketch-session-target-by-id
     session (scad-sketch-shape-source-target-id shape))))

(defun scad-sketch-session--shape-source-name (session shape)
  "Return SHAPE's source variable name, if any."
  (let ((target (scad-sketch-session--shape-source-target session shape)))
    (and target (scad-sketch-target-name target))))

(defun scad-sketch-session--shape-extracted-name (shape)
  "Return SHAPE's generated extraction name, if one exists."
  (plist-get (scad-sketch-shape-metadata shape) :extracted-name))

(defun scad-sketch-session--set-shape-extracted-name (shape name)
  "Record NAME as SHAPE's generated extraction name."
  (setf (scad-sketch-shape-metadata shape)
        (plist-put (scad-sketch-shape-metadata shape)
                   :extracted-name name)))

(defun scad-sketch-session--emit-circle-shape (shape indent)
  "Emit circle SHAPE at INDENT."
  (let* ((md (scad-sketch-shape-metadata shape))
         (cx (plist-get md :cx))
         (cy (plist-get md :cy))
         (r  (plist-get md :r)))
    (if (and (< (abs cx) 0.000001)
             (< (abs cy) 0.000001))
        (format "%scircle(r=%s);\n"
                indent
                (scad-sketch-session--fmt-num r))
      (format "%stranslate([%s, %s])\n%s  circle(r=%s);\n"
              indent
              (scad-sketch-session--fmt-num cx)
              (scad-sketch-session--fmt-num cy)
              indent
              (scad-sketch-session--fmt-num r)))))

(defun scad-sketch-session--emit-text-shape (shape indent)
  "Emit text SHAPE at INDENT."
  (let* ((md    (scad-sketch-shape-metadata shape))
         (str   (or (plist-get md :str) ""))
         (x     (float (or (plist-get md :x) 0.0)))
         (y     (float (or (plist-get md :y) 0.0)))
         (size  (float (or (plist-get md :size) 10.0)))
         (angle (float (or (plist-get md :angle) 0.0)))
         (zero-pos (and (< (abs x) 0.000001)
                        (< (abs y) 0.000001)))
         (zero-ang (< (abs angle) 0.000001))
         (call (format "text(%S, size=%s);"
                       str
                       (scad-sketch-session--fmt-num size))))
    (cond
     ((and zero-pos zero-ang)
      (format "%s%s\n" indent call))

     (zero-pos
      (format "%srotate(%s)\n%s  %s\n"
              indent
              (scad-sketch-session--fmt-num angle)
              indent
              call))

     (zero-ang
      (format "%stranslate([%s, %s])\n%s  %s\n"
              indent
              (scad-sketch-session--fmt-num x)
              (scad-sketch-session--fmt-num y)
              indent
              call))

     (t
      (format "%stranslate([%s, %s])\n%s  rotate(%s)\n%s    %s\n"
              indent
              (scad-sketch-session--fmt-num x)
              (scad-sketch-session--fmt-num y)
              indent
              (scad-sketch-session--fmt-num angle)
              indent
              call)))))

(defun scad-sketch-session--emit-polygon-shape-call (session shape indent)
  "Emit polygon SHAPE call at INDENT."
  (let* ((points    (scad-sketch-shape-points shape))
         (polyround (scad-sketch-shape-polyround shape))
         (source    (scad-sketch-session--shape-source-name session shape)))
    (scad-sketch-session--emit-polygon-call
     points indent polyround source)))

(defun scad-sketch-session--emit-shape-with-assignments (session shape indent)
  "Return (:assignments STR :call STR) for SHAPE in SESSION."
  (pcase (scad-sketch-shape-kind shape)
    ('circle
     (list :assignments ""
           :call (scad-sketch-session--emit-circle-shape shape indent)))

    ('text
     (list :assignments ""
           :call (scad-sketch-session--emit-text-shape shape indent)))

    ('polygon
     (let* ((shape-points (scad-sketch-shape-points shape))
            (polyround    (scad-sketch-shape-polyround shape))
            (source       (scad-sketch-session--shape-source-name session shape)))
       (cond
        (source
         (list :assignments ""
               :call (scad-sketch-session--emit-polygon-call
                      shape-points indent polyround source)))

        ((<= (length shape-points) scad-sketch-session-inline-polygon-threshold)
         (list :assignments ""
               :call (scad-sketch-session--emit-polygon-call
                      shape-points indent polyround)))

        (t
         (let ((name (or (scad-sketch-session--shape-extracted-name shape)
                         (scad-sketch-session--unique-sketch-name session))))
           (scad-sketch-session--set-shape-extracted-name shape name)
           (list :assignments
                 (scad-sketch-session--emit-array-assignment
                  name shape-points "" polyround)
                 :call
                 (scad-sketch-session--emit-polygon-call
                  shape-points indent polyround nil name)))))))

    (_
     (list :assignments "" :call ""))))

(defun scad-sketch-session--emit-tree (session tree indent)
  "Emit TREE for SESSION at INDENT."
  (pcase (plist-get tree :kind)
    ('shape
     (let* ((shape-id (plist-get tree :shape-id))
            (shape (scad-sketch-session-shape-by-id session shape-id))
            (emitted (scad-sketch-session--emit-shape-with-assignments
                      session shape indent)))
       (concat (plist-get emitted :assignments)
               (plist-get emitted :call))))
    ('boolean
     (let ((op (plist-get tree :op))
           (children (plist-get tree :children))
           (body ""))
       (dolist (child children)
         (let ((emitted (scad-sketch-session--emit-tree
                         session child (concat indent "  "))))
           ;; Generated assignments from child tree are already part of EMITTED.
           ;; For now, allow them inline before the child call in the boolean body.
           (setq body (concat body emitted))))
       (format "%s%s() {\n%s%s}\n"
               indent op body indent)))
    (_ "")))

(defun scad-sketch-session--target-indent (target)
  "Return indentation string for TARGET's beginning marker."
  (let ((marker (scad-sketch-target-beg-marker target)))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (beginning-of-line)
        (if (looking-at "[ \t]*")
            (match-string-no-properties 0)
          "")))))

(defun scad-sketch-session--source-target-replacement (session target)
  "Return replacement text for source TARGET in SESSION."
  (let* ((indent (scad-sketch-session--target-indent target))
         (shape
          (cl-find-if
           (lambda (shape)
             (eq (scad-sketch-shape-source-target-id shape)
                 (scad-sketch-target-id target)))
           (scad-sketch-session-shapes session)))
         (points
          (cond (shape
                 (scad-sketch-shape-points shape))
                ((eq (scad-sketch-target-kind target) 'array)
                 (plist-get (scad-sketch-target-node target) :points))
                (t nil))))
    (scad-sketch-session--emit-array-assignment
     (scad-sketch-target-name target)
     points
     indent
     (scad-sketch-target-polyround target))))

(defun scad-sketch-session--root-target-replacement (session root-target)
  "Return replacement text for ROOT-TARGET in SESSION."
  (let ((indent (scad-sketch-session--target-indent root-target)))
    (scad-sketch-session--emit-tree
     session (scad-sketch-session-tree session) indent)))

(defun scad-sketch-session-preview (session)
  "Return the source preview for SESSION."
  (scad-sketch-session-sync-active-shape-from-points session)
  (let ((root (scad-sketch-session-root-target session)))
    (if root
        (scad-sketch-session--root-target-replacement session root)
      (let ((source-target (car (scad-sketch-session-source-targets session))))
        (if source-target
            (scad-sketch-session--source-target-replacement
             session source-target)
          "")))))

(defun scad-sketch-session--write-target-replacement (target replacement)
  "Replace TARGET with REPLACEMENT in its source buffer."
  (when replacement
    (goto-char (scad-sketch-target-beg-marker target))
    (delete-region (scad-sketch-target-beg-marker target)
                   (scad-sketch-target-end-marker target))
    (insert replacement)
    (set-marker (scad-sketch-target-end-marker target) (point))))

(defun scad-sketch-session-write-back (session)
  "Write SESSION edits back to its source buffer."
  (scad-sketch-session-sync-active-shape-from-points session)
  (let* ((source (scad-sketch-session-source-buffer session))
         (root   (scad-sketch-session-root-target session))
         (source-targets (scad-sketch-session-source-targets session)))
    (unless (buffer-live-p source)
      (user-error "Source buffer is gone"))

    (when (and (null root)
               (> (length (scad-sketch-session-shapes session)) 1))
      (signal 'scad-sketch-unsupported-edit-target
              (list "This session only owns arrays; open a polygon, shape, or boolean to write multiple shapes")))

    (let (write-items)
      (dolist (target source-targets)
        (push (cons target
                    (scad-sketch-session--source-target-replacement
                     session target))
              write-items))
      (when root
        (push (cons root
                    (scad-sketch-session--root-target-replacement
                     session root))
              write-items))

      (setq write-items
            (sort write-items
                  (lambda (a b)
                    (> (marker-position
                        (scad-sketch-target-beg-marker (car a)))
                       (marker-position
                        (scad-sketch-target-beg-marker (car b)))))))

      (with-current-buffer source
        (save-excursion
          (dolist (item write-items)
            (scad-sketch-session--write-target-replacement
             (car item) (cdr item))))))

    (setf (scad-sketch-session-dirty session) nil)
    session))

(provide 'scad-sketch-session)
;;; scad-sketch-session.el ends here
