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
  kind             ; 'array, 'polygon-inline, 'polygon-ref, 'union
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
  kind             ; currently only 'polygon
  points
  closed
  polyround
  source-target-id
  call-target-id
  metadata)

(cl-defstruct scad-sketch-session
  name units grid fine-step coarse-step closed
  points point
  marks            ; list of [x y], newest first; (car marks) is current mark
  named-marks selected-index

  ;; Editor object model.
  ;;
  ;; `points' is kept as a compatibility mirror of the active shape.  The
  ;; canonical editor state is `shapes'.  Commands that only understand a single
  ;; point list can continue to use `points' as long as they sync through the
  ;; active shape helpers.
  shapes
  active-shape-id

  ;; Source/write plan.
  ;;
  ;; `targets' are source regions the session knows about.  `root-target-id',
  ;; when non-nil, is the enclosing call/union region that can serialize one or
  ;; more editor shapes.  Array-only sessions have no root target; they can only
  ;; update the owned array source target.
  targets
  root-target-id

  ;; Selection/attention model.
  ;;
  ;; A selection ref is a plist like:
  ;;   (:kind point :shape-id shape-0 :index 2)
  ;;   (:kind shape :shape-id shape-0)
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
  "Set SESSION's active shape to SHAPE-ID and mirror its points into SESSION."
  (let ((shape (scad-sketch-session-shape-by-id session shape-id)))
    (unless shape
      (user-error "No such shape: %S" shape-id))
    (setf (scad-sketch-session-active-shape-id session) shape-id)
    (setf (scad-sketch-session-points session)
          (copy-tree (scad-sketch-shape-points shape)))
    (setf (scad-sketch-session-closed session)
          (scad-sketch-shape-closed shape))
    session))

(defun scad-sketch-session-sync-active-shape-from-points (session)
  "Copy SESSION's compatibility `points' mirror into its active shape."
  (let ((shape (scad-sketch-session-active-shape session)))
    (when shape
      (setf (scad-sketch-shape-points shape)
            (copy-tree (scad-sketch-session-points session)))
      (setf (scad-sketch-shape-closed shape)
            (scad-sketch-session-closed session))))
  session)

(defun scad-sketch-session--make-shape
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

(defun scad-sketch-session-add-shape (session points &optional polyround)
  "Add a new polygon shape with POINTS to SESSION and make it active.

A session can add shapes only when it owns a root target.  Array-only sessions
have no semantically safe place to serialize multiple shapes."
  (unless (scad-sketch-session-root-target session)
    (signal 'scad-sketch-unsupported-edit-target
            (list "This session only owns an array; open a polygon or union to add shapes")))
  (scad-sketch-session-sync-active-shape-from-points session)
  (let* ((shape-id (scad-sketch-session-next-shape-id session))
         (shape
          (scad-sketch-session--make-shape
           shape-id points polyround nil nil
           (list :created-in-session t))))
    (setf (scad-sketch-session-shapes session)
          (append (scad-sketch-session-shapes session) (list shape)))
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


(defun scad-sketch-session--emit-inline-polygon-replacement (session target indent)
  "Emit replacement source for inline polygon TARGET from SESSION."
  (scad-sketch-session-sync-active-shape-from-points session)
  (let* ((points    (scad-sketch-session-points session))
         (polyround (scad-sketch-target-polyround target))
         (n         (length points)))
    (if (<= n scad-sketch-session-inline-polygon-threshold)
        (scad-sketch-session--emit-polygon-call points indent polyround)
      (let ((name (or (plist-get (scad-sketch-target-metadata target)
                                 :extracted-name)
                      (scad-sketch-session--unique-sketch-name session))))
        ;; Remember the generated name so repeated writes from the same editor
        ;; session are stable.
        (setf (scad-sketch-target-metadata target)
              (plist-put (scad-sketch-target-metadata target)
                         :extracted-name name))
        (concat
         (scad-sketch-session--emit-array-assignment
          name points indent polyround)
         (scad-sketch-session--emit-polygon-call
          points indent polyround nil name))))))

(defun scad-sketch-session--shape-polyround (shape)
  "Return SHAPE's polyRound fn value, if any."
  (scad-sketch-shape-polyround shape))

(defun scad-sketch-session--shape-source-name (shape)
  "Return SHAPE's source variable name, if it has one."
  (plist-get (scad-sketch-shape-metadata shape) :source-name))

(defun scad-sketch-session--shape-extracted-name (shape)
  "Return SHAPE's generated extraction name, if one exists."
  (plist-get (scad-sketch-shape-metadata shape) :extracted-name))

(defun scad-sketch-session--set-shape-extracted-name (shape name)
  "Record NAME as SHAPE's generated extraction name."
  (setf (scad-sketch-shape-metadata shape)
        (plist-put (scad-sketch-shape-metadata shape)
                   :extracted-name name)))

(defun scad-sketch-session--emit-union-shape (session shape indent)
  "Return (:assignments STR :call STR) for SHAPE in SESSION."
  (let* ((points    (scad-sketch-shape-points shape))
         (polyround (scad-sketch-session--shape-polyround shape))
         (source    (scad-sketch-session--shape-source-name shape)))
    (cond
     ;; Preserve source references.  The source array is written separately
     ;; through the shape's source target.
     (source
      (list :assignments ""
            :call (scad-sketch-session--emit-polygon-call
                   points indent polyround source)))

     ;; Small inline shapes stay inline.
     ((<= (length points) scad-sketch-session-inline-polygon-threshold)
      (list :assignments ""
            :call (scad-sketch-session--emit-polygon-call
                   points indent polyround)))

     ;; Large inline/new shapes get extracted to a generated assignment.
     (t
      (let ((name (or (scad-sketch-session--shape-extracted-name shape)
                      (scad-sketch-session--unique-sketch-name session))))
        (scad-sketch-session--set-shape-extracted-name shape name)
        (list :assignments
              (scad-sketch-session--emit-array-assignment
               name points "" polyround)
              :call
              (scad-sketch-session--emit-polygon-call
               points indent polyround nil name)))))))

(defun scad-sketch-session--emit-union-replacement (session indent)
  "Emit SESSION as a canonical union at INDENT."
  (scad-sketch-session-sync-active-shape-from-points session)
  (let ((assignments "")
        (calls ""))
    (dolist (shape (scad-sketch-session-shapes session))
      (let* ((emitted (scad-sketch-session--emit-union-shape
                       session shape (concat indent "  ")))
             (shape-assignments (plist-get emitted :assignments))
             (shape-call (plist-get emitted :call)))
        (unless (string= shape-assignments "")
          (setq assignments (concat assignments shape-assignments)))
        (setq calls (concat calls shape-call))))
    (concat assignments
            indent "union() {\n"
            calls
            indent "}\n")))

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

(defun scad-sketch-session--target-replacement (session target)
  "Return replacement source for TARGET using SESSION's current points."
  (let ((indent (scad-sketch-session--target-indent target)))
    (pcase (scad-sketch-target-kind target)
      ('array
       (scad-sketch-session--emit-array-assignment
        (scad-sketch-target-name target)
        (scad-sketch-session-points session)
        indent
        (scad-sketch-target-polyround target)))
      ('polygon-var-source
       ;; For source arrays attached to a shape, use that shape's points.
       ;; Fall back to SESSION points for legacy single-shape sessions.
       (let* ((shape
               (cl-find-if
                (lambda (shape)
                  (eq (scad-sketch-shape-source-target shape) target))
                (scad-sketch-session-shapes session)))
              (points (if shape
                          (scad-sketch-shape-points shape)
                        (scad-sketch-session-points session))))
         (scad-sketch-session--emit-array-assignment
          (scad-sketch-target-name target)
          points
          indent
          (scad-sketch-target-polyround target))))
      ('polygon-inline
       (scad-sketch-session--emit-inline-polygon-replacement
        session target indent))
      ('union-root
       (scad-sketch-session--emit-union-replacement session indent))
      (_
       nil))))

(defun scad-sketch-session-preview (session)
  "Return the source preview for SESSION."
  (scad-sketch-session-sync-active-shape-from-points session)
  (if (eq (scad-sketch-session-root-kind session) 'union)
      (scad-sketch-session--emit-union-replacement session "")
    (let ((primary (car (scad-sketch-session-targets session))))
      (pcase (and primary (scad-sketch-target-kind primary))
        ('polygon-inline
         (scad-sketch-session--emit-inline-polygon-replacement session primary ""))
        ('polygon-call
         (let* ((source-target
                 (cl-find-if (lambda (tgt)
                               (eq (scad-sketch-target-kind tgt)
                                   'polygon-var-source))
                             (scad-sketch-session-targets session)))
                (source-name (and source-target
                                  (scad-sketch-target-name source-target)))
                (polyround (scad-sketch-target-polyround primary)))
           (concat
            (when source-target
              (scad-sketch-session--emit-array-assignment
               source-name
               (scad-sketch-session-points session)
               ""
               polyround))
            (scad-sketch-session--emit-polygon-call
             (scad-sketch-session-points session)
             ""
             polyround
             source-name))))
        (_
         (scad-sketch-session--emit-array-assignment
          (scad-sketch-session-name session)
          (scad-sketch-session-points session)
          ""))))))

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

(defun scad-sketch-session--make-polygon-inline-root-target (node)
  "Build a writable root target for inline polygon NODE."
  (scad-sketch-session--make-root-target node 'polygon-inline 'root-0))

(defun scad-sketch-session--make-polygon-ref-root-target (node)
  "Build a writable root target for polygon variable-reference NODE."
  (scad-sketch-session--make-root-target node 'polygon-ref 'root-0))

(defun scad-sketch-session--make-union-root-target (node)
  "Build a writable root target for union NODE."
  (scad-sketch-session--make-root-target node 'union 'root-0))

(defun scad-sketch-session--make-polygon-inline-target (node)
  "Build a writable inline polygon target for NODE."
  (let ((markers (scad-sketch-session--make-marker-pair node)))
    (make-scad-sketch-target
     :id 'polygon-inline-0
     :kind 'polygon-inline
     :node node
     :source-node nil
     :beg-marker (car markers)
     :end-marker (cdr markers)
     :name "polygon"
     :points (copy-tree (plist-get node :points))
     :polyround (plist-get node :polyround)
     :write-p t
     :metadata nil)))

(defun scad-sketch-session--make-polygon-call-target (node)
  "Build a read-only target representing polygon variable-ref NODE."
  (let ((markers (scad-sketch-session--make-marker-pair node)))
    (make-scad-sketch-target
     :id 'polygon-call-0
     :kind 'polygon-call
     :node node
     :source-node nil
     :beg-marker (car markers)
     :end-marker (cdr markers)
     :name (or (plist-get node :source) "polygon")
     :points nil
     :polyround (plist-get node :polyround)
     :write-p nil
     :metadata nil)))

(defun scad-sketch-session--make-session
    (name shapes active-shape-id targets root-target-id
          beg-marker end-marker
          &optional ast path root-node)
  "Create a sketch session.

NAME is the display name.  SHAPES are editor objects.  ACTIVE-SHAPE-ID chooses
which shape is mirrored into the compatibility `points' slot.  TARGETS and
ROOT-TARGET-ID define the session's write plan."
  (let* ((active-shape
          (or (cl-find-if (lambda (shape)
                            (eq (scad-sketch-shape-id shape) active-shape-id))
                          shapes)
              (car shapes)))
         (shape-id (and active-shape (scad-sketch-shape-id active-shape)))
         (points   (and active-shape
                        (copy-tree (scad-sketch-shape-points active-shape)))))
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


(defun scad-sketch-session--polygon-node->shape-and-targets (ast node index)
  "Return (SHAPE . TARGETS) for polygon NODE in AST.

INDEX determines the generated shape id.  This supports inline polygon nodes
and variable-reference polygon nodes.  Variable-reference nodes produce a
read-only polygon-call target plus a writable source-array target."
  (let ((shape-id (scad-sketch-session--shape-id index)))
    (if (plist-get node :source)
        (let* ((source-name (plist-get node :source))
               (source-node
                (scad-sketch-session--resolve-array-node ast source-name node)))
          (unless source-node
            (user-error "Could not resolve polygon source `%s'" source-name))
          (let* ((call-target
                  (scad-sketch-session--make-polygon-call-target node))
                 (source-target
                  (scad-sketch-session--make-array-target
                   source-node
                   (intern (format "polygon-var-source-%d" index))
                   'polygon-var-source
                   (plist-get node :polyround)))
                 (shape
                  (make-scad-sketch-shape
                   :id shape-id
                   :kind 'polygon
                   :points (copy-tree (plist-get source-node :points))
                   :closed t
                   :polyround (plist-get node :polyround)
                   :source-target source-target
                   :metadata (list :source-name source-name
                                   :call-node node
                                   :source-node source-node))))
            (cons shape (list call-target source-target))))
      (let* ((target
              (scad-sketch-session--make-polygon-inline-target node))
             (shape
              (make-scad-sketch-shape
               :id shape-id
               :kind 'polygon
               :points (copy-tree (plist-get node :points))
               :closed t
               :polyround (plist-get node :polyround)
               :source-target nil
               :metadata (list :inline-node node))))
        (cons shape (list target))))))

(defun scad-sketch-session--union-supported-child-p (node)
  "Return non-nil if NODE is currently supported as a union child."
  (eq (plist-get node :type) 'polygon))

(defun scad-sketch-session--session-from-union (ast path node)
  "Build a multi-shape session for union NODE.

This first union pass supports only direct polygon-ish children.  Transformed
children and nested booleans should be added later once geometry normalization
can preserve/edit them correctly."
  (let ((children (plist-get node :children))
        shapes
        child-targets
        index)
    (setq index 0)
    (dolist (child children)
      (unless (scad-sketch-session--union-supported-child-p child)
        (user-error "Unsupported union child for scad-sketch: %S"
                    (plist-get child :type)))
      (let ((pair (scad-sketch-session--polygon-node->shape-and-targets
                   ast child index)))
        (push (car pair) shapes)
        (setq child-targets (append (cdr pair) child-targets))
        (setq index (1+ index))))
    (setq shapes (nreverse shapes))
    (setq child-targets (nreverse child-targets))
    (unless shapes
      (user-error "Union has no editable polygon children"))
    (let* ((root-target
            (scad-sketch-session--make-root-target node 'union-root))
           (active-shape (car shapes))
           (points (copy-tree (scad-sketch-shape-points active-shape))))
      (scad-sketch-session--make-session
       "union"
       points
       (scad-sketch-target-beg-marker root-target)
       (scad-sketch-target-end-marker root-target)
       ast path node
       (cons root-target child-targets)
       shapes
       (scad-sketch-shape-id active-shape)
       'union
       root-target))))

(defun scad-sketch-session--session-from-array (ast path node)
  "Build a session for direct array NODE."
  (let* ((target (scad-sketch-session--make-array-target node))
         (points (copy-tree (plist-get node :points))))
    (scad-sketch-session--make-session
     (plist-get node :name)
     points
     (scad-sketch-target-beg-marker target)
     (scad-sketch-target-end-marker target)
     ast path node
     (list target))))

(defun scad-sketch-session--session-from-inline-polygon (ast path node)
  "Build a session for inline polygon NODE."
  (let* ((target (scad-sketch-session--make-polygon-inline-target node))
         (points (copy-tree (plist-get node :points))))
    (unless points
      (user-error "Polygon has no inline points"))
    (scad-sketch-session--make-session
     "polygon"
     points
     (scad-sketch-target-beg-marker target)
     (scad-sketch-target-end-marker target)
     ast path node
     (list target))))

(defun scad-sketch-session--session-from-polygon-ref (ast path node)
  "Build a session for variable-ref polygon NODE."
  (let* ((source-name (plist-get node :source))
         (source-node (and source-name
                           (scad-sketch-session--resolve-array-node
                            ast source-name node))))
    (unless source-name
      (user-error "Polygon is not a variable reference"))
    (unless source-node
      (user-error "Could not resolve polygon source `%s'" source-name))
    (let* ((call-target (scad-sketch-session--make-polygon-call-target node))
           (source-target
            (scad-sketch-session--make-array-target
             source-node 'polygon-var-source-0 'polygon-var-source
             (plist-get node :polyround)))
           (points (copy-tree (plist-get source-node :points))))
      (scad-sketch-session--make-session
       source-name
       points
       (scad-sketch-target-beg-marker source-target)
       (scad-sketch-target-end-marker source-target)
       ast path node
       (list call-target source-target)))))

(defun scad-sketch-session--session-from-polygon (ast path node)
  "Build a session for polygon NODE."
  (if (plist-get node :source)
      (scad-sketch-session--session-from-polygon-ref ast path node)
    (scad-sketch-session--session-from-inline-polygon ast path node)))

(defun scad-sketch-session-at-point ()
  "Build a parser-backed sketch session for the supported form at point.

Currently supports:
  - array assignments
  - inline polygon([...])
  - inline polygon(polyRound([...], fn))
  - polygon(name), where NAME resolves to an earlier array assignment
  - polygon(polyRound(name, fn)), where NAME resolves similarly
  - union() containing direct polygon-ish children"
  (let* ((source (scad-sketch-session--buffer-source))
         (pos    (scad-sketch-session--buffer-offset))
         (info   (scad-sketch-session--node-at-point source pos))
         (ast    (plist-get info :ast))
         (path   (plist-get info :path))
         (node   (plist-get info :node))
         (union-node
          (cl-find-if (lambda (n)
                        (eq (plist-get n :type) 'union))
                      path)))
    ;; If point is inside a supported union subtree, edit the whole union.
    ;; This deliberately chooses the first/outermost union in PATH.
    (if union-node
        (scad-sketch-session--session-from-union ast path union-node)
      (pcase (plist-get node :type)
        ('array
         (scad-sketch-session--session-from-array ast path node))
        ('polygon
         (scad-sketch-session--session-from-polygon ast path node))
        (_
         (user-error "Point is not inside a supported scad-sketch form"))))))

(defun scad-sketch-session-insert-array-at-point (name)
  "Insert a new empty array named NAME at point and return its session."
  (let (beg end node target shape)
    (setq beg (point-marker))
    (insert (format "%s = [\n];\n" name))
    (setq end (copy-marker (point) t))
    ;; Construct a minimal parser-shaped node so inserted sessions also expose
    ;; `root-node' and `targets' to later code.
    (setq node (list :type 'array
                     :name name
                     :points nil
                     :beg (1- (marker-position beg))
                     :end (1- (marker-position end))))
    (setq target
          (make-scad-sketch-target
           :id 'array-0
           :kind 'array
           :node node
           :source-node node
           :beg-marker beg
           :end-marker end
           :name name
           :points nil
           :polyround nil
           :write-p t
           :metadata nil))
    (setq shape
          (make-scad-sketch-shape
           :id 'shape-0
           :kind 'polygon
           :points nil
           :closed t
           :polyround nil
           :source-target target
           :metadata (list :source-name name :inserted-array t)))
    (scad-sketch-session--make-session
     name nil beg end nil (list node) node (list target)
     (list shape) 'shape-0 nil nil)))

;;;; Write-back

(defun scad-sketch-session--write-target (session target)
  "Write one writable TARGET using SESSION's current points."
  (let ((replacement (scad-sketch-session--target-replacement session target))
        (beg (scad-sketch-target-beg-marker target))
        (end (scad-sketch-target-end-marker target)))
    (when replacement
      (goto-char beg)
      (delete-region beg end)
      (insert replacement)
      (set-marker end (point)))))

(defun scad-sketch-session-write-back (session)
  "Write SESSION edits back to its source buffer."
  (scad-sketch-session-sync-active-shape-from-points session)
  (let* ((source (scad-sketch-session-source-buffer session))
         (targets
          (if (eq (scad-sketch-session-root-kind session) 'union)
              ;; For union sessions, rewrite the root union and any referenced
              ;; source arrays.  Do not separately rewrite inline child polygon
              ;; targets because they live inside the root region.
              (let ((root (scad-sketch-session-root-target session))
                    (source-targets
                     (delq nil
                           (mapcar #'scad-sketch-shape-source-target
                                   (scad-sketch-session-shapes session)))))
                (delq nil (cons root source-targets)))
            (cl-remove-if-not #'scad-sketch-target-write-p
                              (scad-sketch-session-targets session)))))
    (unless (buffer-live-p source)
      (user-error "Source buffer is gone"))
    ;; Multiple targets must be replaced from later buffer positions to earlier
    ;; positions so earlier edits do not shift later regions.
    (setq targets
          (sort (copy-sequence targets)
                (lambda (a b)
                  (> (marker-position (scad-sketch-target-beg-marker a))
                     (marker-position (scad-sketch-target-beg-marker b))))))

    (with-current-buffer source
      (save-excursion
        (dolist (target targets)
          (scad-sketch-session--write-target session target))))

    (setf (scad-sketch-session-dirty session) nil)
    session))

(provide 'scad-sketch-session)
;;; scad-sketch-session.el ends here
