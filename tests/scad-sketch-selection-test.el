;;; scad-sketch-selection-test.el --- ERT tests for selection/group refs -*- lexical-binding: t; -*-

;;; Commentary:

;; Run from the repository root with:
;;
;;   emacs --batch -Q \
;;     --load tests/scad-sketch-selection-test.el \
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
;; This suite locks down the distinction between boolean group wrapper refs and
;; boolean group member refs:
;;
;;   :boolean          means the group wrapper itself
;;   :boolean-members  means the child objects in the group
;;
;; Interaction policy:
;;
;;   - both refs are attention/focus targets
;;   - both refs select/toggle direct child shape refs
;;   - neither ref is stored in the selection list
;;   - only :boolean is a valid break-apart target
;;   - :boolean-members is not breakable

;;; Code:

(require 'ert)
(require 'cl-lib)

;; ---------------------------------------------------------------------------
;; Locate and load project files.
;; Supports running from the repo root or from tests/.
;; ---------------------------------------------------------------------------

(defvar scad-sketch-selection-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

(defvar scad-sketch-selection-test--root
  (expand-file-name ".." scad-sketch-selection-test--dir)
  "Repository root directory.")

(add-to-list 'load-path scad-sketch-selection-test--root)

(defun ssel-test--load (file feature)
  "Load FILE from the repository root unless FEATURE is already provided."
  (unless (featurep feature)
    (load-file (expand-file-name file scad-sketch-selection-test--root))))

;; Keep this load order explicit.  Some editor modules use forward references.
(ssel-test--load "scad-sketch-parse.el"              'scad-sketch-parse)
(ssel-test--load "scad-sketch-geometry.el"           'scad-sketch-geometry)
(ssel-test--load "scad-sketch-session.el"            'scad-sketch-session)
(ssel-test--load "scad-sketch-editor--refs.el"       'scad-sketch-editor--refs)
(ssel-test--load "scad-sketch-editor--selection.el"  'scad-sketch-editor--selection)
(ssel-test--load "scad-sketch-editor-core.el"        'scad-sketch-editor-core)
(ssel-test--load "scad-sketch-editor--editing.el"    'scad-sketch-editor--editing)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defconst ssel-test--simple-union-source
  (concat "union() {\n"
          "  square([20, 20]);\n"
          "  translate([30, 0])\n"
          "    circle(r=5);\n"
          "}\n")
  "Small boolean fixture with one union group and two child shapes.")

(defconst ssel-test--nested-boolean-source
  (concat "difference() {\n"
          "  union() {\n"
          "    square([80, 40]);\n"
          "    translate([80, 0])\n"
          "      circle(r=20);\n"
          "  }\n"
          "  circle(r=10);\n"
          "}\n")
  "Nested boolean fixture with a root difference and child union.")

(defun ssel-test--goto-substring (needle &optional offset)
  "Move point to NEEDLE's beginning plus OFFSET."
  (goto-char (point-min))
  (unless (search-forward needle nil t)
    (error "Could not find test substring: %S" needle))
  (goto-char (+ (match-beginning 0) (or offset 0))))

(defmacro ssel-test--with-session (source needle &rest body)
  "Create a session from SOURCE at NEEDLE and run BODY with `session' bound."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,source)
     (ssel-test--goto-substring ,needle)
     (let ((session (scad-sketch-session-at-point)))
       ,@body)))

(defun ssel-test--set-point (session xy)
  "Set SESSION point to XY and reset hover index."
  (setf (scad-sketch-session-point session) (copy-sequence xy))
  (setf (scad-sketch-session-hover-index session) 0)
  session)

(defun ssel-test--all-groups (session)
  "Return all boolean tree groups in SESSION."
  (scad-sketch-session--tree-groups
   (scad-sketch-session-tree session)))

(defun ssel-test--first-group (session)
  "Return first boolean tree group in SESSION."
  (or (car (ssel-test--all-groups session))
      (error "No boolean group in session")))

(defun ssel-test--first-group-id (session)
  "Return first boolean tree group id in SESSION."
  (plist-get (ssel-test--first-group session) :group-id))

(defun ssel-test--first-group-shape-ids (session)
  "Return shape ids under the first boolean tree group in SESSION."
  (scad-sketch-session--tree-shape-ids
   (ssel-test--first-group session)))

(defun ssel-test--nested-union-group (session)
  "Return the nested union group from SESSION."
  (or (cl-find-if (lambda (group)
                    (eq (plist-get group :op) 'union))
                  (ssel-test--all-groups session))
      (error "No union group in session")))

(defun ssel-test--ref-present-p (refs ref)
  "Return non-nil if REFS contains REF structurally."
  (cl-some (lambda (candidate)
             (scad-sketch--same-ref-p candidate ref))
           refs))

(defun ssel-test--selection-shape-ids (session)
  "Return whole-shape ids stored in SESSION selection."
  (delq nil
        (mapcar (lambda (ref)
                  (when (eq (scad-sketch--ref-kind ref) 'shape)
                    (scad-sketch--ref-shape-id ref)))
                (scad-sketch-session-selection session))))

(defun ssel-test--selection-has-boolean-ref-p (session)
  "Return non-nil if SESSION selection contains a boolean-ish ref."
  (cl-some (lambda (ref)
             (memq (scad-sketch--ref-kind ref)
                   '(boolean boolean-members)))
           (scad-sketch-session-selection session)))

(defun ssel-test--same-id-set-p (a b)
  "Return non-nil if A and B contain the same ids."
  (and (cl-every (lambda (x) (memq x b)) a)
       (cl-every (lambda (x) (memq x a)) b)))

(defun ssel-test--assert-child-shapes-selected (session shape-ids)
  "Assert SESSION selection contains exactly whole refs for SHAPE-IDS."
  (should (ssel-test--same-id-set-p
           shape-ids
           (ssel-test--selection-shape-ids session)))
  (should-not (ssel-test--selection-has-boolean-ref-p session)))

(defun ssel-test--attention-with-focus (session ref)
  "Return SESSION attention after setting focus to REF and moving point far away."
  ;; Put point far away so hover does not override focus.
  (ssel-test--set-point session '(9999.0 9999.0))
  (setf (scad-sketch-session-focus-ref session) ref)
  (scad-sketch--attention-ref session))


;;;; =========================================================================
;;;; 1. Ref identity and summaries
;;;; =========================================================================

(ert-deftest ssel-boolean-ref-and-members-ref-are-distinct ()
  ":boolean and :boolean-members are distinct refs for the same group id."
  (let* ((group-id 'group-0)
         (wrapper  (scad-sketch--boolean-ref group-id))
         (members  (scad-sketch--boolean-members-ref group-id)))
    (should (eq (scad-sketch--ref-kind wrapper) 'boolean))
    (should (eq (scad-sketch--ref-kind members) 'boolean-members))
    (should (equal (scad-sketch--ref-group-id wrapper) group-id))
    (should (equal (scad-sketch--ref-group-id members) group-id))
    (should-not (scad-sketch--same-ref-p wrapper members))))

(ert-deftest ssel-ref-summary-distinguishes-wrapper-and-members ()
  "Ref summaries should make wrapper/member attention states visible."
  (let* ((group-id 'group-0)
         (wrapper  (scad-sketch--boolean-ref group-id))
         (members  (scad-sketch--boolean-members-ref group-id))
         (ws       (scad-sketch--ref-summary wrapper))
         (ms       (scad-sketch--ref-summary members)))
    (should (string-match-p "group" ws))
    (should (string-match-p "members" ms))
    (should-not (string= ws ms))))


;;;; =========================================================================
;;;; 2. Focus/selectable refs
;;;; =========================================================================

(ert-deftest ssel-selectable-refs-include-boolean-wrapper-and-members ()
  "Global focus cycle includes both group wrapper and group-members refs."
  (ssel-test--with-session ssel-test--simple-union-source "union"
    (let* ((group-id (ssel-test--first-group-id session))
           (wrapper  (scad-sketch--boolean-ref group-id))
           (members  (scad-sketch--boolean-members-ref group-id))
           (refs     (scad-sketch--selectable-refs session)))
      (should (ssel-test--ref-present-p refs wrapper))
      (should (ssel-test--ref-present-p refs members)))))

(ert-deftest ssel-focus-attention-can-be-boolean-wrapper ()
  "A boolean wrapper ref can become attention through focus."
  (ssel-test--with-session ssel-test--simple-union-source "union"
    (let* ((group-id (ssel-test--first-group-id session))
           (wrapper  (scad-sketch--boolean-ref group-id))
           (attention (ssel-test--attention-with-focus session wrapper)))
      (should (scad-sketch--same-ref-p attention wrapper)))))

(ert-deftest ssel-focus-attention-can-be-boolean-members ()
  "A boolean-members ref can become attention through focus."
  (ssel-test--with-session ssel-test--simple-union-source "union"
    (let* ((group-id (ssel-test--first-group-id session))
           (members  (scad-sketch--boolean-members-ref group-id))
           (attention (ssel-test--attention-with-focus session members)))
      (should (scad-sketch--same-ref-p attention members)))))


;;;; =========================================================================
;;;; 3. Hover refs
;;;; =========================================================================

(ert-deftest ssel-hover-candidates-include-boolean-wrapper-and-members ()
  "Hover candidates include both group wrapper and group-members refs."
  (ssel-test--with-session ssel-test--simple-union-source "union"
    ;; Point inside the first square.
    (ssel-test--set-point session '(10.0 10.0))
    (let* ((group-id (ssel-test--first-group-id session))
           (wrapper  (scad-sketch--boolean-ref group-id))
           (members  (scad-sketch--boolean-members-ref group-id))
           (refs     (scad-sketch--hover-candidates session)))
      (should (ssel-test--ref-present-p refs wrapper))
      (should (ssel-test--ref-present-p refs members)))))

(ert-deftest ssel-hover-candidates-include-point-or-shape-before-groups ()
  "Hover stack should include concrete child refs as well as group refs."
  (ssel-test--with-session ssel-test--simple-union-source "union"
    (ssel-test--set-point session '(10.0 10.0))
    (let ((refs (scad-sketch--hover-candidates session)))
      (should (cl-some (lambda (ref)
                         (memq (scad-sketch--ref-kind ref)
                               '(point shape)))
                       refs))
      (should (cl-some (lambda (ref)
                         (memq (scad-sketch--ref-kind ref)
                               '(boolean boolean-members)))
                       refs)))))


;;;; =========================================================================
;;;; 4. Selection expansion
;;;; =========================================================================

(ert-deftest ssel-toggle-boolean-members-selects-child-shapes ()
  "SPC on :boolean-members expands to direct child shape selection."
  (ssel-test--with-session ssel-test--simple-union-source "union"
    (let* ((group-id  (ssel-test--first-group-id session))
           (members   (scad-sketch--boolean-members-ref group-id))
           (shape-ids (ssel-test--first-group-shape-ids session)))
      (scad-sketch--toggle-ref-selection session members)
      (ssel-test--assert-child-shapes-selected session shape-ids)

      ;; Toggle again removes the direct child-shape selection.
      (scad-sketch--toggle-ref-selection session members)
      (should (null (scad-sketch-session-selection session))))))

(ert-deftest ssel-toggle-boolean-wrapper-selects-child-shapes ()
  "SPC on :boolean also expands to direct child shape selection."
  (ssel-test--with-session ssel-test--simple-union-source "union"
    (let* ((group-id  (ssel-test--first-group-id session))
           (wrapper   (scad-sketch--boolean-ref group-id))
           (shape-ids (ssel-test--first-group-shape-ids session)))
      (scad-sketch--toggle-ref-selection session wrapper)
      (ssel-test--assert-child-shapes-selected session shape-ids)

      ;; Toggle again removes the direct child-shape selection.
      (scad-sketch--toggle-ref-selection session wrapper)
      (should (null (scad-sketch-session-selection session))))))

(ert-deftest ssel-selection-never-stores-boolean-refs ()
  "Selection storage should contain direct child shapes, never boolean refs."
  (ssel-test--with-session ssel-test--nested-boolean-source "difference"
    (let* ((group    (ssel-test--nested-union-group session))
           (group-id (plist-get group :group-id))
           (wrapper  (scad-sketch--boolean-ref group-id))
           (members  (scad-sketch--boolean-members-ref group-id)))
      (scad-sketch--toggle-ref-selection session wrapper)
      (should-not (ssel-test--selection-has-boolean-ref-p session))
      (scad-sketch--toggle-ref-selection session wrapper)

      (scad-sketch--toggle-ref-selection session members)
      (should-not (ssel-test--selection-has-boolean-ref-p session)))))


;;;; =========================================================================
;;;; 5. Break target semantics
;;;; =========================================================================

(ert-deftest ssel-break-target-accepts-boolean-wrapper-attention ()
  "Break target accepts :boolean wrapper attention."
  (ssel-test--with-session ssel-test--simple-union-source "union"
    (let* ((group-id (ssel-test--first-group-id session))
           (wrapper  (scad-sketch--boolean-ref group-id)))
      (ssel-test--attention-with-focus session wrapper)
      (should (scad-sketch--same-ref-p
               (scad-sketch--current-breakable-group-ref session)
               wrapper)))))

(ert-deftest ssel-break-target-rejects-boolean-members-attention ()
  "Break target rejects :boolean-members attention."
  (ssel-test--with-session ssel-test--simple-union-source "union"
    (let* ((group-id (ssel-test--first-group-id session))
           (members  (scad-sketch--boolean-members-ref group-id)))
      (ssel-test--attention-with-focus session members)
      (should (null (scad-sketch--current-breakable-group-ref session))))))

(ert-deftest ssel-break-target-rejects-child-shape-attention ()
  "Break target rejects ordinary child shape attention."
  (ssel-test--with-session ssel-test--simple-union-source "union"
    (let* ((shape-id (car (ssel-test--first-group-shape-ids session)))
           (shape-ref (scad-sketch--shape-ref shape-id)))
      (ssel-test--attention-with-focus session shape-ref)
      (should (null (scad-sketch--current-breakable-group-ref session))))))

(ert-deftest ssel-tree-break-ref-breaks-wrapper ()
  "Low-level tree break works on :boolean wrapper refs."
  (ssel-test--with-session ssel-test--simple-union-source "union"
    (let* ((tree      (scad-sketch-session-tree session))
           (shape-ids (ssel-test--first-group-shape-ids session))
           (group-id  (ssel-test--first-group-id session))
           (wrapper   (scad-sketch--boolean-ref group-id))
           (new-tree  (scad-sketch--tree-break-ref tree wrapper)))
      ;; Breaking the root union yields a sequence of its children.
      (should (eq (plist-get new-tree :kind) 'sequence))
      (should (ssel-test--same-id-set-p
               shape-ids
               (scad-sketch-session--tree-shape-ids new-tree))))))

(ert-deftest ssel-tree-break-ref-rejects-boolean-members ()
  "Low-level tree break does not accept :boolean-members refs."
  (ssel-test--with-session ssel-test--simple-union-source "union"
    (let* ((tree     (scad-sketch-session-tree session))
           (group-id (ssel-test--first-group-id session))
           (members  (scad-sketch--boolean-members-ref group-id)))
      (should-error
       (scad-sketch--tree-break-ref tree members)
       :type 'user-error))))


(provide 'scad-sketch-selection-test)
;;; scad-sketch-selection-test.el ends here
