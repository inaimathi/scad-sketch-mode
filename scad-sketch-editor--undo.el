;;; scad-sketch-editor--undo.el --- Undo infrastructure for scad-sketch editor -*- lexical-binding: t; -*-

;;; Commentary:

;; Undo support for the sketch editor.
;;
;; This module owns:
;;
;;   - full session snapshot undo entries
;;   - operation undo entries
;;   - dirty-state restoration
;;   - the interactive `scad-sketch-undo' command
;;
;; There are two undo entry kinds:
;;
;;   snapshot
;;     Captures the editable session state before a mutation.  This is the
;;     default behavior used by `scad-sketch--edit'.
;;
;;   action
;;     Captures a custom inverse operation.  This is useful for operations where
;;     the inverse is more precise than restoring an entire session snapshot,
;;     such as moving selected geometry.
;;
;; This module is loaded after `scad-sketch-editor-core'.  It intentionally
;; depends on core for `scad-sketch--assert-session',
;; `scad-sketch--normalize-attention', and `scad-sketch--render'.  Core calls
;; `scad-sketch--push-undo' only at command runtime, after this file has loaded.

;;; Code:

(require 'cl-lib)
(require 'scad-sketch-session)
(require 'scad-sketch-editor-core)

;;; Snapshot copying

(defun scad-sketch--mark-dirty (session)
  "Mark SESSION as having unsaved edits."
  (setf (scad-sketch-session-dirty session) t))

(defun scad-sketch-undo--copy-shape (shape)
  "Return a deep-enough copy of SHAPE for undo snapshots.

`copy-tree' does not copy `cl-defstruct' vectors, so shape structs need an
explicit copy.  This copies the struct and the mutable slots edited by the
editor."
  (let ((copy (copy-scad-sketch-shape shape)))
    (setf (scad-sketch-shape-points copy)
          (copy-tree (scad-sketch-shape-points shape)))
    (setf (scad-sketch-shape-metadata copy)
          (copy-tree (scad-sketch-shape-metadata shape)))
    copy))

(defun scad-sketch-undo--copy-shapes (shapes)
  "Return deep-enough copies of SHAPES for undo snapshots."
  (mapcar #'scad-sketch-undo--copy-shape shapes))

(defun scad-sketch-undo--snapshot (session)
  "Return a full undo snapshot for SESSION."
  (scad-sketch-session-sync-active-shape-from-points session)
  (list :undo-kind       'snapshot
        :points          (copy-tree (scad-sketch-session-points session))
        :point           (copy-tree (scad-sketch-session-point session))
        :marks           (copy-tree (scad-sketch-session-marks session))
        :named-marks     (copy-tree (scad-sketch-session-named-marks session))
        :selected-index  (scad-sketch-session-selected-index session)
        :closed          (scad-sketch-session-closed session)
        :shapes          (scad-sketch-undo--copy-shapes
                          (scad-sketch-session-shapes session))
        :active-shape-id (scad-sketch-session-active-shape-id session)
        :targets         (copy-tree (scad-sketch-session-targets session))
        :root-target-id  (scad-sketch-session-root-target-id session)
        :selection       (copy-tree (scad-sketch-session-selection session))
        :focus-ref       (copy-tree (scad-sketch-session-focus-ref session))
        :tree            (copy-tree (scad-sketch-session-tree session))
        :dirty           (scad-sketch-session-dirty session)))

(defun scad-sketch-undo--restore-snapshot (session entry)
  "Restore SESSION from snapshot ENTRY."
  (setf (scad-sketch-session-points session)
        (copy-tree (plist-get entry :points)))
  (setf (scad-sketch-session-point session)
        (copy-tree (plist-get entry :point)))
  (setf (scad-sketch-session-marks session)
        (copy-tree (plist-get entry :marks)))
  (setf (scad-sketch-session-named-marks session)
        (copy-tree (plist-get entry :named-marks)))
  (setf (scad-sketch-session-selected-index session)
        (plist-get entry :selected-index))
  (setf (scad-sketch-session-closed session)
        (plist-get entry :closed))
  (setf (scad-sketch-session-shapes session)
        (scad-sketch-undo--copy-shapes
         (plist-get entry :shapes)))
  (setf (scad-sketch-session-active-shape-id session)
        (plist-get entry :active-shape-id))
  (setf (scad-sketch-session-targets session)
        (copy-tree (plist-get entry :targets)))
  (setf (scad-sketch-session-root-target-id session)
        (plist-get entry :root-target-id))
  (setf (scad-sketch-session-selection session)
        (copy-tree (plist-get entry :selection)))
  (setf (scad-sketch-session-focus-ref session)
        (copy-tree (plist-get entry :focus-ref)))
  (when (plist-member entry :tree)
    (setf (scad-sketch-session-tree session)
          (copy-tree (plist-get entry :tree))))
  (setf (scad-sketch-session-dirty session)
        (if (plist-member entry :dirty)
            (plist-get entry :dirty)
          t)))

;;; Public push APIs used by core/editing
(defun scad-sketch--push-undo (session)
  "Push a full snapshot undo entry for SESSION."
  (push (scad-sketch-undo--snapshot session)
        (scad-sketch-session-undo-stack session)))

(defun scad-sketch--push-undo-action (session undo-fn &optional description)
  "Push an operation undo entry for SESSION.

UNDO-FN is called with SESSION when undoing.  DESCRIPTION is optional
human-readable metadata for debugging."
  (push (list :undo-kind   'action
              :undo-fn     undo-fn
              :description description
              :dirty       (scad-sketch-session-dirty session))
        (scad-sketch-session-undo-stack session)))

(defun scad-sketch--edit-with-undo-action (undo-fn fn &optional description)
  "Apply FN as a dirty edit with custom UNDO-FN.

UNDO-FN is pushed instead of a full session snapshot.  It is called with the
current session by `scad-sketch-undo'."
  (let ((session (scad-sketch--assert-session)))
    (scad-sketch--push-undo-action session undo-fn description)
    (funcall fn session)
    (scad-sketch--normalize-attention session)
    (scad-sketch--mark-dirty session)
    (scad-sketch--render)))

;;; Small operation-undo record helpers

(defun scad-sketch-undo-capture-shape-state (session shape-id)
  "Return an undo record for SHAPE-ID in SESSION."
  (let ((shape (scad-sketch-session-shape-by-id session shape-id)))
    (when shape
      (list :kind      'shape-state
            :shape-id  shape-id
            :points    (copy-tree (scad-sketch-shape-points shape))
            :metadata  (copy-tree (scad-sketch-shape-metadata shape))
            :closed    (scad-sketch-shape-closed shape)
            :polyround (scad-sketch-shape-polyround shape)))))

(defun scad-sketch-undo-capture-point-state (session shape-id index)
  "Return an undo record for point INDEX in SHAPE-ID."
  (let* ((shape  (scad-sketch-session-shape-by-id session shape-id))
         (points (and shape (scad-sketch-shape-points shape)))
         (point  (and points (nth index points))))
    (when point
      (list :kind     'point-state
            :shape-id shape-id
            :index    index
            :point    (copy-tree point)))))

(defun scad-sketch-undo-restore-state-record (session record)
  "Restore one operation undo RECORD in SESSION."
  (pcase (plist-get record :kind)
    ('shape-state
     (let ((shape (scad-sketch-session-shape-by-id
                   session
                   (plist-get record :shape-id))))
       (when shape
         (setf (scad-sketch-shape-points shape)
               (copy-tree (plist-get record :points)))
         (setf (scad-sketch-shape-metadata shape)
               (copy-tree (plist-get record :metadata)))
         (setf (scad-sketch-shape-closed shape)
               (plist-get record :closed))
         (setf (scad-sketch-shape-polyround shape)
               (plist-get record :polyround)))))

    ('point-state
     (let* ((shape-id (plist-get record :shape-id))
            (idx      (plist-get record :index))
            (shape    (scad-sketch-session-shape-by-id session shape-id))
            (points   (and shape (scad-sketch-shape-points shape))))
       (when (and points (>= idx 0) (< idx (length points)))
         (setf (scad-sketch-shape-points shape)
               (scad-sketch--replace-nth
                idx
                (copy-tree (plist-get record :point))
                points)))))))

;;; Undo dispatch

(defun scad-sketch-undo--restore-dirty (session entry)
  "Restore SESSION dirty flag from ENTRY."
  (setf (scad-sketch-session-dirty session)
        (if (plist-member entry :dirty)
            (plist-get entry :dirty)
          t)))

(defun scad-sketch-undo--apply-entry (session entry)
  "Apply undo ENTRY to SESSION."
  (pcase (plist-get entry :undo-kind)
    ('action
     (funcall (plist-get entry :undo-fn) session)
     (scad-sketch-undo--restore-dirty session entry))

    ('snapshot
     (scad-sketch-undo--restore-snapshot session entry))

    ;; Backwards compatibility for older ad-hoc snapshot plists that did not
    ;; have :undo-kind.
    (_
     (if (plist-member entry :undo-fn)
         (progn
           (funcall (plist-get entry :undo-fn) session)
           (scad-sketch-undo--restore-dirty session entry))
       (scad-sketch-undo--restore-snapshot session entry)))))

(defun scad-sketch-undo ()
  "Undo the most recent sketch edit."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (entry   (pop (scad-sketch-session-undo-stack session))))
    (unless entry
      (user-error "No sketch undo available"))
    (scad-sketch-undo--apply-entry session entry)
    (scad-sketch--normalize-attention session)
    (scad-sketch--render)))

(provide 'scad-sketch-editor--undo)
;;; scad-sketch-editor--undo.el ends here
