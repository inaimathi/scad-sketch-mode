;;; scad-sketch-editor--refs.el --- Selection ref data type -*- lexical-binding: t; -*-

;;; Commentary:

;; A "ref" is a plist identifying a selectable object in a session.
;; Kinds: 'shape (identifies a whole shape) or 'point (identifies one
;; vertex by shape-id + index).  This file owns construction, accessors,
;; and structural predicates; no session state is read or written here.

;;; Code:

(require 'scad-sketch-session)

;;; Constructors

(defun scad-sketch--shape-ref (&optional shape-id)
  "Return a shape selection ref for SHAPE-ID.
When SHAPE-ID is nil, the caller must supply it before use."
  (list :kind 'shape :shape-id shape-id))

(defun scad-sketch--point-ref (idx &optional shape-id)
  "Return a point selection ref for IDX in SHAPE-ID."
  (list :kind 'point :shape-id shape-id :index idx))

;;; Accessors

(defun scad-sketch--ref-kind (ref)
  "Return REF kind symbol ('shape or 'point)."
  (plist-get ref :kind))

(defun scad-sketch--ref-index (ref)
  "Return point index from REF, or nil for shape refs."
  (plist-get ref :index))

(defun scad-sketch--ref-shape-id (ref)
  "Return shape id from REF."
  (plist-get ref :shape-id))

;;; Predicates

(defun scad-sketch--same-ref-p (a b)
  "Return non-nil if selection refs A and B describe the same object."
  (and a b
       (eq  (scad-sketch--ref-kind     a) (scad-sketch--ref-kind     b))
       (eq  (scad-sketch--ref-shape-id a) (scad-sketch--ref-shape-id b))
       (equal (scad-sketch--ref-index  a) (scad-sketch--ref-index    b))))

;;; Summary

(defun scad-sketch--ref-summary (ref)
  "Return compact human-readable text for REF."
  (pcase (and ref (scad-sketch--ref-kind ref))
    ('shape (format "%s"    (scad-sketch--ref-shape-id ref)))
    ('point (format "%s[%s]"
                   (scad-sketch--ref-shape-id ref)
                   (scad-sketch--ref-index    ref)))
    (_      "none")))

(provide 'scad-sketch-editor--refs)
;;; scad-sketch-editor--refs.el ends here
