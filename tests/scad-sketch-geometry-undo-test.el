;;; scad-sketch-geometry-undo-test.el --- ERT tests for movement undo -*- lexical-binding: t; -*-

;;; Commentary:

;; Run all tests with:
;;
;;   bash unittest.sh

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar scad-sketch-geometry-undo-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

(defvar scad-sketch-geometry-undo-test--root
  (expand-file-name ".." scad-sketch-geometry-undo-test--dir)
  "Repository root directory.")

(add-to-list 'load-path scad-sketch-geometry-undo-test--root)

(defun sgeom-test--load (file feature)
  "Load FILE from the repository root unless FEATURE is already provided."
  (unless (featurep feature)
    (load-file (expand-file-name file scad-sketch-geometry-undo-test--root))))

(sgeom-test--load "scad-sketch-parse.el"              'scad-sketch-parse)
(sgeom-test--load "scad-sketch-geometry.el"           'scad-sketch-geometry)
(sgeom-test--load "scad-sketch-session.el"            'scad-sketch-session)
(sgeom-test--load "scad-sketch-editor--refs.el"       'scad-sketch-editor--refs)
(sgeom-test--load "scad-sketch-editor--selection.el"  'scad-sketch-editor--selection)
(sgeom-test--load "scad-sketch-editor-core.el"        'scad-sketch-editor-core)
(sgeom-test--load "scad-sketch-editor--undo.el"       'scad-sketch-editor--undo)
(sgeom-test--load "scad-sketch-editor--cursor.el"     'scad-sketch-editor--cursor)
(sgeom-test--load "scad-sketch-editor--editing.el"    'scad-sketch-editor--editing)

(unless (fboundp 'scad-sketch--render)
  (defun scad-sketch--render ()
    "No-op render stub for movement undo tests."
    nil))

(defun sgeom-test--goto-substring (needle &optional offset)
  "Move point to NEEDLE's beginning plus OFFSET."
  (goto-char (point-min))
  (unless (search-forward needle nil t)
    (error "Could not find test substring: %S" needle))
  (goto-char (+ (match-beginning 0) (or offset 0))))

(defmacro sgeom-test--with-editor-session (source needle &rest body)
  "Create a session from SOURCE at NEEDLE and run BODY in fake editor buffer."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,source)
     (sgeom-test--goto-substring ,needle)
     (let ((session (scad-sketch-session-at-point)))
       (with-temp-buffer
         (setq-local scad-sketch--session session)
         ,@body))))

(defun sgeom-test--active-shape (session)
  "Return SESSION's active shape."
  (or (scad-sketch-session-active-shape session)
      (car (scad-sketch-session-shapes session))))

(defun sgeom-test--active-points (session)
  "Return active polygon points from SESSION."
  (copy-tree (scad-sketch-shape-points
              (sgeom-test--active-shape session))))

(defun sgeom-test--circle-center (session)
  "Return active circle center from SESSION."
  (let ((md (scad-sketch-shape-metadata
             (sgeom-test--active-shape session))))
    (list (plist-get md :cx)
          (plist-get md :cy))))

(defun sgeom-test--circle-radius (session)
  "Return active circle radius from SESSION."
  (plist-get (scad-sketch-shape-metadata
              (sgeom-test--active-shape session))
             :r))

(defun sgeom-test--select-ref (session ref)
  "Replace SESSION selection with REF."
  (setf (scad-sketch-session-selection session) (list ref))
  session)

(defun sgeom-test--set-point (session point)
  "Set SESSION cursor point."
  (setf (scad-sketch-session-point session) (copy-sequence point))
  session)

(ert-deftest sgeom-moving-selected-polygon-point-is-undoable ()
  "Moving a selected polygon vertex restores that vertex on undo."
  (sgeom-test--with-editor-session
      "polygon([[0,0], [30,0], [15,26]]);\n"
      "polygon"
    (let* ((shape    (sgeom-test--active-shape session))
           (shape-id (scad-sketch-shape-id shape))
           (before   (sgeom-test--active-points session))
           (old-pt   (copy-tree (scad-sketch-session-point session))))
      (sgeom-test--select-ref session (scad-sketch--point-ref 1 shape-id))

      (scad-sketch-move-selected-right)
      (should-not (equal before (sgeom-test--active-points session)))
      (should (scad-sketch-session-dirty session))

      (scad-sketch-undo)
      (should (equal before (sgeom-test--active-points session)))
      (should (equal old-pt (scad-sketch-session-point session)))
      (should-not (scad-sketch-session-dirty session)))))

(ert-deftest sgeom-moving-selected-whole-polygon-is-undoable ()
  "Moving a selected polygon shape restores all points on undo."
  (sgeom-test--with-editor-session
      "polygon([[0,0], [30,0], [15,26]]);\n"
      "polygon"
    (let* ((shape    (sgeom-test--active-shape session))
           (shape-id (scad-sketch-shape-id shape))
           (before   (sgeom-test--active-points session)))
      (sgeom-test--select-ref session (scad-sketch--shape-ref shape-id))

      (scad-sketch-move-selected-right)
      (should-not (equal before (sgeom-test--active-points session)))

      (scad-sketch-undo)
      (should (equal before (sgeom-test--active-points session)))
      (should-not (scad-sketch-session-dirty session)))))

(ert-deftest sgeom-moving-selected-circle-shape-is-undoable ()
  "Moving a selected circle shape restores center metadata on undo."
  (sgeom-test--with-editor-session
      "circle(r=5);\n"
      "circle"
    (let* ((shape    (sgeom-test--active-shape session))
           (shape-id (scad-sketch-shape-id shape))
           (before   (sgeom-test--circle-center session)))
      (sgeom-test--select-ref session (scad-sketch--shape-ref shape-id))

      (scad-sketch-move-selected-right)
      (should-not (equal before (sgeom-test--circle-center session)))

      (scad-sketch-undo)
      (should (equal before (sgeom-test--circle-center session)))
      (should-not (scad-sketch-session-dirty session)))))

(ert-deftest sgeom-moving-selected-circle-radius-handle-is-undoable ()
  "Moving a selected circle radius handle restores radius metadata on undo."
  (sgeom-test--with-editor-session
      "circle(r=5);\n"
      "circle"
    (let* ((shape    (sgeom-test--active-shape session))
           (shape-id (scad-sketch-shape-id shape))
           (before   (sgeom-test--circle-radius session)))
      ;; Circle handle convention: 1 is east radius.
      (sgeom-test--select-ref session (scad-sketch--point-ref 1 shape-id))
      (sgeom-test--set-point session '(5.0 0.0))

      (scad-sketch-move-selected-right)
      (should-not (= before (sgeom-test--circle-radius session)))

      (scad-sketch-undo)
      (should (= before (sgeom-test--circle-radius session)))
      (should-not (scad-sketch-session-dirty session)))))

(ert-deftest sgeom-move-undo-entry-is-operation-entry ()
  "Movement should push an operation undo entry, not a full snapshot entry."
  (sgeom-test--with-editor-session
      "polygon([[0,0], [30,0], [15,26]]);\n"
      "polygon"
    (let* ((shape    (sgeom-test--active-shape session))
           (shape-id (scad-sketch-shape-id shape)))
      (sgeom-test--select-ref session (scad-sketch--shape-ref shape-id))
      (scad-sketch-move-selected-right)
      (let ((entry (car (scad-sketch-session-undo-stack session))))
        (should (eq (plist-get entry :undo-kind) 'action))
        (should (plist-member entry :undo-fn))
        (should-not (plist-member entry :shapes))))))

(provide 'scad-sketch-geometry-undo-test)
;;; scad-sketch-geometry-undo-test.el ends here
