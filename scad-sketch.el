;;; scad-sketch.el --- Keyboard sketch editor for OpenSCAD arrays -*- lexical-binding: t; -*-

;; Author: Leo Zovic + ChatGPT
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: cad, openscad, svg, tools

;;; Commentary:

;; First-cut keyboard/SVG sketch editor for OpenSCAD array literals.
;;
;; Usage in a .scad buffer:
;;
;;   // scad-sketch: name=profile kind=2d closed=true grid=1 units=mm
;;   profile = [
;;     [0, 0],
;;     [40, 0],
;;     [40, 12],
;;     [0, 12]
;;   ];
;;   // end-scad-sketch
;;
;; Put point inside the block and run:
;;
;;   M-x scad-sketch-at-point
;;
;; To wrap an existing literal array assignment in scad-sketch comments,
;; put point inside the assignment and run:
;;
;;   M-x scad-sketch-adopt-array-at-point
;;
;; Supported `kind' values:
;;   kind=2d              -> emits [[x, y], ...]
;;   kind=3d              -> emits [[x, y, z], ...], edited through a 2D plane
;;   kind=2d-with-curves  -> emits [[x, y, r], ...], polyRound-style radii
;;
;; For 3D sketches, use metadata such as:
;;   plane=xy fixed-z=0
;;   plane=xz fixed-y=0
;;   plane=yz fixed-x=0
;;
;; This is intentionally not a general SVG editor. SVG is just the view;
;; OpenSCAD-ish points are the model and output.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'svg)

(defgroup scad-sketch nil
  "Keyboard sketch editor for OpenSCAD array literals."
  :group 'tools
  :prefix "scad-sketch-")

(defcustom scad-sketch-default-grid 1.0
  "Default grid step in sketch units."
  :type 'number
  :group 'scad-sketch)

(defcustom scad-sketch-default-fine-step 0.1
  "Default fine movement step in sketch units."
  :type 'number
  :group 'scad-sketch)

(defcustom scad-sketch-default-coarse-step 5.0
  "Default coarse movement step in sketch units."
  :type 'number
  :group 'scad-sketch)

(defcustom scad-sketch-canvas-width 900
  "Sketch editor canvas width in pixels."
  :type 'integer
  :group 'scad-sketch)

(defcustom scad-sketch-canvas-height 650
  "Sketch editor canvas height in pixels."
  :type 'integer
  :group 'scad-sketch)

(defcustom scad-sketch-margin 48
  "Canvas margin in pixels."
  :type 'integer
  :group 'scad-sketch)

(defcustom scad-sketch-metadata-regexp
  "^[[:space:]]*//[[:space:]]*scad-sketch:[[:space:]]*\\(.*\\)$"
  "Regexp matching a scad-sketch metadata line."
  :type 'regexp
  :group 'scad-sketch)

(defcustom scad-sketch-end-regexp
  "^[[:space:]]*//[[:space:]]*end-scad-sketch[[:space:]]*$"
  "Regexp matching a scad-sketch end line."
  :type 'regexp
  :group 'scad-sketch)

(cl-defstruct scad-sketch-session
  name
  kind
  units
  grid
  fine-step
  coarse-step
  closed
  plane
  fixed-x
  fixed-y
  fixed-z
  points
  point
  mark
  mark-ring
  named-marks
  selected-index
  source-buffer
  content-beg
  content-end
  metadata
  dirty
  undo-stack)

(defvar-local scad-sketch--session nil)
(defvar scad-sketch--editor-buffer-prefix "*scad-sketch: ")

(defvar scad-sketch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") #'scad-sketch-at-point)
    (define-key map (kbd "C-c C-a") #'scad-sketch-adopt-array-at-point)
    map)
  "Keymap for `scad-sketch-mode'.")

;;;###autoload
(define-minor-mode scad-sketch-mode
  "Minor mode for opening scad-sketch blocks from OpenSCAD buffers."
  :lighter " Sketch"
  :keymap scad-sketch-mode-map)

(defvar scad-sketch-editor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Point movement.
    (define-key map (kbd "<left>")  #'scad-sketch-move-point-left)
    (define-key map (kbd "<right>") #'scad-sketch-move-point-right)
    (define-key map (kbd "<up>")    #'scad-sketch-move-point-up)
    (define-key map (kbd "<down>")  #'scad-sketch-move-point-down)
    (define-key map (kbd "M-<left>")  #'scad-sketch-move-point-fine-left)
    (define-key map (kbd "M-<right>") #'scad-sketch-move-point-fine-right)
    (define-key map (kbd "M-<up>")    #'scad-sketch-move-point-fine-up)
    (define-key map (kbd "M-<down>")  #'scad-sketch-move-point-fine-down)
    (define-key map (kbd "C-<left>")  #'scad-sketch-move-point-coarse-left)
    (define-key map (kbd "C-<right>") #'scad-sketch-move-point-coarse-right)
    (define-key map (kbd "C-<up>")    #'scad-sketch-move-point-coarse-up)
    (define-key map (kbd "C-<down>")  #'scad-sketch-move-point-coarse-down)
    ;; Selected vertex movement.
    (define-key map (kbd "S-<left>")  #'scad-sketch-move-selected-left)
    (define-key map (kbd "S-<right>") #'scad-sketch-move-selected-right)
    (define-key map (kbd "S-<up>")    #'scad-sketch-move-selected-up)
    (define-key map (kbd "S-<down>")  #'scad-sketch-move-selected-down)
    ;; Editing.
    (define-key map (kbd "m") #'scad-sketch-set-mark)
    (define-key map (kbd "M") #'scad-sketch-push-mark)
    (define-key map (kbd "'") #'scad-sketch-jump-to-mark)
    (define-key map (kbd "p") #'scad-sketch-append-point)
    (define-key map (kbd "i") #'scad-sketch-insert-point-after-selected)
    (define-key map (kbd "k") #'scad-sketch-delete-selected)
    (define-key map (kbd "l") #'scad-sketch-line-from-mark)
    (define-key map (kbd "r") #'scad-sketch-rectangle-from-mark)
    (define-key map (kbd "c") #'scad-sketch-toggle-closed)
    (define-key map (kbd "R") #'scad-sketch-set-radius)
    (define-key map (kbd "TAB") #'scad-sketch-next-point)
    (define-key map (kbd "<backtab>") #'scad-sketch-previous-point)
    (define-key map (kbd "x") #'scad-sketch-set-x)
    (define-key map (kbd "y") #'scad-sketch-set-y)
    (define-key map (kbd "z") #'scad-sketch-set-z)
    (define-key map (kbd "X") #'scad-sketch-set-delta-x)
    (define-key map (kbd "Y") #'scad-sketch-set-delta-y)
    (define-key map (kbd "Z") #'scad-sketch-set-delta-z)
    (define-key map (kbd "d") #'scad-sketch-set-distance-from-mark)
    (define-key map (kbd "a") #'scad-sketch-set-angle-from-mark)
    (define-key map (kbd "g") #'scad-sketch-set-grid)
    (define-key map (kbd "u") #'scad-sketch-undo)
    (define-key map (kbd "w") #'scad-sketch-write-back)
    (define-key map (kbd "q") #'scad-sketch-quit)
    (define-key map (kbd "?") #'scad-sketch-help)
    (define-key map (kbd "C") #'scad-sketch-clear-mark)
    (define-key map (kbd ".") #'scad-sketch-move-selected-to-point)
    (define-key map (kbd "RET") #'scad-sketch-move-selected-to-point)
    map)
  "Keymap for `scad-sketch-editor-mode'.")

(define-derived-mode scad-sketch-editor-mode special-mode "SCAD-Sketch"
  "Major mode for editing one OpenSCAD sketch block."
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defun scad-sketch--metadata-value (value)
  "Parse metadata VALUE into a Lisp value."
  (let ((s (string-trim value)))
    (cond
     ((member s '("true" "t" "yes" "on")) t)
     ((member s '("false" "nil" "no" "off")) nil)
     ((string-match-p "\\`[-+]?[0-9]*\\.?[0-9]+\\(?:[eE][-+]?[0-9]+\\)?\\'" s)
      (string-to-number s))
     (t s))))

(defun scad-sketch--parse-metadata (line)
  "Parse a scad-sketch metadata LINE into an alist."
  (unless (string-match scad-sketch-metadata-regexp line)
    (user-error "Not a scad-sketch metadata line"))
  (let ((body (match-string 1 line))
        result)
    (dolist (tok (split-string body "[[:space:]]+" t))
      (when (string-match "\\`\\([^=[:space:]]+\\)=\\(.+\\)\\'" tok)
        (push (cons (intern (match-string 1 tok))
                    (scad-sketch--metadata-value (match-string 2 tok)))
              result)))
    (nreverse result)))

(defun scad-sketch--meta (metadata key &optional default)
  "Return KEY from METADATA or DEFAULT."
  (let ((cell (assq key metadata)))
    (if cell (cdr cell) default)))

(defun scad-sketch--find-block ()
  "Find the scad-sketch block around point.
Return plist with :metadata-line, :metadata, :content-beg, and :content-end."
  (save-excursion
    (let ((origin (point))
          meta-beg meta-end meta-line metadata content-beg content-end)
      (unless (re-search-backward scad-sketch-metadata-regexp nil t)
        (user-error "No scad-sketch metadata line before point"))
      (setq meta-beg (line-beginning-position))
      (setq meta-end (line-end-position))
      (setq meta-line (buffer-substring-no-properties meta-beg meta-end))
      (setq metadata (scad-sketch--parse-metadata meta-line))
      (setq content-beg (min (point-max) (1+ meta-end)))
      (goto-char content-beg)
      (unless (re-search-forward scad-sketch-end-regexp nil t)
        (user-error "No // end-scad-sketch after metadata line"))
      (setq content-end (line-beginning-position))
      (unless (and (<= meta-beg origin) (<= origin (match-end 0)))
        (user-error "Point is not inside the nearest scad-sketch block"))
      (list :metadata-line meta-line
            :metadata metadata
            :content-beg content-beg
            :content-end content-end))))

(defconst scad-sketch--number-re
  "[-+]?[0-9]*\\.?[0-9]+\\(?:[eE][-+]?[0-9]+\\)?")

(defun scad-sketch--strip-line-comments (s)
  "Return S with // comments removed line-by-line."
  (mapconcat
   (lambda (line)
     (if (string-match "//" line)
         (substring line 0 (match-beginning 0))
       line))
   (split-string s "\n")
   "\n"))

(defun scad-sketch--parse-points (text kind)
  "Parse literal point arrays from TEXT according to KIND."
  (let* ((clean (scad-sketch--strip-line-comments text))
         (n scad-sketch--number-re)
         (pair-re (concat "\\[\\s-*\\(" n "\\)\\s-*,\\s-*\\(" n "\\)"
                          "\\(?:\\s-*,\\s-*\\(" n "\\)\\)?"
                          "\\s-*\\]"))
         points)
    (with-temp-buffer
      (insert clean)
      (goto-char (point-min))
      (while (re-search-forward pair-re nil t)
        (let ((x (string-to-number (match-string 1)))
              (y (string-to-number (match-string 2)))
              (z (when (match-string 3)
                   (string-to-number (match-string 3)))))
          (push (cond
                 ((string= kind "2d") (list x y))
                 ((or (string= kind "2d-with-curves")
                      (string= kind "2d-curves")
                      (string= kind "2d-polyround"))
                  (list x y (or z 0)))
                 ((string= kind "3d") (list x y (or z 0)))
                 (t (list x y)))
                points))))
    (nreverse points)))

(defun scad-sketch--initial-point (points session)
  "Return an initial cursor point for POINTS and SESSION."
  (if points
      (scad-sketch--point-xy (car points) session)
    (list 0.0 0.0)))

(defun scad-sketch--make-session (block)
  "Create a `scad-sketch-session' from BLOCK."
  (let* ((metadata (plist-get block :metadata))
         (name (format "%s" (scad-sketch--meta metadata 'name "sketch")))
         (kind (format "%s" (scad-sketch--meta metadata 'kind "2d")))
         (grid (float (scad-sketch--meta metadata 'grid scad-sketch-default-grid)))
         (fine (float (scad-sketch--meta metadata 'fine scad-sketch-default-fine-step)))
         (coarse (float (scad-sketch--meta metadata 'coarse scad-sketch-default-coarse-step)))
         (closed (if (assq 'closed metadata)
                     (scad-sketch--meta metadata 'closed t)
		   t))
         (units (format "%s" (scad-sketch--meta metadata 'units "mm")))
         (plane (format "%s" (scad-sketch--meta metadata 'plane "xy")))
         (fixed-x (float (scad-sketch--meta metadata 'fixed-x 0)))
         (fixed-y (float (scad-sketch--meta metadata 'fixed-y 0)))
         (fixed-z (float (scad-sketch--meta metadata 'fixed-z 0)))
         (content (buffer-substring-no-properties
                   (plist-get block :content-beg)
                   (plist-get block :content-end)))
         (points (scad-sketch--parse-points content kind))
         (beg-marker (copy-marker (plist-get block :content-beg)))
         (end-marker (copy-marker (plist-get block :content-end) t)))
    (let ((session (make-scad-sketch-session
                    :name name
                    :kind kind
                    :units units
                    :grid grid
                    :fine-step fine
                    :coarse-step coarse
                    :closed closed
                    :plane plane
                    :fixed-x fixed-x
                    :fixed-y fixed-y
                    :fixed-z fixed-z
                    :points points
                    :mark nil
                    :mark-ring nil
                    :named-marks nil
                    :selected-index (if points 0 nil)
                    :source-buffer (current-buffer)
                    :content-beg beg-marker
                    :content-end end-marker
                    :metadata metadata
                    :dirty nil
                    :undo-stack nil)))
      (setf (scad-sketch-session-point session)
            (scad-sketch--initial-point points session))
      session)))

(defun scad-sketch--find-target-at-point ()
  "Find either an explicit scad-sketch block or a raw array assignment at point."
  (condition-case nil
      (scad-sketch--find-block)
    (user-error
     (scad-sketch--block-from-array-assignment
      (scad-sketch--find-array-assignment-at-point)))))

(defun scad-sketch--forward-balanced-bracket (pos)
  "Return position just after the bracketed form starting at POS."
  (save-excursion
    (goto-char pos)
    (unless (eq (char-after) ?[)
      (user-error "Expected `[' at array start"))
    (let ((depth 0)
          (done nil)
          (in-string nil)
          (escape nil)
          (line-comment nil)
          (block-comment nil))
      (while (and (not done) (< (point) (point-max)))
        (let ((ch (char-after))
              (next (char-after (1+ (point)))))
          (cond
           (line-comment
            (when (eq ch ?\n)
              (setq line-comment nil))
            (forward-char 1))
           (block-comment
            (if (and (eq ch ?*) (eq next ?/))
                (progn
                  (setq block-comment nil)
                  (forward-char 2))
              (forward-char 1)))
           (in-string
            (cond
             (escape
              (setq escape nil)
              (forward-char 1))
             ((eq ch ?\\)
              (setq escape t)
              (forward-char 1))
             ((eq ch ?\")
              (setq in-string nil)
              (forward-char 1))
             (t
              (forward-char 1))))
           ((and (eq ch ?/) (eq next ?/))
            (setq line-comment t)
            (forward-char 2))
           ((and (eq ch ?/) (eq next ?*))
            (setq block-comment t)
            (forward-char 2))
           ((eq ch ?\")
            (setq in-string t)
            (forward-char 1))
           ((eq ch ?[)
            (setq depth (1+ depth))
            (forward-char 1))
           ((eq ch ?])
            (setq depth (1- depth))
            (forward-char 1)
            (when (= depth 0)
              (setq done t)))
           (t
            (forward-char 1)))))
      (unless done
        (user-error "Malformed array: missing closing `]'"))
      (point))))

(defun scad-sketch--find-array-assignment-at-point ()
  "Find a literal SCAD array assignment surrounding point.

Accepts forms like:

  pts = [[1, 2], [3, 4]];

Return plist with :name, :beg, :end, and :text."
  (let ((origin (point))
        name beg open end)
    (save-excursion
      (unless (re-search-backward
               "\\_<\\([[:alpha:]_$][[:alnum:]_$]*\\)\\_>[[:space:]\n\r]*=[[:space:]\n\r]*\\["
               nil t)
        (user-error "No literal array assignment before point"))
      (setq name (match-string-no-properties 1))
      (setq beg (match-beginning 0))
      (setq open (1- (match-end 0)))
      (setq end (scad-sketch--forward-balanced-bracket open))
      (goto-char end)
      (skip-chars-forward " \t\r\n")
      (unless (eq (char-after) ?\;)
        (user-error "Malformed array assignment: expected semicolon after array"))
      (forward-char 1)
      (setq end (point))
      (unless (and (<= beg origin) (<= origin end))
        (user-error "Point is not inside the nearest literal array assignment"))
      (list :name name
            :beg beg
            :end end
            :text (buffer-substring-no-properties beg end)))))

(defun scad-sketch--literal-point-dimensions (text)
  "Return point dimensions found in literal point arrays in TEXT."
  (let* ((clean (scad-sketch--strip-line-comments text))
         (n scad-sketch--number-re)
         (point-re
          (concat "\\[\\s-*\\(" n "\\)\\s-*,\\s-*\\(" n "\\)"
                  "\\(?:\\s-*,\\s-*\\(" n "\\)\\)?"
                  "\\s-*\\]"))
         dims)
    (with-temp-buffer
      (insert clean)
      (goto-char (point-min))
      (while (re-search-forward point-re nil t)
        (push (if (match-string 3) 3 2) dims)))
    (nreverse dims)))

(defun scad-sketch--infer-kind-for-existing-array (text)
  "Infer sketch kind from raw array assignment TEXT."
  (let* ((dims (scad-sketch--literal-point-dimensions text))
         (uniq (delete-dups (copy-sequence dims))))
    (unless dims
      (user-error "No literal [x, y] or [x, y, z/r] points found"))
    (cond
     ((equal uniq '(2))
      "2d")
     ((equal uniq '(3))
      (completing-read
       "This is a 3-column array. Treat as: "
       '("3d" "2d-with-curves")
       nil t nil nil "3d"))
     (t
      (completing-read
       "Mixed 2/3-column points. Treat missing third values as zero for: "
       '("2d-with-curves" "3d")
       nil t nil nil "2d-with-curves")))))

(defun scad-sketch--block-from-array-assignment (assignment)
  "Convert raw ASSIGNMENT plist into a sketch block plist."
  (let* ((name (plist-get assignment :name))
         (text (plist-get assignment :text))
         (kind (scad-sketch--infer-kind-for-existing-array text))
         (plane (if (string= kind "3d")
                    (completing-read
                     "3D edit plane: "
                     '("xy" "xz" "yz")
                     nil t nil nil "xy")
                  "xy"))
         (metadata
          `((name . ,name)
            (kind . ,kind)
            (closed . t)
            (grid . 1)
            (units . "mm")
            (plane . ,plane)
            (fixed-x . 0)
            (fixed-y . 0)
            (fixed-z . 0))))
    (list :metadata metadata
          :content-beg (plist-get assignment :beg)
          :content-end (plist-get assignment :end))))

;;;###autoload
(defun scad-sketch-at-point ()
  "Open a visual keyboard editor for a SCAD point array at point.

If point is inside an explicit scad-sketch comment block, use its metadata.
Otherwise, open the surrounding literal array assignment directly."
  (interactive)
  (unless (image-type-available-p 'svg)
    (user-error "This Emacs was not built with SVG image support"))
  (let* ((block (scad-sketch--find-target-at-point))
         (session (scad-sketch--make-session block))
         (buf (get-buffer-create
               (format "%s%s*" scad-sketch--editor-buffer-prefix
                       (scad-sketch-session-name session)))))
    (with-current-buffer buf
      (scad-sketch-editor-mode)
      (setq-local scad-sketch--session session)
      (scad-sketch--render))
    (pop-to-buffer buf)))

(defun scad-sketch--assert-session ()
  "Return the current sketch session or signal an error."
  (unless (and (boundp 'scad-sketch--session) scad-sketch--session)
    (user-error "No active scad-sketch session"))
  scad-sketch--session)

(defun scad-sketch--2d-kind-p (session)
  "Return non-nil if SESSION uses 2D array points."
  (string= (scad-sketch-session-kind session) "2d"))

(defun scad-sketch--curve-kind-p (session)
  "Return non-nil if SESSION uses polyRound-style points."
  (member (scad-sketch-session-kind session)
          '("2d-with-curves" "2d-curves" "2d-polyround")))

(defun scad-sketch--3d-kind-p (session)
  "Return non-nil if SESSION uses 3D points."
  (string= (scad-sketch-session-kind session) "3d"))

(defun scad-sketch--point-xy (point session)
  "Project model POINT to visible [x y] coordinates for SESSION."
  (cond
   ((or (scad-sketch--2d-kind-p session) (scad-sketch--curve-kind-p session))
    (list (float (or (nth 0 point) 0))
          (float (or (nth 1 point) 0))))
   ((scad-sketch--3d-kind-p session)
    (pcase (scad-sketch-session-plane session)
      ("xy" (list (float (or (nth 0 point) 0)) (float (or (nth 1 point) 0))))
      ("xz" (list (float (or (nth 0 point) 0)) (float (or (nth 2 point) 0))))
      ("yz" (list (float (or (nth 1 point) 0)) (float (or (nth 2 point) 0))))
      (_    (list (float (or (nth 0 point) 0)) (float (or (nth 1 point) 0))))))
   (t (list 0.0 0.0))))

(defun scad-sketch--point-radius (point session)
  "Return polyRound radius for POINT in SESSION, or nil."
  (when (scad-sketch--curve-kind-p session)
    (or (nth 2 point) 0)))

(defun scad-sketch--make-model-point (xy session &optional old-point)
  "Create a model point from visible XY in SESSION.
When OLD-POINT is non-nil, preserve hidden coordinates/radii where possible."
  (let ((x (float (nth 0 xy)))
        (y (float (nth 1 xy))))
    (cond
     ((scad-sketch--2d-kind-p session)
      (list x y))
     ((scad-sketch--curve-kind-p session)
      (list x y (float (or (nth 2 old-point) 0))))
     ((scad-sketch--3d-kind-p session)
      (pcase (scad-sketch-session-plane session)
        ("xy" (list x y (float (or (nth 2 old-point)
                                    (scad-sketch-session-fixed-z session)))))
        ("xz" (list x (float (or (nth 1 old-point)
                                  (scad-sketch-session-fixed-y session))) y))
        ("yz" (list (float (or (nth 0 old-point)
                                (scad-sketch-session-fixed-x session))) x y))
        (_ (list x y (float (or (nth 2 old-point)
                                (scad-sketch-session-fixed-z session)))))))
     (t (list x y)))))

(defun scad-sketch--replace-nth (n value list)
  "Return LIST with element N replaced by VALUE."
  (let ((copy (copy-sequence list)))
    (setf (nth n copy) value)
    copy))

(defun scad-sketch--push-undo (session)
  "Save SESSION state for undo."
  (push (list :points (copy-tree (scad-sketch-session-points session))
              :point (copy-tree (scad-sketch-session-point session))
              :mark (copy-tree (scad-sketch-session-mark session))
              :mark-ring (copy-tree (scad-sketch-session-mark-ring session))
              :named-marks (copy-tree (scad-sketch-session-named-marks session))
              :selected-index (scad-sketch-session-selected-index session)
              :closed (scad-sketch-session-closed session))
        (scad-sketch-session-undo-stack session)))

(defun scad-sketch--mark-dirty (session)
  "Mark SESSION as dirty."
  (setf (scad-sketch-session-dirty session) t))

(defun scad-sketch--mutate (fn)
  "Push undo state, call FN with the current session, and render."
  (let ((session (scad-sketch--assert-session)))
    (scad-sketch--push-undo session)
    (funcall fn session)
    (scad-sketch--mark-dirty session)
    (scad-sketch--render)))

(defun scad-sketch--selected-point (session)
  "Return selected model point for SESSION, or nil."
  (let ((idx (scad-sketch-session-selected-index session)))
    (when (and idx (>= idx 0) (< idx (length (scad-sketch-session-points session))))
      (nth idx (scad-sketch-session-points session)))))

(defun scad-sketch--set-selected-point (session point)
  "Replace selected model point in SESSION with POINT."
  (let ((idx (scad-sketch-session-selected-index session)))
    (unless (and idx (>= idx 0) (< idx (length (scad-sketch-session-points session))))
      (user-error "No selected point"))
    (setf (scad-sketch-session-points session)
          (scad-sketch--replace-nth idx point
                                    (scad-sketch-session-points session)))))

(defun scad-sketch--move-xy (xy dx dy)
  "Return XY moved by DX and DY."
  (list (+ (float (nth 0 xy)) dx)
        (+ (float (nth 1 xy)) dy)))

(defun scad-sketch--move-point (dx dy)
  "Move the sketch cursor point by DX and DY."
  (scad-sketch--mutate
   (lambda (session)
     (setf (scad-sketch-session-point session)
           (scad-sketch--move-xy (scad-sketch-session-point session) dx dy)))))

(defun scad-sketch--move-selected (dx dy)
  "Move the selected model point by DX and DY in visible coordinates."
  (scad-sketch--mutate
   (lambda (session)
     (let* ((old (or (scad-sketch--selected-point session)
                     (user-error "No selected point")))
            (xy (scad-sketch--point-xy old session))
            (new-xy (scad-sketch--move-xy xy dx dy))
            (new (scad-sketch--make-model-point new-xy session old)))
       (scad-sketch--set-selected-point session new)
       (setf (scad-sketch-session-point session) new-xy)))))

(defun scad-sketch-clear-mark ()
  "Clear the primary mark and mark ring."
  (interactive)
  (scad-sketch--mutate
   (lambda (session)
     (setf (scad-sketch-session-mark session) nil)
     (setf (scad-sketch-session-mark-ring session) nil))))

(defun scad-sketch-move-selected-to-point ()
  "Move the selected vertex to the current cursor point."
  (interactive)
  (scad-sketch--mutate
   (lambda (session)
     (let* ((old (or (scad-sketch--selected-point session)
                     (user-error "No selected point")))
            (new (scad-sketch--make-model-point
                  (scad-sketch-session-point session)
                  session
                  old)))
       (scad-sketch--set-selected-point session new)))))

(defun scad-sketch--grid (session) (float (scad-sketch-session-grid session)))
(defun scad-sketch--fine (session) (float (scad-sketch-session-fine-step session)))
(defun scad-sketch--coarse (session) (float (scad-sketch-session-coarse-step session)))

(defun scad-sketch-move-point-left ()  (interactive) (scad-sketch--move-point (- (scad-sketch--grid (scad-sketch--assert-session))) 0))
(defun scad-sketch-move-point-right () (interactive) (scad-sketch--move-point (scad-sketch--grid (scad-sketch--assert-session)) 0))
(defun scad-sketch-move-point-up ()    (interactive) (scad-sketch--move-point 0 (scad-sketch--grid (scad-sketch--assert-session))))
(defun scad-sketch-move-point-down ()  (interactive) (scad-sketch--move-point 0 (- (scad-sketch--grid (scad-sketch--assert-session)))))

(defun scad-sketch-move-point-fine-left ()  (interactive) (scad-sketch--move-point (- (scad-sketch--fine (scad-sketch--assert-session))) 0))
(defun scad-sketch-move-point-fine-right () (interactive) (scad-sketch--move-point (scad-sketch--fine (scad-sketch--assert-session)) 0))
(defun scad-sketch-move-point-fine-up ()    (interactive) (scad-sketch--move-point 0 (scad-sketch--fine (scad-sketch--assert-session))))
(defun scad-sketch-move-point-fine-down ()  (interactive) (scad-sketch--move-point 0 (- (scad-sketch--fine (scad-sketch--assert-session)))))

(defun scad-sketch-move-point-coarse-left ()  (interactive) (scad-sketch--move-point (- (scad-sketch--coarse (scad-sketch--assert-session))) 0))
(defun scad-sketch-move-point-coarse-right () (interactive) (scad-sketch--move-point (scad-sketch--coarse (scad-sketch--assert-session)) 0))
(defun scad-sketch-move-point-coarse-up ()    (interactive) (scad-sketch--move-point 0 (scad-sketch--coarse (scad-sketch--assert-session))))
(defun scad-sketch-move-point-coarse-down ()  (interactive) (scad-sketch--move-point 0 (- (scad-sketch--coarse (scad-sketch--assert-session)))))

(defun scad-sketch-move-selected-left ()  (interactive) (scad-sketch--move-selected (- (scad-sketch--grid (scad-sketch--assert-session))) 0))
(defun scad-sketch-move-selected-right () (interactive) (scad-sketch--move-selected (scad-sketch--grid (scad-sketch--assert-session)) 0))
(defun scad-sketch-move-selected-up ()    (interactive) (scad-sketch--move-selected 0 (scad-sketch--grid (scad-sketch--assert-session))))
(defun scad-sketch-move-selected-down ()  (interactive) (scad-sketch--move-selected 0 (- (scad-sketch--grid (scad-sketch--assert-session)))))

(defun scad-sketch-set-mark ()
  "Set the primary mark to the current point."
  (interactive)
  (scad-sketch--mutate
   (lambda (session)
     (setf (scad-sketch-session-mark session)
           (copy-sequence (scad-sketch-session-point session))))))

(defun scad-sketch-push-mark ()
  "Push the current point onto the mark ring and make it the primary mark."
  (interactive)
  (scad-sketch--mutate
   (lambda (session)
     (let ((pt (copy-sequence (scad-sketch-session-point session))))
       (when (scad-sketch-session-mark session)
         (push (copy-sequence (scad-sketch-session-mark session))
               (scad-sketch-session-mark-ring session)))
       (setf (scad-sketch-session-mark session) pt)))))

(defun scad-sketch-jump-to-mark ()
  "Move point to the primary mark."
  (interactive)
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-mark session)
      (user-error "No mark set"))
    (scad-sketch--mutate
     (lambda (s)
       (setf (scad-sketch-session-point s)
             (copy-sequence (scad-sketch-session-mark s)))))))

(defun scad-sketch--append-model-point (session point)
  "Append POINT to SESSION and select it."
  (setf (scad-sketch-session-points session)
        (append (scad-sketch-session-points session) (list point)))
  (setf (scad-sketch-session-selected-index session)
        (1- (length (scad-sketch-session-points session)))))

(defun scad-sketch-append-point ()
  "Append the current cursor point to the active point array."
  (interactive)
  (scad-sketch--mutate
   (lambda (session)
     (scad-sketch--append-model-point
      session
      (scad-sketch--make-model-point (scad-sketch-session-point session) session)))))

(defun scad-sketch-insert-point-after-selected ()
  "Insert the current point after the selected point."
  (interactive)
  (scad-sketch--mutate
   (lambda (session)
     (let* ((idx (or (scad-sketch-session-selected-index session) -1))
            (pt (scad-sketch--make-model-point (scad-sketch-session-point session) session))
            (points (scad-sketch-session-points session))
            (head (cl-subseq points 0 (min (1+ idx) (length points))))
            (tail (nthcdr (min (1+ idx) (length points)) points)))
       (setf (scad-sketch-session-points session)
             (append head (list pt) tail))
       (setf (scad-sketch-session-selected-index session)
             (max 0 (1+ idx)))))))

(defun scad-sketch-delete-selected ()
  "Delete the selected point."
  (interactive)
  (scad-sketch--mutate
   (lambda (session)
     (let ((idx (or (scad-sketch-session-selected-index session)
                    (user-error "No selected point")))
           (points (scad-sketch-session-points session)))
       (unless (< idx (length points))
         (user-error "Selected point out of range"))
       (setf (scad-sketch-session-points session)
             (append (cl-subseq points 0 idx) (nthcdr (1+ idx) points)))
       (setf (scad-sketch-session-selected-index session)
             (cond
              ((null (scad-sketch-session-points session)) nil)
              ((>= idx (length (scad-sketch-session-points session)))
               (1- (length (scad-sketch-session-points session))))
              (t idx)))))))

(defun scad-sketch-line-from-mark ()
  "Append a line represented by mark and point.
For plain polygon arrays this appends the mark and current point as vertices."
  (interactive)
  (scad-sketch--mutate
   (lambda (session)
     (unless (scad-sketch-session-mark session)
       (user-error "No mark set"))
     (scad-sketch--append-model-point
      session
      (scad-sketch--make-model-point (scad-sketch-session-mark session) session))
     (scad-sketch--append-model-point
      session
      (scad-sketch--make-model-point (scad-sketch-session-point session) session)))))

(defun scad-sketch-rectangle-from-mark ()
  "Append a rectangle using the mark and point as opposite corners."
  (interactive)
  (scad-sketch--mutate
   (lambda (session)
     (let ((mark (or (scad-sketch-session-mark session)
                     (user-error "No mark set")))
           (point (scad-sketch-session-point session)))
       (let* ((x1 (nth 0 mark))
              (y1 (nth 1 mark))
              (x2 (nth 0 point))
              (y2 (nth 1 point))
              (corners (list (list x1 y1)
                             (list x2 y1)
                             (list x2 y2)
                             (list x1 y2))))
         (dolist (xy corners)
           (scad-sketch--append-model-point
            session (scad-sketch--make-model-point xy session))))))))

(defun scad-sketch-toggle-closed ()
  "Toggle whether the editor renders the point array as closed."
  (interactive)
  (scad-sketch--mutate
   (lambda (session)
     (setf (scad-sketch-session-closed session)
           (not (scad-sketch-session-closed session))))))

(defun scad-sketch-set-radius (radius)
  "Set selected point RADIUS for kind=2d-with-curves."
  (interactive
   (list (read-number "Radius: " 0)))
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch--curve-kind-p session)
      (user-error "Radius is only meaningful for kind=2d-with-curves")))
  (scad-sketch--mutate
   (lambda (session)
     (let ((pt (or (scad-sketch--selected-point session)
                   (user-error "No selected point"))))
       (scad-sketch--set-selected-point
        session
        (list (nth 0 pt) (nth 1 pt) (float radius)))))))

(defun scad-sketch-next-point ()
  "Select the next point and move cursor point to it."
  (interactive)
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-points session)
      (user-error "No points"))
    (scad-sketch--mutate
     (lambda (s)
       (let* ((n (length (scad-sketch-session-points s)))
              (idx (mod (1+ (or (scad-sketch-session-selected-index s) -1)) n))
              (pt (nth idx (scad-sketch-session-points s))))
         (setf (scad-sketch-session-selected-index s) idx)
         (setf (scad-sketch-session-point s)
               (scad-sketch--point-xy pt s)))))))

(defun scad-sketch-previous-point ()
  "Select the previous point and move cursor point to it."
  (interactive)
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-points session)
      (user-error "No points"))
    (scad-sketch--mutate
     (lambda (s)
       (let* ((n (length (scad-sketch-session-points s)))
              (idx (mod (1- (or (scad-sketch-session-selected-index s) 0)) n))
              (pt (nth idx (scad-sketch-session-points s))))
         (setf (scad-sketch-session-selected-index s) idx)
         (setf (scad-sketch-session-point s)
               (scad-sketch--point-xy pt s)))))))

(defun scad-sketch--set-point-axis (axis value)
  "Set visible cursor AXIS to VALUE. AXIS is 0 for x or 1 for y."
  (scad-sketch--mutate
   (lambda (session)
     (let ((pt (copy-sequence (scad-sketch-session-point session))))
       (setf (nth axis pt) (float value))
       (setf (scad-sketch-session-point session) pt)))))

(defun scad-sketch-set-x (x)
  "Set cursor X coordinate."
  (interactive (list (read-number "X: " (nth 0 (scad-sketch-session-point (scad-sketch--assert-session))))))
  (scad-sketch--set-point-axis 0 x))

(defun scad-sketch-set-y (y)
  "Set cursor Y coordinate."
  (interactive (list (read-number "Y: " (nth 1 (scad-sketch-session-point (scad-sketch--assert-session))))))
  (scad-sketch--set-point-axis 1 y))

(defun scad-sketch-set-z (z)
  "Set fixed/hidden Z coordinate for 3D xy-plane sketches, or selected Z."
  (interactive (list (read-number "Z/fixed-Z: " 0)))
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch--3d-kind-p session)
      (user-error "Z is only meaningful for kind=3d")))
  (scad-sketch--mutate
   (lambda (session)
     (if (scad-sketch--selected-point session)
         (let ((pt (copy-sequence (scad-sketch--selected-point session))))
           (setf (nth 2 pt) (float z))
           (scad-sketch--set-selected-point session pt))
       (setf (scad-sketch-session-fixed-z session) (float z))))))

(defun scad-sketch--set-delta-axis (axis value)
  "Set visible cursor coordinate AXIS to mark + VALUE."
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-mark session)
      (user-error "No mark set"))
    (scad-sketch--set-point-axis
     axis
     (+ (nth axis (scad-sketch-session-mark session)) (float value)))))

(defun scad-sketch-set-delta-x (dx)
  "Set cursor X to mark X plus DX."
  (interactive (list (read-number "ΔX from mark: " 0)))
  (scad-sketch--set-delta-axis 0 dx))

(defun scad-sketch-set-delta-y (dy)
  "Set cursor Y to mark Y plus DY."
  (interactive (list (read-number "ΔY from mark: " 0)))
  (scad-sketch--set-delta-axis 1 dy))

(defun scad-sketch-set-delta-z (dz)
  "Set fixed Z by delta; placeholder for 3D workflows."
  (interactive (list (read-number "ΔZ: " 0)))
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch--3d-kind-p session)
      (user-error "Z is only meaningful for kind=3d"))
    (scad-sketch--mutate
     (lambda (s)
       (setf (scad-sketch-session-fixed-z s)
             (+ (scad-sketch-session-fixed-z s) (float dz)))))))

(defun scad-sketch-set-distance-from-mark (distance)
  "Set distance from mark to point, preserving current angle."
  (interactive (list (read-number "Distance from mark: " 0)))
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-mark session)
      (user-error "No mark set")))
  (scad-sketch--mutate
   (lambda (session)
     (let* ((m (scad-sketch-session-mark session))
            (p (scad-sketch-session-point session))
            (dx (- (nth 0 p) (nth 0 m)))
            (dy (- (nth 1 p) (nth 1 m)))
            (angle (atan dy dx)))
       (setf (scad-sketch-session-point session)
             (list (+ (nth 0 m) (* (float distance) (cos angle)))
                   (+ (nth 1 m) (* (float distance) (sin angle)))))))))

(defun scad-sketch-set-angle-from-mark (degrees)
  "Set angle from mark to point in DEGREES, preserving distance."
  (interactive (list (read-number "Angle degrees from mark: " 0)))
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-mark session)
      (user-error "No mark set")))
  (scad-sketch--mutate
   (lambda (session)
     (let* ((m (scad-sketch-session-mark session))
            (p (scad-sketch-session-point session))
            (dx (- (nth 0 p) (nth 0 m)))
            (dy (- (nth 1 p) (nth 1 m)))
            (dist (sqrt (+ (* dx dx) (* dy dy))))
            (angle (* pi (/ (float degrees) 180.0))))
       (setf (scad-sketch-session-point session)
             (list (+ (nth 0 m) (* dist (cos angle)))
                   (+ (nth 1 m) (* dist (sin angle)))))))))

(defun scad-sketch-set-grid (grid)
  "Set grid step for this editing session."
  (interactive (list (read-number "Grid step: " (scad-sketch-session-grid (scad-sketch--assert-session)))))
  (scad-sketch--mutate
   (lambda (session)
     (setf (scad-sketch-session-grid session) (float grid)))))

(defun scad-sketch-undo ()
  "Undo the last scad-sketch editing command."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (entry (pop (scad-sketch-session-undo-stack session))))
    (unless entry
      (user-error "No sketch undo available"))
    (setf (scad-sketch-session-points session) (plist-get entry :points))
    (setf (scad-sketch-session-point session) (plist-get entry :point))
    (setf (scad-sketch-session-mark session) (plist-get entry :mark))
    (setf (scad-sketch-session-mark-ring session) (plist-get entry :mark-ring))
    (setf (scad-sketch-session-named-marks session) (plist-get entry :named-marks))
    (setf (scad-sketch-session-selected-index session) (plist-get entry :selected-index))
    (setf (scad-sketch-session-closed session) (plist-get entry :closed))
    (setf (scad-sketch-session-dirty session) t)
    (scad-sketch--render)))

(defun scad-sketch--bounds (session)
  "Return visible bounds for SESSION as (MIN-X MAX-X MIN-Y MAX-Y)."
  (let* ((points (mapcar (lambda (p) (scad-sketch--point-xy p session))
                         (scad-sketch-session-points session)))
         (extra (delq nil (list (scad-sketch-session-point session)
                                (scad-sketch-session-mark session))))
         (all (append points extra)))
    (if (null all)
        (list -10 10 -10 10)
      (let ((min-x (apply #'min (mapcar #'car all)))
            (max-x (apply #'max (mapcar #'car all)))
            (min-y (apply #'min (mapcar #'cadr all)))
            (max-y (apply #'max (mapcar #'cadr all))))
        (when (= min-x max-x)
          (setq min-x (- min-x 10)
                max-x (+ max-x 10)))
        (when (= min-y max-y)
          (setq min-y (- min-y 10)
                max-y (+ max-y 10)))
        (let* ((pad-x (max 1 (* 0.15 (- max-x min-x))))
               (pad-y (max 1 (* 0.15 (- max-y min-y)))))
          (list (- min-x pad-x) (+ max-x pad-x)
                (- min-y pad-y) (+ max-y pad-y)))))))

(defun scad-sketch--transform (bounds)
  "Return a coordinate transform function for BOUNDS."
  (pcase-let ((`(,min-x ,max-x ,min-y ,max-y) bounds))
    (let* ((w scad-sketch-canvas-width)
           (h scad-sketch-canvas-height)
           (m scad-sketch-margin)
           (sx (/ (- w (* 2 m)) (- max-x min-x)))
           (sy (/ (- h (* 2 m)) (- max-y min-y)))
           (scale (min sx sy)))
      (lambda (xy)
        (let ((x (nth 0 xy))
              (y (nth 1 xy)))
          (list (+ m (* (- x min-x) scale))
                (- h (+ m (* (- y min-y) scale)))))))))

(defun scad-sketch--svg-line (svg transform a b &rest args)
  "Draw line from A to B on SVG using TRANSFORM and ARGS."
  (let* ((pa (funcall transform a))
         (pb (funcall transform b)))
    (apply #'svg-line svg (nth 0 pa) (nth 1 pa) (nth 0 pb) (nth 1 pb) args)))

(defun scad-sketch--draw-grid (svg bounds transform session)
  "Draw a grid for SESSION on SVG."
  (pcase-let ((`(,min-x ,max-x ,min-y ,max-y) bounds))
    (let* ((grid (max 0.0001 (scad-sketch-session-grid session)))
           (start-x (* grid (floor (/ min-x grid))))
           (end-x (* grid (ceiling (/ max-x grid))))
           (start-y (* grid (floor (/ min-y grid))))
           (end-y (* grid (ceiling (/ max-y grid))))
           (x start-x)
           (y start-y))
      (while (<= x end-x)
        (scad-sketch--svg-line svg transform (list x min-y) (list x max-y)
                               :stroke "#e8e8e8" :stroke-width 1)
        (setq x (+ x grid)))
      (while (<= y end-y)
        (scad-sketch--svg-line svg transform (list min-x y) (list max-x y)
                               :stroke "#e8e8e8" :stroke-width 1)
        (setq y (+ y grid)))
      ;; Axes.
      (when (and (<= min-x 0) (<= 0 max-x))
        (scad-sketch--svg-line svg transform (list 0 min-y) (list 0 max-y)
                               :stroke "#d0d0d0" :stroke-width 2))
      (when (and (<= min-y 0) (<= 0 max-y))
        (scad-sketch--svg-line svg transform (list min-x 0) (list max-x 0)
                               :stroke "#d0d0d0" :stroke-width 2)))))

(defun scad-sketch--draw-path (svg transform session)
  "Draw SESSION points/path on SVG."
  (let* ((points (scad-sketch-session-points session))
         (xy-points (mapcar (lambda (p) (scad-sketch--point-xy p session)) points))
         (idx 0))
    (cl-loop for a on xy-points
             for b = (cadr a)
             when b do
             (scad-sketch--svg-line svg transform (car a) b
                                    :stroke "#111111" :stroke-width 3))
    (when (and (scad-sketch-session-closed session)
               (> (length xy-points) 2))
      (scad-sketch--svg-line svg transform (car (last xy-points)) (car xy-points)
                             :stroke "#111111" :stroke-width 3))
    (dolist (pt points)
      (let* ((xy (scad-sketch--point-xy pt session))
             (screen (funcall transform xy))
             (selected (= idx (or (scad-sketch-session-selected-index session) -1)))
             (radius (scad-sketch--point-radius pt session)))
        (svg-circle svg (nth 0 screen) (nth 1 screen) (if selected 7 5)
                    :stroke (if selected "#d13f00" "#111111")
                    :stroke-width (if selected 3 2)
                    :fill (if selected "#fff0e8" "#ffffff"))
        (svg-text svg (number-to-string idx)
                  :x (+ (nth 0 screen) 8)
                  :y (- (nth 1 screen) 8)
                  :font-size 12
                  :fill "#333333")
        (when (and radius (> radius 0))
          (svg-text svg (format "r=%s" (scad-sketch--fmt-num radius))
                    :x (+ (nth 0 screen) 8)
                    :y (+ (nth 1 screen) 18)
                    :font-size 11
                    :fill "#804000")))
      (setq idx (1+ idx)))))

(defun scad-sketch--draw-point-and-mark (svg transform session)
  "Draw cursor point and mark for SESSION."
  (let* ((point (scad-sketch-session-point session))
         (p (funcall transform point)))
    (svg-circle svg (nth 0 p) (nth 1 p) 5
                :stroke "#0057c2" :stroke-width 2 :fill "#dfefff")
    (svg-line svg (- (nth 0 p) 10) (nth 1 p) (+ (nth 0 p) 10) (nth 1 p)
              :stroke "#0057c2" :stroke-width 2)
    (svg-line svg (nth 0 p) (- (nth 1 p) 10) (nth 0 p) (+ (nth 1 p) 10)
              :stroke "#0057c2" :stroke-width 2)
    (svg-text svg "point" :x (+ (nth 0 p) 12) :y (+ (nth 1 p) 4)
              :font-size 12 :fill "#0057c2"))
  (when (scad-sketch-session-mark session)
    (let* ((m (funcall transform (scad-sketch-session-mark session))))
      (svg-circle svg (nth 0 m) (nth 1 m) 6
                  :stroke "#008a2e" :stroke-width 2 :fill "#e2ffe9")
      (svg-text svg "mark" :x (+ (nth 0 m) 10) :y (+ (nth 1 m) 4)
                :font-size 12 :fill "#008a2e")
      (scad-sketch--svg-line svg transform
                             (scad-sketch-session-mark session)
                             (scad-sketch-session-point session)
                             :stroke "#008a2e"
                             :stroke-width 1
                             :stroke-dasharray "4,4"))))

(defun scad-sketch--fmt-xy (xy)
  "Format visible XY coordinate."
  (format "(%s, %s)"
          (scad-sketch--fmt-num (nth 0 xy))
          (scad-sketch--fmt-num (nth 1 xy))))

(defun scad-sketch--draw-hud (svg session)
  "Draw heads-up text on SVG for SESSION."
  (let* ((dirty (if (scad-sketch-session-dirty session) "*dirty*" "saved"))
         (selected (scad-sketch-session-selected-index session))
         (text (format "%s  kind=%s  grid=%s%s  point=%s  mark=%s  selected=%s  %s"
                       (scad-sketch-session-name session)
                       (scad-sketch-session-kind session)
                       (scad-sketch--fmt-num (scad-sketch-session-grid session))
                       (scad-sketch-session-units session)
                       (scad-sketch--fmt-xy (scad-sketch-session-point session))
                       (if (scad-sketch-session-mark session)
                           (scad-sketch--fmt-xy (scad-sketch-session-mark session))
                         "none")
                       (if selected (number-to-string selected) "none")
                       dirty)))
    (svg-rectangle svg 0 0 scad-sketch-canvas-width 28 :fill "#f8f8f8")
    (svg-text svg text :x 10 :y 19 :font-size 13 :fill "#111111")))

(defun scad-sketch--render ()
  "Render the current sketch session into the editor buffer."
  (let* ((session (scad-sketch--assert-session))
         (svg (svg-create scad-sketch-canvas-width scad-sketch-canvas-height))
         (bounds (scad-sketch--bounds session))
         (transform (scad-sketch--transform bounds)))
    (svg-rectangle svg 0 0 scad-sketch-canvas-width scad-sketch-canvas-height
                   :fill "#ffffff")
    (scad-sketch--draw-grid svg bounds transform session)
    (scad-sketch--draw-path svg transform session)
    (scad-sketch--draw-point-and-mark svg transform session)
    (scad-sketch--draw-hud svg session)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert-image (svg-image svg :ascent 'center))
      (insert "\n\n")
      (insert (scad-sketch--status-text session))
      (goto-char (point-min)))))

(defun scad-sketch--status-text (session)
  "Return textual status/help for SESSION."
  (concat
   "Keys: arrows move point; M-arrows fine; C-arrows coarse; S-arrows move selected vertex\n"
   "      m mark, M push mark, ' jump mark, p append point, i insert, k delete\n"
   "      l line from mark, r rectangle from mark, c toggle closed, R radius\n"
   "      TAB/S-TAB select, x/y/z set coord, X/Y/Z delta, d distance, a angle\n"
   "      g grid, u undo, w write back, q quit, ? help\n\n"
   (format "Source: %s\n"
           (buffer-name (scad-sketch-session-source-buffer session)))
   (format "Points: %d\n" (length (scad-sketch-session-points session)))
   (format "Array preview:\n%s"
           (scad-sketch--emit-content session))))

(defun scad-sketch--fmt-num (n)
  "Format number N compactly for OpenSCAD."
  (let ((x (float n)))
    (if (< (abs (- x (round x))) 0.000001)
        (number-to-string (round x))
      (let ((s (format "%.4f" x)))
        (setq s (replace-regexp-in-string "0+\\'" "" s))
        (setq s (replace-regexp-in-string "\\.\\'" "" s))
        (if (or (string= s "-0") (string= s "")) "0" s)))))

(defun scad-sketch--emit-point (point session)
  "Emit one model POINT for SESSION."
  (cond
   ((scad-sketch--2d-kind-p session)
    (format "[%s, %s]"
            (scad-sketch--fmt-num (nth 0 point))
            (scad-sketch--fmt-num (nth 1 point))))
   ((or (scad-sketch--curve-kind-p session)
        (scad-sketch--3d-kind-p session))
    (format "[%s, %s, %s]"
            (scad-sketch--fmt-num (nth 0 point))
            (scad-sketch--fmt-num (nth 1 point))
            (scad-sketch--fmt-num (nth 2 point))))
   (t
    (format "%S" point))))

(defun scad-sketch--emit-content (session)
  "Emit SESSION as the SCAD assignment content between sketch comments."
  (let* ((name (scad-sketch-session-name session))
         (points (scad-sketch-session-points session))
         (lines (mapcar (lambda (p) (concat "  " (scad-sketch--emit-point p session)))
                        points)))
    (concat name " = [\n"
            (mapconcat #'identity lines ",\n")
            (if lines "\n" "")
            "];\n")))

(defun scad-sketch-write-back ()
  "Write the edited sketch array back into the original SCAD buffer."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (source (scad-sketch-session-source-buffer session))
         (beg (scad-sketch-session-content-beg session))
         (end (scad-sketch-session-content-end session))
         (content (scad-sketch--emit-content session)))
    (unless (buffer-live-p source)
      (user-error "Source buffer is gone"))
    (with-current-buffer source
      (save-excursion
        (goto-char beg)
        (delete-region beg end)
        (insert content)
        (set-marker end (point))))
    (setf (scad-sketch-session-dirty session) nil)
    (scad-sketch--render)
    (message "Wrote scad-sketch `%s' back to %s"
             (scad-sketch-session-name session)
             (buffer-name source))))

(defun scad-sketch-quit ()
  "Quit the current sketch editor buffer."
  (interactive)
  (let ((session (scad-sketch--assert-session)))
    (when (and (scad-sketch-session-dirty session)
               (y-or-n-p "Sketch has unwritten edits. Write back first? "))
      (scad-sketch-write-back)))
  (kill-buffer (current-buffer)))

(defun scad-sketch-help ()
  "Show scad-sketch key help."
  (interactive)
  (message "%s" (replace-regexp-in-string "\n" "  "
                                       (scad-sketch--status-text (scad-sketch--assert-session)))))

;;; Adopting existing SCAD arrays

(defun scad-sketch--inside-existing-block-p (&optional pos)
  "Return non-nil if POS is already inside a scad-sketch block."
  (let ((origin (or pos (point))))
    (save-excursion
      (goto-char origin)
      (when (re-search-backward scad-sketch-metadata-regexp nil t)
        (let ((beg (line-beginning-position)))
          (goto-char (line-end-position))
          (and (re-search-forward scad-sketch-end-regexp nil t)
               (<= beg origin)
               (<= origin (match-end 0))))))))

(defun scad-sketch--forward-balanced-bracket (pos)
  "Return position just after the bracketed form starting at POS.
This is a small scanner for SCAD-ish literals. It ignores brackets inside
strings, line comments, and block comments."
  (save-excursion
    (goto-char pos)
    (unless (= (char-after) 91)
      (user-error "Expected `[' at array start"))
    (let ((depth 0)
          (done nil)
          (in-string nil)
          (escape nil)
          (line-comment nil)
          (block-comment nil))
      (while (and (not done) (< (point) (point-max)))
        (let ((ch (char-after))
              (next (char-after (1+ (point)))))
          (cond
           (line-comment
            (when (= ch 10)
              (setq line-comment nil))
            (forward-char 1))
           (block-comment
            (if (and next (= ch 42) (= next 47))
                (progn
                  (setq block-comment nil)
                  (forward-char 2))
              (forward-char 1)))
           (in-string
            (cond
             (escape
              (setq escape nil)
              (forward-char 1))
             ((= ch 92)
              (setq escape t)
              (forward-char 1))
             ((= ch 34)
              (setq in-string nil)
              (forward-char 1))
             (t
              (forward-char 1))))
           ((and next (= ch 47) (= next 47))
            (setq line-comment t)
            (forward-char 2))
           ((and next (= ch 47) (= next 42))
            (setq block-comment t)
            (forward-char 2))
           ((= ch 34)
            (setq in-string t)
            (forward-char 1))
           ((= ch 91)
            (setq depth (1+ depth))
            (forward-char 1))
           ((= ch 93)
            (setq depth (1- depth))
            (forward-char 1)
            (when (= depth 0)
              (setq done t)))
           (t
            (forward-char 1)))))
      (unless done
        (user-error "Could not find the end of this array literal"))
      (point))))

(defun scad-sketch--find-array-assignment-at-point ()
  "Find a literal SCAD array assignment surrounding point.
Return plist (:name NAME :beg BEG :end END :open-bracket OPEN :text TEXT)."
  (let ((origin (point))
        name beg open end)
    (save-excursion
      (unless (re-search-backward
               (rx (group (+ (any "A-Za-z0-9_$"))) (* space) "=" (* space) "[")
               nil t)
        (user-error "No literal array assignment before point"))
      (setq name (match-string-no-properties 1))
      (setq beg (match-beginning 0))
      (setq open (1- (match-end 0)))
      (setq end (scad-sketch--forward-balanced-bracket open))
      (goto-char end)
      (skip-chars-forward "
")
      (unless (= (char-after) 59)
        (user-error "Array assignment must end with a semicolon"))
      (forward-char 1)
      (setq end (point))
      (unless (and (<= beg origin) (<= origin end))
        (user-error "Point is not inside the nearest literal array assignment"))
      (list :name name
            :beg beg
            :end end
            :open-bracket open
            :text (buffer-substring-no-properties beg end)))))

(defun scad-sketch--literal-point-dimensions (text)
  "Return a list of point dimensions found in literal point arrays in TEXT."
  (let ((point-re (rx "[" (* space)
                      (group (+ (any "0-9+-.eE"))) (* space) "," (* space)
                      (group (+ (any "0-9+-.eE")))
                      (? (* space) "," (* space)
                         (group (+ (any "0-9+-.eE"))))
                      (* space) "]"))
        dims)
    (with-temp-buffer
      (insert (scad-sketch--strip-line-comments text))
      (goto-char (point-min))
      (while (re-search-forward point-re nil t)
        (push (if (match-string 3) 3 2) dims)))
    (nreverse dims)))

(defun scad-sketch--infer-kind-for-existing-array (text)
  "Infer or prompt for the scad-sketch kind for existing assignment TEXT."
  (let* ((dims (scad-sketch--literal-point-dimensions text))
         (uniq (delete-dups (copy-sequence dims))))
    (unless dims
      (user-error "No literal point arrays found"))
    (cond
     ((equal uniq '(2)) "2d")
     ((equal uniq '(3))
      (completing-read
       "This is a 3-column array. Treat as: "
       '("3d" "2d-with-curves") nil t nil nil "3d"))
     (t
      (completing-read
       "Mixed 2/3-column points. Treat missing third values as zero for: "
       '("2d-with-curves" "3d") nil t nil nil "2d-with-curves")))))

(defun scad-sketch--metadata-for-adopted-array (name kind)
  "Build a scad-sketch metadata line for adopted NAME and KIND."
  (let* ((closed (if (y-or-n-p "Mark sketch as closed? ") "true" "false"))
         (grid (read-number "Grid step: " scad-sketch-default-grid))
         (units (read-string "Units: " "mm"))
         (plane (when (string= kind "3d")
                  (completing-read "3D edit plane: " '("xy" "xz" "yz") nil t nil nil "xy"))))
    (concat "// scad-sketch: "
            (format "name=%s kind=%s closed=%s grid=%s units=%s"
                    name kind closed (scad-sketch--fmt-num grid) units)
            (when (string= kind "3d")
              (format " plane=%s fixed-x=0 fixed-y=0 fixed-z=0" plane)))))

;;;###autoload
(defun scad-sketch-adopt-array-at-point ()
  "Decorate an existing literal SCAD array assignment as a scad-sketch block.
For 2-column point arrays, this uses kind=2d. For 3-column arrays, prompt
whether the third column is a Z coordinate, kind=3d, or a polyRound-style
radius, kind=2d-with-curves."
  (interactive)
  (when (scad-sketch--inside-existing-block-p)
    (user-error "This array already appears to be inside a scad-sketch block"))
  (let* ((assignment (scad-sketch--find-array-assignment-at-point))
         (name (plist-get assignment :name))
         (kind (scad-sketch--infer-kind-for-existing-array
                (plist-get assignment :text)))
         (metadata (scad-sketch--metadata-for-adopted-array name kind))
         (beg (plist-get assignment :beg))
         (end (copy-marker (plist-get assignment :end) t))
         (origin (copy-marker (point) t)))
    (save-excursion
      (goto-char end)
      (end-of-line)
      (insert (string 10) "// end-scad-sketch")
      (goto-char beg)
      (beginning-of-line)
      (insert metadata (string 10)))
    (goto-char origin)
    (message "Adopted `%s' as scad-sketch kind=%s. Run M-x scad-sketch-at-point to edit."
             name kind)))

;;; Convenience snippets

;;;###autoload
(defun scad-sketch-insert-2d-block (name)
  "Insert a new empty 2D scad-sketch block named NAME."
  (interactive "sSketch name: ")
  (insert (format "// scad-sketch: name=%s kind=2d closed=true grid=1 units=mm\n%s = [\n];\n// end-scad-sketch\n"
                  name name)))

;;;###autoload
(defun scad-sketch-insert-polyround-block (name)
  "Insert a new empty 2D-with-curves/polyRound scad-sketch block named NAME."
  (interactive "sSketch name: ")
  (insert (format "// scad-sketch: name=%s kind=2d-with-curves closed=true grid=1 units=mm\n%s = [\n];\n// end-scad-sketch\n"
                  name name)))

;;;###autoload
(defun scad-sketch-insert-3d-block (name plane)
  "Insert a new empty 3D scad-sketch block named NAME edited through PLANE."
  (interactive
   (list (read-string "Sketch name: ")
         (completing-read "Plane: " '("xy" "xz" "yz") nil t nil nil "xy")))
  (insert (format "// scad-sketch: name=%s kind=3d plane=%s fixed-x=0 fixed-y=0 fixed-z=0 open=true grid=1 units=mm\n%s = [\n];\n// end-scad-sketch\n"
                  name plane name)))

(provide 'scad-sketch)
;;; scad-sketch.el ends here
