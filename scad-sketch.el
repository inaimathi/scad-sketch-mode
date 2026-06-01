;;; scad-sketch.el --- Keyboard sketch editor for OpenSCAD arrays -*- lexical-binding: t; -*-

;; Author: inaimathi, Claude Sonnet
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: cad, openscad, svg, tools

;;; Commentary:

;; Keyboard-driven SVG sketch editor for OpenSCAD point-array literals.
;; Supports plain 2D polygons and polyRound-style rounded polygons.
;;
;; QUICK START
;; -----------
;; In any .scad buffer, position point inside or at the opening line of a
;; literal array assignment and run:
;;
;;   M-x scad-sketch-at-point
;;
;; To open or create: if no array is found at point, a fresh named array is
;; inserted and opened immediately:
;;
;;   M-x scad-sketch-or-insert-at-point   (C-c C-. in scad-sketch-mode)
;;
;; To annotate an existing bare array with scad-sketch metadata comments:
;;
;;   M-x scad-sketch-adopt-array-at-point  (C-c C-a in scad-sketch-mode)
;;
;; ARRAY KINDS
;; -----------
;; kind=2d              [[x, y], ...]          plain polygon vertices
;; kind=2d-with-curves  [[x, y, r], ...]       polyRound radii at each vertex
;;
;; Kind is inferred automatically when opening bare arrays:
;;   all 2-column points  ->  kind=2d
;;   any 3-column points  ->  kind=2d-with-curves
;;
;; METADATA COMMENT FORMAT
;; -----------------------
;; Blocks are delimited by:
;;
;;   // scad-sketch: name=NAME kind=KIND closed=true grid=1 units=mm
;;   NAME = [ ... ];
;;   // end-scad-sketch
;;
;; Optional metadata keys: fine=0.1  coarse=5
;;
;; EDITOR
;; ------
;; The editor buffer shows an SVG canvas with the polygon and a live array
;; preview below it.  Use `describe-mode' (C-h m) or press `?' to see all
;; key bindings.
;;
;; Key highlights:
;;   arrows / C-arrows / M-arrows  move cursor (grid / coarse / fine)
;;   M-arrows go off-grid; arrows and C-arrows snap back to grid
;;   S-arrows                      move the selected vertex
;;   TAB / S-TAB                   cycle through vertices
;;   p / i / k                     append / insert-after-selected / delete vertex
;;   m / M / ` / ' / C            set / push / pop / jump / clear marks
;;   i with marks set              inserts mark points then cursor point after selected
;;   l / r                         line or rectangle from marks
;;   R                             set polyRound radius on selected vertex
;;   w / q                         write back to source buffer / quit
;;
;; POLYROUND RADII
;; ---------------
;; For kind=2d-with-curves, each vertex carries an optional rounding radius
;; compatible with the Round-Anything/polyround.scad library.  The sketch
;; renders the actual arc geometry (including edge-length capping) so what
;; you see matches what OpenSCAD produces.  A dashed circle around each
;; rounded vertex shows the effective radius; it turns orange and displays
;; "r=REQ->ACT" when the radius has been capped by a short adjacent edge.
;;
;; This is intentionally not a general SVG editor.  SVG is just the view;
;; OpenSCAD-compatible point arrays are the model and output.

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
  :type 'number :group 'scad-sketch)

(defcustom scad-sketch-default-fine-step 0.1
  "Default fine movement step in sketch units."
  :type 'number :group 'scad-sketch)

(defcustom scad-sketch-default-coarse-step 5.0
  "Default coarse movement step in sketch units."
  :type 'number :group 'scad-sketch)

(defcustom scad-sketch-canvas-width 900
  "Sketch editor canvas width in pixels."
  :type 'integer :group 'scad-sketch)

(defcustom scad-sketch-canvas-height 650
  "Sketch editor canvas height in pixels."
  :type 'integer :group 'scad-sketch)

(defcustom scad-sketch-margin 48
  "Canvas margin in pixels."
  :type 'integer :group 'scad-sketch)

(defcustom scad-sketch-metadata-regexp
  "^[[:space:]]*//[[:space:]]*scad-sketch:[[:space:]]*\\(.*\\)$"
  "Regexp matching a scad-sketch metadata line."
  :type 'regexp :group 'scad-sketch)

(defcustom scad-sketch-end-regexp
  "^[[:space:]]*//[[:space:]]*end-scad-sketch[[:space:]]*$"
  "Regexp matching a scad-sketch end line."
  :type 'regexp :group 'scad-sketch)

(cl-defstruct scad-sketch-session
  name kind units grid fine-step coarse-step closed
  points point
  marks          ; list of [x y], newest first; (car marks) is the current mark
  named-marks selected-index
  source-buffer content-beg content-end
  metadata dirty undo-stack)

(defvar-local scad-sketch--session nil)
(defvar scad-sketch--editor-buffer-prefix "*scad-sketch: ")

(defvar scad-sketch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'scad-sketch-adopt-array-at-point)
    (define-key map (kbd "C-c C-.") #'scad-sketch-or-insert-at-point)
    map)
  "Keymap for `scad-sketch-mode'.
C-c C-s and C-c C-o are intentionally left free for `scad-mode'.")

;;;###autoload
(define-minor-mode scad-sketch-mode
  "Minor mode for opening scad-sketch blocks from OpenSCAD buffers."
  :lighter " Sketch"
  :keymap scad-sketch-mode-map)

(defvar scad-sketch-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    ;; Cursor movement
    (define-key map (kbd "<left>")    #'scad-sketch-move-point-left)
    (define-key map (kbd "<right>")   #'scad-sketch-move-point-right)
    (define-key map (kbd "<up>")      #'scad-sketch-move-point-up)
    (define-key map (kbd "<down>")    #'scad-sketch-move-point-down)
    (define-key map (kbd "M-<left>")  #'scad-sketch-move-point-fine-left)
    (define-key map (kbd "M-<right>") #'scad-sketch-move-point-fine-right)
    (define-key map (kbd "M-<up>")    #'scad-sketch-move-point-fine-up)
    (define-key map (kbd "M-<down>")  #'scad-sketch-move-point-fine-down)
    (define-key map (kbd "C-<left>")  #'scad-sketch-move-point-coarse-left)
    (define-key map (kbd "C-<right>") #'scad-sketch-move-point-coarse-right)
    (define-key map (kbd "C-<up>")    #'scad-sketch-move-point-coarse-up)
    (define-key map (kbd "C-<down>")  #'scad-sketch-move-point-coarse-down)
    ;; Selected vertex movement
    (define-key map (kbd "S-<left>")  #'scad-sketch-move-selected-left)
    (define-key map (kbd "S-<right>") #'scad-sketch-move-selected-right)
    (define-key map (kbd "S-<up>")    #'scad-sketch-move-selected-up)
    (define-key map (kbd "S-<down>")  #'scad-sketch-move-selected-down)
    ;; Marks
    (define-key map (kbd "m") #'scad-sketch-set-mark)
    (define-key map (kbd "M") #'scad-sketch-push-mark)
    (define-key map (kbd "`") #'scad-sketch-pop-mark)
    (define-key map (kbd "'") #'scad-sketch-jump-to-mark)
    (define-key map (kbd "C") #'scad-sketch-clear-marks)
    ;; Editing
    (define-key map (kbd "p")         #'scad-sketch-append-point)
    (define-key map (kbd "i")         #'scad-sketch-insert-point-after-selected)
    (define-key map (kbd "k")         #'scad-sketch-delete-selected)
    (define-key map (kbd "l")         #'scad-sketch-line-from-mark)
    (define-key map (kbd "r")         #'scad-sketch-rectangle-from-mark)
    (define-key map (kbd "c")         #'scad-sketch-toggle-closed)
    (define-key map (kbd "R")         #'scad-sketch-set-radius)
    (define-key map (kbd "TAB")       #'scad-sketch-next-point)
    (define-key map (kbd "<backtab>") #'scad-sketch-previous-point)
    (define-key map (kbd "x")         #'scad-sketch-set-x)
    (define-key map (kbd "y")         #'scad-sketch-set-y)
    (define-key map (kbd "X")         #'scad-sketch-set-delta-x)
    (define-key map (kbd "Y")         #'scad-sketch-set-delta-y)
    (define-key map (kbd "d")         #'scad-sketch-set-distance-from-mark)
    (define-key map (kbd "a")         #'scad-sketch-set-angle-from-mark)
    (define-key map (kbd "g")         #'scad-sketch-set-grid)
    (define-key map (kbd "u")         #'scad-sketch-undo)
    (define-key map (kbd "w")         #'scad-sketch-write-back)
    (define-key map (kbd "q")         #'scad-sketch-quit)
    (define-key map (kbd "?")         #'scad-sketch-help)
    map)
  "Keymap for `scad-sketch-editor-mode'.")

(define-derived-mode scad-sketch-editor-mode special-mode "SCAD-Sketch"
  "Major mode for the scad-sketch visual editor.

The buffer shows an SVG canvas followed by a live OpenSCAD array preview.

The canvas displays:
  - a grid (step set with `g')
  - the polygon path with arcs where polyRound radii are set
  - vertex dots numbered from 0; the selected vertex is highlighted in orange
  - dashed radius circles on rounded vertices (orange = capped by edge length)
  - the cursor crosshair in blue, marks in green
  - a status bar: name, kind, grid size, cursor coords, dirty flag

Movement:
  <arrow>             move cursor one grid step; snaps to grid
  C-<arrow>           move cursor one coarse step; snaps to grid
  M-<arrow>           move cursor one fine step; intentionally off-grid
  S-<arrow>           move selected vertex one grid step

Vertex editing:
  TAB / S-TAB         select next / previous vertex (cursor jumps to it)
  p                   append cursor as a new vertex at end of array
  i                   insert cursor after selected vertex; if marks are set,
                        inserts each mark (oldest first) then cursor
  k                   delete the selected vertex

Marks:
  m                   replace all marks with cursor position
  M                   push cursor position onto mark stack
  `                   pop most recent mark and jump cursor there
  '                   jump cursor to most recent mark (non-destructive)
  C                   clear all marks

Geometry:
  x / y               set cursor X or Y coordinate
  X / Y               set cursor X or Y relative to most recent mark (delta)
  d                   set distance from mark, preserving angle
  a                   set angle from mark in degrees, preserving distance
  R                   set polyRound radius on selected vertex
  c                   toggle closed / open polygon
  l                   append marks (oldest first) then cursor as vertices
  r                   append rectangle from most recent mark to cursor
  g                   change grid step

Session:
  w                   write array back to source buffer
  u                   undo
  q                   quit (offers to write back if dirty)
  ?                   key summary in the echo area

\{scad-sketch-editor-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t))

;;; Metadata

(defun scad-sketch--metadata-value (value)
  "Parse metadata VALUE string into a Lisp value."
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
  (let ((body (match-string 1 line)) result)
    (dolist (tok (split-string body "[[:space:]]+" t))
      (when (string-match "\\`\\([^=[:space:]]+\\)=\\(.+\\)\\'" tok)
        (push (cons (intern (match-string 1 tok))
                    (scad-sketch--metadata-value (match-string 2 tok)))
              result)))
    (nreverse result)))

(defun scad-sketch--meta (metadata key &optional default)
  "Return KEY from METADATA alist, or DEFAULT."
  (let ((cell (assq key metadata)))
    (if cell (cdr cell) default)))

;;; Block finding

(defun scad-sketch--find-block ()
  "Find the scad-sketch block around point.
Returns plist with :metadata, :content-beg, :content-end."
  (save-excursion
    (let ((origin (point)) meta-beg meta-end meta-line metadata content-beg content-end)
      (unless (re-search-backward scad-sketch-metadata-regexp nil t)
        (user-error "No scad-sketch metadata line before point"))
      (setq meta-beg  (line-beginning-position)
            meta-end  (line-end-position)
            meta-line (buffer-substring-no-properties meta-beg meta-end)
            metadata  (scad-sketch--parse-metadata meta-line)
            content-beg (min (point-max) (1+ meta-end)))
      (goto-char content-beg)
      (unless (re-search-forward scad-sketch-end-regexp nil t)
        (user-error "No // end-scad-sketch after metadata line"))
      (setq content-end (line-beginning-position))
      (unless (and (<= meta-beg origin) (<= origin (match-end 0)))
        (user-error "Point is not inside the nearest scad-sketch block"))
      (list :metadata metadata :content-beg content-beg :content-end content-end))))

(defconst scad-sketch--number-re
  "[-+]?[0-9]*\\.?[0-9]+\\(?:[eE][-+]?[0-9]+\\)?")

(defun scad-sketch--strip-line-comments (s)
  "Remove // line comments from each line of S."
  (mapconcat (lambda (line)
               (if (string-match "//" line)
                   (substring line 0 (match-beginning 0))
                 line))
             (split-string s "\n") "\n"))

(defun scad-sketch--parse-points (text kind)
  "Parse literal point arrays from TEXT for KIND."
  (let* ((clean (scad-sketch--strip-line-comments text))
         (n scad-sketch--number-re)
         (re (concat "\\[\\s-*\\(" n "\\)\\s-*,\\s-*\\(" n "\\)"
                     "\\(?:\\s-*,\\s-*\\(" n "\\)\\)?" "\\s-*\\]"))
         points)
    (with-temp-buffer
      (insert clean)
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (let ((x (string-to-number (match-string 1)))
              (y (string-to-number (match-string 2)))
              (z (when (match-string 3) (string-to-number (match-string 3)))))
          (push (if (or (string= kind "2d-with-curves")
                        (string= kind "2d-curves")
                        (string= kind "2d-polyround"))
                    (list x y (or z 0))
                  (list x y))
                points))))
    (nreverse points)))

;;; Session construction

(defun scad-sketch--make-session (block)
  "Create a `scad-sketch-session' from BLOCK plist."
  (let* ((metadata (plist-get block :metadata))
         (name   (format "%s" (scad-sketch--meta metadata 'name "sketch")))
         (kind   (format "%s" (scad-sketch--meta metadata 'kind "2d")))
         (grid   (float (scad-sketch--meta metadata 'grid scad-sketch-default-grid)))
         (fine   (float (scad-sketch--meta metadata 'fine scad-sketch-default-fine-step)))
         (coarse (float (scad-sketch--meta metadata 'coarse scad-sketch-default-coarse-step)))
         (closed (if (assq 'closed metadata) (scad-sketch--meta metadata 'closed t) t))
         (units  (format "%s" (scad-sketch--meta metadata 'units "mm")))
         (content (buffer-substring-no-properties
                   (plist-get block :content-beg) (plist-get block :content-end)))
         (points (scad-sketch--parse-points content kind))
         (beg-marker (copy-marker (plist-get block :content-beg)))
         (end-marker (copy-marker (plist-get block :content-end) t)))
    (make-scad-sketch-session
     :name name :kind kind :units units :grid grid :fine-step fine :coarse-step coarse
     :closed closed :points points
     :point (if points
                (list (float (nth 0 (car points))) (float (nth 1 (car points))))
              (list 0.0 0.0))
     :marks nil :named-marks nil
     :selected-index (if points 0 nil)
     :source-buffer (current-buffer)
     :content-beg beg-marker :content-end end-marker
     :metadata metadata :dirty nil :undo-stack nil)))

;;; Array-assignment finding

(defun scad-sketch--forward-balanced-bracket (pos)
  "Return position just after the `[...]' form starting at POS."
  (save-excursion
    (goto-char pos)
    (unless (= (char-after) ?\[)
      (user-error "Expected `[' at array start"))
    (let ((depth 0) done in-string escape line-comment block-comment)
      (while (and (not done) (< (point) (point-max)))
        (let ((ch   (char-after))
              (next (char-after (1+ (point)))))
          (cond
           (line-comment  (when (= ch ?\n) (setq line-comment nil)) (forward-char 1))
           (block-comment (if (and next (= ch ?*) (= next ?/))
                              (progn (setq block-comment nil) (forward-char 2))
                            (forward-char 1)))
           (in-string     (cond (escape (setq escape nil) (forward-char 1))
                                ((= ch ?\\) (setq escape t) (forward-char 1))
                                ((= ch ?\") (setq in-string nil) (forward-char 1))
                                (t (forward-char 1))))
           ((and next (= ch ?/) (= next ?/)) (setq line-comment t)  (forward-char 2))
           ((and next (= ch ?/) (= next ?*)) (setq block-comment t) (forward-char 2))
           ((= ch ?\") (setq in-string t) (forward-char 1))
           ((= ch ?\[) (setq depth (1+ depth)) (forward-char 1))
           ((= ch ?\]) (setq depth (1- depth)) (forward-char 1)
            (when (= depth 0) (setq done t)))
           (t (forward-char 1)))))
      (unless done (user-error "Could not find the end of this array literal"))
      (point))))

(defun scad-sketch--find-array-assignment-at-point ()
  "Find a literal SCAD array assignment at or surrounding point.
Returns plist (:name :beg :end :text)."
  (let ((origin (point)) name beg open end)
    (save-excursion
      (goto-char (line-end-position))
      (unless (re-search-backward
               (rx (group (+ (any "A-Za-z0-9_$"))) (* space) "=" (* space) "[")
               nil t)
        (user-error "No literal array assignment before point"))
      (setq name (match-string-no-properties 1)
            beg  (match-beginning 0)
            open (1- (match-end 0))
            end  (scad-sketch--forward-balanced-bracket open))
      (goto-char end)
      (skip-chars-forward " \t\r\n")
      (unless (= (char-after) ?\;)
        (user-error "Array assignment must end with a semicolon"))
      (forward-char 1)
      (setq end (point))
      (unless (<= origin end)
        (user-error "Point is not inside the nearest literal array assignment"))
      (list :name name :beg beg :end end
            :text (buffer-substring-no-properties beg end)))))

(defun scad-sketch--literal-point-dimensions (text)
  "Return list of point dimensions (2 or 3) found in TEXT."
  (let* ((clean (scad-sketch--strip-line-comments text))
         (n scad-sketch--number-re)
         (re (concat "\\[\\s-*\\(" n "\\)\\s-*,\\s-*\\(" n "\\)"
                     "\\(?:\\s-*,\\s-*\\(" n "\\)\\)?" "\\s-*\\]"))
         dims)
    (with-temp-buffer
      (insert clean)
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (push (if (match-string 3) 3 2) dims)))
    (nreverse dims)))

(defun scad-sketch--infer-kind (text)
  "Infer kind from array TEXT: 2-column -> 2d, any 3-column -> 2d-with-curves."
  (let* ((dims (scad-sketch--literal-point-dimensions text))
         (uniq (delete-dups (copy-sequence dims))))
    (unless dims
      (user-error "No literal [x, y] or [x, y, r] points found"))
    (if (equal uniq '(2)) "2d" "2d-with-curves")))

(defun scad-sketch--block-from-array-assignment (assignment)
  "Convert raw ASSIGNMENT plist into a sketch block plist."
  (let* ((name (plist-get assignment :name))
         (kind (scad-sketch--infer-kind (plist-get assignment :text)))
         (metadata `((name . ,name) (kind . ,kind) (closed . t)
                     (grid . 1) (units . "mm"))))
    (list :metadata metadata
          :content-beg (plist-get assignment :beg)
          :content-end (plist-get assignment :end))))

(defun scad-sketch--find-target-at-point ()
  "Find a scad-sketch block or array assignment at point."
  (condition-case nil
      (scad-sketch--find-block)
    (user-error
     (scad-sketch--block-from-array-assignment
      (scad-sketch--find-array-assignment-at-point)))))

;;; Opening the editor

(defun scad-sketch--open-session (session)
  "Open an editor buffer for SESSION."
  (let ((buf (get-buffer-create
              (format "%s%s*" scad-sketch--editor-buffer-prefix
                      (scad-sketch-session-name session)))))
    (with-current-buffer buf
      (scad-sketch-editor-mode)
      (setq-local scad-sketch--session session)
      (scad-sketch--render))
    (pop-to-buffer buf)))

;;;###autoload
(defun scad-sketch-at-point ()
  "Open the sketch editor for the array or scad-sketch block at point."
  (interactive)
  (unless (image-type-available-p 'svg)
    (user-error "This Emacs was not built with SVG image support"))
  (scad-sketch--open-session
   (scad-sketch--make-session (scad-sketch--find-target-at-point))))

;;;###autoload
(defun scad-sketch-insert-array-at-point (name)
  "Insert a new empty named 2D array at point and open the sketch editor."
  (interactive "sArray name: ")
  (unless (image-type-available-p 'svg)
    (user-error "This Emacs was not built with SVG image support"))
  (insert (format "// scad-sketch: name=%s kind=2d closed=true grid=1 units=mm\n" name))
  (let ((content-beg (point-marker)))
    (insert (format "%s = [\n];\n" name))
    (let ((content-end (copy-marker (point) t)))
      (insert "// end-scad-sketch\n")
      (scad-sketch--open-session
       (scad-sketch--make-session
        (list :metadata `((name . ,name) (kind . "2d") (closed . t)
                          (grid . 1) (units . "mm"))
              :content-beg content-beg
              :content-end content-end))))))

;;;###autoload
(defun scad-sketch-or-insert-at-point ()
  "Edit the array at point, or insert and open a new one if none is found."
  (interactive)
  (unless (image-type-available-p 'svg)
    (user-error "This Emacs was not built with SVG image support"))
  (condition-case nil
      (scad-sketch-at-point)
    (user-error
     (call-interactively #'scad-sketch-insert-array-at-point))))

;;; Session helpers

(defun scad-sketch--assert-session ()
  "Return the current sketch session or signal an error."
  (unless (and (boundp 'scad-sketch--session) scad-sketch--session)
    (user-error "No active scad-sketch session"))
  scad-sketch--session)

(defun scad-sketch--curve-kind-p (session)
  "Non-nil if SESSION uses polyRound-style [x y r] points."
  (member (scad-sketch-session-kind session)
          '("2d-with-curves" "2d-curves" "2d-polyround")))

(defun scad-sketch--point-xy (point)
  "Return the visible [x y] of model POINT."
  (list (float (or (nth 0 point) 0))
        (float (or (nth 1 point) 0))))

(defun scad-sketch--point-radius (point session)
  "Return polyRound radius for POINT in SESSION, or nil."
  (when (scad-sketch--curve-kind-p session) (or (nth 2 point) 0)))

(defun scad-sketch--make-model-point (xy session &optional old-point)
  "Build a model point from visible XY, preserving radius from OLD-POINT."
  (let ((x (float (nth 0 xy))) (y (float (nth 1 xy))))
    (if (scad-sketch--curve-kind-p session)
        (list x y (float (or (nth 2 old-point) 0)))
      (list x y))))

(defun scad-sketch--replace-nth (n value list)
  "Return LIST with element N replaced by VALUE."
  (let ((copy (copy-sequence list)))
    (setf (nth n copy) value)
    copy))

;;; Undo

(defun scad-sketch--push-undo (session)
  "Push SESSION state onto the undo stack."
  (push (list :points         (copy-tree (scad-sketch-session-points session))
              :point          (copy-tree (scad-sketch-session-point session))
              :marks          (copy-tree (scad-sketch-session-marks session))
              :named-marks    (copy-tree (scad-sketch-session-named-marks session))
              :selected-index (scad-sketch-session-selected-index session)
              :closed         (scad-sketch-session-closed session))
        (scad-sketch-session-undo-stack session)))

(defun scad-sketch--mark-dirty (session)
  "Mark SESSION as having unsaved edits."
  (setf (scad-sketch-session-dirty session) t))

(defun scad-sketch--mutate (fn)
  "Push undo, call FN with session, mark dirty, re-render."
  (let ((session (scad-sketch--assert-session)))
    (scad-sketch--push-undo session)
    (funcall fn session)
    (scad-sketch--mark-dirty session)
    (scad-sketch--render)))

;;; Point selection

(defun scad-sketch--selected-point (session)
  "Return the currently selected model point, or nil."
  (let ((idx (scad-sketch-session-selected-index session)))
    (when (and idx (>= idx 0) (< idx (length (scad-sketch-session-points session))))
      (nth idx (scad-sketch-session-points session)))))

(defun scad-sketch--set-selected-point (session point)
  "Replace the selected model point in SESSION with POINT."
  (let ((idx (scad-sketch-session-selected-index session)))
    (unless (and idx (>= idx 0) (< idx (length (scad-sketch-session-points session))))
      (user-error "No selected point"))
    (setf (scad-sketch-session-points session)
          (scad-sketch--replace-nth idx point (scad-sketch-session-points session)))))

;;; Movement

(defun scad-sketch--move-xy (xy dx dy)
  "Return XY shifted by DX, DY."
  (list (+ (float (nth 0 xy)) dx) (+ (float (nth 1 xy)) dy)))

(defun scad-sketch--snap-to-grid (v grid)
  "Round V to the nearest multiple of GRID."
  (* grid (round (/ v grid))))

(defun scad-sketch--snap-xy (xy grid)
  "Snap both coordinates of XY to GRID."
  (list (scad-sketch--snap-to-grid (nth 0 xy) grid)
        (scad-sketch--snap-to-grid (nth 1 xy) grid)))

(defun scad-sketch--move-point (dx dy &optional snap)
  "Move the cursor by DX, DY.
When SNAP is non-nil, snap the result to the session grid."
  (scad-sketch--mutate
   (lambda (s)
     (let ((new (scad-sketch--move-xy (scad-sketch-session-point s) dx dy)))
       (setf (scad-sketch-session-point s)
             (if snap
                 (scad-sketch--snap-xy new (scad-sketch--grid s))
               new))))))

(defun scad-sketch--move-selected (dx dy &optional snap)
  "Move the selected vertex by DX, DY.
When SNAP is non-nil, snap the result to the session grid."
  (scad-sketch--mutate
   (lambda (s)
     (let* ((old    (or (scad-sketch--selected-point s) (user-error "No selected point")))
            (new-xy (scad-sketch--move-xy (scad-sketch--point-xy old) dx dy))
            (snapped (if snap (scad-sketch--snap-xy new-xy (scad-sketch--grid s)) new-xy))
            (new    (scad-sketch--make-model-point snapped s old)))
       (scad-sketch--set-selected-point s new)
       (setf (scad-sketch-session-point s) snapped)))))

(defun scad-sketch--grid   (s) (float (scad-sketch-session-grid s)))
(defun scad-sketch--fine   (s) (float (scad-sketch-session-fine-step s)))
(defun scad-sketch--coarse (s) (float (scad-sketch-session-coarse-step s)))

(defun scad-sketch-move-point-left ()         (interactive) (scad-sketch--move-point (- (scad-sketch--grid   (scad-sketch--assert-session))) 0 t))
(defun scad-sketch-move-point-right ()        (interactive) (scad-sketch--move-point    (scad-sketch--grid   (scad-sketch--assert-session))  0 t))
(defun scad-sketch-move-point-up ()           (interactive) (scad-sketch--move-point 0  (scad-sketch--grid   (scad-sketch--assert-session))    t))
(defun scad-sketch-move-point-down ()         (interactive) (scad-sketch--move-point 0  (- (scad-sketch--grid   (scad-sketch--assert-session))) t))
(defun scad-sketch-move-point-fine-left ()    (interactive) (scad-sketch--move-point (- (scad-sketch--fine   (scad-sketch--assert-session))) 0))
(defun scad-sketch-move-point-fine-right ()   (interactive) (scad-sketch--move-point    (scad-sketch--fine   (scad-sketch--assert-session))  0))
(defun scad-sketch-move-point-fine-up ()      (interactive) (scad-sketch--move-point 0  (scad-sketch--fine   (scad-sketch--assert-session))))
(defun scad-sketch-move-point-fine-down ()    (interactive) (scad-sketch--move-point 0  (- (scad-sketch--fine   (scad-sketch--assert-session)))))
(defun scad-sketch-move-point-coarse-left ()  (interactive) (scad-sketch--move-point (- (scad-sketch--coarse (scad-sketch--assert-session))) 0 t))
(defun scad-sketch-move-point-coarse-right () (interactive) (scad-sketch--move-point    (scad-sketch--coarse (scad-sketch--assert-session))  0 t))
(defun scad-sketch-move-point-coarse-up ()    (interactive) (scad-sketch--move-point 0  (scad-sketch--coarse (scad-sketch--assert-session))    t))
(defun scad-sketch-move-point-coarse-down ()  (interactive) (scad-sketch--move-point 0  (- (scad-sketch--coarse (scad-sketch--assert-session))) t))
(defun scad-sketch-move-selected-left ()      (interactive) (scad-sketch--move-selected (- (scad-sketch--grid (scad-sketch--assert-session))) 0 t))
(defun scad-sketch-move-selected-right ()     (interactive) (scad-sketch--move-selected    (scad-sketch--grid (scad-sketch--assert-session))  0 t))
(defun scad-sketch-move-selected-up ()        (interactive) (scad-sketch--move-selected 0  (scad-sketch--grid (scad-sketch--assert-session))    t))
(defun scad-sketch-move-selected-down ()      (interactive) (scad-sketch--move-selected 0  (- (scad-sketch--grid (scad-sketch--assert-session))) t))

;;; Mark commands

(defun scad-sketch-set-mark ()
  "Replace all marks with just the current cursor point."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (setf (scad-sketch-session-marks s)
           (list (copy-sequence (scad-sketch-session-point s)))))))

(defun scad-sketch-push-mark ()
  "Push the current cursor point onto the marks list."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (push (copy-sequence (scad-sketch-session-point s))
           (scad-sketch-session-marks s)))))

(defun scad-sketch-pop-mark ()
  "Pop the most recent mark and jump cursor to it."
  (interactive)
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-marks session) (user-error "No marks set")))
  (scad-sketch--mutate
   (lambda (s)
     (setf (scad-sketch-session-point s)
           (copy-sequence (pop (scad-sketch-session-marks s)))))))

(defun scad-sketch-jump-to-mark ()
  "Move cursor to the most recent mark without consuming it."
  (interactive)
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-marks session) (user-error "No marks set")))
  (scad-sketch--mutate
   (lambda (s)
     (setf (scad-sketch-session-point s)
           (copy-sequence (car (scad-sketch-session-marks s)))))))

(defun scad-sketch-clear-marks ()
  "Clear all marks."
  (interactive)
  (scad-sketch--mutate
   (lambda (s) (setf (scad-sketch-session-marks s) nil))))

;;; Vertex editing

(defun scad-sketch--append-model-point (session point)
  "Append POINT to SESSION and select it."
  (setf (scad-sketch-session-points session)
        (append (scad-sketch-session-points session) (list point)))
  (setf (scad-sketch-session-selected-index session)
        (1- (length (scad-sketch-session-points session)))))

(defun scad-sketch-append-point ()
  "Append the cursor point to the array."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (scad-sketch--append-model-point
      s (scad-sketch--make-model-point (scad-sketch-session-point s) s)))))

(defun scad-sketch-insert-point-after-selected ()
  "Insert points after the selected vertex.
If marks are set, inserts one point per mark (oldest first) then the cursor
point.  With no marks, inserts only the cursor point.
The last inserted point becomes the new selection."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (let* ((idx       (or (scad-sketch-session-selected-index s) -1))
            (points    (scad-sketch-session-points s))
            (insert-at (min (1+ idx) (length points)))
            ;; marks list is newest-first; reverse to get oldest-first order
            (mark-pts  (mapcar (lambda (m) (scad-sketch--make-model-point m s))
                               (reverse (scad-sketch-session-marks s))))
            (cursor-pt (scad-sketch--make-model-point (scad-sketch-session-point s) s))
            (new-pts   (append mark-pts (list cursor-pt)))
            (new-idx   (+ insert-at (length new-pts) -1)))
       (setf (scad-sketch-session-points s)
             (append (cl-subseq points 0 insert-at)
                     new-pts
                     (nthcdr insert-at points)))
       (setf (scad-sketch-session-selected-index s) new-idx)))))

(defun scad-sketch-delete-selected ()
  "Delete the selected vertex."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (let ((idx    (or (scad-sketch-session-selected-index s)
                       (user-error "No selected point")))
           (points (scad-sketch-session-points s)))
       (unless (< idx (length points)) (user-error "Selected point out of range"))
       (setf (scad-sketch-session-points s)
             (append (cl-subseq points 0 idx) (nthcdr (1+ idx) points)))
       (setf (scad-sketch-session-selected-index s)
             (cond ((null (scad-sketch-session-points s)) nil)
                   ((>= idx (length (scad-sketch-session-points s)))
                    (1- (length (scad-sketch-session-points s))))
                   (t idx)))))))

(defun scad-sketch-line-from-mark ()
  "Append marks (oldest first) then cursor point as new vertices."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (unless (scad-sketch-session-marks s) (user-error "No marks set"))
     (dolist (m (reverse (scad-sketch-session-marks s)))
       (scad-sketch--append-model-point s (scad-sketch--make-model-point m s)))
     (scad-sketch--append-model-point
      s (scad-sketch--make-model-point (scad-sketch-session-point s) s)))))

(defun scad-sketch-rectangle-from-mark ()
  "Append a rectangle from the most recent mark to the cursor point."
  (interactive)
  (scad-sketch--mutate
   (lambda (s)
     (let ((mark (or (car (scad-sketch-session-marks s)) (user-error "No marks set")))
           (pt   (scad-sketch-session-point s)))
       (let ((x1 (nth 0 mark)) (y1 (nth 1 mark))
             (x2 (nth 0 pt))   (y2 (nth 1 pt)))
         (dolist (xy (list (list x1 y1) (list x2 y1) (list x2 y2) (list x1 y2)))
           (scad-sketch--append-model-point
            s (scad-sketch--make-model-point xy s))))))))

(defun scad-sketch-toggle-closed ()
  "Toggle the closed flag."
  (interactive)
  (scad-sketch--mutate
   (lambda (s) (setf (scad-sketch-session-closed s) (not (scad-sketch-session-closed s))))))

(defun scad-sketch-set-radius (radius)
  "Set the polyRound radius of the selected vertex."
  (interactive (list (read-number "Radius: " 0)))
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch--curve-kind-p session)
      (user-error "Radius is only meaningful for kind=2d-with-curves")))
  (scad-sketch--mutate
   (lambda (s)
     (let ((pt (or (scad-sketch--selected-point s) (user-error "No selected point"))))
       (scad-sketch--set-selected-point s (list (nth 0 pt) (nth 1 pt) (float radius)))))))

(defun scad-sketch-next-point ()
  "Select the next vertex, moving cursor to it."
  (interactive)
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-points session) (user-error "No points")))
  (scad-sketch--mutate
   (lambda (s)
     (let* ((n   (length (scad-sketch-session-points s)))
            (idx (mod (1+ (or (scad-sketch-session-selected-index s) -1)) n)))
       (setf (scad-sketch-session-selected-index s) idx)
       (setf (scad-sketch-session-point s)
             (scad-sketch--point-xy (nth idx (scad-sketch-session-points s))))))))

(defun scad-sketch-previous-point ()
  "Select the previous vertex, moving cursor to it."
  (interactive)
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-points session) (user-error "No points")))
  (scad-sketch--mutate
   (lambda (s)
     (let* ((n   (length (scad-sketch-session-points s)))
            (idx (mod (1- (or (scad-sketch-session-selected-index s) 0)) n)))
       (setf (scad-sketch-session-selected-index s) idx)
       (setf (scad-sketch-session-point s)
             (scad-sketch--point-xy (nth idx (scad-sketch-session-points s))))))))

;;; Coordinate commands

(defun scad-sketch--set-point-axis (axis value)
  "Set cursor coordinate AXIS (0=x 1=y) to VALUE."
  (scad-sketch--mutate
   (lambda (s)
     (let ((pt (copy-sequence (scad-sketch-session-point s))))
       (setf (nth axis pt) (float value))
       (setf (scad-sketch-session-point s) pt)))))

(defun scad-sketch-set-x (x)
  "Set cursor X."
  (interactive (list (read-number "X: " (nth 0 (scad-sketch-session-point (scad-sketch--assert-session))))))
  (scad-sketch--set-point-axis 0 x))

(defun scad-sketch-set-y (y)
  "Set cursor Y."
  (interactive (list (read-number "Y: " (nth 1 (scad-sketch-session-point (scad-sketch--assert-session))))))
  (scad-sketch--set-point-axis 1 y))

(defun scad-sketch--set-delta-axis (axis value)
  "Set cursor AXIS to (most recent mark AXIS) + VALUE."
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-marks session) (user-error "No marks set"))
    (scad-sketch--set-point-axis
     axis (+ (nth axis (car (scad-sketch-session-marks session))) (float value)))))

(defun scad-sketch-set-delta-x (dx)
  "Set cursor X to (most recent mark X) + DX."
  (interactive (list (read-number "ΔX from mark: " 0)))
  (scad-sketch--set-delta-axis 0 dx))

(defun scad-sketch-set-delta-y (dy)
  "Set cursor Y to (most recent mark Y) + DY."
  (interactive (list (read-number "ΔY from mark: " 0)))
  (scad-sketch--set-delta-axis 1 dy))

(defun scad-sketch-set-distance-from-mark (distance)
  "Set distance from the most recent mark to cursor, keeping the angle."
  (interactive (list (read-number "Distance from mark: " 0)))
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-marks session) (user-error "No marks set")))
  (scad-sketch--mutate
   (lambda (s)
     (let* ((m  (car (scad-sketch-session-marks s)))
            (p  (scad-sketch-session-point s))
            (angle (atan (- (nth 1 p) (nth 1 m)) (- (nth 0 p) (nth 0 m)))))
       (setf (scad-sketch-session-point s)
             (list (+ (nth 0 m) (* (float distance) (cos angle)))
                   (+ (nth 1 m) (* (float distance) (sin angle)))))))))

(defun scad-sketch-set-angle-from-mark (degrees)
  "Set angle from the most recent mark to cursor in DEGREES, keeping distance."
  (interactive (list (read-number "Angle degrees from mark: " 0)))
  (let ((session (scad-sketch--assert-session)))
    (unless (scad-sketch-session-marks session) (user-error "No marks set")))
  (scad-sketch--mutate
   (lambda (s)
     (let* ((m     (car (scad-sketch-session-marks s)))
            (p     (scad-sketch-session-point s))
            (dx    (- (nth 0 p) (nth 0 m)))
            (dy    (- (nth 1 p) (nth 1 m)))
            (dist  (sqrt (+ (* dx dx) (* dy dy))))
            (angle (* pi (/ (float degrees) 180.0))))
       (setf (scad-sketch-session-point s)
             (list (+ (nth 0 m) (* dist (cos angle)))
                   (+ (nth 1 m) (* dist (sin angle)))))))))

(defun scad-sketch-set-grid (grid)
  "Set the grid step."
  (interactive (list (read-number "Grid step: " (scad-sketch-session-grid (scad-sketch--assert-session)))))
  (scad-sketch--mutate
   (lambda (s) (setf (scad-sketch-session-grid s) (float grid)))))

;;; Undo command

(defun scad-sketch-undo ()
  "Undo the last sketch edit."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (entry   (pop (scad-sketch-session-undo-stack session))))
    (unless entry (user-error "No sketch undo available"))
    (setf (scad-sketch-session-points session)         (plist-get entry :points))
    (setf (scad-sketch-session-point session)          (plist-get entry :point))
    (setf (scad-sketch-session-marks session)          (plist-get entry :marks))
    (setf (scad-sketch-session-named-marks session)    (plist-get entry :named-marks))
    (setf (scad-sketch-session-selected-index session) (plist-get entry :selected-index))
    (setf (scad-sketch-session-closed session)         (plist-get entry :closed))
    (setf (scad-sketch-session-dirty session) t)
    (scad-sketch--render)))

;;; Rendering

(defun scad-sketch--bounds (session)
  "Return (min-x max-x min-y max-y) encompassing all points, marks, cursor."
  (let* ((pts   (mapcar #'scad-sketch--point-xy (scad-sketch-session-points session)))
         (extra (delq nil (cons (scad-sketch-session-point session)
                                (scad-sketch-session-marks session))))
         (all   (append pts extra)))
    (if (null all) (list -10 10 -10 10)
      (let ((min-x (apply #'min (mapcar #'car  all)))
            (max-x (apply #'max (mapcar #'car  all)))
            (min-y (apply #'min (mapcar #'cadr all)))
            (max-y (apply #'max (mapcar #'cadr all))))
        (when (= min-x max-x) (setq min-x (- min-x 10) max-x (+ max-x 10)))
        (when (= min-y max-y) (setq min-y (- min-y 10) max-y (+ max-y 10)))
        (let ((px (max 1 (* 0.15 (- max-x min-x))))
              (py (max 1 (* 0.15 (- max-y min-y)))))
          (list (- min-x px) (+ max-x px) (- min-y py) (+ max-y py)))))))

(defun scad-sketch--transform (bounds)
  "Return a pixel-coordinate closure for BOUNDS."
  (pcase-let ((`(,min-x ,max-x ,min-y ,max-y) bounds))
    (let* ((w scad-sketch-canvas-width) (h scad-sketch-canvas-height)
           (m scad-sketch-margin)
           (scale (min (/ (- w (* 2 m)) (- max-x min-x))
                       (/ (- h (* 2 m)) (- max-y min-y)))))
      (lambda (xy)
        (list (+ m (* (- (nth 0 xy) min-x) scale))
              (- h (+ m (* (- (nth 1 xy) min-y) scale))))))))

(defun scad-sketch--svg-line (svg transform a b &rest args)
  "Draw a model-space line A→B."
  (let ((pa (funcall transform a)) (pb (funcall transform b)))
    (apply #'svg-line svg (nth 0 pa) (nth 1 pa) (nth 0 pb) (nth 1 pb) args)))

(defun scad-sketch--draw-grid (svg bounds transform session)
  "Draw the grid."
  (pcase-let ((`(,min-x ,max-x ,min-y ,max-y) bounds))
    (let* ((grid (max 0.0001 (scad-sketch-session-grid session)))
           (x (* grid (floor (/ min-x grid))))
           (y (* grid (floor (/ min-y grid)))))
      (while (<= x (* grid (ceiling (/ max-x grid))))
        (scad-sketch--svg-line svg transform (list x min-y) (list x max-y)
                               :stroke "#e8e8e8" :stroke-width 1)
        (setq x (+ x grid)))
      (while (<= y (* grid (ceiling (/ max-y grid))))
        (scad-sketch--svg-line svg transform (list min-x y) (list max-x y)
                               :stroke "#e8e8e8" :stroke-width 1)
        (setq y (+ y grid))))
    (when (and (<= min-x 0) (<= 0 max-x))
      (scad-sketch--svg-line svg transform (list 0 min-y) (list 0 max-y)
                             :stroke "#d0d0d0" :stroke-width 2))
    (when (and (<= min-y 0) (<= 0 max-y))
      (scad-sketch--svg-line svg transform (list min-x 0) (list max-x 0)
                             :stroke "#d0d0d0" :stroke-width 2))))

(defun scad-sketch--corner-unit-vecs (A B C)
  "Return (U V HALF-ANGLE) for the corner at B, or nil if degenerate."
  (let* ((bx (nth 0 B)) (by (nth 1 B))
         (ba (list (- (nth 0 A) bx) (- (nth 1 A) by)))
         (bc (list (- (nth 0 C) bx) (- (nth 1 C) by)))
         (len-ba (sqrt (+ (* (nth 0 ba) (nth 0 ba)) (* (nth 1 ba) (nth 1 ba)))))
         (len-bc (sqrt (+ (* (nth 0 bc) (nth 0 bc)) (* (nth 1 bc) (nth 1 bc))))))
    (when (and (> len-ba 1e-10) (> len-bc 1e-10))
      (let* ((u    (list (/ (nth 0 ba) len-ba) (/ (nth 1 ba) len-ba)))
             (v    (list (/ (nth 0 bc) len-bc) (/ (nth 1 bc) len-bc)))
             (dot  (max -1.0 (min 1.0 (+ (* (nth 0 u) (nth 0 v))
                                          (* (nth 1 u) (nth 1 v))))))
             (half (/ (acos dot) 2)))
        (when (> (sin half) 1e-10)
          (list u v half))))))

(defun scad-sketch--corner-geometry-from-tlens (B u v half t1-len t2-len)
  "Build a corner plist from pre-clamped tangent lengths.
T1-LEN is the tangent along the incoming edge (toward prev point).
T2-LEN is the tangent along the outgoing edge (toward next point).
Uses the minimum to ensure a valid circular arc."
  (let* ((bx     (nth 0 B)) (by (nth 1 B))
         (t-len  (min t1-len t2-len))
         (actual-r (* t-len (tan half)))
         (t1    (list (+ bx (* t-len (nth 0 u))) (+ by (* t-len (nth 1 u)))))
         (t2    (list (+ bx (* t-len (nth 0 v))) (+ by (* t-len (nth 1 v)))))
         (cross (- (* (nth 0 u) (nth 1 v)) (* (nth 1 u) (nth 0 v))))
         (sweep (if (> cross 0) 1 0)))
    (list :t1 t1 :t2 t2 :radius actual-r :sweep sweep)))

(defun scad-sketch--corner-geometry (A B C r)
  "Compute polyRound arc geometry for corner at B with radius R.
Returns plist (:t1 :t2 :radius :sweep), or nil if degenerate.
Uses 0.49*edge-length clamping per side.  For path rendering use
`scad-sketch--polyround-path-d' which does proper edge-pair clamping."
  (when (and r (> r 0))
    (let ((uvh (scad-sketch--corner-unit-vecs A B C)))
      (when uvh
        (let* ((u    (nth 0 uvh)) (v (nth 1 uvh)) (half (nth 2 uvh))
               (bx   (nth 0 B))  (by (nth 1 B))
               (ba   (list (- (nth 0 A) bx) (- (nth 1 A) by)))
               (bc   (list (- (nth 0 C) bx) (- (nth 1 C) by)))
               (l-ba (sqrt (+ (* (nth 0 ba) (nth 0 ba)) (* (nth 1 ba) (nth 1 ba)))))
               (l-bc (sqrt (+ (* (nth 0 bc) (nth 0 bc)) (* (nth 1 bc) (nth 1 bc)))))
               (t-len (min (/ r (tan half)) (* l-ba 0.49) (* l-bc 0.49))))
          (scad-sketch--corner-geometry-from-tlens B u v half t-len t-len))))))
(defun scad-sketch--pixel-radius (model-r transform)
  "Convert model-space radius MODEL-R to screen pixels via TRANSFORM."
  (let* ((o  (funcall transform '(0 0)))
         (r  (funcall transform (list model-r 0)))
         (dx (- (nth 0 r) (nth 0 o)))
         (dy (- (nth 1 r) (nth 1 o))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun scad-sketch--edge-len (P Q)
  "Model-space distance between points P and Q."
  (let ((dx (- (nth 0 Q) (nth 0 P)))
        (dy (- (nth 1 Q) (nth 1 P))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun scad-sketch--polyround-path-d (points closed transform)
  "Build an SVG path data string for POINTS with polyRound radii.
POINTS are model-space [x y r] triples (r may be nil/0).
CLOSED determines whether to end with Z.
TRANSFORM converts model [x y] to screen [px py].
Returns nil if fewer than 2 points.

Uses edge-aware tangent-length clamping so that adjacent rounded corners
on a short edge never produce overlapping/reversed segments."
  (let ((n (length points)))
    (when (>= n 2)
      (let* (;; Step 1: compute ideal tangent lengths for each corner on each edge.
             ;; t-out[i] = ideal t_len from point[i] toward point[i+1]
             ;; t-in[i]  = ideal t_len from point[i] toward point[i-1]
             ;; These may overlap on short edges; we clamp in step 2.
             (t-out (make-vector n 0.0))
             (t-in  (make-vector n 0.0))
             (uvh-vec (make-vector n nil)))  ; cached unit vecs + half-angle
        ;; Compute unit vectors and ideal tangent lengths.
        (dotimes (i n)
          (let ((r (nth 2 (nth i points))))
            (when (and r (> r 0))
              (let* ((prev (cond ((> i 0)      (nth (1- i) points))
                                 (closed        (nth (1- n) points))))
                     (next (cond ((< i (1- n)) (nth (1+ i) points))
                                 (closed        (nth 0 points)))))
                (when (and prev next)
                  (let* ((A (scad-sketch--point-xy prev))
                         (B (scad-sketch--point-xy (nth i points)))
                         (C (scad-sketch--point-xy next))
                         (uvh (scad-sketch--corner-unit-vecs A B C)))
                    (when uvh
                      (aset uvh-vec i uvh)
                      (let ((t-ideal (/ r (tan (nth 2 uvh)))))
                        (aset t-in  i t-ideal)   ; toward prev
                        (aset t-out i t-ideal)))))))))  ; toward next
        ;; Step 2: for each edge, clamp t-out[i] and t-in[i+1] so they
        ;; don't sum to more than the edge length (leaving a small gap).
        (dotimes (i n)
          (let* ((j    (mod (1+ i) n))
                 (Pi   (scad-sketch--point-xy (nth i points)))
                 (Pj   (scad-sketch--point-xy (nth j points)))
                 (edge (scad-sketch--edge-len Pi Pj))
                 (sum  (+ (aref t-out i) (aref t-in j))))
            (when (and (or closed (< i (1- n)))  ; skip last edge of open path
                       (> sum (* edge 0.999)))
              (let* ((scale (/ (* edge 0.499) sum)))
                (aset t-out i (* (aref t-out i) scale))
                (aset t-in  j (* (aref t-in  j) scale))))))
        ;; Step 3: build corner geometry using clamped tangent lengths.
        (let ((corners (make-vector n nil)))
          (dotimes (i n)
            (let ((uvh (aref uvh-vec i)))
              (when uvh
                (aset corners i
                      (scad-sketch--corner-geometry-from-tlens
                       (scad-sketch--point-xy (nth i points))
                       (nth 0 uvh) (nth 1 uvh) (nth 2 uvh)
                       (aref t-in i) (aref t-out i))))))
          ;; Step 4: build SVG path string.
          (let* ((c0       (aref corners 0))
                 (start-xy (if (and c0 closed)
                               (funcall transform (plist-get c0 :t1))
                             (funcall transform (scad-sketch--point-xy (nth 0 points)))))
                 (fmt      (lambda (xy)
                             (format "%.3f %.3f"
                                     (float (nth 0 xy)) (float (nth 1 xy)))))
                 (parts    (list (format "M %s" (funcall fmt start-xy)))))
            (dotimes (i n)
              (let* ((corner (aref corners i))
                     (pt-s   (funcall transform (scad-sketch--point-xy (nth i points)))))
                (if corner
                    (let* ((t1s   (funcall transform (plist-get corner :t1)))
                           (t2s   (funcall transform (plist-get corner :t2)))
                           (rs    (scad-sketch--pixel-radius
                                   (plist-get corner :radius) transform))
                           (sweep (plist-get corner :sweep)))
                      (push (format "L %s" (funcall fmt t1s)) parts)
                      (push (format "A %.3f %.3f 0 0 %d %s"
                                    rs rs sweep (funcall fmt t2s))
                            parts))
                  (push (format "L %s" (funcall fmt pt-s)) parts))))
            (when closed (push "Z" parts))
            (mapconcat #'identity (nreverse parts) " ")))))))
(defun scad-sketch--draw-path (svg transform session)
  "Draw the polygon path (with arcs for polyRound radii) and vertex circles."
  (let* ((points  (scad-sketch-session-points session))
         (closed  (scad-sketch-session-closed session))
         (curve-p (scad-sketch--curve-kind-p session))
         (n       (length points))
         (idx     0))
    ;; Draw the path outline.
    (when (>= n 2)
      (if curve-p
          ;; polyRound: build a single SVG path with arcs.
          (let ((d (scad-sketch--polyround-path-d points closed transform)))
            (when d
              (svg-node svg 'path :d d
                        :stroke "#111111" :stroke-width 3
                        :fill "none")))
        ;; Plain 2d: straight lines.
        (let ((xy-points (mapcar #'scad-sketch--point-xy points)))
          (cl-loop for a on xy-points for b = (cadr a) when b do
                   (scad-sketch--svg-line svg transform (car a) b
                                          :stroke "#111111" :stroke-width 3))
          (when (and closed (> n 2))
            (scad-sketch--svg-line svg transform (car (last xy-points)) (car xy-points)
                                   :stroke "#111111" :stroke-width 3)))))
    ;; Draw vertex circles on top of the path.
    (let* ((n      (length points))
           (closed (scad-sketch-session-closed session)))
      (dolist (pt points)
        (let* ((xy     (scad-sketch--point-xy pt))
               (screen (funcall transform xy))
               (sel    (= idx (or (scad-sketch-session-selected-index session) -1)))
               (radius (scad-sketch--point-radius pt session)))
          (svg-circle svg (nth 0 screen) (nth 1 screen) (if sel 7 5)
                      :stroke (if sel "#d13f00" "#111111") :stroke-width (if sel 3 2)
                      :fill   (if sel "#fff0e8" "#ffffff"))
          (svg-text svg (number-to-string idx)
                    :x (+ (nth 0 screen) 8) :y (- (nth 1 screen) 8)
                    :font-size 12 :fill "#333333")
          (when (and radius (> radius 0))
            ;; Recompute corner geometry to get the actual clamped radius,
            ;; which is what polyRound will use (and what the arc shows).
            (let* ((prev    (cond ((> idx 0)      (nth (1- idx) points))
                                  (closed         (nth (1- n)   points))))
                   (next    (cond ((< idx (1- n)) (nth (1+ idx) points))
                                  (closed         (nth 0        points))))
                   (corner  (when (and prev next)
                              (scad-sketch--corner-geometry
                               (scad-sketch--point-xy prev)
                               xy
                               (scad-sketch--point-xy next)
                               radius)))
                   (actual-r (if corner (plist-get corner :radius) radius))
                   (capped   (and corner (< (+ actual-r 0.001) radius))))
              ;; Dashed circle at the actual (possibly clamped) radius.
              (svg-circle svg (nth 0 screen) (nth 1 screen)
                          (scad-sketch--pixel-radius actual-r transform)
                          :stroke (if capped "#c04000" "#804000")
                          :stroke-width 1
                          :stroke-dasharray "3,3" :fill "none")
              ;; Label showing actual radius; highlights if capped.
              (svg-text svg (if capped
                                (format "r=%s→%s"
                                        (scad-sketch--fmt-num radius)
                                        (scad-sketch--fmt-num actual-r))
                              (format "r=%s" (scad-sketch--fmt-num actual-r)))
                        :x (+ (nth 0 screen) 8) :y (+ (nth 1 screen) 18)
                        :font-size 11
                        :fill (if capped "#c04000" "#804000")))))
        (setq idx (1+ idx))))))

(defun scad-sketch--draw-point-and-marks (svg transform session)
  "Draw all marks and the cursor point."
  (let* ((marks  (scad-sketch-session-marks session))
         (cursor (scad-sketch-session-point session)))
    ;; Dashed lines: oldest→...→newest mark→cursor, forming a chain.
    ;; marks is newest-first, so reverse it to get oldest-first, then
    ;; walk consecutive pairs and finally connect newest mark to cursor.
    (let ((ordered (reverse marks)))  ; oldest first
      (cl-loop for a on ordered for b = (cadr a) when b do
               (scad-sketch--svg-line svg transform (car a) b
                                      :stroke "#008a2e" :stroke-width 1 :stroke-dasharray "4,4"))
      (when ordered
        (scad-sketch--svg-line svg transform (car (last ordered)) cursor
                               :stroke "#008a2e" :stroke-width 1 :stroke-dasharray "4,4")))
    ;; Marks: oldest drawn first so newest (car) is on top.
    (dolist (m (reverse marks))
      (let* ((screen  (funcall transform m))
             (current (equal m (car marks)))
             (color   (if current "#008a2e" "#50a870")))
        (svg-circle svg (nth 0 screen) (nth 1 screen) 6
                    :stroke color :stroke-width 2 :fill "#e2ffe9")
        (when current
          (svg-text svg "mark" :x (+ (nth 0 screen) 10) :y (+ (nth 1 screen) 4)
                    :font-size 12 :fill color))))
    ;; Cursor on top.
    (let ((p (funcall transform cursor)))
      (svg-circle svg (nth 0 p) (nth 1 p) 5
                  :stroke "#0057c2" :stroke-width 2 :fill "#dfefff")
      (svg-line svg (- (nth 0 p) 10) (nth 1 p) (+ (nth 0 p) 10) (nth 1 p)
                :stroke "#0057c2" :stroke-width 2)
      (svg-line svg (nth 0 p) (- (nth 1 p) 10) (nth 0 p) (+ (nth 1 p) 10)
                :stroke "#0057c2" :stroke-width 2)
      (svg-text svg "point" :x (+ (nth 0 p) 12) :y (+ (nth 1 p) 4)
                :font-size 12 :fill "#0057c2"))))

(defun scad-sketch--fmt-xy (xy)
  "Format XY pair."
  (format "(%s, %s)" (scad-sketch--fmt-num (nth 0 xy)) (scad-sketch--fmt-num (nth 1 xy))))

(defun scad-sketch--draw-hud (svg session)
  "Draw the status bar."
  (let* ((marks    (scad-sketch-session-marks session))
         (sel      (scad-sketch-session-selected-index session))
         (mark-str (cond ((null marks) "none")
                         ((= 1 (length marks)) (scad-sketch--fmt-xy (car marks)))
                         (t (format "%s (+%d)" (scad-sketch--fmt-xy (car marks))
                                    (1- (length marks))))))
         (text (format "%s  kind=%s  grid=%s%s  point=%s  mark=%s  sel=%s  %s"
                       (scad-sketch-session-name session)
                       (scad-sketch-session-kind session)
                       (scad-sketch--fmt-num (scad-sketch-session-grid session))
                       (scad-sketch-session-units session)
                       (scad-sketch--fmt-xy (scad-sketch-session-point session))
                       mark-str
                       (if sel (number-to-string sel) "none")
                       (if (scad-sketch-session-dirty session) "*dirty*" "saved"))))
    (svg-rectangle svg 0 0 scad-sketch-canvas-width 28 :fill "#f8f8f8")
    (svg-text svg text :x 10 :y 19 :font-size 13 :fill "#111111")))

(defun scad-sketch--render ()
  "Re-render the editor buffer."
  (let* ((session   (scad-sketch--assert-session))
         (svg       (svg-create scad-sketch-canvas-width scad-sketch-canvas-height))
         (bounds    (scad-sketch--bounds session))
         (transform (scad-sketch--transform bounds)))
    (svg-rectangle svg 0 0 scad-sketch-canvas-width scad-sketch-canvas-height :fill "#ffffff")
    (scad-sketch--draw-grid svg bounds transform session)
    (scad-sketch--draw-path svg transform session)
    (scad-sketch--draw-point-and-marks svg transform session)
    (scad-sketch--draw-hud svg session)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((beg (point)))
        (insert-image (svg-image svg :ascent 'center))
        (remove-text-properties beg (point) '(keymap nil)))
      (insert "\n\n")
      (insert (scad-sketch--emit-content session))
      (goto-char (point-min)))))



;;; Output

(defun scad-sketch--fmt-num (n)
  "Format N compactly."
  (let ((x (float n)))
    (if (< (abs (- x (round x))) 0.000001)
        (number-to-string (round x))
      (let ((s (format "%.4f" x)))
        (setq s (replace-regexp-in-string "0+\\'" "" s))
        (setq s (replace-regexp-in-string "\\.\\'" "" s))
        (if (or (string= s "-0") (string= s "")) "0" s)))))

(defun scad-sketch--emit-point (point session)
  "Format one model POINT for OpenSCAD."
  (if (scad-sketch--curve-kind-p session)
      (format "[%s, %s, %s]"
              (scad-sketch--fmt-num (nth 0 point))
              (scad-sketch--fmt-num (nth 1 point))
              (scad-sketch--fmt-num (nth 2 point)))
    (format "[%s, %s]"
            (scad-sketch--fmt-num (nth 0 point))
            (scad-sketch--fmt-num (nth 1 point)))))

(defun scad-sketch--emit-content (session)
  "Emit the full SCAD array assignment."
  (let* ((name  (scad-sketch-session-name session))
         (lines (mapcar (lambda (p) (concat "  " (scad-sketch--emit-point p session)))
                        (scad-sketch-session-points session))))
    (concat name " = [\n"
            (mapconcat #'identity lines ",\n")
            (if lines "\n" "")
            "];\n")))

;;; Write-back / quit

(defun scad-sketch-write-back ()
  "Write the edited array back to the source buffer."
  (interactive)
  (let* ((session (scad-sketch--assert-session))
         (source  (scad-sketch-session-source-buffer session))
         (beg     (scad-sketch-session-content-beg session))
         (end     (scad-sketch-session-content-end session))
         (content (scad-sketch--emit-content session)))
    (unless (buffer-live-p source) (user-error "Source buffer is gone"))
    (with-current-buffer source
      (save-excursion
        (goto-char beg)
        (delete-region beg end)
        (insert content)
        (set-marker end (point))))
    (setf (scad-sketch-session-dirty session) nil)
    (scad-sketch--render)
    (message "Wrote scad-sketch `%s' back to %s"
             (scad-sketch-session-name session) (buffer-name source))))

(defun scad-sketch-quit ()
  "Quit the sketch editor."
  (interactive)
  (let ((session (scad-sketch--assert-session)))
    (when (and (scad-sketch-session-dirty session)
               (y-or-n-p "Sketch has unwritten edits. Write back first? "))
      (scad-sketch-write-back)))
  (kill-buffer (current-buffer)))

(defun scad-sketch-help ()
  "Display a key binding summary in the echo area.
For full documentation use \\[describe-mode]."
  (interactive)
  (scad-sketch--assert-session)
  (message (concat "arrows=move  C-arrows=coarse  M-arrows=fine(off-grid)  S-arrows=move-vertex | "
                   "TAB/S-TAB=select  p=append  i=insert  k=delete | "
                   "m=set-mark  M=push  `=pop  '=jump  C=clear | "
                   "R=radius  c=closed  l=line  r=rect | "
                   "x/y=coord  X/Y=delta  d=dist  a=angle  g=grid | "
                   "w=write  u=undo  q=quit  C-h m=full help")))

;;; Adopting existing arrays

(defun scad-sketch--inside-existing-block-p (&optional pos)
  "Return non-nil if POS is inside a scad-sketch block."
  (let ((origin (or pos (point))))
    (save-excursion
      (goto-char origin)
      (when (re-search-backward scad-sketch-metadata-regexp nil t)
        (let ((beg (line-beginning-position)))
          (goto-char (line-end-position))
          (and (re-search-forward scad-sketch-end-regexp nil t)
               (<= beg origin) (<= origin (match-end 0))))))))

(defun scad-sketch--metadata-for-adopted-array (name kind)
  "Build a metadata comment for adopted NAME/KIND."
  (let* ((closed (if (y-or-n-p "Mark sketch as closed? ") "true" "false"))
         (grid   (read-number "Grid step: " scad-sketch-default-grid))
         (units  (read-string "Units: " "mm")))
    (format "// scad-sketch: name=%s kind=%s closed=%s grid=%s units=%s"
            name kind closed (scad-sketch--fmt-num grid) units)))

;;;###autoload
(defun scad-sketch-adopt-array-at-point ()
  "Wrap the array at point in scad-sketch comments.
Kind is inferred: 2-column -> 2d, 3-column -> 2d-with-curves."
  (interactive)
  (when (scad-sketch--inside-existing-block-p)
    (user-error "Already inside a scad-sketch block"))
  (let* ((assignment (scad-sketch--find-array-assignment-at-point))
         (name       (plist-get assignment :name))
         (kind       (scad-sketch--infer-kind (plist-get assignment :text)))
         (metadata   (scad-sketch--metadata-for-adopted-array name kind))
         (beg        (plist-get assignment :beg))
         (end        (copy-marker (plist-get assignment :end) t))
         (origin     (copy-marker (point) t)))
    (save-excursion
      (goto-char end) (end-of-line)
      (insert "\n// end-scad-sketch")
      (goto-char beg) (beginning-of-line)
      (insert metadata "\n"))
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
  "Insert a new empty 2D-with-curves scad-sketch block named NAME."
  (interactive "sSketch name: ")
  (insert (format "// scad-sketch: name=%s kind=2d-with-curves closed=true grid=1 units=mm\n%s = [\n];\n// end-scad-sketch\n"
                  name name)))

(provide 'scad-sketch)
;;; scad-sketch.el ends here
