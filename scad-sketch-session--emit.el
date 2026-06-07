;;; scad-sketch-session--emit.el --- Source emission helpers for scad-sketch -*- lexical-binding: t; -*-

;;; Commentary:

;; Source emission/formatting for scad-sketch sessions.
;;
;; This module owns:
;;
;;   - canonical number / point / point-array formatting
;;   - Lisp-style point-array indentation
;;   - shape emission
;;   - tree emission
;;   - target indentation helpers
;;   - replacement normalization
;;
;; It deliberately does not own parsing, target discovery, or source-buffer
;; mutation.  Those remain in `scad-sketch-session.el'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'scad-sketch-parse)

(declare-function scad-sketch-session--shape-source-name "scad-sketch-session")
(declare-function scad-sketch-session--shape-points-var-name "scad-sketch-session")
(declare-function scad-sketch-session--points-have-radii-p "scad-sketch-session")

;;; Basic formatting

(defun scad-sketch-session--fmt-num (n)
  "Format number N compactly for OpenSCAD output."
  (let ((x (float n)))
    (if (< (abs (- x (round x))) 0.000001)
        (number-to-string (round x))
      (let ((s (format "%.6f" x)))
        (setq s (replace-regexp-in-string "0+\\'" "" s))
        (replace-regexp-in-string "\\.\\'" "" s)))))

(defun scad-sketch-session--fmt-point (point &optional force-r)
  "Format POINT as [x, y] or [x, y, r].

When FORCE-R is non-nil, always emit the radius component."
  (let ((x (nth 0 point))
        (y (nth 1 point))
        (r (or (nth 2 point) 0.0)))
    (if force-r
        (format "[%s, %s, %s]"
                (scad-sketch-session--fmt-num x)
                (scad-sketch-session--fmt-num y)
                (scad-sketch-session--fmt-num r))
      (format "[%s, %s]"
              (scad-sketch-session--fmt-num x)
              (scad-sketch-session--fmt-num y)))))

(defun scad-sketch-session--points-force-r-p (points &optional force-r)
  "Return non-nil if POINTS should be emitted as [x, y, r]."
  (or force-r
      (cl-some (lambda (p)
                 (and (nth 2 p)
                      (> (float (nth 2 p)) 0.0)))
               points)))

(defun scad-sketch-session--fmt-points-array-lisp (points prefix
                                                        &optional force-r suffix)
  "Format POINTS as a Lisp-style OpenSCAD array.

PREFIX is emitted before the opening array bracket.  SUFFIX is emitted after the
closing array bracket.

Example:

  prefix = \"pts = \"

emits:

  pts = [[0, 0],
         [10, 0]];

Example:

  prefix = \"polygon(\"

emits:

  polygon([[0, 0],
           [10, 0]]);"
  (let* ((suffix  (or suffix ""))
         (force-r (scad-sketch-session--points-force-r-p points force-r))
         (align   (make-string (+ (length prefix) 1) ?\s)))
    (cond
     ((null points)
      (concat prefix "[]" suffix))

     ((null (cdr points))
      (concat prefix
              "["
              (scad-sketch-session--fmt-point (car points) force-r)
              "]"
              suffix))

     (t
      (concat
       prefix
       "["
       (scad-sketch-session--fmt-point (car points) force-r)
       (mapconcat
        (lambda (p)
          (concat ",\n" align
                  (scad-sketch-session--fmt-point p force-r)))
        (cdr points)
        "")
       "]"
       suffix)))))

(defun scad-sketch-session--fmt-inline-array (points &optional force-r)
  "Format POINTS as a canonical multiline inline array.

Despite the historical name, this now means \"inline in the emitted source\",
not \"single-line\"."
  (scad-sketch-session--fmt-points-array-lisp points "" force-r ""))

(defun scad-sketch-session--normalize-replacement (source)
  "Normalize emitted replacement SOURCE for buffer insertion.

Replacement strings should not carry trailing newlines.  The target region owns
its surrounding whitespace; adding a final newline here causes accidental blank
lines on write-back."
  (replace-regexp-in-string "[\n\t ]+\\'" "" (or source "")))

(defun scad-sketch-session--marker-indent (marker)
  "Return leading whitespace indentation at MARKER's line."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (beginning-of-line)
      (if (looking-at "[ \t]*")
          (match-string 0)
        ""))))

(defun scad-sketch-session--target-indent (target)
  "Return source indentation for TARGET."
  (scad-sketch-session--marker-indent
   (scad-sketch-target-beg-marker target)))

;;; Polygon polyRound policy

(defun scad-sketch-session--effective-polyround-fn (shape)
  "Return effective polyRound fn value for polygon SHAPE.

Explicit polyRound values are preserved.  If no explicit value exists but the
polygon has positive point radii, use `scad-sketch-default-polyround-fn'."
  (let ((explicit (scad-sketch-shape-polyround shape)))
    (or explicit
        (and (eq (scad-sketch-shape-kind shape) 'polygon)
             (scad-sketch-session--points-have-radii-p
              (scad-sketch-shape-points shape))
             scad-sketch-default-polyround-fn))))

;;; Shape emission
(defun scad-sketch-session--emit-points-assignment
    (name points indent &optional force-r)
  "Emit NAME = POINTS assignment at INDENT."
  (scad-sketch-session--fmt-points-array-lisp
   points
   (format "%s%s = " indent name)
   force-r
   ";"))

(defun scad-sketch-session--emit-polygon-call
    (points indent &optional polyround source-name)
  "Emit a polygon call for POINTS at INDENT.

When SOURCE-NAME is non-nil, call polygon or polyRound through that name.
When POLYROUND is non-nil, emit polygon(polyRound(..., POLYROUND))."
  (cond
   ((and source-name polyround)
    (format "%spolygon(polyRound(%s, %s));"
            indent
            source-name
            (scad-sketch-session--fmt-num polyround)))

   (source-name
    (format "%spolygon(%s);" indent source-name))

   (polyround
    (scad-sketch-session--fmt-points-array-lisp
     points
     (concat indent "polygon(polyRound(")
     t
     (format ", %s));"
             (scad-sketch-session--fmt-num polyround))))

   (t
    (scad-sketch-session--fmt-points-array-lisp
     points
     (concat indent "polygon(")
     nil
     ");"))))

(defun scad-sketch-session--emit-circle-shape (shape indent)
  "Emit circle SHAPE at INDENT."
  (let* ((md (scad-sketch-shape-metadata shape))
         (cx (float (or (plist-get md :cx) 0.0)))
         (cy (float (or (plist-get md :cy) 0.0)))
         (r  (float (or (plist-get md :r)  1.0)))
         (call (format "circle(r=%s);"
                       (scad-sketch-session--fmt-num r))))
    (if (and (< (abs cx) 0.000001)
             (< (abs cy) 0.000001))
        (concat indent call)
      (format "%stranslate([%s, %s])\n%s  %s"
              indent
              (scad-sketch-session--fmt-num cx)
              (scad-sketch-session--fmt-num cy)
              indent
              call))))

(defun scad-sketch-session--emit-square-shape (shape indent)
  "Emit square SHAPE at INDENT."
  (let* ((md       (scad-sketch-shape-metadata shape))
         (x        (float (or (plist-get md :x) 0.0)))
         (y        (float (or (plist-get md :y) 0.0)))
         (w        (float (or (plist-get md :w) 1.0)))
         (h        (float (or (plist-get md :h) 1.0)))
         (angle    (float (or (plist-get md :angle) 0.0)))
         (centered (and (< (abs (+ x (/ w 2.0))) 0.000001)
                        (< (abs (+ y (/ h 2.0))) 0.000001)))
         (base     (format "square([%s, %s]%s);"
                           (scad-sketch-session--fmt-num w)
                           (scad-sketch-session--fmt-num h)
                           (if centered ", center=true" ""))))
    (cond
     ((> (abs angle) 0.000001)
      (format "%srotate(%s)\n%s  %s"
              indent
              (scad-sketch-session--fmt-num angle)
              indent
              base))

     ((or (> (abs x) 0.000001)
          (> (abs y) 0.000001))
      (format "%stranslate([%s, %s])\n%s  %s"
              indent
              (scad-sketch-session--fmt-num x)
              (scad-sketch-session--fmt-num y)
              indent
              base))

     (t
      (concat indent base)))))

(defun scad-sketch-session--emit-text-shape (shape indent)
  "Emit text SHAPE at INDENT."
  (let* ((md    (scad-sketch-shape-metadata shape))
         (str   (or (plist-get md :str) ""))
         (x     (float (or (plist-get md :x) 0.0)))
         (y     (float (or (plist-get md :y) 0.0)))
         (size  (float (or (plist-get md :size) 10.0)))
         (font  (plist-get md :font))
         (angle (float (or (plist-get md :angle) 0.0)))
         (call  (format "text(%S, size=%s%s);"
                        str
                        (scad-sketch-session--fmt-num size)
                        (if font
                            (format ", font=%S" font)
                          ""))))
    (cond
     ((> (abs angle) 0.000001)
      (format "%srotate(%s)\n%s  %s"
              indent
              (scad-sketch-session--fmt-num angle)
              indent
              call))

     ((or (> (abs x) 0.000001)
          (> (abs y) 0.000001))
      (format "%stranslate([%s, %s])\n%s  %s"
              indent
              (scad-sketch-session--fmt-num x)
              (scad-sketch-session--fmt-num y)
              indent
              call))

     (t
      (concat indent call)))))

(defun scad-sketch-session--join-assignment-and-call (assignments call)
  "Join ASSIGNMENTS and CALL without trailing whitespace."
  (if (string-empty-p assignments)
      call
    (concat assignments "\n" call)))

(defun scad-sketch-session--emit-shape-with-assignments (session shape indent)
  "Return (:assignments STR :call STR) for SHAPE in SESSION."
  (pcase (scad-sketch-shape-kind shape)
    ('circle
     (list :assignments ""
           :call (scad-sketch-session--emit-circle-shape shape indent)))

    ('square
     (list :assignments ""
           :call (scad-sketch-session--emit-square-shape shape indent)))

    ('text
     (list :assignments ""
           :call (scad-sketch-session--emit-text-shape shape indent)))

    ('polygon
     (let* ((points      (scad-sketch-shape-points shape))
            (polyround   (scad-sketch-session--effective-polyround-fn shape))
            (local-name  (scad-sketch-session--shape-points-var-name shape))
            (source-name (or local-name
                             (scad-sketch-session--shape-source-name
                              session shape)))
            (force-r     (or polyround
                             (scad-sketch-session--points-have-radii-p
                              points)))
            (assignments (if local-name
                             (scad-sketch-session--emit-points-assignment
                              local-name points indent force-r)
                           "")))
       (list :assignments assignments
             :call (scad-sketch-session--emit-polygon-call
                    points indent polyround source-name))))

    (_
     (list :assignments "" :call ""))))

(defun scad-sketch-session--emit-shape-source (session shape indent)
  "Emit SHAPE source at INDENT, including local assignments if needed."
  (let* ((parts       (scad-sketch-session--emit-shape-with-assignments
                      session shape indent))
         (assignments (plist-get parts :assignments))
         (call        (plist-get parts :call)))
    (scad-sketch-session--join-assignment-and-call assignments call)))

;;; Tree emission

(defun scad-sketch-session--emit-tree (session tree indent)
  "Emit SESSION TREE at INDENT."
  (pcase (and tree (plist-get tree :kind))
    ('shape
     (let ((shape (scad-sketch-session-shape-by-id
                   session
                   (plist-get tree :shape-id))))
       (if shape
           (scad-sketch-session--emit-shape-source session shape indent)
         "")))

    ('sequence
     (mapconcat (lambda (child)
                  (scad-sketch-session--emit-tree session child indent))
                (plist-get tree :children)
                "\n"))

    ('boolean
     (let* ((op       (plist-get tree :op))
            (children (plist-get tree :children))
            (child-ind (concat indent "  ")))
       (concat
        indent
        (symbol-name op)
        "() {\n"
        (mapconcat (lambda (child)
                     (scad-sketch-session--emit-tree
                      session child child-ind))
                   children
                   "\n")
        "\n"
        indent
        "}")))

    ('mirror
     (let ((mx (or (plist-get tree :mx) 1.0))
           (my (or (plist-get tree :my) 0.0)))
       (concat
        indent
        (format "mirror([%s, %s])"
                (scad-sketch-session--fmt-num mx)
                (scad-sketch-session--fmt-num my))
        "\n"
        (scad-sketch-session--emit-tree
         session
         (plist-get tree :child)
         (concat indent "  ")))))

    (_ "")))

(provide 'scad-sketch-session--emit)
;;; scad-sketch-session--emit.el ends here
