#!/usr/bin/env bash
set -euo pipefail

export LC_ALL="${LC_ALL:-C.UTF-8}"
export LANG="${LANG:-C.UTF-8}"

files=()
while IFS= read -r -d '' file; do
  files+=("$file")
done < <(find . -maxdepth 1 -type f -name '*.el' -print0 | sort -z)

pkgdir="$(mktemp -d)"
trap 'rm -rf "$pkgdir"' EXIT

echo "== Source files =="
printf '  %s\n' "${files[@]}"

echo
echo "== Installing lint dependencies =="
emacs --batch -Q \
  --eval "(progn
            (require 'package)
            (setq package-user-dir \"$pkgdir\")
            (setq package-archives
                  '((\"gnu\" . \"https://elpa.gnu.org/packages/\")
                    (\"nongnu\" . \"https://elpa.nongnu.org/nongnu/\")
                    (\"melpa\" . \"https://melpa.org/packages/\")))
            (package-initialize)
            (package-refresh-contents)
            (unless (package-installed-p 'package-lint)
              (package-install 'package-lint)))"

echo
echo "== Byte-compiling all source files =="
emacs --batch -Q \
  --eval "(setq package-user-dir \"$pkgdir\")" \
  --eval "(require 'package)" \
  --eval "(package-initialize)" \
  -L . \
  -f batch-byte-compile \
  "${files[@]}"

echo
echo "== package-lint package entrypoint =="
emacs --batch -Q \
  --eval "(setq package-user-dir \"$pkgdir\")" \
  --eval "(require 'package)" \
  --eval "(package-initialize)" \
  -l package-lint \
  -f package-lint-batch-and-exit \
  scad-sketch.el

echo
echo "== checkdoc package entrypoint =="
emacs --batch -Q \
  --eval "(require 'checkdoc)" \
  --eval "(let ((files (remove \"--\" command-line-args-left))
                (failed nil))
            (dolist (file files)
              (message \"checkdoc: %s\" file)
              (condition-case err
                  (checkdoc-file file)
                (error
                 (setq failed t)
                 (message \"checkdoc failed for %s: %S\" file err))))
            (when failed
              (kill-emacs 1)))" \
  -- \
  scad-sketch.el

echo
echo "== checkdoc all files, advisory only =="
emacs --batch -Q \
  --eval "(require 'checkdoc)" \
  --eval "(let ((files (remove \"--\" command-line-args-left)))
            (dolist (file files)
              (message \"checkdoc: %s\" file)
              (condition-case err
                  (checkdoc-file file)
                (error
                 (message \"checkdoc warning/error for %s: %S\" file err)))))" \
  -- \
  "${files[@]}" || true

echo
echo "Lint checks completed."
