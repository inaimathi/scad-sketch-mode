#!/usr/bin/env bash
set -euo pipefail

emacs --batch -Q \
  --eval '(progn
            (message "Emacs: %s" emacs-version)
            (message "Load path root: %s" default-directory)
            (dolist (file (directory-files "tests" t "-test\\.el\\'"))
              (message "Loading test file: %s" file)
              (load-file file))
            (ert-run-tests-batch-and-exit))'
