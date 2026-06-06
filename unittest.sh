emacs --batch -Q \
  --eval "(progn
            (dolist (file (directory-files \"tests\" t \"-test\\\\.el\\\\'\"))
              (load-file file))
            (ert-run-tests-batch-and-exit))"
