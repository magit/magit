;; Usage:
;;
;;   emacs -Q -l tests/run-test.el           # interactive mode
;;   emacs -batch -Q -l tests/run-test.el    # batch mode

;; Setup `load-path'
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (root-dir (file-name-directory (directory-file-name test-dir)))
       (sitelisp (file-name-directory (directory-file-name root-dir))))
  (add-to-list 'load-path test-dir)
  (add-to-list 'load-path root-dir)
  (mapc (lambda (p)
          (when (file-directory-p p)
            (add-to-list 'load-path p)))
        (list (expand-file-name "mocker" sitelisp)
              (expand-file-name "cl-lib" sitelisp)
              (expand-file-name (convert-standard-filename "lib/mocker") root-dir)
              (expand-file-name (convert-standard-filename "lib/cl-lib") root-dir)))

  ;; Use `ert' from github when this Emacs does not have it
  (unless (locate-library "ert")
    (add-to-list 'load-path
                 (expand-file-name
                  (convert-standard-filename "lib/ert/lisp/emacs-lisp")
                  root-dir))
    (require 'ert-batch)
    (require 'ert-ui)))

;; Load tests
(load "magit-tests")

;; Run tests
(if noninteractive
    (ert-run-tests-batch-and-exit)
  (ert t))
