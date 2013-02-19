;; Usage:
;;
;;   emacs -Q -l tests/run-test.el           # interactive mode
;;   emacs -batch -Q -l tests/run-test.el    # batch mode


;; Utils
(defun magit-test-join-path (p &rest ps)
  "Join a list of PATHS with appropriate separator (such as /).

\(fn &rest paths)"
  (if ps
      (concat (file-name-as-directory p) (apply 'magit-test-join-path ps))
    p))

(defvar magit-test-dir (file-name-directory load-file-name))
(defvar magit-root-dir (concat magit-test-dir ".."))


;; Setup `load-path'
(mapc (lambda (p) (add-to-list 'load-path p))
      (list magit-test-dir
            magit-root-dir
            (magit-test-join-path magit-root-dir "lib" "mocker")))


;; Use ERT from github when this Emacs does not have it
(unless (locate-library "ert")
  (add-to-list
   'load-path
   (magit-test-join-path magit-root-dir "lib" "ert" "lisp" "emacs-lisp"))
  (require 'ert-batch)
  (require 'ert-ui))


;; Load tests
(load "magit-tests")


;; Run tests
(if noninteractive
    (ert-run-tests-batch-and-exit)
  (ert t))
