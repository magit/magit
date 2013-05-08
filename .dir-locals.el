;;; Directory Local Variables
;;; See Info node `(emacs) Directory Variables' for more information.

((emacs-lisp-mode
  (indent-tabs-mode)
  (tab-width . 8)
  (eval . (require 'magit))
  (eval . (magit-maybe-add-font-lock-keywords))))
