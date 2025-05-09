((nil
  (indent-tabs-mode . nil))
 (makefile-mode
  (indent-tabs-mode . t)
  (outline-regexp . "#\\(#+\\)")
  (mode . outline-minor))
 (emacs-lisp-mode
  (checkdoc-allow-quoting-nil-and-t . t))
 (git-commit-mode
  (git-commit-major-mode . git-commit-elisp-text-mode))
 (".github/PULL_REQUEST_TEMPLATE"
  (nil (truncate-lines . nil)))
 ("CHANGELOG"
  (nil (fill-column . 70)
       (mode . display-fill-column-indicator))))
