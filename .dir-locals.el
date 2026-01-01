((nil
  (indent-tabs-mode . nil))
 (emacs-lisp-mode
  (checkdoc-allow-quoting-nil-and-t . t)
  (lisp-indent-local-overrides . ((cond . 0) (interactive . 0))))
 (makefile-mode
  (indent-tabs-mode . t)
  (mode . outline-minor)
  (outline-regexp . "#\\(#+\\)"))
 (git-commit-mode
  (git-commit-major-mode . git-commit-elisp-text-mode))
 (".github/PULL_REQUEST_TEMPLATE"
  (nil (truncate-lines . nil)))
 ("CHANGELOG"
  (nil (fill-column . 70)
       (mode . display-fill-column-indicator))))
