((emacs-lisp-mode
  (indent-tabs-mode . nil)
  (checkdoc-allow-quoting-nil-and-t . t))
 (git-commit-mode
  (git-commit-major-mode . git-commit-elisp-text-mode))
 (makefile-gmake-mode
  (outline-regexp . "#\\(#+\\)")
  (mode . outline-minor))
 ("docs/RelNotes"
  (org-mode
   (fill-column . 80)
   (mode . display-fill-column-indicator)))
 )
