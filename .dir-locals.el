((nil
  (bug-reference-bug-regexp . "#\\(?2:[0-9]+\\)")
  (bug-reference-url-format . "https://github.com/magit/magit/issues/%s"))
 (emacs-lisp-mode
  (eval . (bug-reference-mode 1))
  (indent-tabs-mode . nil))
 (makefile-gmake-mode
  (outline-regexp . "##"))
 (git-commit-mode
  (git-commit-major-mode . git-commit-elisp-text-mode)))
