;;; magit-grep.el --- git grep support for magit

;; Copyright (C) 2012 Pekka Pessi

;; Author: Pekka Pessi <nospam@pessi.fi>
;; Keywords: magit, git search

;; This file is part of Magit.

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Most of the code has been lifted from standard grep.el.

;;; Code:

(require 'magit)
(require 'grep)

;;;###autoload
(defcustom magit-grep-command "git grep -nH -E -e "
  "The default git grep command for \\[magit-grep]."
  :type 'string
  :group 'magit)

(defvar magit-grep-history nil "History list for magit-grep.")
(defvar magit-grep-string-history nil "History list for magit-grep-string.")

(defun magit-grep-setup-function ()
  (setenv "GIT_PAGER" "cat"))

(add-hook 'grep-setup-hook 'magit-grep-setup-function)

;;;###autoload
(defun magit-grep (command-args &optional dir)
  "Run git grep with user-specified args COMMAND-ARGS in DIR.

With \\[universal-argument] prefix, the git grep command is executed at git
toplevel directory.

Collect output in a buffer.  While git grep runs asynchronously,
can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error] \
in the git grep output buffer,
to go to the lines where git grep found matches.

This command uses a special history list for its arguments, so you can
easily repeat a git grep command."
  (interactive
   (progn
     (list (read-shell-command
            (format "Run git grep %s(like this): "
                    (if current-prefix-arg "in whole git tree " ""))
            (or magit-grep-command "git grep -nH -e ")
            'magit-grep-history)
           (if current-prefix-arg
               (magit-toplevel)
               default-directory))))
  (when command-args
    (let ((null-device nil)		; see grep
          (default-directory (or dir default-directory)))
      ; use grep for benefit of grep-a-lot
      (grep command-args))))

;;;###autoload
(defun magit-grep-string (fixed-string &optional dir)
  "Run git grep for FIXED-STRING in DIR.

With \\[universal-argument] prefix, the git grep command is executed at git
toplevel directory.

Collect output in a buffer.  While git grep runs asynchronously,
can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error] \
in the git grep output buffer,
to go to the lines where git grep found matches."
  (interactive
   (progn
     (list (magit-grep-read-string current-prefix-arg)
           (if current-prefix-arg
               (magit-toplevel)
               default-directory))))
  (when fixed-string
    (magit-grep (format "git grep -nH -F -e '%s'" fixed-string) dir)))

(defun magit-grep-read-string (whole-tree)
  "Read string arg for interactive grep."
  (let ((default (grep-tag-default)))
    (read-string
     (format "Search for%s%s: "
	     (if (and default (> (length default) 0))
		 (format " (default \"%s\")" default) "")
	     (if whole-tree " in whole git tree" ""))
     nil 'magit-grep-string-history default)))

(provide 'magit-grep)
;;; magit-grep.el ends here
