;;; git-commit-global.el --- Support for autoloading git-commit.el -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2021  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Authors: Jonas Bernoulli <jonas@bernoul.li>
;;	Sebastian Wiesner <lunaryorn@gmail.com>
;;	Florian Ragwitz <rafl@debian.org>
;;	Marius Vollmer <marius.vollmer@gmail.com>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Implementation of `global-git-commit-mode'. This library has no
;; dependencies, which makes it suitable for use during Emacs
;; initialization when start times matter. Just put
;;
;;    (global-git-commit-mode 1)
;;
;; to the initialization file and git-commit.el will autoload later,
;; when it is actually needed.

;;; Code:

;;;###autoload
(define-minor-mode global-git-commit-mode
  "Edit Git commit messages.

This global mode arranges for `git-commit-setup' to be called
when a Git commit message file is opened.  That usually happens
when Git uses the Emacsclient as $GIT_EDITOR to have the user
provide such a commit message.

Loading of full `git-commit' library is deferred until it is
actually needed. This helps to reduce Emacs start times."
  :group 'git-commit
  :type 'boolean
  :global t
  :init-value t
  :initialize (lambda (symbol exp)
                (custom-initialize-default symbol exp)
                (when global-git-commit-mode
                  (add-hook 'find-file-hook 'git-commit-setup-check-buffer)))
  (if global-git-commit-mode
      (add-hook  'find-file-hook 'git-commit-setup-check-buffer)
    (remove-hook 'find-file-hook 'git-commit-setup-check-buffer)))

;;;###autoload
(defconst git-commit-filename-regexp "/\\(\
\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\
\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'")

(declare-function git-commit-setup "git-commit" ())

(defun git-commit-setup-check-buffer ()
  (and buffer-file-name
       (string-match-p git-commit-filename-regexp buffer-file-name)
       (git-commit-setup)))

(provide 'git-commit-global)
;;; git-commit-global.el ends here
