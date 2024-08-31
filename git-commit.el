;;; git-commit.el --- This package has been merged into magit  -*- lexical-binding:t; -*-

;; Copyright (C) 2008-2024 The Magit Project Contributors

;; Author: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>
;;     Sebastian Wiesner <lunaryorn@gmail.com>
;;     Florian Ragwitz <rafl@debian.org>
;;     Marius Vollmer <marius.vollmer@gmail.com>
;; Maintainer: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>
;; Homepage: https://github.com/magit/magit
;; Keywords: git tools vc
;; Package-Version: 4.1.0
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package has been merged into `magit' itself.
;;
;; The "git-commit.el" library is no longer distributed as a separate
;; package.  It is now distributed as part of the `magit' package.
;;
;; The `git-commit' package now does nothing but display a warning.
;; If it is located earlier on the `load-path' than `magit' is, then that
;; prevents the proper "git-commit.el" library from `magit' from being
;; loaded.
;;
;; Do not install the `git-commit' package and if it is still installed,
;; then please uninstalling it and restart Emacs.

;;; Code:

(display-warning 'emacsql "Uninstall the `git-commit' package.

The \"git-commit.el\" library is no longer distributed as a separate
package.  It is now distributed as part of the `magit' package.

The `git-commit' package now does nothing but display this warning.
If it is located earlier on the `load-path' than `magit' is, then that
prevents the proper \"git-commit.el\" libraries from `magit' from being
loaded.

Please uninstalling `git-commit' and then restart Emacs.
" :emergency)

(provide 'git-commit)
;;; git-commit.el ends here
