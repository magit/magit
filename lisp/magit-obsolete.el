;;; magit-obsolete.el --- obsolete definitions  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2021  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

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
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This library defines aliases for obsolete variables and functions.

;;; Code:

(require 'magit)

;;; Obsolete since v3.0.0

(define-obsolete-function-alias 'magit-diff-visit-file-worktree
  'magit-diff-visit-worktree-file "Magit 3.0.0")

(define-obsolete-function-alias 'magit-status-internal
  'magit-status-setup-buffer "Magit 3.0.0")

(define-obsolete-variable-alias 'magit-mode-setup-hook
  'magit-setup-buffer-hook "Magit 3.0.0")

(define-obsolete-variable-alias 'magit-branch-popup-show-variables
  'magit-branch-direct-configure "Magit 3.0.0")

(define-obsolete-function-alias 'magit-dispatch-popup
  'magit-dispatch "Magit 3.0.0")

(define-obsolete-function-alias 'magit-repolist-column-dirty
  'magit-repolist-column-flag "Magit 3.0.0")

(define-obsolete-variable-alias 'magit-disable-line-numbers
  'magit-section-disable-line-numbers "Magit 3.0.0")

(define-obsolete-variable-alias 'inhibit-magit-refresh
  'magit-inhibit-refresh "Magit 3.0.0")

(defun magit--magit-popup-warning ()
  (display-warning 'magit "\
Magit no longer uses Magit-Popup.
It now uses Transient.
See https://emacsair.me/2019/02/14/transient-0.1.

However your configuration and/or some third-party package that
you use still depends on the `magit-popup' package.  But because
`magit' no longer depends on that, `package' has removed it from
your system.

If some package that you use still depends on `magit-popup' but
does not declare it as a dependency, then please contact its
maintainer about that and install `magit-popup' explicitly.

If you yourself use functions that are defined in `magit-popup'
in your configuration, then the next step depends on what you use
that for.

* If you use `magit-popup' to define your own popups but do not
  modify any of Magit's old popups, then you have to install
  `magit-popup' explicitly.  (You can also migrate to Transient,
  but there is no need to rush that.)

* If you add additional arguments and/or actions to Magit's popups,
  then you have to port that to modify the new \"transients\" instead.
  See https://github.com/magit/magit/wiki/\
Converting-popup-modifications-to-transient-modifications

To find installed packages that still use `magit-popup' you can
use e.g. \"M-x rgrep RET magit-popup RET RET ~/.emacs.d/ RET\"."))
(cl-eval-when (eval load)
  (unless (require (quote magit-popup) nil t)
    (defun magit-define-popup-switch (&rest _)
      (magit--magit-popup-warning))
    (defun magit-define-popup-option (&rest _)
      (magit--magit-popup-warning))
    (defun magit-define-popup-variable (&rest _)
      (magit--magit-popup-warning))
    (defun magit-define-popup-action (&rest _)
      (magit--magit-popup-warning))
    (defun magit-define-popup-sequence-action (&rest _)
      (magit--magit-popup-warning))
    (defun magit-define-popup-key (&rest _)
      (magit--magit-popup-warning))
    (defun magit-define-popup-keys-deferred (&rest _)
      (magit--magit-popup-warning))
    (defun magit-change-popup-key (&rest _)
      (magit--magit-popup-warning))
    (defun magit-remove-popup-key (&rest _)
      (magit--magit-popup-warning))))

;;; _
(provide 'magit-obsolete)
;;; magit-obsolete.el ends here
