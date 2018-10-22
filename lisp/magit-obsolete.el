;;; magit-obsolete.el --- obsolete definitions  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2018  The Magit Project Contributors
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

;;; Obsolete since v2.90.0

(define-obsolete-function-alias 'magit-reset-head 'magit-reset-mixed "Magit 2.90.0")
(define-obsolete-function-alias 'magit-gitignore 'magit-gitignore-globally "Magit 2.90.0")
(define-obsolete-function-alias 'magit-branch 'magit-branch-create "Magit 2.90.0")
(define-obsolete-function-alias 'magit-tag 'magit-tag-create "Magit 2.90.0")
(define-obsolete-function-alias 'magit-fetch 'magit-fetch-other "Magit 2.90.0")
(define-obsolete-function-alias 'magit-pull 'magit-pull-branch "Magit 2.90.0")
(define-obsolete-function-alias 'magit-rebase 'magit-rebase-branch "Magit 2.90.0")
(define-obsolete-function-alias 'magit-blame 'magit-blame-addition "Magit 2.90.0")
(define-obsolete-function-alias 'magit-revert 'magit-revert-and-commit "Magit 2.90.0")
(define-obsolete-function-alias 'magit-reset 'magit-reset-quickly "Magit 2.90.0")
(define-obsolete-function-alias 'magit-merge 'magit-merge-plain "Magit 2.90.0")
(define-obsolete-function-alias 'magit-stash 'magit-stash-both "Magit 2.90.0")
(define-obsolete-function-alias 'magit-snapshot 'magit-snapshot-both "Magit 2.90.0")
(define-obsolete-function-alias 'magit-push 'magit-push-other "Magit 2.90.0")
(define-obsolete-function-alias 'magit-commit 'magit-commit-create "Magit 2.90.0")
(define-obsolete-function-alias 'magit-log 'magit-log-other "Magit 2.90.0")
(define-obsolete-function-alias 'magit-reflog 'magit-reflog-other "Magit 2.90.0")
(define-obsolete-function-alias 'magit-diff 'magit-diff-range "Magit 2.90.0")

;;; _
(provide 'magit-obsolete)
;;; magit-obsolete.el ends here
