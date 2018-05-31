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

;;; Obsolete since v2.1.0

(define-obsolete-variable-alias 'magit-git-standard-options
  'magit-git-global-arguments "Magit 2.1.0")

;;; Obsolete since v2.2.0

(define-obsolete-variable-alias 'magit-log-section-args
  'magit-log-section-arguments "Magit 2.2.0")

;;; Obsolete since v2.3.0

(define-obsolete-function-alias 'global-magit-file-buffer-mode
  'global-magit-file-mode "Magit 2.3.0")

;;; Obsolete since v2.4.0

(define-obsolete-function-alias 'magit-get-tracked-ref
  'magit-get-upstream-ref "Magit 2.4.0")

(define-obsolete-function-alias 'magit-get-tracked-branch
  'magit-get-upstream-branch "Magit 2.4.0")

(define-obsolete-function-alias 'magit-get-tracked-remote
  'magit-get-upstream-remote "Magit 2.4.0")

(define-obsolete-function-alias 'magit-insert-head-header
  'magit-insert-head-branch-header "Magit 2.4.0")

(define-obsolete-function-alias 'magit-insert-pull-branch-header
  'magit-insert-upstream-branch-header "Magit 2.4.0")

(define-obsolete-function-alias 'magit-insert-unpulled-or-recent-commits
  'magit-insert-unpulled-from-upstream-or-recent "Magit 2.4.0")

(define-obsolete-function-alias 'magit-insert-upstream-header
  'magit-insert-upstream-branch-header "Magit 2.4.0")

(make-obsolete-variable 'magit-revert-buffers
                        'magit-auto-revert-mode
                        "Magit 2.4.0")

(make-obsolete-variable 'magit-status-refresh-hook "\
use `magit-pre-refresh-hook', `magit-post-refresh-hook',
  `magit-refresh-buffer-hook', or `magit-status-mode-hook' instead.

  If you want to run a function every time the status buffer is
  refreshed, in order to do something with that buffer, then use:

    (add-hook 'magit-refresh-buffer-hook
              (lambda ()
                (when (derived-mode-p 'magit-status-mode)
                  ...)))

  If your hook function should run regardless of whether the
  status buffer exists or not, then use `magit-pre-refresh-hook'
  or `magit-post-refresh-hook'.

  If your hook function only has to be run once, when the buffer
  is first created, then `magit-status-mode-hook' instead.
" "Magit 2.4.0")

;;; Obsolete since v2.6.0

(define-obsolete-function-alias 'magit-insert-unpulled-module-commits
  'magit-insert-modules-unpulled-from-upstream "Magit 2.6.0")

(define-obsolete-function-alias 'magit-insert-unpushed-module-commits
  'magit-insert-modules-unpushed-to-upstream "Magit 2.6.0")

;;; Obsolete since v2.8.0

(make-obsolete-variable 'magit-repository-directories-depth
                        'magit-repository-directories "Magit 2.8.0")

;;; Obsolete since v2.9.0

(define-obsolete-variable-alias 'magit-duration-spec
  'magit--age-spec "Magit 2.9.0")

(make-obsolete-variable 'magit-diff-show-lines-boundary
                        'magit-diff-highlight-hunk-region-functions
                        "Magit 2.9.0")

(define-obsolete-variable-alias 'magit-log-output-coding-system
  'magit-git-output-coding-system "Magit 2.9.0")

;;; Obsolete since v2.9.1

(define-obsolete-function-alias 'magit-checkout-file
  'magit-file-checkout "Magit 2.9.1")

;;; Obsolete since v2.11.0

(define-obsolete-function-alias 'magit-modified-files
  'magit-unstaged-files "Magit 2.11.0")

(define-obsolete-function-alias 'magit-insert-submodules
  'magit-insert-modules-overview "Magit 2.11.0")

;;; Obsolete since v2.12.0

(define-obsolete-function-alias 'magit-insert-unpulled-from-upstream-or-recent
  'magit-insert-unpulled-from-upstream "Magit 2.12.0")

(define-obsolete-function-alias 'magit-get-submodules
  'magit-list-module-paths "Magit 2.12.0")

(make-obsolete-variable 'magit-status-expand-stashes
                        'magit-section-initial-visibility-alist
                        "Magit 2.12.0")

(make-obsolete 'magit-section-type     "use (oref ... type) instead"     "Magit 2.12.0")
(make-obsolete 'magit-section-value    "use (oref ... value) instead"    "Magit 2.12.0")
(make-obsolete 'magit-section-start    "use (oref ... start) instead"    "Magit 2.12.0")
(make-obsolete 'magit-section-content  "use (oref ... content) instead"  "Magit 2.12.0")
(make-obsolete 'magit-section-end      "use (oref ... end) instead"      "Magit 2.12.0")
(make-obsolete 'magit-section-hidden   "use (oref ... hidden) instead"   "Magit 2.12.0")
(make-obsolete 'magit-section-washer   "use (oref ... washer) instead"   "Magit 2.12.0")
(make-obsolete 'magit-section-parent   "use (oref ... parent) instead"   "Magit 2.12.0")
(make-obsolete 'magit-section-children "use (oref ... children) instead" "Magit 2.12.0")

(provide 'magit-obsolete)
;;; magit-obsolete.el ends here
