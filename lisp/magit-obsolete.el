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

;;; Obsolete since v2.4.0

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

;;; Obsolete since v2.12.0

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
