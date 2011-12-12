;;; magit-simple-keys.el --- simple keybindings for Magit

;; Copyright (C) 2011  Ramkumar Ramachandra
;;
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

;; This plug-in overrides the keybindings in magit-key-mode with
;; simpler keybindings; it does this by picking the most obviously
;; used command in each key group

;;; Code:

(require 'magit)

(defvar magit-key-mode-mapping
  '((logging magit-display-log)
    (running magit-shell-command)
    (fetching magit-fetch-current)
    (pushing magit-push)
    (pulling magit-pull)
    (branching magit-checkout)
    (tagging magit-tag)
    (stashing magit-stash)
    (merging magit-merge)
    (submodule magit-submodule-update)))

(defun magit-simple-keys-key-mode-generate (term mapping-function)
  "Generate alias for the key-group term"
  (eval
   `(defalias ',(intern (concat "magit-key-mode-popup-" (symbol-name term)))
	mapping-function)))

;; generate the aliases using the mapping in key-mode-mapping
(mapc (lambda (g)
        (magit-simple-keys-key-mode-generate (car g) (cadr g)))
      magit-key-mode-mapping)

(provide 'magit-simple-keys)
;;; magit-simple-keys.el ends here
