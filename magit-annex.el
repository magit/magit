;;; magit-annex.el --- control Git annex from Emacs

;; Copyright (C) 2013  The Magit Project Developers.
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

;; Author: Rémi Vanicat      <vanicat@debian.org>

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

;; This library implements an interface to git annex
;; (http://git-annex.branchable.com/).

;; The function magit-annex-edit-dired and magit-annex-get-dired should be
;; bin in dired-mode.
;;
;; You could for example add to your .emacs:

;; (eval-after-load 'dired
;;   '(progn
;;     (define-key dired-mode-map [@ e] 'magit-annex-edit-dired)
;;     (define-key dired-mode-map [@ g] 'magit-annex-get-dired)))

;;; Code:

(require 'magit)

(defvar magit-annex-sync-content ()
  "If not nil, magit-annex-sync will default to sync content too.
If nil, it will only sync git data")

(defun magit-annex-add ()
  "add to git annex."
  (interactive)
  (magit-section-action annex-add (info)
    ([file untracked]
     (magit-run-git "annex" "add" (if (use-region-p)
                                      (magit-section-region-siblings #'magit-section-info)
                                      info)))
    (untracked
       (apply #'magit-run-git "annex" "add" "--"
              (magit-git-lines "ls-files" "--other" "--exclude-standard")))
    ([diff unstaged]
     (magit-run-git "annex" "add"
                    (if (use-region-p)
                        (magit-section-region-siblings #'magit-section-info)
                        info)))
    (unstaged
     (magit-run-git "annex" "add"))
    ([* staged]
     (user-error "Already staged"))))

(defun magit-annex-sync (&optional content)
  "run git annex sync.

With prefix argument, use the --content option to also sync content

When `magit-annex-sync-content' is non nil, it reverse the action of the prefix argument"
  (interactive "P")
  (magit-run-git-async "annex" "sync" (if (if magit-annex-sync-content (not content) content) "--content" ())))

(defun magit-annex-get-auto ()
  "run git annex get --auto to get all needed files"
  (interactive)
  (magit-run-git-async "annex" "get" "--auto"))

(defun magit-annex-edit (file)
  "unlock an annexed file"
  (interactive "ffile to edit: ")
  (setq file (expand-file-name file))
  (let ((default-directory (file-name-directory file)))
    (magit-run-git "annex" "edit" (file-name-nondirectory file))))

(defun magit-annex-edit-dired (file-list)
  "unlock an annexed file in dired"
  (interactive (list (dired-get-marked-files)))
  (mapc #'magit-annex-edit file-list))

(defun magit-annex-get (file)
  "get a file from where ever it is"
  (interactive "ffile to get: ")
  (setq file (expand-file-name file))
  (let ((default-directory (file-name-directory file)))
    (magit-run-git "annex" "get" (file-name-nondirectory file))))

(defun magit-annex-get-dired (file-list)
  "get a file from git annex in dired"
  (interactive (list (dired-get-marked-files)))
  (mapc #'magit-annex-get file-list))

(magit-key-mode-add-group 'git-annex)
(magit-key-mode-insert-action 'git-annex "a" "Add" #'magit-annex-add)
(magit-key-mode-insert-action 'git-annex "s" "Sync" #'magit-annex-sync)
(magit-key-mode-insert-action 'git-annex "@" "Add" #'magit-annex-add)
(magit-key-mode-insert-action 'git-annex "e" "edit" #'magit-annex-edit)
(magit-key-mode-insert-action 'git-annex "g" "get" #'magit-annex-get)
(magit-key-mode-generate 'git-annex)

(define-key magit-mode-map (kbd "@") 'magit-key-mode-popup-git-annex)
