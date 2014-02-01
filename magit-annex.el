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

;;; Code:

(require 'magit)

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

(defun magit-annex-get (file)
  "get a file from where ever it is"
  (interactive "ffile to get: ")
  (setq file (expand-file-name file))
  (let ((default-directory (file-name-directory file)))
    (magit-run-git "annex" "get" (file-name-nondirectory file))))

(magit-key-mode-add-group 'git-annex)
(magit-key-mode-insert-action 'git-annex "a" "Add" #'magit-annex-add)
(magit-key-mode-insert-action 'git-annex "@" "Add" #'magit-annex-add)
(magit-key-mode-insert-action 'git-annex "e" "edit" #'magit-annex-edit)
(magit-key-mode-insert-action 'git-annex "g" "get" #'magit-annex-get)
(magit-key-mode-generate 'git-annex)

(define-key magit-mode-map (kbd "@") 'magit-key-mode-popup-git-annex)
