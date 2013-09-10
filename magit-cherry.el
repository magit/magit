;;; magit-cherry.el --- "git cherry" support for Magit

;; Copyright (C) 2013  The Magit Project Developers.
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

;; Author: Moritz Bunkus <moritz@bunkus.org>
;; Package: magit

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

;; Control git-cherry from Magit.

;;; Code:

(require 'magit)

(defvar magit--cherry-buffer-name "*magit-cherry*")

(define-derived-mode magit-cherry-mode magit-mode "Magit Cherry"
  "Magit Cherry")

(magit-define-command cherry (upstream head)
  (interactive
   (let ((branch (or (magit-get-current-branch)
                     (error "Don't cherry on a detached head."))))
     (list (magit-read-rev "Cherry upstream"
                           (magit-get-tracked-branch branch nil t))
           (magit-read-rev "Cherry head" branch))))
  (let ((topdir (magit-get-top-dir default-directory)))
    (magit-display-mode-buffer magit--cherry-buffer-name)
    (magit-mode-init topdir
                     #'magit-cherry-mode
                     #'magit--refresh-cherry-buffer
                     upstream
                     head)))

(defun magit--refresh-cherry-buffer (cherry-upstream cherry-head)
  (magit-create-buffer-sections
    (let ((branch-head (magit-format-commit "HEAD" "%h %s")))
      (insert-before-markers
       (format "Repository:  %s %s\n"
               (propertize (magit-get-current-branch) 'face 'magit-branch)
               (abbreviate-file-name default-directory))
       (format "Branch head: %s\n" (or branch-head "nothing committed (yet)"))
       "\n"
       (format "%s means: equivalent exists in '%s'\n"
               (propertize " - " 'face 'magit-diff-del)
               cherry-upstream)
       (format "%s means: only exists in '%s'\n"
               (propertize " + " 'face 'magit-diff-add)
               cherry-head)
       "\n"
       (propertize "Cherry commits:" 'face 'magit-section-title) "\n"))
    (magit-git-section 'commit nil 'magit--wash-cherry-output
                       "cherry" "-v" cherry-upstream cherry-head)))

;;; Format of "git cherry -v ..." output:
;;+ 8927349872a908bf hello world
;;- 098340983094fffa chunky bacon

(defun magit--wash-cherry-output ()
  (while (looking-at "^\\(\\+\\|-\\) +\\([0-9a-f]+ *\\)")
    (let* ((summary-start (match-end 2))
           (direction (match-string 1))
           (revision  (replace-regexp-in-string " +" "" (match-string 2))))
      ;; Delete direction mark and revision before reconstructing
      ;; them.
      (beginning-of-line)
      (delete-region (point) summary-start)

      ;; Re-create output and propertize properly.
      (insert (propertize (concat " " direction " ")
                          'face (if (string= direction "+")
                                    'magit-diff-add
                                  'magit-diff-del))
              " "
              (propertize revision 'face 'magit-log-sha1)
              " ")

      ;; Set section info to commit's SHA
      (let ((section (magit-set-section revision 'commit
                                        (line-beginning-position)
                                        (min (1+ (line-end-position))
                                             (point-max)))))
        (magit-set-section-info revision section))

      ;; Go to beginning of next line.
      (beginning-of-line)
      (forward-line))))

(provide 'magit-cherry)
;;; magit-cherry.el ends here
