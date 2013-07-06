;;; magit-cherry.el --- "git cherry" support for Magit

;; Copyright (C) 2013  Moritz Bunkus

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

(magit-define-command cherry (&optional upstream head)
  (interactive)
  (let ((branch (magit-get-current-branch)))
    (if (not branch)
        (error "Don't cherry on a detached head.")
      (magit-buffer-switch magit--cherry-buffer-name)
      (magit-mode-init
       (magit-get-top-dir)
       'magit-cherry-mode
       #'magit--refresh-cherry-buffer
       (or upstream (magit-read-rev "Upstream"
                                    (magit-format-ref
                                     (magit-remote-branch-for branch t))))
       (or head (magit-read-rev "Head" branch))))))

(defun magit--refresh-cherry-buffer (cherry-upstream cherry-head)
  (magit-create-buffer-sections
    (let ((branch-head (magit-format-commit "HEAD" "%h %s")))
      (insert-before-markers
       (format "Repository:  %s %s\n"
               (propertize (magit-get-current-branch) 'face 'magit-branch)
               (abbreviate-file-name default-directory))
       (format "Branch head: %s\n" (or branch-head "nothing committed (yet)"))
       "\n"
       (format "%s means: present in '%s' but not in '%s'\n"
               (propertize " - " 'face 'magit-diff-del)
               cherry-upstream cherry-head)
       (format "%s means: present in '%s' but not in '%s'\n"
               (propertize " + " 'face 'magit-diff-add)
               cherry-head cherry-upstream)
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
