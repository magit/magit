;;; magit-gitignore.el --- Intentionally untracked files  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2026 The Magit Project Contributors

;; Author: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>
;; Maintainer: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Magit is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements gitignore commands.

;;; Code:

(require 'magit)

;;; Transient

;;;###autoload(autoload 'magit-gitignore "magit-gitignore" nil t)
(transient-define-prefix magit-gitignore ()
  "Instruct Git to ignore a file or pattern."
  :man-page "gitignore"
  ["Gitignore"
   ("t" magit-gitignore-in-topdir)
   ("s" magit-gitignore-in-subdir)
   ("p" magit-gitignore-in-gitdir)
   ("g" magit-gitignore-on-system)]
  ["Skip worktree"
   (7 "w" "do skip worktree"     magit-skip-worktree)
   (7 "W" "do not skip worktree" magit-no-skip-worktree)]
  ["Assume unchanged"
   (7 "u" "do assume unchanged"     magit-assume-unchanged)
   (7 "U" "do not assume unchanged" magit-no-assume-unchanged)])

;;; Gitignore Commands

;;;###autoload(autoload 'magit-gitignore-in-topdir "magit-gitignore" nil t)
(transient-define-suffix magit-gitignore-in-topdir (rule)
  "Add the Git ignore RULE to the top-level \".gitignore\" file.
Since this file is tracked, it is shared with other clones of the
repository.  Also stage the file."
  :description "shared at toplevel (.gitignore)"
  (interactive (list (magit-gitignore-read-pattern)))
  (magit--gitignore rule (expand-file-name ".gitignore" (magit-toplevel)) t))

;;;###autoload(autoload 'magit-gitignore-in-subdir "magit-gitignore" nil t)
(transient-define-suffix magit-gitignore-in-subdir (rule directory)
  "Add the Git ignore RULE to a \".gitignore\" file in DIRECTORY.
Prompt the user for a directory and add the rule to the
\".gitignore\" file in that directory.  Since such files are
tracked, they are shared with other clones of the repository.
Also stage the file."
  :description "shared in subdirectory (path/to/.gitignore)"
  (interactive (let ((dir (expand-file-name
                           (read-directory-name
                            "Limit rule to files in: "
                            (and$ (magit-current-file)
                                  (file-name-directory
                                   (expand-file-name $ (magit-toplevel))))))))
                 (list (magit-gitignore-read-pattern dir) dir)))
  (magit--gitignore rule (expand-file-name ".gitignore" directory) t))

;;;###autoload(autoload 'magit-gitignore-in-gitdir "magit-gitignore" nil t)
(transient-define-suffix magit-gitignore-in-gitdir (rule)
  "Add the Git ignore RULE to \"$GIT_DIR/info/exclude\".
Rules in that file only affects this clone of the repository."
  :description "privately (.git/info/exclude)"
  (interactive (list (magit-gitignore-read-pattern)))
  (magit--gitignore rule (expand-file-name "info/exclude" (magit-gitdir))))

;;;###autoload(autoload 'magit-gitignore-on-system "magit-gitignore" nil t)
(transient-define-suffix magit-gitignore-on-system (rule)
  "Add the Git ignore RULE to the file specified by `core.excludesFile'.
Rules that are defined in that file affect all local repositories."
  :inapt-if-not (##magit-get "core.excludesfile")
  :description (##format "privately for all repositories (%s)"
                         (or (magit-get "core.excludesfile")
                             "core.excludesfile is not set"))
  (interactive (list (magit-gitignore-read-pattern)))
  (if-let ((file (magit-get "core.excludesFile")))
      (magit--gitignore rule file)
    (error "Variable `core.excludesFile' isn't set")))

(defun magit--gitignore (rule file &optional stage)
  (when$ (file-name-directory file)
    (make-directory $ t))
  (with-temp-buffer
    (when (file-exists-p file)
      (insert-file-contents file))
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (insert (replace-regexp-in-string "\\(\\\\*\\)" "\\1\\1" rule))
    (insert "\n")
    (write-region nil nil file))
  (if stage
      (magit-with-toplevel
        (magit-run-git "add" (magit-convert-filename-for-git file)))
    (magit-refresh)))

(defun magit-gitignore-read-pattern (&optional directory)
  (let ((choices (magit--gitignore-patterns directory))
        (default (magit-current-file)))
    (when default
      (when directory
        (setq default
              (substring default
                         (length
                          (file-relative-name directory (magit-toplevel))))))
      (setq default (concat "/" default))
      (unless (member default choices)
        (setq default (concat "*." (file-name-extension default)))
        (unless (member default choices)
          (setq default nil))))
    (magit-completing-read "File or pattern to ignore"
                           choices nil 'any nil nil default)))

(defun magit--gitignore-patterns (&optional directory)
  (let* ((topdir (magit-toplevel))
         (default-directory (or directory topdir))
         (files (magit-untracked-files t directory))
         ;; Include directories that contain only untracked files.
         (dirs (seq-filter (##equal (substring % -1) "/")
                           (magit-untracked-files nil directory)))
         (globs nil)
         (dirglobs nil))
    (when directory
      (let ((beg (length (file-relative-name directory topdir))))
        (setq files (mapcar (##substring % beg) files))
        (setq dirs  (mapcar (##substring % beg) dirs))))
    (dolist (file files)
      (when-let ((ext (file-name-extension file)))
        (cl-pushnew (concat "*." ext) globs :test #'equal)
        (when-let ((dir (file-name-directory file)))
          (cl-pushnew (concat dir "*." ext) dirglobs :test #'equal))))
    (sort (nconc globs
                 (mapcar (##concat "/" %) (nconc files dirs dirglobs)))
          #'string<)))

;;; Skip Worktree Commands

;;;###autoload
(defun magit-skip-worktree (file)
  "Call \"git update-index --skip-worktree -- FILE\"."
  (interactive
    (list (magit-read-file-choice "Skip worktree for"
                                  (cl-set-difference
                                   (magit-list-files)
                                   (magit-skip-worktree-files)
                                   :test #'equal))))
  (magit-with-toplevel
    (magit-run-git "update-index" "--skip-worktree" "--" file)))

;;;###autoload
(defun magit-no-skip-worktree (file)
  "Call \"git update-index --no-skip-worktree -- FILE\"."
  (interactive
    (list (magit-read-file-choice "Do not skip worktree for"
                                  (magit-skip-worktree-files))))
  (magit-with-toplevel
    (magit-run-git "update-index" "--no-skip-worktree" "--" file)))

;;; Assume Unchanged Commands

;;;###autoload
(defun magit-assume-unchanged (file)
  "Call \"git update-index --assume-unchanged -- FILE\"."
  (interactive
    (list (magit-read-file-choice "Assume file to be unchanged"
                                  (cl-set-difference
                                   (magit-list-files)
                                   (magit-assume-unchanged-files)
                                   :test #'equal))))
  (magit-with-toplevel
    (magit-run-git "update-index" "--assume-unchanged" "--" file)))

;;;###autoload
(defun magit-no-assume-unchanged (file)
  "Call \"git update-index --no-assume-unchanged -- FILE\"."
  (interactive
    (list (magit-read-file-choice "Do not assume file to be unchanged"
                                  (magit-assume-unchanged-files))))
  (magit-with-toplevel
    (magit-run-git "update-index" "--no-assume-unchanged" "--" file)))

;;; _
(provide 'magit-gitignore)
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("and$"         . "cond-let--and$")
;;   ("and>"         . "cond-let--and>")
;;   ("and-let"      . "cond-let--and-let")
;;   ("if-let"       . "cond-let--if-let")
;;   ("when$"        . "cond-let--when$")
;;   ("when-let"     . "cond-let--when-let")
;;   ("while-let"    . "cond-let--while-let")
;;   ("match-string" . "match-string")
;;   ("match-str"    . "match-string-no-properties"))
;; End:
;;; magit-gitignore.el ends here
