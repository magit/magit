;;; magit-extras.el --- additional functionality for Magit

;; Copyright (C) 2008-2015  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

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

;; Additional functionality for Magit.

;;; Code:

(require 'magit)

(defgroup magit-extras nil
  "Additional functionality for Magit."
  :group 'magit-extensions)

;;; External Tools

(defcustom magit-gitk-executable
  (or (and (eq system-type 'windows-nt)
           (let ((exe (expand-file-name
                       "gitk" (file-name-nondirectory magit-git-executable))))
             (and (file-executable-p exe) exe)))
      (executable-find "gitk") "gitk")
  "The Gitk executable."
  :group 'magit-extras
  :set-after '(magit-git-executable)
  :type 'string)

;;;###autoload
(defun magit-run-git-gui ()
  "Run `git gui' for the current git repository."
  (interactive)
  (magit-with-toplevel
    (call-process magit-git-executable nil 0 nil "gui")))

;;;###autoload
(defun magit-run-git-gui-blame (commit filename &optional linenum)
  "Run `git gui blame' on the given FILENAME and COMMIT.
Interactively run it for the current file and the HEAD, with a
prefix or when the current file cannot be determined let the user
choose.  When the current buffer is visiting FILENAME instruct
blame to center around the line point is on."
  (interactive
   (let (revision filename)
     (when (or current-prefix-arg
               (not (setq revision "HEAD"
                          filename (magit-file-relative-name))))
       (setq revision (magit-read-branch-or-commit "Blame from revision")
             filename (magit-read-file-from-rev revision "Blame file")))
     (list revision filename
           (and (equal filename
                       (ignore-errors
                         (magit-file-relative-name (buffer-file-name))))
                (line-number-at-pos)))))
  (let ((default-directory (magit-toplevel)))
    (apply #'call-process magit-git-executable nil 0 nil "gui" "blame"
           `(,@(and linenum (list (format "--line=%d" linenum)))
             ,commit
             ,filename))))

;;;###autoload
(defun magit-run-gitk ()
  "Run `gitk' in the current repository."
  (interactive)
  (call-process magit-gitk-executable nil 0))

;;;###autoload
(defun magit-run-gitk-branches ()
  "Run `gitk --branches' in the current repository."
  (interactive)
  (call-process magit-gitk-executable nil 0 nil "--branches"))

;;;###autoload
(defun magit-run-gitk-all ()
  "Run `gitk --all' in the current repository."
  (interactive)
  (call-process magit-gitk-executable nil 0 nil "--all"))

;;; Clean

;;;###autoload
(defun magit-clean (&optional arg)
  "Remove untracked files from the working tree.
With a prefix argument also remove ignored files,
with two prefix arguments remove ignored files only.
\n(git clean -f -d [-x|-X])"
  (interactive "p")
  (when (yes-or-no-p (format "Remove %s files? "
                             (pcase arg
                               (1 "untracked")
                               (4 "untracked and ignored")
                               (_ "ignored"))))
    (magit-wip-commit-before-change)
    (magit-run-git "clean" "-f" "-d" (pcase arg (4 "-x") (16 "-X")))))

(put 'magit-clean 'disabled t)

;;; Gitignore

;;;###autoload
(defun magit-gitignore (file-or-pattern &optional local)
  "Instruct Git to ignore FILE-OR-PATTERN.
With a prefix argument only ignore locally."
  (interactive (magit-gitignore-read-args current-prefix-arg))
  (let ((gitignore
         (if local
             (magit-git-dir (convert-standard-filename "info/exclude"))
           (expand-file-name ".gitignore" (magit-toplevel)))))
    (make-directory (file-name-directory gitignore) t)
    (with-temp-buffer
      (when (file-exists-p gitignore)
        (insert-file-contents gitignore))
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert (replace-regexp-in-string "\\(\\\\*\\)" "\\1\\1" file-or-pattern))
      (insert "\n")
      (write-region nil nil gitignore))
    (if local
        (magit-refresh)
      (magit-run-git "add" ".gitignore"))))

;;;###autoload
(defun magit-gitignore-locally (file-or-pattern &optional local)
  "Instruct Git to locally ignore FILE-OR-PATTERN.
\n(fn FILE-OR-PATTERN)"
  (interactive (magit-gitignore-read-args t))
  (magit-gitignore file-or-pattern t))

(defun magit-gitignore-read-args (local)
  (let* ((default (magit-current-file))
         (choices
          (delete-dups
           (--mapcat
            (cons (concat "/" it)
                  (-when-let (ext (file-name-extension it))
                    (list (concat "/" (file-name-directory "foo") "*." ext)
                          (concat "*." ext))))
            (magit-untracked-files)))))
    (when default
      (setq default (concat "/" default))
      (unless (member default choices)
        (setq default (concat "*." (file-name-extension default)))
        (unless (member default choices)
          (setq default nil))))
    (list (magit-completing-read
           (concat "File or pattern to ignore" (and local " locally"))
           choices nil nil nil nil default)
          local)))

;;; ChangeLog

;;;###autoload
(defun magit-add-change-log-entry (&optional whoami file-name other-window)
  "Find change log file and add date entry and item for current change.
This differs from `add-change-log-entry' (which see) in that
it acts on the current hunk in a Magit buffer instead of on
a position in a file-visiting buffer."
  (interactive (list current-prefix-arg
                     (prompt-for-change-log-name)))
  (let (buf pos)
    (save-window-excursion
      (call-interactively #'magit-diff-visit-file)
      (setq buf (current-buffer)
            pos (point)))
    (save-excursion
      (with-current-buffer buf
        (goto-char pos)
        (add-change-log-entry whoami file-name other-window)))))

;;;###autoload
(defun magit-add-change-log-entry-other-window (&optional whoami file-name)
  "Find change log file in other window and add entry and item.
This differs from `add-change-log-entry-other-window' (which see)
in that it acts on the current hunk in a Magit buffer instead of
on a position in a file-visiting buffer."
  (interactive (and current-prefix-arg
                    (list current-prefix-arg
                          (prompt-for-change-log-name))))
  (magit-add-change-log-entry whoami file-name t))

;;; magit-extras.el ends soon
(provide 'magit-extras)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-extras.el ends here
