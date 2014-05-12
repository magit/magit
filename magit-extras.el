;;; magit-extras.el --- additional functionality for Magit

;; Copyright (C) 2008-2014  The Magit Project Developers
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

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

;; Additional functionality for Magit.

;;; Code:

(require 'magit)

(defgroup magit-extras nil
  "Additional functionality for Magit."
  :group 'magit-extensions)

;;; Commit Mark

;; Add this to your init.el file:
;;   (define-key magit-mode-map "." 'magit-mark-commit)
;;   (define-key magit-mode-map "=" 'magit-diff-with-mark)
;;   (add-hook 'magit-mode-refresh-buffer-hook
;;             'magit-refresh-marked-commit)

(defface magit-marked-commit '((t :inherit highlight))
  "Face for marked commit."
  :group 'magit-extras)

(defvar magit-marked-commit nil)

(defvar-local magit-marked-commit-overlay nil)
(put 'magit-marked-commit-overlay 'permanent-local t)

(defun magit-mark-commit (&optional unmark)
  "Mark the commit at point."
  (interactive "P")
  (if unmark
      (setq magit-marked-commit nil)
    (magit-section-when commit
      (let ((value (magit-section-value it)))
        (setq magit-marked-commit
              (if (equal magit-marked-commit value) nil value)))))
  (magit-map-magit-buffers #'magit-refresh-marked-commit)
  (run-hooks 'magit-mark-commit-hook))

(defun magit-refresh-marked-commit ()
  (unless magit-marked-commit-overlay
    (setq magit-marked-commit-overlay (make-overlay 1 1))
    (overlay-put magit-marked-commit-overlay 'face 'magit-marked-commit))
  (delete-overlay magit-marked-commit-overlay)
  (magit-map-sections
   (lambda (section)
     (when (and (eq (magit-section-type section) 'commit)
                (equal (magit-section-value section)
                       magit-marked-commit))
       (move-overlay magit-marked-commit-overlay
                     (magit-section-start section)
                     (magit-section-end section)
                     (current-buffer))))
   magit-root-section))

(defun magit-diff-with-mark (range)
  "Show changes between the marked commit and the one at point.
If there is no commit at point, then prompt for one."
  (interactive
   (let* ((marked (or magit-marked-commit (user-error "No commit marked")))
          (commit (magit-read-rev (format "Diff marked commit %s with" marked)
                                  (or (magit-branch-at-point)
                                      (magit-get-current-branch)))))
     (list (concat marked ".." commit))))
  (magit-diff range))

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
  (let* ((default-directory (magit-get-top-dir)))
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
                          filename (magit-buffer-file-name t))))
       (setq revision (magit-read-rev "Retrieve from revision" "HEAD")
             filename (magit-read-file-from-rev revision)))
     (list revision filename
           (and (equal filename
                       (ignore-errors
                         (magit-file-relative-name
                          (file-name-directory (buffer-file-name)))))
                (line-number-at-pos)))))
  (let ((default-directory (magit-get-top-dir)))
    (apply #'call-process magit-git-executable nil 0 nil "gui" "blame"
           `(,@(and linenum (list (format "--line=%d" linenum)))
             ,commit
             ,filename))))

;;;###autoload
(defun magit-run-gitk (arg)
  "Run Gitk for the current git repository.
Without a prefix argument run `gitk --all', with
a prefix argument run gitk without any arguments."
  (interactive "P")
  (apply #'call-process magit-gitk-executable nil 0 nil
         (if arg nil (list "--all"))))

;;; Gitignore

(defun magit-gitignore (file-or-pattern &optional local)
  "Instruct Git to ignore FILE-OR-PATTERN.
With a prefix argument only ignore locally."
  (interactive (magit-gitignore-read-args current-prefix-arg))
  (let ((gitignore
         (if local
             (magit-git-dir (convert-standard-filename "info/exclude"))
           (expand-file-name ".gitignore" (magit-get-top-dir)))))
    (make-directory (file-name-directory gitignore) t)
    (with-temp-buffer
      (when (file-exists-p gitignore)
        (insert-file-contents gitignore))
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert file-or-pattern "\n")
      (write-region nil nil gitignore))
    (if local
        (magit-refresh)
      (magit-run-git "add" ".gitignore"))))

(defun magit-gitignore-locally (file-or-pattern &optional local)
  "Instruct Git to locally ignore FILE-OR-PATTERN.
\n(fn FILE-OR-PATTERN)"
  (interactive (magit-gitignore-read-args t))
  (magit-gitignore file-or-pattern t))

(defun magit-gitignore-read-args (local)
  (let* ((default (magit-file-at-point))
         (choices
          (delete-dups
           (--mapcat
            (cons (concat "/" it)
                  (-when-let (ext (file-name-extension it))
                    (list (concat "/" (file-name-directory "foo") "*." ext)
                          (concat "*." ext))))
            (magit-git-lines "ls-files" "--exclude-standard" "--other")))))
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
      (call-interactively #'magit-visit-file)
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
