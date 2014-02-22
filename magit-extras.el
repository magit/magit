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
;;   (define-key magit-mode-map "." 'magit-mark-item)
;;   (define-key magit-mode-map "." 'magit-diff-with-mark)
;;   (add-hook 'magit-mode-refresh-buffer-hook
;;             'magit-refresh-marked-commits-in-buffer))

(defface magit-item-mark '((t :inherit highlight))
  "Face for highlighting marked item."
  :group 'magit-extras)

(defvar magit-marked-commit nil)

(defvar-local magit-mark-overlay nil)
(put 'magit-mark-overlay 'permanent-local t)

(defun magit-mark-item (&optional unmark)
  "Mark the commit at point.
Some commands act on the marked commit by default or use it as
default when prompting for a commit."
  (interactive "P")
  (if unmark
      (setq magit-marked-commit nil)
    (magit-section-action mark (info)
      (commit (setq magit-marked-commit
                    (if (equal magit-marked-commit info) nil info)))))
  (magit-refresh-marked-commits)
  (run-hooks 'magit-mark-commit-hook))

(defun magit-refresh-marked-commits ()
  (magit-map-magit-buffers #'magit-refresh-marked-commits-in-buffer))

(defun magit-refresh-marked-commits-in-buffer ()
  (unless magit-mark-overlay
    (setq magit-mark-overlay (make-overlay 1 1))
    (overlay-put magit-mark-overlay 'face 'magit-item-mark))
  (delete-overlay magit-mark-overlay)
  (magit-map-sections
   (lambda (section)
     (when (and (eq (magit-section-type section) 'commit)
                (equal (magit-section-info section)
                       magit-marked-commit))
       (move-overlay magit-mark-overlay
                     (magit-section-beginning section)
                     (magit-section-end section)
                     (current-buffer))))
   magit-root-section))

(defun magit-diff-with-mark (range)
  "Show changes between the marked commit and the one at point.
If there is no commit at point, then prompt for one."
  (interactive
   (let* ((marked (or magit-marked-commit (user-error "No commit marked")))
          (current (magit-get-current-branch))
          (is-current (string= (magit-get-refname marked) current))
          (commit (or (magit-guess-branch)
                      (magit-read-rev
                       (format "Diff marked commit %s with" marked)
                       (unless is-current current)
                       current))))
     (list (concat marked ".." commit))))
  (magit-diff range))

;;; External Tools

(defcustom magit-gitk-executable (executable-find "gitk")
  "The Gitk executable."
  :group 'magit-extras
  :type 'string)

;;;###autoload
(defun magit-run-git-gui ()
  "Run `git gui' for the current git repository."
  (interactive)
  (let ((default-directory (magit-get-top-dir)))
    (start-file-process "Git Gui" nil magit-git-executable "gui")))

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
    (apply 'start-file-process "Git Gui Blame" nil
           magit-git-executable "gui" "blame"
           `(,@(and linenum (list (format "--line=%d" linenum)))
             ,commit
             ,filename))))

;;;###autoload
(defun magit-run-gitk ()
  "Run `gitk --all' for the current git repository."
  (interactive)
  (let ((default-directory (magit-get-top-dir)))
    (cond
     ((eq system-type 'windows-nt)
      ;; Gitk is a shell script, and Windows doesn't know how to
      ;; "execute" it.  The Windows version of Git comes with an
      ;; implementation of "sh" and everything else it needs, but
      ;; Windows users might not have added the directory where it's
      ;; installed to their path
      (let* ((git-bin-dir
             ;; According to #824, when using stand-alone installation
             ;; Gitk maybe installed in ...cmd or ...bin; while Sh
             ;; is installed in ...bin.
             (expand-file-name "bin"
                               (file-name-directory
                                (directory-file-name
                                 (file-name-directory
                                  magit-gitk-executable)))))
            ;; Adding it onto the end so that anything the user
            ;; specified will get tried first.  Emacs looks in
            ;; exec-path; PATH is the environment variable inherited by
            ;; the process.  I need to change both.
            (exec-path (append exec-path (list git-bin-dir)))
            (process-environment process-environment))
        (setenv "PATH"
                (format "%s;%s"
                        (getenv "PATH")
                        (replace-regexp-in-string "/" "\\\\" git-bin-dir)))
        (start-file-process "Gitk" nil "sh" magit-gitk-executable "--all")))
     (t
      (start-file-process "Gitk" nil magit-gitk-executable "--all")))))

(provide 'magit-extras)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-extras.el ends here
