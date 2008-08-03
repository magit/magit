;;; git-status -- control git from Emacs.

;; Copyright (C) 2008  Marius Vollmer
;;
;; Git-status is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; Git-status is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Introduction

;; Invoking the git-status function will show a buffer with the
;; current status of the current git repository and its checkout.
;; That buffer offers key bindings for manipulating the status in
;; simple ways.
;;
;; The status buffer mainly shows the difference between the working
;; tree and the index, and the difference between the index and the
;; current HEAD.  You can 'stage' individual hunks from the working
;; tree to the index, and you can commit the index.  The commit
;; message needs to be prepared in a special area of the status buffer
;; before committing.
;;
;; The status buffer also supports resolving conflicts.
;;
;; Maybe there will be some support for history browsing in the
;; future.

;;; TODO

;; - Untracked files
;; - Fontifying diffs
;; - Staging/unstaging hunks
;; - Removing files
;; - History browsing

;;; Utilities

(defun gits-shell (cmd &rest args)
  (let ((str (shell-command-to-string (apply 'format cmd args))))
    (if (string= str "")
	nil
      (if (equal (elt str (- (length str) 1)) ?\n)
	  (substring str 0 (- (length str) 1))
	str))))

(defun gits-concat-with-delim (delim seqs)
  (cond ((null seqs)
	 nil)
	((null (cdr seqs))
	 (car seqs))
	(t
	 (concat (car seqs) delim (gits-concat-with-delim delim (cdr seqs))))))

(defun gits-get (&rest keys)
  (gits-shell "git-config %s" (gits-concat-with-delim "." keys)))

(defun gits-get-top-dir (cwd)
  (let* ((cwd (expand-file-name cwd))
	 (git-dir (gits-shell "cd '%s' && git-rev-parse --git-dir 2>/dev/null"
			      cwd)))
    (if git-dir
	(file-name-as-directory (or (file-name-directory git-dir) cwd))
      nil)))

(defun gits-get-ref (ref)
  (gits-shell "git-symbolic-ref -q %s" ref))

(defun gits-get-current-branch ()
  (let* ((head (gits-get-ref "HEAD"))
	 (pos (and head (string-match "^refs/heads/" head))))
    (if pos
	(substring head 11)
      nil)))

(defun gits-read-top-dir (prefix)
  (let ((dir (gits-get-top-dir default-directory)))
    (if prefix
	(gits-get-top-dir (read-directory-name "Git repository: " dir))
      dir)))

(defun gits-insert-output (title post cmd &rest args)
  (let ((tmp (get-buffer-create " *git-tmp*")))
    (save-excursion
      (set-buffer tmp)
      (erase-buffer)
      (let ((status (apply 'call-process cmd nil t nil args)))
	(if post
	    (funcall post status))))
    (when (> (buffer-size tmp) 0)
      (insert title "\n")
      (insert-buffer-substring tmp)
      (insert "\n"))))

;;; Running asynchronous commands

(defvar gits-process nil)

(defun gits-run (cmd &rest args)
  (or (not gits-process)
      (error "Git is already running."))
  (let ((dir default-directory)
	(buf (get-buffer-create "*git-process*")))
    (save-excursion
      (set-buffer buf)
      (setq default-directory dir)
      (erase-buffer)
      (insert "$ " (gits-concat-with-delim " " (cons cmd args)) "\n")
      (setq gits-process (apply 'start-process "git" buf cmd args))
      (set-process-sentinel gits-process 'gits-process-sentinel))))

(defun gits-process-sentinel (process event)
  (cond ((string= event "finished\n")
	 (message "Git finished.")
	 (setq gits-process nil))
	((string= event "killed\n")
	 (message "Git was killed.")
	 (setq gits-process nil))
	((string-match "exited abnormally" event)
	 (message "Git failed.")
	 (setq gits-process nil))
	(t
	 (message "Git is weird.")))
  (gits-update-status))

(defun gits-display-process ()
  (interactive)
  (display-buffer "*git-process*"))

;;; Keymap

(defvar gits-keymap nil)

(when (not gits-keymap)
  (setq gits-keymap (make-keymap))
  (suppress-keymap gits-keymap)
  (define-key gits-keymap (kbd "g") 'git-status)
  (define-key gits-keymap (kbd "S") 'git-stage-all)
  (define-key gits-keymap (kbd "c") 'git-commit)
  (define-key gits-keymap (kbd "p") 'gits-display-process))

;;; Status

(defun gits-update-status ()
  (let ((buf (get-buffer "*git-status*")))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only t)
      (use-local-map gits-keymap)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert (format "Repository:  %s\n"
			(abbreviate-file-name default-directory)))
	(let ((branch (gits-get-current-branch)))
	  (insert (format "Branch:      %s\n"
			  (or branch "(detached)")))
	  (if branch
	      (let ((remote (gits-get "branch" branch "remote")))
		(if remote
		    (let* ((push (gits-get "remote" remote "push"))
			   (pull (gits-get "remote" remote "fetch"))
			   (desc (if (equal push pull)
				     push
				   (format "%s -> %s" pull push))))
		      (insert (format "Remote:      %s\n             %s\n"
				      desc
				      (gits-get "remote" remote "url"))))))))
	(insert "\n")
	(gits-insert-output "Untracked files:" nil
			    "git" "ls-files" "--others")
	(gits-insert-output "Local changes:" nil
			    "git" "diff")
	(gits-insert-output "Staged changes:" nil
			    "git" "diff" "--cached")))))

;;; Main

(defun git-status (dir)
  (interactive (list (gits-read-top-dir current-prefix-arg)))
  (let ((buf (get-buffer-create "*git-status*")))
    (switch-to-buffer buf)
    (setq default-directory dir)
    (gits-update-status)))

(defun git-pull ()
  (interactive)
  (gits-run "git-pull"))

(defun git-push ()
  (interactive)
  (gits-run "git-push"))

(defun git-stage-all ()
  (interactive)
  (gits-run "git-add" "-u" "."))

(defun git-commit ()
  (interactive)
  (gits-run "git-commit" "-m" "(changes)"))
