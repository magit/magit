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
;; current HEAD.  You can add individual hunks from the working tree
;; to the index, and you can commit the index.

;;; TODO

;; - Washing diffs
;; - Staging/unstaging hunks
;; - Commit messages
;; - Dealing with merges
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

(defun gits-set (val &rest keys)
  (if val
      (gits-shell "git-config %s %s" (gits-concat-with-delim "." keys) val)
    (gits-shell "git-config --unset %s" (gits-concat-with-delim "." keys))))

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

(defun gits-insert-output (title washer cmd &rest args)
  (let ((dir default-directory)
	(tmp (get-buffer-create " *git-tmp*")))
    (save-excursion
      (set-buffer tmp)
      (erase-buffer)
      (setq default-directory dir)
      (let ((status (apply 'call-process cmd nil tmp nil args)))
	(if washer
	    (funcall washer status))))
    (when (> (buffer-size tmp) 0)
      (insert title "\n")
      (insert-buffer-substring tmp)
      (insert "\n"))))

(defun gits-put-line-property (prop val)
  (put-text-property (line-beginning-position) (line-end-position)
		     prop val))

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
  (define-key gits-keymap (kbd "a") 'gits-add-thing-at-point)
  (define-key gits-keymap (kbd "i") 'gits-ignore-thing-at-point)
  (define-key gits-keymap (kbd "c") 'git-commit)
  (define-key gits-keymap (kbd "p") 'gits-display-process))

;;; Status

(defun gits-wash-other-files (status)
  (goto-char (point-min))
  (while (not (eobp))
    (let ((filename (buffer-substring (point) (line-end-position))))
      (insert "  ")
      (gits-put-line-property 'face '(:foreground "red"))
      (gits-put-line-property 'gits-info (list 'other-file filename)))
    (forward-line)
    (beginning-of-line)))

(defun gits-update-status ()
  (let ((buf (get-buffer "*git-status*")))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only t)
      (use-local-map gits-keymap)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(let ((branch (gits-get-current-branch)))
	  (insert (format "Local:  %s %s\n"
			  (or branch "(detached)")
			  (abbreviate-file-name default-directory)))
	  (if branch
	      (let ((remote (gits-get "branch" branch "remote")))
		(if remote
		    (let* ((push (gits-get "remote" remote "push"))
			   (pull (gits-get "remote" remote "fetch"))
			   (desc (if (equal push pull)
				     push
				   (format "%s>%s" pull push))))
		      (insert (format "Remote: %s %s\n"
				      desc
				      (gits-get "remote" remote "url"))))))))
	(insert "\n")
	(gits-insert-output "Untracked files:" 'gits-wash-other-files
			    "git" "ls-files" "--others" "--exclude-standard")
	(gits-insert-output "Local changes:" nil
			    "git" "diff")
	(gits-insert-output "Staged changes:" nil
			    "git" "diff" "--cached")))))

(defun git-status (dir)
  (interactive (list (gits-read-top-dir current-prefix-arg)))
  (let ((buf (get-buffer-create "*git-status*")))
    (switch-to-buffer buf)
    (setq default-directory dir)
    (gits-update-status)))

;;; Staging

(defun gits-add-thing-at-point ()
  (interactive)
  (let ((info (get-char-property (point) 'gits-info)))
    (if info
	(case (car info)
	  ((other-file)
	   (gits-run "git" "add" (cadr info)))))))

(defun gits-ignore-thing-at-point ()
  (interactive)
  (let ((info (get-char-property (point) 'gits-info)))
    (if info
	(case (car info)
	  ((other-file)
	   (append-to-file (concat (cadr info) "\n") nil ".gitignore")
	   (gits-update-status))))))

;;; Push and pull

(defstruct gits-remote-info
  nick url push pull dirty-p)

(defun gits-get-remote-config (nick)
  (let* ((branch (gits-get-current-branch))
	 (nick (or nick
		   (and branch (gits-get "branch" branch "remote"))
		   "origin"))
	 (url (gits-get "remote" nick "url"))
	 (push (gits-get "remote" nick "push"))
	 (pull (gits-get "remote" nick "fetch")))
    (make-gits-remote-info :nick nick :url url :push push :pull pull
			   :dirty-p nil)))

(defun gits-set-remote-config (config)
  (let ((nick (gits-remote-info-nick config)))
    (let ((branch (gits-get-current-branch)))
      (if branch
	  (gits-set nick "branch" branch "remote")))
    (gits-set (gits-remote-info-url config) "remote" nick "url")
    (gits-set (gits-remote-info-push config) "remote" nick "push")
    (gits-set (gits-remote-info-pull config) "remote" nick "fetch")))

(defun gits-remote-info-complete (conf field prompt)
  (when (or current-prefix-arg
	    (null (funcall field conf)))
    (let ((url (read-string (format prompt (gits-remote-info-nick conf))
			    (funcall field conf))))
      (eval `(setf (,field ',conf) ',url))
      (setf (gits-remote-info-dirty-p conf) t))))
  
(defun gits-read-push-pull-args (push-p)
  (let ((conf (gits-get-remote-config nil)))
    (when current-prefix-arg
      (let ((nick (read-string "Remote repository nickname: "
			       (gits-remote-info-nick conf))))
	(setq conf (gits-get-remote-config nick))
	(setf (gits-remote-info-dirty-p conf) t)))
    (gits-remote-info-complete conf 'gits-remote-info-url
			       "Repository url of '%s': ")
    (if push-p
	(gits-remote-info-complete conf 'gits-remote-info-push
				   "Branch of '%s' to push to: ")
      (gits-remote-info-complete conf 'gits-remote-info-pull
				 "Branch of '%s' to pull from: "))
    (if (and (gits-remote-info-dirty-p conf)
	     (y-or-n-p "Save as defaults? "))
	(gits-set-remote-config conf))
    (list (gits-remote-info-url conf)
	  (if push-p
	      (gits-remote-info-push conf)
	    (gits-remote-info-pull conf)))))

(defun git-pull (nick branch)
  (interactive (gits-read-push-pull-args nil))
  (gits-run "git" "pull" nick branch))

(defun git-push (nick branch)
  (interactive (gits-read-push-pull-args t))
  (gits-run "git" "push" nick branch))

;;; Commit

(defun git-commit ()
  (interactive)
  (gits-run "git-commit" "-m" "(changes)"))

;;; Misc

(defun git-stage-all ()
  (interactive)
  (gits-run "git-add" "-u" "."))
