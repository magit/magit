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

;; - Shortcut for staging/unstaging of whole files
;; - Dealing with merges from pulls
;; - Hide titles of empty sections
;; - Branch creation/switching
;; - Explicit diffing/merging of branches
;; - Detect and handle renames and copies.
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
  (insert title "\n")
  (let* ((beg (point))
	 (status (apply 'call-process cmd nil t nil args))
	 (end (point)))
    (if washer
	(save-restriction
	  (narrow-to-region beg (point))
	  (funcall washer status)
	  (goto-char (point-max))
	  (insert "\n")))))

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
  (define-key gits-keymap (kbd "a") 'gits-stage-thing-at-point)
  (define-key gits-keymap (kbd "u") 'gits-unstage-thing-at-point)
  (define-key gits-keymap (kbd "i") 'gits-ignore-thing-at-point)
  (define-key gits-keymap (kbd "RET") 'gits-visit-thing-at-point)
  (define-key gits-keymap (kbd "c") 'git-log-edit)
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

(defun gits-wash-diff-propertize-diff (head-beg head-end)
  (let ((head-end (or head-end (point))))
    (when head-beg
      (put-text-property head-beg head-end
			 'gits-info (list 'diff
					  head-beg (point))))))

(defun gits-wash-diff-propertize-hunk (head-beg head-end hunk-beg)
  (when hunk-beg
    (put-text-property hunk-beg (point)
		       'gits-info (list 'hunk
					head-beg head-end
					hunk-beg (point)))))

(defun gits-wash-diff (status)
  (goto-char (point-min))
  (let ((n-files 1)
	(head-beg nil)
	(head-end nil)
	(hunk-beg nil))
    (while (not (eobp))
      (let ((prefix (buffer-substring-no-properties
		     (point) (+ (point) n-files))))
	(cond ((looking-at "^diff")
	       (gits-wash-diff-propertize-diff head-beg head-end)
	       (setq head-beg (point))
	       (setq head-end nil))
	      ((looking-at "^@+")
	       (setq n-files (- (length (match-string 0)) 1))
	       (if (null head-end)
		   (setq head-end (point)))
	       (gits-wash-diff-propertize-hunk head-beg head-end hunk-beg)
	       (setq hunk-beg (point)))
	      ((string-match "\\+" prefix)
	       (gits-put-line-property 'face '(:foreground "blue1")))
	      ((string-match "-" prefix)
	       (gits-put-line-property 'face '(:foreground "red")))))
      (forward-line)
      (beginning-of-line))
    (gits-wash-diff-propertize-diff head-beg head-end)
    (gits-wash-diff-propertize-hunk head-beg head-end hunk-beg)))

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
			  (abbreviate-file-name default-directory))))
	(insert "\n")
	(gits-insert-output "Untracked files:" 'gits-wash-other-files
			    "git" "ls-files" "--others" "--exclude-standard")
	(gits-insert-output "Local changes:" 'gits-wash-diff
			    "git" "diff")
	(gits-insert-output "Staged changes:" 'gits-wash-diff
			    "git" "diff" "--cached")))))

(defun git-status (dir)
  (interactive (list (gits-read-top-dir current-prefix-arg)))
  (let ((buf (get-buffer-create "*git-status*")))
    (switch-to-buffer buf)
    (setq default-directory dir)
    (gits-update-status)))

;;; Staging

(defun gits-write-diff-patch (info file)
  (write-region (elt info 1) (elt info 2) file))

(defun gits-write-hunk-patch (info file)
  (write-region (elt info 1) (elt info 2) file)
  (write-region (elt info 3) (elt info 4) file t))

(defun gits-stage-thing-at-point ()
  (interactive)
  (let ((info (get-char-property (point) 'gits-info)))
    (if info
	(case (car info)
	  ((other-file)
	   (gits-run "git" "add" (cadr info)))
	  ((hunk)
	   (gits-write-hunk-patch info ".git/gits-tmp")
	   (gits-run "git" "apply" "--cached" ".git/gits-tmp"))
	  ((diff)
	   (gits-write-diff-patch info ".git/gits-tmp")
	   (gits-run "git" "apply" "--cached" ".git/gits-tmp"))))))

(defun gits-unstage-thing-at-point ()
  (interactive)
  (let ((info (get-char-property (point) 'gits-info)))
    (if info
	(case (car info)
	  ((hunk)
	   (gits-write-hunk-patch info ".git/gits-tmp")
	   (gits-run "git" "apply" "--cached" "--reverse" ".git/gits-tmp"))
	  ((diff)
	   (gits-write-diff-patch info ".git/gits-tmp")
	   (gits-run "git" "apply" "--cached" "--reverse" ".git/gits-tmp"))))))

(defun gits-ignore-thing-at-point ()
  (interactive)
  (let ((info (get-char-property (point) 'gits-info)))
    (if info
	(case (car info)
	  ((other-file)
	   (append-to-file (concat (cadr info) "\n") nil ".gitignore")
	   (gits-update-status))))))

(defun gits-visit-thing-at-point ()
  (interactive)
  (let ((info (get-char-property (point) 'gits-info)))
    (message "visit %S" info)
    (if info
	(case (car info)
	  ((other-file)
	   (find-file (cadr info)))))))

;;; Push and pull

(defun git-pull ()
  (interactive)
  (gits-run "git" "pull" "-v"))

(defun git-push ()
  (interactive)
  (gits-run "git" "push" "-v"))

;;; Commit

(defvar gits-log-edit-map nil)

(when (not gits-log-edit-map)
  (setq gits-log-edit-map (make-sparse-keymap))
  (define-key gits-log-edit-map (kbd "C-c C-c") 'gits-log-edit-commit))

(defvar gits-pre-log-edit-window-configuration nil)

(defun gits-log-edit-commit ()
  (interactive)
  (if (> (buffer-size) 0)
      (write-region (point-min) (point-max) ".git/gits-log")
    (write-region "(Empty description)" nil ".git/gits-log"))
  (erase-buffer)
  (gits-run "git-commit" "-F" ".git/gits-log")
  (bury-buffer)
  (when gits-pre-log-edit-window-configuration
    (set-window-configuration gits-pre-log-edit-window-configuration)
    (setq gits-pre-log-edit-window-configuration nil)))

(defun git-log-edit ()
  (interactive)
  (let ((dir default-directory)
	(buf (get-buffer-create "*git-log-edit*")))
    (setq gits-pre-log-edit-window-configuration (current-window-configuration))
    (pop-to-buffer buf)
    (setq default-directory dir)
    (use-local-map gits-log-edit-map)
    (message "Use C-c C-c when done.")))

;;; Misc

(defun git-stage-all ()
  (interactive)
  (gits-run "git-add" "-u" "."))
