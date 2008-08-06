;;; magit -- control git from Emacs.

;; Copyright (C) 2008  Marius Vollmer
;;
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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Introduction

;; Invoking the magit-status function will show a buffer with the
;; current status of the current git repository and its checkout.
;; That buffer offers key bindings for manipulating the status in
;; simple ways.
;;
;; The status buffer mainly shows the difference between the working
;; tree and the index, and the difference between the index and the
;; current HEAD.  You can add individual hunks from the working tree
;; to the index, and you can commit the index.

;;; TODO

;; - Documentation, major mode, menu, ...
;; - History browsing
;; - Detect and handle renames and copies.

;;; Utilities

(defun magit-shell (cmd &rest args)
  (let ((str (shell-command-to-string (apply 'format cmd args))))
    (if (string= str "")
	nil
      (if (equal (elt str (- (length str) 1)) ?\n)
	  (substring str 0 (- (length str) 1))
	str))))

(defun magit-shell-lines (cmd &rest args)
  (let ((str (shell-command-to-string (apply 'format cmd args))))
    (if (string= str "")
	nil
      (let ((lines (nreverse (split-string str "\n"))))
	(if (string= (car lines) "")
	    (setq lines (cdr lines)))
	(nreverse lines)))))

(defun magit-file-lines (file)
  (if (file-exists-p file)
      (magit-shell-lines "cat '%s'" file)
    nil))

(defun magit-concat-with-delim (delim seqs)
  (cond ((null seqs)
	 nil)
	((null (cdr seqs))
	 (car seqs))
	(t
	 (concat (car seqs) delim (magit-concat-with-delim delim (cdr seqs))))))

(defun magit-get (&rest keys)
  (magit-shell "git-config %s" (magit-concat-with-delim "." keys)))

(defun magit-set (val &rest keys)
  (if val
      (magit-shell "git-config %s %s" (magit-concat-with-delim "." keys) val)
    (magit-shell "git-config --unset %s" (magit-concat-with-delim "." keys))))

(defun magit-get-top-dir (cwd)
  (let* ((cwd (expand-file-name cwd))
	 (magit-dir (magit-shell "cd '%s' && git-rev-parse --git-dir 2>/dev/null"
			      cwd)))
    (if magit-dir
	(file-name-as-directory (or (file-name-directory magit-dir) cwd))
      nil)))

(defun magit-get-ref (ref)
  (magit-shell "git-symbolic-ref -q %s" ref))

(defun magit-get-current-branch ()
  (let* ((head (magit-get-ref "HEAD"))
	 (pos (and head (string-match "^refs/heads/" head))))
    (if pos
	(substring head 11)
      nil)))

(defun magit-read-top-dir (prefix)
  (let ((dir (magit-get-top-dir default-directory)))
    (if prefix
	(magit-get-top-dir (read-directory-name "Git repository: " dir))
      dir)))

(defun magit-insert-output (title washer cmd &rest args)
  (if title
      (insert (propertize title 'face 'bold) "\n"))
  (let* ((beg (point))
	 (status (apply 'call-process cmd nil t nil args))
	 (end (point)))
    (if washer
	(save-restriction
	  (narrow-to-region beg (point))
	  (funcall washer status)
	  (goto-char (point-max))
	  (insert "\n")))))

(defun magit-put-line-property (prop val)
  (put-text-property (line-beginning-position) (line-end-position)
		     prop val))

;;; Running asynchronous commands

(defvar magit-process nil)

(defun magit-run (cmd &rest args)
  (or (not magit-process)
      (error "Git is already running."))
  (let ((dir default-directory)
	(buf (get-buffer-create "*git-process*")))
    (save-excursion
      (set-buffer buf)
      (setq default-directory dir)
      (erase-buffer)
      (insert "$ " (magit-concat-with-delim " " (cons cmd args)) "\n")
      (setq magit-process (apply 'start-process "git" buf cmd args))
      (set-process-sentinel magit-process 'magit-process-sentinel))))

(defun magit-revert-files ()
  (let ((files (magit-shell-lines "git ls-files")))
    (dolist (file files)
      (let ((buffer (find-buffer-visiting file)))
	(when (and buffer
		   (not (verify-visited-file-modtime buffer))
		   (not (buffer-modified-p buffer)))
	  (with-current-buffer buffer
	    (ignore-errors
	      (revert-buffer t t t))))))))

(defun magit-process-sentinel (process event)
  (cond ((string= event "finished\n")
	 (message "Git finished.")
	 (setq magit-process nil))
	((string= event "killed\n")
	 (message "Git was killed.")
	 (setq magit-process nil))
	((string-match "exited abnormally" event)
	 (message "Git failed.")
	 (setq magit-process nil))
	(t
	 (message "Git is weird.")))
  (magit-revert-files)
  (magit-update-status))

(defun magit-display-process ()
  (interactive)
  (display-buffer "*git-process*"))

;;; Keymap

(defvar magit-keymap nil)

(when (not magit-keymap)
  (setq magit-keymap (make-keymap))
  (suppress-keymap magit-keymap)
  (define-key magit-keymap (kbd "g") 'magit-status)
  (define-key magit-keymap (kbd "A") 'magit-stage-all)
  (define-key magit-keymap (kbd "a") 'magit-stage-thing-at-point)
  (define-key magit-keymap (kbd "u") 'magit-unstage-thing-at-point)
  (define-key magit-keymap (kbd "i") 'magit-ignore-thing-at-point)
  (define-key magit-keymap (kbd "RET") 'magit-visit-thing-at-point)
  (define-key magit-keymap (kbd "b") 'magit-switch-branch)
  (define-key magit-keymap (kbd "B") 'magit-create-branch)
  (define-key magit-keymap (kbd "m") 'magit-manual-merge)
  (define-key magit-keymap (kbd "M") 'magit-automatic-merge)
  (define-key magit-keymap (kbd "U") 'magit-pull)
  (define-key magit-keymap (kbd "P") 'magit-push)
  (define-key magit-keymap (kbd "c") 'magit-log-edit)
  (define-key magit-keymap (kbd "p") 'magit-display-process))

;;; Status

(defun magit-wash-other-files (status)
  (goto-char (point-min))
  (while (not (eobp))
    (let ((filename (buffer-substring (point) (line-end-position))))
      (insert "  ")
      (magit-put-line-property 'face '(:foreground "red"))
      (magit-put-line-property 'magit-info (list 'other-file filename)))
    (forward-line)
    (beginning-of-line)))

(defun magit-wash-diff-propertize-diff (head-beg head-end)
  (let ((head-end (or head-end (point))))
    (when head-beg
      (put-text-property head-beg head-end
			 'magit-info (list 'diff
					  head-beg (point))))))

(defun magit-wash-diff-propertize-hunk (head-beg head-end hunk-beg)
  (when hunk-beg
    (put-text-property hunk-beg (point)
		       'magit-info (list 'hunk
					head-beg head-end
					hunk-beg (point)))))

(defun magit-wash-diff (status)
  (goto-char (point-min))
  (let ((n-files 1)
	(head-beg nil)
	(head-end nil)
	(hunk-beg nil))
    (while (not (eobp))
      (let ((prefix (buffer-substring-no-properties
		     (point) (+ (point) n-files))))
	(cond ((looking-at "^diff")
	       (magit-wash-diff-propertize-diff head-beg head-end)
	       (setq head-beg (point))
	       (setq head-end nil))
	      ((looking-at "^@+")
	       (setq n-files (- (length (match-string 0)) 1))
	       (if (null head-end)
		   (setq head-end (point)))
	       (magit-wash-diff-propertize-hunk head-beg head-end hunk-beg)
	       (setq hunk-beg (point)))
	      ((string-match "\\+" prefix)
	       (magit-put-line-property 'face '(:foreground "blue1")))
	      ((string-match "-" prefix)
	       (magit-put-line-property 'face '(:foreground "red")))))
      (forward-line)
      (beginning-of-line))
    (magit-wash-diff-propertize-diff head-beg head-end)
    (magit-wash-diff-propertize-hunk head-beg head-end hunk-beg)))

(defun magit-update-status ()
  (let ((buf (get-buffer "*git-status*")))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only t)
      (use-local-map magit-keymap)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(let* ((branch (magit-get-current-branch))
	       (remote (and branch (magit-get "branch" branch "remote"))))
	  (if remote
	      (insert (format "Remote: %s %s\n"
			      remote (magit-get "remote" remote "url"))))
	  (insert (format "Local:  %s %s\n"
			  (propertize (or branch "(detached)") 'face 'bold)
			  (abbreviate-file-name default-directory)))
	  (insert "\n")
	  (magit-insert-output "Untracked files:" 'magit-wash-other-files
			      "git" "ls-files" "--others" "--exclude-standard")
	  (magit-insert-output "Unstaged changes:" 'magit-wash-diff
			      "git" "diff")
	  (magit-insert-output "Staged changes:" 'magit-wash-diff
			      "git" "diff" "--cached")
	  (if remote
	      (magit-insert-output "Unpushed changes:" 'nil
				  "git" "diff" "--stat"
				  (format "%s/%s..HEAD" remote branch))))))))

(defun magit-status (dir)
  (interactive (list (magit-read-top-dir current-prefix-arg)))
  (save-some-buffers)
  (let ((buf (get-buffer-create "*git-status*")))
    (switch-to-buffer buf)
    (setq default-directory dir)
    (magit-update-status)))

;;; Staging

(defun magit-write-diff-patch (info file)
  (write-region (elt info 1) (elt info 2) file))

(defun magit-write-hunk-patch (info file)
  (write-region (elt info 1) (elt info 2) file)
  (write-region (elt info 3) (elt info 4) file t))

(defun magit-hunk-is-conflict-p (info)
  (save-excursion
    (goto-char (elt info 1))
    (looking-at-p "^diff --cc")))

(defun magit-diff-conflict-file (info)
  (save-excursion
    (goto-char (elt info 1))
    (if (looking-at "^diff --cc +\\(.*\\)$")
	(match-string 1)
      nil)))

(defun magit-diff-info-file (info)
  (save-excursion
    (goto-char (elt info 1))
    (cond ((looking-at "^diff --git a/\\(.*\\) b/\\(.*\\)$")
	   (match-string 2))
	  ((looking-at "^diff --cc +\\(.*\\)$")
	   (match-string 1))
	  (t
	   nil))))

(defun magit-diff-info-position (info)
  (save-excursion
    (cond ((eq (car info) 'hunk)
	   (goto-char (elt info 3))
	   (if (looking-at "@@+ .* \\+\\([0-9]+\\),[0-9]+ @@+")
	       (parse-integer (match-string 1))
	     nil))
	  (t nil))))
  
(defun magit-stage-thing-at-point ()
  (interactive)
  (let ((info (get-char-property (point) 'magit-info)))
    (if info
	(case (car info)
	  ((other-file)
	   (magit-run "git" "add" (cadr info)))
	  ((hunk)
	   (if (magit-hunk-is-conflict-p info)
	       (error
"Can't stage individual resolution hunks.  Please stage the whole file."))
	   (magit-write-hunk-patch info ".git/magit-tmp")
	   (magit-run "git" "apply" "--cached" ".git/magit-tmp"))
	  ((diff)
	   (magit-run "git" "add" (magit-diff-info-file info)))))))

(defun magit-unstage-thing-at-point ()
  (interactive)
  (let ((info (get-char-property (point) 'magit-info)))
    (if info
	(case (car info)
	  ((hunk)
	   (magit-write-hunk-patch info ".git/magit-tmp")
	   (magit-run "git" "apply" "--cached" "--reverse" ".git/magit-tmp"))
	  ((diff)
	   (magit-run "git" "reset" "HEAD" (magit-diff-info-file info)))))))

(defun magit-stage-all ()
  (interactive)
  (magit-run "git-add" "-u" "."))

;;; Branches

(defun magit-list-branches ()
  (magit-shell-lines "git branch -a | cut -c3-"))

(defun magit-read-other-branch (prompt)
  (completing-read prompt (delete (magit-get-current-branch)
				  (magit-list-branches))
		   nil t))

(defun magit-switch-branch (branch)
  (interactive (list (magit-read-other-branch "Switch to branch: ")))
  (if (and branch (not (string= branch "")))
      (magit-run "git" "checkout" branch)))
  
(defun magit-read-create-branch-args ()
  (let* ((branches (magit-list-branches))
	 (cur-branch (magit-get-current-branch))
	 (branch (read-string "Create branch: "))
	 (parent (completing-read "Parent: " branches nil t cur-branch)))
    (list branch parent)))

(defun magit-create-branch (branch parent)
  (interactive (magit-read-create-branch-args))
  (if (and branch (not (string= branch ""))
	   parent (not (string= parent "")))
      (magit-run "git" "checkout" "-b" branch parent)))

;;; Merging

(defun magit-manual-merge (branch)
  (interactive (list (magit-read-other-branch "Manually merge from branch: ")))
  (magit-run "git" "merge" "--no-ff" "--no-commit" branch))

(defun magit-automatic-merge (branch)
  (interactive (list (magit-read-other-branch "Merge from branch: ")))
  (magit-run "git" "merge" branch))

;;; Push and pull

(defun magit-pull ()
  (interactive)
  (magit-run "git" "pull" "-v"))

(defun magit-push ()
  (interactive)
  (magit-run "git" "push" "-v"))

;;; Commit

(defvar magit-log-edit-map nil)

(when (not magit-log-edit-map)
  (setq magit-log-edit-map (make-sparse-keymap))
  (define-key magit-log-edit-map (kbd "C-c C-c") 'magit-log-edit-commit))

(defvar magit-pre-log-edit-window-configuration nil)

(defun magit-log-edit-cleanup ()
  (save-excursion
    (goto-char (point-min))
    (flush-lines "^#")
    (goto-char (point-min))
    (replace-regexp "[ \t\n]*\\'" "\n")))

(defun magit-log-edit-commit ()
  (interactive)
  (magit-log-edit-cleanup)
  (if (> (buffer-size) 0)
      (write-region (point-min) (point-max) ".git/magit-log")
    (write-region "(Empty description)" nil ".git/magit-log"))
  (erase-buffer)
  (magit-run "git-commit" "-F" ".git/magit-log")
  (bury-buffer)
  (when magit-pre-log-edit-window-configuration
    (set-window-configuration magit-pre-log-edit-window-configuration)
    (setq magit-pre-log-edit-window-configuration nil)))

(defun magit-log-edit ()
  (interactive)
  (let ((dir default-directory)
	(buf (get-buffer-create "*git-log-edit*")))
    (setq magit-pre-log-edit-window-configuration (current-window-configuration))
    (pop-to-buffer buf)
    (setq default-directory dir)
    (use-local-map magit-log-edit-map)
    (save-excursion
      (magit-log-edit-cleanup)
      (if  (and (= (buffer-size) 0)
		(file-exists-p ".git/MERGE_MSG"))
	  (insert-file-contents ".git/MERGE_MSG")))
    (message "Use C-c C-c when done.")))

;;; Miscellaneous

(defun magit-ignore-thing-at-point ()
  (interactive)
  (let ((info (get-char-property (point) 'magit-info)))
    (if info
	(case (car info)
	  ((other-file)
	   (append-to-file (concat (cadr info) "\n") nil ".gitignore")
	   (magit-update-status))))))

(defun magit-visit-thing-at-point ()
  (interactive)
  (let ((info (get-char-property (point) 'magit-info)))
    (if info
	(case (car info)
	  ((other-file)
	   (find-file (cadr info)))
	  ((diff hunk)
	   (let ((file (magit-diff-info-file info))
		 (position (magit-diff-info-position info)))
	     (find-file file)
	     (if position
		 (goto-line position))))))))

