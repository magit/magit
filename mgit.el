;;; mgit -- control git from Emacs.

;; Copyright (C) 2008  Marius Vollmer
;;
;; Mgit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Mgit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Introduction

;; Invoking the mgit-status function will show a buffer with the
;; current status of the current git repository and its checkout.
;; That buffer offers key bindings for manipulating the status in
;; simple ways.
;;
;; The status buffer mainly shows the difference between the working
;; tree and the index, and the difference between the index and the
;; current HEAD.  You can add individual hunks from the working tree
;; to the index, and you can commit the index.

;;; TODO

;; - Reloading of changed files after finishing commands
;; - Detect and handle renames and copies.
;; - History browsing

;;; Utilities

(defun mgit-shell (cmd &rest args)
  (let ((str (shell-command-to-string (apply 'format cmd args))))
    (if (string= str "")
	nil
      (if (equal (elt str (- (length str) 1)) ?\n)
	  (substring str 0 (- (length str) 1))
	str))))

(defun mgit-concat-with-delim (delim seqs)
  (cond ((null seqs)
	 nil)
	((null (cdr seqs))
	 (car seqs))
	(t
	 (concat (car seqs) delim (mgit-concat-with-delim delim (cdr seqs))))))

(defun mgit-get (&rest keys)
  (mgit-shell "git-config %s" (mgit-concat-with-delim "." keys)))

(defun mgit-set (val &rest keys)
  (if val
      (mgit-shell "git-config %s %s" (mgit-concat-with-delim "." keys) val)
    (mgit-shell "git-config --unset %s" (mgit-concat-with-delim "." keys))))

(defun mgit-get-top-dir (cwd)
  (let* ((cwd (expand-file-name cwd))
	 (mgit-dir (mgit-shell "cd '%s' && git-rev-parse --git-dir 2>/dev/null"
			      cwd)))
    (if mgit-dir
	(file-name-as-directory (or (file-name-directory mgit-dir) cwd))
      nil)))

(defun mgit-get-ref (ref)
  (mgit-shell "git-symbolic-ref -q %s" ref))

(defun mgit-get-current-branch ()
  (let* ((head (mgit-get-ref "HEAD"))
	 (pos (and head (string-match "^refs/heads/" head))))
    (if pos
	(substring head 11)
      nil)))

(defun mgit-read-top-dir (prefix)
  (let ((dir (mgit-get-top-dir default-directory)))
    (if prefix
	(mgit-get-top-dir (read-directory-name "Git repository: " dir))
      dir)))

(defun mgit-insert-output (title washer cmd &rest args)
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

(defun mgit-put-line-property (prop val)
  (put-text-property (line-beginning-position) (line-end-position)
		     prop val))

;;; Running asynchronous commands

(defvar mgit-process nil)

(defun mgit-run (cmd &rest args)
  (or (not mgit-process)
      (error "Git is already running."))
  (let ((dir default-directory)
	(buf (get-buffer-create "*git-process*")))
    (save-excursion
      (set-buffer buf)
      (setq default-directory dir)
      (erase-buffer)
      (insert "$ " (mgit-concat-with-delim " " (cons cmd args)) "\n")
      (setq mgit-process (apply 'start-process "git" buf cmd args))
      (set-process-sentinel mgit-process 'mgit-process-sentinel))))

(defun mgit-process-sentinel (process event)
  (cond ((string= event "finished\n")
	 (message "Git finished.")
	 (setq mgit-process nil))
	((string= event "killed\n")
	 (message "Git was killed.")
	 (setq mgit-process nil))
	((string-match "exited abnormally" event)
	 (message "Git failed.")
	 (setq mgit-process nil))
	(t
	 (message "Git is weird.")))
  (mgit-update-status))

(defun mgit-display-process ()
  (interactive)
  (display-buffer "*git-process*"))

;;; Keymap

(defvar mgit-keymap nil)

(when (not mgit-keymap)
  (setq mgit-keymap (make-keymap))
  (suppress-keymap mgit-keymap)
  (define-key mgit-keymap (kbd "g") 'mgit-status)
  (define-key mgit-keymap (kbd "A") 'mgit-stage-all)
  (define-key mgit-keymap (kbd "a") 'mgit-stage-thing-at-point)
  (define-key mgit-keymap (kbd "u") 'mgit-unstage-thing-at-point)
  (define-key mgit-keymap (kbd "i") 'mgit-ignore-thing-at-point)
  (define-key mgit-keymap (kbd "RET") 'mgit-visit-thing-at-point)
  (define-key mgit-keymap (kbd "b") 'mgit-switch-branch)
  (define-key mgit-keymap (kbd "B") 'mgit-create-branch)
  (define-key mgit-keymap (kbd "m") 'mgit-manual-merge)
  (define-key mgit-keymap (kbd "M") 'mgit-automatic-merge)
  (define-key mgit-keymap (kbd "U") 'mgit-pull)
  (define-key mgit-keymap (kbd "P") 'mgit-push)
  (define-key mgit-keymap (kbd "c") 'mgit-log-edit)
  (define-key mgit-keymap (kbd "p") 'mgit-display-process))

;;; Status

(defun mgit-wash-other-files (status)
  (goto-char (point-min))
  (while (not (eobp))
    (let ((filename (buffer-substring (point) (line-end-position))))
      (insert "  ")
      (mgit-put-line-property 'face '(:foreground "red"))
      (mgit-put-line-property 'mgit-info (list 'other-file filename)))
    (forward-line)
    (beginning-of-line)))

(defun mgit-wash-diff-propertize-diff (head-beg head-end)
  (let ((head-end (or head-end (point))))
    (when head-beg
      (put-text-property head-beg head-end
			 'mgit-info (list 'diff
					  head-beg (point))))))

(defun mgit-wash-diff-propertize-hunk (head-beg head-end hunk-beg)
  (when hunk-beg
    (put-text-property hunk-beg (point)
		       'mgit-info (list 'hunk
					head-beg head-end
					hunk-beg (point)))))

(defun mgit-wash-diff (status)
  (goto-char (point-min))
  (let ((n-files 1)
	(head-beg nil)
	(head-end nil)
	(hunk-beg nil))
    (while (not (eobp))
      (let ((prefix (buffer-substring-no-properties
		     (point) (+ (point) n-files))))
	(cond ((looking-at "^diff")
	       (mgit-wash-diff-propertize-diff head-beg head-end)
	       (setq head-beg (point))
	       (setq head-end nil))
	      ((looking-at "^@+")
	       (setq n-files (- (length (match-string 0)) 1))
	       (if (null head-end)
		   (setq head-end (point)))
	       (mgit-wash-diff-propertize-hunk head-beg head-end hunk-beg)
	       (setq hunk-beg (point)))
	      ((string-match "\\+" prefix)
	       (mgit-put-line-property 'face '(:foreground "blue1")))
	      ((string-match "-" prefix)
	       (mgit-put-line-property 'face '(:foreground "red")))))
      (forward-line)
      (beginning-of-line))
    (mgit-wash-diff-propertize-diff head-beg head-end)
    (mgit-wash-diff-propertize-hunk head-beg head-end hunk-beg)))

(defun mgit-update-status ()
  (let ((buf (get-buffer "*git-status*")))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only t)
      (use-local-map mgit-keymap)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(let* ((branch (mgit-get-current-branch))
	       (remote (and branch (mgit-get "branch" branch "remote"))))
	  (if remote
	      (insert (format "Remote: %s %s\n"
			      remote (mgit-get "remote" remote "url"))))
	  (insert (format "Local:  %s %s\n"
			  (propertize (or branch "(detached)") 'face 'bold)
			  (abbreviate-file-name default-directory)))
	  (insert "\n")
	  (mgit-insert-output "Untracked files:" 'mgit-wash-other-files
			      "git" "ls-files" "--others" "--exclude-standard")
	  (mgit-insert-output "Unstaged changes:" 'mgit-wash-diff
			      "git" "diff")
	  (mgit-insert-output "Staged changes:" 'mgit-wash-diff
			      "git" "diff" "--cached")
	  (if remote
	      (mgit-insert-output "Unpushed changes:" 'mgit-wash-diff
				  "git" "diff" "--stat"
				  (format "%s/%s..HEAD" remote branch))))))))

(defun mgit-status (dir)
  (interactive (list (mgit-read-top-dir current-prefix-arg)))
  (save-some-buffers)
  (let ((buf (get-buffer-create "*git-status*")))
    (switch-to-buffer buf)
    (setq default-directory dir)
    (mgit-update-status)))

;;; Staging

(defun mgit-write-diff-patch (info file)
  (write-region (elt info 1) (elt info 2) file))

(defun mgit-write-hunk-patch (info file)
  (write-region (elt info 1) (elt info 2) file)
  (write-region (elt info 3) (elt info 4) file t))

(defun mgit-hunk-is-conflict-p (info)
  (save-excursion
    (goto-char (elt info 1))
    (looking-at-p "^diff --cc")))

(defun mgit-diff-conflict-file (info)
  (save-excursion
    (goto-char (elt info 1))
    (if (looking-at "^diff --cc +\\(.*\\)$")
	(match-string 1)
      nil)))

(defun mgit-diff-info-file (info)
  (save-excursion
    (goto-char (elt info 1))
    (cond ((looking-at "^diff --git a/\\(.*\\) b/\\(.*\\)$")
	   (match-string 2))
	  ((looking-at "^diff --cc +\\(.*\\)$")
	   (match-string 1))
	  (t
	   nil))))

(defun mgit-diff-info-position (info)
  (save-excursion
    (cond ((eq (car info) 'hunk)
	   (goto-char (elt info 3))
	   (if (looking-at "@@+ .* \\+\\([0-9]+\\),[0-9]+ @@+")
	       (parse-integer (match-string 1))
	     nil))
	  (t nil))))
  
(defun mgit-stage-thing-at-point ()
  (interactive)
  (let ((info (get-char-property (point) 'mgit-info)))
    (if info
	(case (car info)
	  ((other-file)
	   (mgit-run "git" "add" (cadr info)))
	  ((hunk)
	   (if (mgit-hunk-is-conflict-p info)
	       (error
"Can't stage individual resolution hunks.  Please stage the whole file."))
	   (mgit-write-hunk-patch info ".git/mgit-tmp")
	   (mgit-run "git" "apply" "--cached" ".git/mgit-tmp"))
	  ((diff)
	   (mgit-run "git" "add" (mgit-diff-info-file info)))))))

(defun mgit-unstage-thing-at-point ()
  (interactive)
  (let ((info (get-char-property (point) 'mgit-info)))
    (if info
	(case (car info)
	  ((hunk)
	   (mgit-write-hunk-patch info ".git/mgit-tmp")
	   (mgit-run "git" "apply" "--cached" "--reverse" ".git/mgit-tmp"))
	  ((diff)
	   (mgit-run "git" "reset" "HEAD" (mgit-diff-info-file info)))))))

(defun mgit-ignore-thing-at-point ()
  (interactive)
  (let ((info (get-char-property (point) 'mgit-info)))
    (if info
	(case (car info)
	  ((other-file)
	   (append-to-file (concat (cadr info) "\n") nil ".gitignore")
	   (mgit-update-status))))))

(defun mgit-visit-thing-at-point ()
  (interactive)
  (let ((info (get-char-property (point) 'mgit-info)))
    (if info
	(case (car info)
	  ((other-file)
	   (find-file (cadr info)))
	  ((diff hunk)
	   (let ((file (mgit-diff-info-file info))
		 (position (mgit-diff-info-position info)))
	     (find-file file)
	     (if position
		 (goto-line position))))))))

;;; Branches

(defun mgit-list-branches ()
  (delete "." (delete ".." (directory-files ".git/refs/heads/"))))

(defun mgit-read-other-branch (prompt)
  (completing-read prompt (delete (mgit-get-current-branch)
				  (mgit-list-branches))
		   nil t))

(defun mgit-switch-branch (branch)
  (interactive (list (mgit-read-other-branch "Switch to branch: ")))
  (mgit-run "git" "checkout" branch))

(defun mgit-read-create-branch-args ()
  (let* ((branches (mgit-list-branches))
	 (cur-branch (mgit-get-current-branch))
	 (branch (read-string "Create branch: "))
	 (parent (completing-read "Parent: " branches nil t cur-branch)))
    (list branch parent)))

(defun mgit-create-branch (branch parent)
  (interactive (mgit-read-create-branch-args))
  (mgit-run "git" "checkout" "-b" branch parent))

;;; Merging

(defun mgit-manual-merge (branch)
  (interactive (list (mgit-read-other-branch "Manually merge from branch: ")))
  (mgit-run "git" "merge" "--no-ff" "--no-commit" branch))

(defun mgit-automatic-merge (branch)
  (interactive (list (mgit-read-other-branch "Merge from branch: ")))
  (mgit-run "git" "merge" branch))

;;; Push and pull

(defun mgit-pull ()
  (interactive)
  (mgit-run "git" "pull" "-v"))

(defun mgit-push ()
  (interactive)
  (mgit-run "git" "push" "-v"))

;;; Commit

(defvar mgit-log-edit-map nil)

(when (not mgit-log-edit-map)
  (setq mgit-log-edit-map (make-sparse-keymap))
  (define-key mgit-log-edit-map (kbd "C-c C-c") 'mgit-log-edit-commit))

(defvar mgit-pre-log-edit-window-configuration nil)

(defun mgit-log-edit-cleanup ()
  (save-excursion
    (goto-char (point-min))
    (flush-lines "^#")
    (goto-char (point-min))
    (replace-regexp "[ \t\n]*\\'" "\n")))

(defun mgit-log-edit-commit ()
  (interactive)
  (mgit-log-edit-cleanup)
  (if (> (buffer-size) 0)
      (write-region (point-min) (point-max) ".git/mgit-log")
    (write-region "(Empty description)" nil ".git/mgit-log"))
  (erase-buffer)
  (mgit-run "git-commit" "-F" ".git/mgit-log")
  (bury-buffer)
  (when mgit-pre-log-edit-window-configuration
    (set-window-configuration mgit-pre-log-edit-window-configuration)
    (setq mgit-pre-log-edit-window-configuration nil)))

(defun mgit-log-edit ()
  (interactive)
  (let ((dir default-directory)
	(buf (get-buffer-create "*git-log-edit*")))
    (setq mgit-pre-log-edit-window-configuration (current-window-configuration))
    (pop-to-buffer buf)
    (setq default-directory dir)
    (use-local-map mgit-log-edit-map)
    (save-excursion
      (mgit-log-edit-cleanup)
      (if  (and (= (buffer-size) 0)
		(file-exists-p ".git/MERGE_MSG"))
	  (insert-file-contents ".git/MERGE_MSG")))
    (message "Use C-c C-c when done.")))

;;; Misc

(defun mgit-stage-all ()
  (interactive)
  (mgit-run "git-add" "-u" "."))
