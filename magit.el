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

;; - Polish history browsing.  Scroll when leaning on RET, etc.
;; - Indicate active command in modeline.  Show progress in message area?
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

(defun magit-name-rev (rev)
  (magit-shell "git name-rev --always --name-only %s" rev))

(defun magit-put-line-property (prop val)
  (put-text-property (line-beginning-position) (line-end-position)
		     prop val))

;;; Sections

(defun magit-insert-section (section title washer cmd &rest args)
  (let ((section-beg (point)))
    (if title
	(insert (propertize title 'face 'bold) "\n"))
    (let* ((beg (point))
	   (status (apply 'call-process cmd nil t nil args))
	   (end (point)))
      (insert "\n")
      (put-text-property section-beg (point) 'magit-section (list section))
      (if washer
	  (save-restriction
	    (narrow-to-region beg (point))
	    (funcall washer status)
	    (goto-char (point-max)))))))

(defun magit-section-head (section n)
  (if (<= (length section) n)
      (subseq section 0 n)
    (append section (make-list (- n (length section)) nil))))

(defun magit-section-prefix-p (prefix section)
  (and prefix 
       (<= (length prefix) (length section))
       (equal prefix (subseq section 0 (length prefix)))))

(defun magit-mark-subsection (beg end subsection level)
  (let* ((section (get-text-property beg 'magit-section))
	 (new (append (magit-section-head section level)
		      (list subsection))))
    (put-text-property beg end 'magit-section new)))

(defun magit-section-at-point ()
  (get-text-property (point) 'magit-section))

(defun magit-goto-section (section)
  (let ((goal-pos (point)))
    (goto-char (point-min))
    (while (not (eobp))
      (if (magit-section-prefix-p (get-text-property (point) 'magit-section)
				  section)
	  (setq goal-pos (point)))
      (goto-char (or (next-single-property-change (point)
						  'magit-section)
		     (point-max))))
    (goto-char goal-pos)))

;;; Running asynchronous commands

(defvar magit-process nil)

(defun magit-run (cmd &rest args)
  (or (not magit-process)
      (error "Git is already running."))
  (let ((dir default-directory)
	(buf (get-buffer-create "*magit-process*")))
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
  (magit-update-status (magit-find-status-buffer)))

(defun magit-display-process ()
  (interactive)
  (display-buffer "*magit-process*"))

;;; Mode

(defvar magit-mode-map nil)

(when (not magit-mode-map)
  (setq magit-mode-map (make-keymap))
  (suppress-keymap magit-mode-map)
  (define-key magit-mode-map (kbd "g") 'magit-status)
  (define-key magit-mode-map (kbd "A") 'magit-stage-all)
  (define-key magit-mode-map (kbd "a") 'magit-stage-thing-at-point)
  (define-key magit-mode-map (kbd "u") 'magit-unstage-thing-at-point)
  (define-key magit-mode-map (kbd "i") 'magit-ignore-thing-at-point)
  (define-key magit-mode-map (kbd "?") 'magit-describe-thing-at-point)
  (define-key magit-mode-map (kbd "x") 'magit-reset-soft)
  (define-key magit-mode-map (kbd "X") 'magit-reset-hard)
  (define-key magit-mode-map (kbd "RET") 'magit-visit-thing-at-point)
  (define-key magit-mode-map (kbd "b") 'magit-switch-branch)
  (define-key magit-mode-map (kbd "B") 'magit-create-branch)
  (define-key magit-mode-map (kbd "m") 'magit-manual-merge)
  (define-key magit-mode-map (kbd "M") 'magit-automatic-merge)
  (define-key magit-mode-map (kbd "U") 'magit-pull)
  (define-key magit-mode-map (kbd "P") 'magit-push)
  (define-key magit-mode-map (kbd "c") 'magit-log-edit)
  (define-key magit-mode-map (kbd "C") 'magit-add-log)
  (define-key magit-mode-map (kbd "l") 'magit-browse-log)
  (define-key magit-mode-map (kbd "L") 'magit-browse-branch-log)
  (define-key magit-mode-map (kbd "p") 'magit-display-process))

(defvar magit-mode-hook nil)

(put 'magit-mode 'mode-class 'special)

(defun magit-mode ()
;;; XXX - the formatting is all screwed up because of the \\[...]
;;;       constructs.
  "Review the status of a git repository and act on it.
\\<magit-mode-map>
The buffer shows your changes (in the form of 'diff hunks') on
their way into the local and remote repository.  Normally, you
collect a set of changes into the staging area that you want to
commit as a unit, and then you commit them.

Typing `\\[magit-stage-thing-at-point]' will move the diff hunk
that point is in into the staging area.  Typing
`\\[magit-unstage-thing-at-point]' will remove it from the staging
area.  When point is on a diff header for a file when you type
`\\[magit-stage-thing-at-point]' or
`\\[magit-unstage-thing-at-point]', all changes belonging to the
file will move together.  You can also type `\\[magit-stage-all]'
to stage all unstaged changes in one go.

To commit the staged changes, type `\\[magit-log-edit]'.  A
buffer will pop up where you can describe the changes.  Typing
`C-c C-c' in that buffer will peform the commit.  The content of
that buffer is preserved until you finally do commit it, so you
can go back and forth between your source files, the status
buffer, and the commit message buffer until you are ready to
commit.

The status buffer also lists untracked files; you should either
start tracking them by typing `\\[magit-stage-thing-at-point]' when
point is on one of them, or tell git to ignore them by typing
`\\[magit-ignore-thing-at-point]'.

If you make changes to your git status outside of Emacs, type
`\\[magit-status]' to refresh the status buffer.  You also need
to do this after saving one or more files.  For performance
reasons, the status buffer is not updated every time you save.
Instead, you should run `magit-status' whenever you want to look
at the status buffer.  Of course, you can bind `magit-status' to
a key to make this convenient.

If you want to remove or rename tracked files, use \"git rm\" or
\"git mv\" directly (and then refresh the status buffer to see
the changes).

Type `\\[magit-switch-branch]' to switch to another branch.  To
create a new branch, type `\\[magit-create-branch]'.

There are two commands for merging a branch into the current
branch.  Type `\\[magit-manual-merge]' to initiate a `manual'
merge: the changes resulting from the merge will not be
committed, even when there are no conflicts.  You can review them
and make modifications until you are ready to commit them.
Typing `\\[magit-automatic-merge]' will initiate a `automatic'
merge which will automatically commit the merged changes when
there are no conflicts.

While a merge is ongoing (i.e., until the merge commit has been
added to the repository), the status buffer shows the branches
that are being merged at the top.

You can `soft reset' your repository by typing
`\\[magit-reset-soft]'.  The current head will be set to the
commit that you specify, but your working tree and the staging
area are not changed.  Typing `\\[magit-reset-hard]' will do a
`hard reset': all of the current head, your working tree, and the
the index will be reverted to the commit that you specify.  Doing
a hard reset without actually changing the current head will thus
throw away all your uncommitted changes.  You can do this to
abort a merge, for example.

When you have a remote repository configured for the current
branch (such as when \"git clone\" has done this for you
automatically), it is shown at the top of the status buffer.  You
can type `\\[magit-pull]' to run \"git pull\" and
`\\[magit-push]' to run \"git push\".  Typing
`\\[magit-display-process]' will pop up a buffer with a
transcript of these commands.  There is also a section at the
bottom that gives a summary of the changes that you have not yet
pushed.

\\{magit-mode-map}"
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq major-mode 'magit-mode
	mode-name "Magit")
  (use-local-map magit-mode-map)
  (run-mode-hooks 'magit-mode-hook))

;;; Status

(defun magit-wash-other-files (status)
  (goto-char (point-min))
  (let ((seq 0))
    (while (not (eobp))
      (let ((filename (buffer-substring (point) (line-end-position))))
	(magit-mark-subsection (line-beginning-position) 
			       (+ (line-end-position) 1)
			       seq 1)
	(magit-put-line-property 'face '(:foreground "red"))
	(magit-put-line-property 'magit-info (list 'other-file filename)))
      (setq seq (+ seq 1))
      (forward-line)
    (beginning-of-line))))

(defun magit-wash-diff-propertize-diff (head-seq head-beg head-end)
  (let ((head-end (or head-end (point))))
    (when head-beg
      (put-text-property head-beg head-end
			 'magit-info (list 'diff
					   head-beg (point)))
      (magit-mark-subsection head-beg head-end head-seq 1))))

(defun magit-wash-diff-propertize-hunk (head-seq hunk-seq
					head-beg head-end hunk-beg)
  (when hunk-beg
    (put-text-property hunk-beg (point)
		       'magit-info (list 'hunk
					head-beg head-end
					hunk-beg (point)))
    (magit-mark-subsection hunk-beg (point) head-seq 1)
    (magit-mark-subsection hunk-beg (point) hunk-seq 2)))

(defun magit-wash-diff (status)
  (goto-char (point-min))
  (let ((n-files 1)
	(head-seq 0)
	(head-beg nil)
	(head-end nil)
	(hunk-seq 0)
	(hunk-beg nil))
    (while (not (eobp))
      (let ((prefix (buffer-substring-no-properties
		     (point) (+ (point) n-files))))
	(cond ((looking-at "^diff")
	       (magit-wash-diff-propertize-diff head-seq head-beg head-end)
	       (magit-wash-diff-propertize-hunk head-seq hunk-seq
						head-beg head-end hunk-beg)
	       (setq head-seq (+ head-seq 1))
	       (setq head-beg (point))
	       (setq head-end nil)
	       (setq hunk-seq 0)
	       (setq hunk-beg nil))
	      ((looking-at "^@+")
	       (setq n-files (- (length (match-string 0)) 1))
	       (if (null head-end)
		   (setq head-end (point)))
	       (magit-wash-diff-propertize-hunk head-seq hunk-seq
						head-beg head-end hunk-beg)
	       (setq hunk-seq (+ hunk-seq 1))
	       (setq hunk-beg (point)))
	      ((string-match "\\+" prefix)
	       (magit-put-line-property 'face '(:foreground "blue1")))
	      ((string-match "-" prefix)
	       (magit-put-line-property 'face '(:foreground "red")))))
      (forward-line)
      (beginning-of-line))
    (magit-wash-diff-propertize-diff head-seq head-beg head-end)
    (magit-wash-diff-propertize-hunk head-seq hunk-seq
				     head-beg head-end hunk-beg)))

(defun magit-update-status (buf)
  (with-current-buffer buf
    (let ((old-line (line-number-at-pos))
	  (old-section (magit-section-at-point))
	  (inhibit-read-only t))
      (erase-buffer)
      (let* ((branch (magit-get-current-branch))
	     (remote (and branch (magit-get "branch" branch "remote"))))
	(if remote
	    (insert (format "Remote: %s %s\n"
			    remote (magit-get "remote" remote "url"))))
	(insert (format "Local:  %s %s\n"
			(propertize (or branch "(detached)") 'face 'bold)
			(abbreviate-file-name default-directory)))
	(insert
	 (format "Head:   %s\n"
		 (magit-shell
		  "git log --max-count=1 --abbrev-commit --pretty=oneline")))
	(let ((merge-heads (magit-file-lines ".git/MERGE_HEAD")))
	  (if merge-heads
	      (insert (format "Merging: %s\n"
			      (magit-concat-with-delim
			       ", "
			       (mapcar 'magit-name-rev merge-heads))))))
	(insert "\n")
	(magit-insert-section 'untracked
			      "Untracked files:" 'magit-wash-other-files
			     "git" "ls-files" "--others" "--exclude-standard")
	(magit-insert-section 'unstaged
			      "Unstaged changes:" 'magit-wash-diff
			      "git" "diff")
	(magit-insert-section 'staged
			      "Staged changes:" 'magit-wash-diff
			      "git" "diff" "--cached")
	(if remote
	    (magit-insert-section 'unpushed
				  "Unpushed changes:" 'nil
				  "git" "diff" "--stat"
				  (format "%s/%s..HEAD" remote branch))))
      (goto-line old-line)
      (magit-goto-section old-section))))

(defun magit-find-status-buffer (&optional dir)
  (let ((topdir (magit-get-top-dir (or dir default-directory))))
    (dolist (buf (buffer-list))
      (if (save-excursion
	    (set-buffer buf)
	    (and (equal default-directory topdir)
		 (eq major-mode 'magit-mode)))
	  (return buf)))))

(defun magit-status (dir)
  (interactive (list (magit-read-top-dir current-prefix-arg)))
  (save-some-buffers)
  (let* ((topdir (magit-get-top-dir dir))
	 (buf (or (magit-find-status-buffer topdir)
		  (create-file-buffer (file-name-nondirectory
				       (directory-file-name topdir))))))
    (switch-to-buffer buf)
    (setq default-directory topdir)
    (magit-mode)
    (magit-update-status buf)))

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

(defun magit-hunk-target-line (info)
  ;; XXX - deal with combined diffs
  (save-excursion
    (beginning-of-line)
    (let ((line (line-number-at-pos)))
      (if (looking-at "-")
	  (error "Can't visit removed lines."))
      (goto-char (elt info 3))
      (if (not (looking-at "@@+ .* \\+\\([0-9]+\\),[0-9]+ @@+"))
	  (error "Hunk header not found."))
      (let ((target (parse-integer (match-string 1))))
	(forward-line)
	(while (< (line-number-at-pos) line)
	  (if (not (looking-at "-"))
	      (setq target (+ target 1)))
	  (forward-line))
	target))))

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

;;; Resetting

(defun magit-reset-soft (target)
  (interactive (list (read-string "Reset history to: " "HEAD^")))
  (magit-run "git" "reset" "--soft" target))

(defun magit-reset-hard (target)
  (interactive (list (read-string "Reset working tree (and history) to: "
				  "HEAD")))
  (if (yes-or-no-p
       (format "Hard reset to %s and throw away all uncommitted changes? "
	       target))
      (magit-run "git" "reset" "--hard" target)))

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
    (setq magit-pre-log-edit-window-configuration
	  (current-window-configuration))
    (pop-to-buffer buf)
    (setq default-directory dir)
    (use-local-map magit-log-edit-map)
    (message "Type C-c C-c to commit.")))

(defun magit-add-log ()
  (interactive)
  (let* ((fun (save-window-excursion
		 (save-excursion
		   (magit-visit-thing-at-point)
		   (add-log-current-defun))))
	 (file (magit-diff-info-file (get-text-property (point)
							'magit-info))))
    (magit-log-edit)
    (goto-char (point-min))
    (cond ((not (search-forward-regexp (format "^\\* %s" (regexp-quote file))
				       nil t))
	   ;; No entry for file, create it.
	   (goto-char (point-max))
	   (insert (format "\n* %s (%s): " file fun)))
	  (t
	   ;; found entry for file, look for fun
	   (let ((limit (or (search-forward-regexp "^\\* " nil t)
			    (point-max))))
	     (cond ((search-forward-regexp (format "(.*\\<%s\\>.*):"
						   (regexp-quote fun))
					   limit t)
		    ;; found it, goto end of current entry
		    (if (search-forward-regexp "^(" limit t)
			(backward-char 2)
		      (goto-char limit)))
		   (t
		    ;; not found insert new entry
		    (goto-char limit)
		    (beginning-of-line)
		    (open-line 1)
		    (insert (format "(%s): " fun)))))))))

;;; History browsing

(defvar magit-log-mode-map nil)

(when (not magit-log-mode-map)
  (setq magit-log-mode-map (make-keymap))
  (suppress-keymap magit-log-mode-map)
  (define-key magit-log-mode-map (kbd "RET") 'magit-show-commit)
  (define-key magit-log-mode-map (kbd "R") 'magit-revert-commit)
  (define-key magit-log-mode-map (kbd "P") 'magit-pick-commit)
  (define-key magit-log-mode-map (kbd "C") 'magit-checkout-commit)
  (define-key magit-log-mode-map (kbd "l") 'magit-log-commit)
  (define-key magit-log-mode-map (kbd "L") 'magit-browse-branch-log)
  (define-key magit-log-mode-map (kbd "q") 'magit-quit))

(defvar magit-log-mode-hook nil)

(put 'magit-log-mode 'mode-class 'special)

(defun magit-log-mode ()
  "Review commit history. \\<magit-log-mode-map>

The buffer shows a summary of the (usually non-linear) history of
changes starting form a given commit.  You can see the details of
a the commit on the current line by typing
`\\[magit-show-commit]'.  Typing `\\[magit-log-commit] will use
the commit on the current line as the new starting point for the
summary.  Typing `\\[magit-browse-branch-log]' will ask you for a
branch and show its history.

You can modify your working tree and staging area by using the
commit on the current line in a number of ways.  Typing
`\\[magit-revert-commit]' will revert the change made by the
commit in your working tree (and staging area).  Typing
`\\[magit-pick-commit]' will apply the commit.  You can use this
to `cherry pick' changes from another branch.

Typing `\\[magit-checkout-commit]' will checkout the commit on
the current line into your working tree.

\\{magit-log-mode-map}"
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (toggle-truncate-lines t)
  (setq major-mode 'magit-log-mode
	mode-name "Magit Log")
  (use-local-map magit-log-mode-map)
  (run-mode-hooks 'magit-log-mode-hook))

(defun magit-commit-at-point ()
  (let* ((info (get-text-property (point) 'magit-info))
	 (commit (and info
		      (eq (car info) 'commit)
		      (cadr info))))
    (or commit
	(error "No commit at point."))))

(defun magit-revert-commit ()
  (interactive)
  (magit-run "git" "revert" "--no-commit" (magit-commit-at-point)))

(defun magit-pick-commit ()
  (interactive)
  (magit-run "git" "cherry-pick" "--no-commit" (magit-commit-at-point)))

(defun magit-checkout-commit ()
  (interactive)
  (magit-run "git" "checkout" (magit-commit-at-point)))

(defun magit-log-commit ()
  (interactive)
  (magit-browse-log (magit-commit-at-point)))

(defun magit-show-commit ()
  (interactive)
  (let ((dir default-directory)
	(commit (magit-commit-at-point))
	(buf (get-buffer-create "*magit-commit*")))
    (display-buffer buf)
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only t)
      (setq default-directory dir)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(magit-insert-section 'commit nil 'magit-wash-diff
			      "git" "log" "--max-count=1" "--cc" "-p"
			      commit)))))

(defun magit-quit ()
  (interactive)
  (bury-buffer))

(defun magit-wash-log (status)
  (goto-char (point-min))
  (while (not (eobp))
    (when (search-forward-regexp "[0-9a-fA-F]\\{40\\}" (line-end-position) t)
      (let ((commit (match-string-no-properties 0)))
	(delete-region (match-beginning 0) (match-end 0))
	(goto-char (match-beginning 0))
	(fixup-whitespace)
	(put-text-property (line-beginning-position) (line-end-position)
			   'magit-info (list 'commit commit))))
    (forward-line)))

(defun magit-browse-log (head)
  (interactive (list (magit-get-current-branch)))
  (let* ((topdir (magit-get-top-dir default-directory)))
    (switch-to-buffer "*magit-log*")
    (setq default-directory topdir)
    (magit-log-mode)
    (let ((inhibit-read-only t))
      (save-excursion
	(erase-buffer)
	(magit-insert-section 'history (format "History of %s" head)
			      'magit-wash-log
			      "git" "log" "--graph" "--max-count=10000"
			      "--pretty=oneline" head)))))

(defun magit-browse-branch-log ()
  (interactive)
  (magit-browse-log (magit-read-other-branch "Browse history of branch: ")))

;;; Miscellaneous

(defun magit-ignore-thing-at-point ()
  (interactive)
  (let ((info (get-char-property (point) 'magit-info)))
    (if info
	(case (car info)
	  ((other-file)
	   (append-to-file (concat (cadr info) "\n") nil ".gitignore")
	   (magit-update-status (magit-find-status-buffer)))))))

(defun magit-visit-thing-at-point ()
  (interactive)
  (let ((info (get-char-property (point) 'magit-info)))
    (if info
	(case (car info)
	  ((other-file)
	   (find-file (cadr info)))
	  ((diff hunk)
	   (let ((file (magit-diff-info-file info))
		 (line (if (eq (car info) 'hunk)
			   (magit-hunk-target-line info)
			 (magit-diff-info-position info))))
	     (find-file file)
	     (if line
		 (goto-line line))))))))

(defun magit-describe-thing-at-point ()
  (interactive)
  (let ((info (get-char-property (point) 'magit-info)))
    (message "Thing: %s" info)))
