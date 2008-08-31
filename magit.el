;;; magit -- control Git from Emacs.

;; Copyright (C) 2008  Marius Vollmer
;; Copyright (C) 2008  Linh Dang
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

;;; Commentary

;; Invoking the magit-status function will show a buffer with the
;; current status of the current git repository and its checkout.
;; That buffer offers key bindings for manipulating the status in
;; simple ways.
;;
;; The status buffer mainly shows the difference between the working
;; tree and the index, and the difference between the index and the
;; current HEAD.  You can add individual hunks from the working tree
;; to the index, and you can commit the index.
;;
;; See the Magit User Manual for more information.

;;; TODO

;; - Putting author information from log-edit buffer into commit environment.
;; - Visiting hunks from staged hunks doesn't work since the line
;;   number don't refer to the working tree.  Fix that somehow.
;; - 'C' is very unreliable and often makes a mess.
;; - Update commit details when using n and p with commits.
;; - Tags
;; - Equivalent of interactive rebase
;; - 'Subsetting', only looking at a subset of all files.
;; - Detect and handle renames and copies.

(require 'cl)
(require 'parse-time)

(defgroup magit nil
  "Controlling Git from Emacs."
  :prefix "magit-"
  :group 'tools)

(defface magit-header
  '((t))
  "Face for generic header lines.

Many Magit faces inherit from this one by default."
  :group 'magit)

(defface magit-section-title
  '((t :weight bold :inherit magit-header))
  "Face for section titles."
  :group 'magit)

(defface magit-branch
  '((t :weight bold :inherit magit-header))
  "Face for the current branch."
  :group 'magit)

(defface magit-diff-file-header
  '((t :inherit magit-header))
  "Face for diff file header lines."
  :group 'magit)

(defface magit-diff-hunk-header
  '((t :slant italic :inherit magit-header))
  "Face for diff hunk header lines."
  :group 'magit)

(defface magit-diff-add
  '((((class color) (background light))
     :foreground "blue1")
    (((class color) (background dark))
     :foreground "white"))
  "Face for lines in a diff that have been added."
  :group 'magit)

(defface magit-diff-none
  '((t))
  "Face for lines in a diff that are unchanged."
  :group 'magit)

(defface magit-diff-del
  '((((class color) (background light))
     :foreground "red")
    (((class color) (background dark))
     :foreground "OrangeRed"))
  "Face for lines in a diff that have been deleted."
  :group 'magit)

(defface magit-item-highlight
  '((((class color) (background light))
     :background "gray95")
    (((class color) (background dark))
     :background "dim gray"))
  "Face for highlighting the current item."
  :group 'magit)

;;; Utilities

(defun magit-goto-line (line)
  (save-restriction
    (widen)
    (goto-char 1)
    (forward-line (1- line))))

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

(defun magit-shell-exit-code (cmd &rest args)
  (call-process shell-file-name nil nil nil
		shell-command-switch (apply #'format cmd args)))

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
  (magit-shell "git config %s" (magit-concat-with-delim "." keys)))

(defun magit-set (val &rest keys)
  (if val
      (magit-shell "git config %s %s" (magit-concat-with-delim "." keys) val)
    (magit-shell "git config --unset %s" (magit-concat-with-delim "." keys))))

(defun magit-get-top-dir (cwd)
  (let* ((cwd (expand-file-name cwd))
	 (magit-dir (magit-shell "cd '%s' && git rev-parse --git-dir 2>/dev/null"
				 cwd)))
    (if magit-dir
	(file-name-as-directory (or (file-name-directory magit-dir) cwd))
      nil)))

(defun magit-get-ref (ref)
  (magit-shell "git symbolic-ref -q %s" ref))

(defun magit-get-current-branch ()
  (let* ((head (magit-get-ref "HEAD"))
	 (pos (and head (string-match "^refs/heads/" head))))
    (if pos
	(substring head 11)
      nil)))

(defun magit-read-top-dir (prefix)
  (let ((dir (magit-get-top-dir default-directory)))
    (if (or (not dir) prefix)
	(magit-get-top-dir (read-directory-name "Git repository: " dir))
      dir)))

(defun magit-name-rev (rev)
  (magit-shell "git name-rev --always --name-only %s" rev))

(defun magit-put-line-property (prop val)
  (put-text-property (line-beginning-position) (line-beginning-position 2)
		     prop val))

(defun magit-escape-for-shell (str)
  (concat "'" (replace-regexp-in-string "'" "'\\''" str) "'"))

(defun magit-format-commit (commit format)
  (magit-shell "git log --max-count=1 --pretty=format:%s %s" 
	       (magit-escape-for-shell format)
	       commit))

;;; Revisions and ranges

(defun magit-list-interesting-revisions ()
  (magit-shell-lines "git branch -a | cut -c3-"))

(defun magit-read-rev (prompt &optional def)
  (let* ((prompt (if def
		     (format "%s (default %s): " prompt def)
		   (format "%s: " prompt)))
	 (rev (completing-read prompt (magit-list-interesting-revisions)
			       nil nil nil nil def)))
    (if (string= rev "")
	nil
      rev)))

(defun magit-read-rev-range (op &optional def-beg def-end)
  (if current-prefix-arg
      (read-string (format "%s range: " op))
    (let ((beg (magit-read-rev (format "%s start" op)
			       def-beg)))
      (if (not beg)
	  nil
	(let ((end (magit-read-rev (format "%s end" op) def-end)))
	  (cons beg end))))))

(defun magit-rev-to-git (rev)
  (or rev
      (error "No revision specified"))
  (if (string= rev ".")
      (or (magit-marked-object)
	  (error "Commit mark not set"))
    rev))

(defun magit-rev-range-to-git (range)
  (or range
      (error "No revision range specified"))
  (if (stringp range)
      range
    (if (cdr range)
	(format "%s..%s"
		(magit-rev-to-git (car range))
		(magit-rev-to-git (cdr range)))
      (format "%s" (magit-rev-to-git (car range))))))

(defun magit-rev-describe (rev)
  (or rev
      (error "No revision specified"))
  (if (string= rev ".")
      "mark"
    (magit-name-rev rev)))

(defun magit-rev-range-describe (range things)
  (or range
      (error "No revision range specified"))
  (if (stringp range)
      (format "%s in %s" things range)
    (if (cdr range)
	(format "%s from %s to %s" things 
		(magit-rev-describe (car range))
		(magit-rev-describe (cdr range)))
      (format "%s at %s" things (magit-rev-describe (car range))))))

(defun magit-default-rev ()
  (magit-commit-at-point t))

(defun magit-file-uptodate-p (file)
  (eq (magit-shell-exit-code "git diff --quiet %s" file) 0))

(defun magit-anything-staged-p ()
  (not (eq (magit-shell-exit-code "git diff --quiet --cached") 0)))

;;; Items

(defvar *magit-item-context* nil)

(defstruct magit-item
  type beginning ending context info)

(defun magit-markup-item (beg end type info &optional active-beg active-end)
  (let ((item (make-magit-item :type type
			       :beginning beg
			       :ending end
			       :context *magit-item-context*
			       :info info)))
    (put-text-property (or active-beg beg) (or active-end end)
		       'magit-item item)
    item))

(defun magit-get-item ()
  (get-text-property (point) 'magit-item))

(defvar magit-highlight-overlay nil)

(defvar magit-highlighted-item nil)

(defun magit-highlight-item ()
  (let ((item (magit-get-item)))
    (when (not (eq item magit-highlighted-item))
      (setq magit-highlighted-item item)
      (if (not magit-highlight-overlay)
	  (let ((ov (make-overlay 1 1)))
	    (overlay-put ov 'face 'magit-item-highlight)
	    (setq magit-highlight-overlay ov)))
      (if item
	  (move-overlay magit-highlight-overlay
			(magit-item-beginning item)
			(magit-item-ending item)
			(current-buffer))
	(delete-overlay magit-highlight-overlay)))))

(defmacro magit-item-case (head &rest body)
  (declare (indent 1))
  (let ((item (car head))
	(info (cadr head))
	(opname (caddr head)))
    `(let* ((,item (magit-get-item))
	    (,info (and ,item (magit-item-info ,item))))
       (case (and ,item (magit-item-type ,item))
	 ,@body
	 ,@(if opname
	       `(((nil)
		  (error "Nothing to %s here." ,opname))
		 (t
		  (error "Can't %s a %s." ,opname
			 (let ((type (magit-item-type ,item)))
			   (or (get type 'magit-description)
			       type))))))))))

;;; Sections

;; Sections give a tree structure to the buffer content.  They are
;; used to navigate and for showing/hiding parts of it.
;;
;; The section tree is implemented by putting 'magit-section
;; properties of the form "(DEPTH ID)" on text regions.

(defun magit-markup-section (beg end depth seq)
  (put-text-property beg end 'magit-section (list depth seq)))

(defun magit-section-mark (p)
  (get-text-property p 'magit-section))

(defun magit-section-higher-p (s1 s2)
  (or (not s2)
      (< (car s2) (car s1))))

(defun magit-section-child-p (s1 s2)
  (and s2
       (> (car s2) (car s1))))

(defun magit-section-sibling-or-higher-p (s1 s2)
  (or (not s2)
      (< (car s2) (car s1))
      (and (= (car s2) (car s1))
	   (not (eq (cadr s2) (cadr s1))))))

(defun magit-section-beginning-position (p)
  (previous-single-property-change (if (= p (point-max))
				       p
				     (+ p 1))
				   'magit-section
				   nil (point-min)))

(defun magit-section-ending-position (p)
  (let ((s1 (magit-section-mark p)))
    (if (not s1)
	nil
      (while (let ((s2 (magit-section-mark p)))
	       (not (magit-section-sibling-or-higher-p s1 s2)))
	(setq p (next-single-property-change p 'magit-section
					     nil (point-max))))
      p)))

(defun magit-section-parent-position (p)
  (let ((s1 (magit-section-mark p)))
    (if (not s1)
	nil
      (while (and p
		  (let ((s2 (magit-section-mark p)))
		    (not (magit-section-higher-p s1 s2))))
	(setq p (previous-single-property-change p 'magit-section)))
      (and p 
	   (magit-section-beginning-position p)))))

(defun magit-section-first-child-position (p)
  (let ((s1 (magit-section-mark p)))
    (if (not s1)
	nil
      (while (and p
		  (let ((s2 (magit-section-mark p)))
		    (not (magit-section-child-p s1 s2))))
	(setq p (next-single-property-change p 'magit-section)))
      p)))

(defun magit-search-section-forward (section-mark)
  (let ((p (point)))
    (while (and p
		(not (equal section-mark (magit-section-mark p))))
      (setq p (next-single-property-change p 'magit-section)))
    (if p
	(goto-char p)
      nil)))

(defun magit-section-at-point ()
  (do ((p (point) (magit-section-parent-position p))
       (path nil (cons (cadr (magit-section-mark p)) path)))
      ((not p) path)))

(defun magit-goto-section (section-path)
  (let ((goal-pos nil))
    (save-excursion
      (goto-char (point-min))
      (do ((p section-path (cdr p))
	   (d 0 (+ d 1)))
	  ((cond ((null p)
		  (setq goal-pos (point))
		  t)
		 (t
		  (not (magit-search-section-forward (list d (car p)))))))))
    (if goal-pos
	(goto-char goal-pos))))

(defun magit-section-set-visibility (beg state)
  (let ((body-beg (save-excursion
		    (goto-char beg)
		    (forward-line)
		    (point)))
	(end (magit-section-ending-position beg))
	(inhibit-read-only t))
    (put-text-property beg (+ beg 1) 'magit-visibility-state state)
    (case state
      ((visible)
       (put-text-property body-beg end 'invisible nil)
       (magit-section-set-children-visibility beg 'visible))
      ((collapsed)
       (put-text-property body-beg end 'invisible nil)
       (magit-section-set-children-visibility beg 'hidden))
      ((hidden)
       (put-text-property body-beg end 'invisible t)
       (magit-section-set-children-visibility beg 'hidden)))))

(defun magit-section-set-children-visibility (beg state)
  (save-excursion
    (let ((end (magit-section-ending-position beg))
	  (start (magit-section-first-child-position beg)))
      (when start
	(goto-char start)
	(while (and (< (point) end) (not (eobp)))
	  (magit-section-set-visibility (point) state)
	  (goto-char (magit-section-ending-position (point))))))))

(defun magit-section-toggle-visibility ()
  (interactive)
  (goto-char (magit-section-beginning-position (point)))
  (let ((state (case (get-text-property (point) 'magit-visibility-state)
		 ((nil visible)
		  'hidden)
		 ((hidden)
		  (if (magit-section-first-child-position (point))
		      'collapsed
		    'visible))
		 ((collapsed)
		  'visible))))
    (magit-section-set-visibility (point) state)))

(defun magit-insert-section (id title washer cmd &rest args)
  (let ((chapter-beg (point)))
    (if title
	(insert (propertize title 'face 'magit-section-title) "\n"))
    (magit-markup-section chapter-beg (point) 0 id)
    (let* ((beg (point))
	   (status (apply 'call-process cmd nil t nil args)))
      (if washer
	  (save-restriction
	    (narrow-to-region beg (point))
	    (let ((*magit-item-context* id))
	      (funcall washer status))
	    (goto-char (point-max))))
      (if (/= beg (point))
	  (insert "\n")
	(delete-region chapter-beg (point))))))

(defun magit-next-section ()
  (interactive)
  (let ((next-section (next-single-property-change (point)
						   'magit-section)))
    (if next-section
	(goto-char next-section)
      (error "No next section."))))

(defun magit-previous-section ()
  (interactive)
  (let ((prev-section (previous-single-property-change (point)
						       'magit-section)))
    (if prev-section
	(goto-char prev-section)
      (error "No previous section."))))

(defmacro magit-define-section-jumper (sym title)
  (let ((fun (intern (format "magit-jump-to-%s" sym)))
	(doc (format "Jump to section `%s'." title)))
    `(defun ,fun ()
       (interactive)
       (magit-goto-section '(,sym)))))

;;; Running asynchronous commands

(defun magit-set-mode-line-process (str)
  (save-excursion
    (set-buffer (magit-find-status-buffer))
    (setq mode-line-process (if str
				(concat " " str)
			      ""))))

(defun magit-process-indicator-from-command (cmd args)
  (cond ((or (null args)
	     (not (equal cmd "git")))
	 cmd)
	((or (null (cdr args))
	     (not (member (car args) '("remote"))))
	 (car args))
	(t
	 (concat (car args) " " (cadr args)))))

(defvar magit-process nil)

(defun magit-run-command (logline cmd &rest args)
  (or (not magit-process)
      (error "Git is already running."))
  (let ((dir default-directory)
	(buf (get-buffer-create "*magit-process*")))
    (magit-set-mode-line-process
     (magit-process-indicator-from-command cmd args))
    (save-excursion
      (set-buffer buf)
      (setq default-directory dir)
      (erase-buffer)
      (insert "$ " logline "\n")
      (setq magit-process (apply 'start-process "git" buf cmd args))
      (set-process-sentinel magit-process 'magit-process-sentinel))))

(defun magit-run (cmd &rest args)
  (apply #'magit-run-command
	 (magit-concat-with-delim " " (cons cmd args))
	 cmd args))

(defun magit-run-shell (fmt &rest args)
  (let ((cmd (apply #'format fmt args)))
    (magit-run-command cmd shell-file-name shell-command-switch cmd)))

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
  (with-current-buffer (process-buffer process)
    (let ((msg (format "Git %s." (substring event 0 -1))))
      (insert msg "\n")
      (message msg)))
  (setq magit-process nil)
  (magit-set-mode-line-process nil)
  (magit-revert-files)
  (magit-update-status (magit-find-status-buffer)))

(defun magit-display-process ()
  (interactive)
  (display-buffer "*magit-process*"))

;;; Mode

;; We define individual functions (instead of using lambda etc) so
;; that the online help can show something meaningful.

(magit-define-section-jumper untracked "Untracked changes")
(magit-define-section-jumper unstaged  "Unstaged changes")
(magit-define-section-jumper staged    "Staged changes")
(magit-define-section-jumper unpushed  "Unpushed changes")

(defvar magit-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "n") 'magit-next-section)
    (define-key map (kbd "p") 'magit-previous-section)
    (define-key map (kbd "TAB") 'magit-section-toggle-visibility)
    (define-key map (kbd "1") 'magit-jump-to-untracked)
    (define-key map (kbd "2") 'magit-jump-to-unstaged)
    (define-key map (kbd "3") 'magit-jump-to-staged)
    (define-key map (kbd "4") 'magit-jump-to-unpushed)
    (define-key map (kbd "g") 'magit-status)
    (define-key map (kbd "s") 'magit-stage-item)
    (define-key map (kbd "S") 'magit-stage-all)
    (define-key map (kbd "u") 'magit-unstage-item)
    (define-key map (kbd "U") 'magit-unstage-all)
    (define-key map (kbd "i") 'magit-ignore-item)
    (define-key map (kbd "?") 'magit-describe-item)
    (define-key map (kbd ".") 'magit-mark-item)
    (define-key map (kbd "=") 'magit-diff-with-mark)
    (define-key map (kbd "l") 'magit-log-head)
    (define-key map (kbd "L") 'magit-log)
    (define-key map (kbd "h") 'magit-reflog-head)
    (define-key map (kbd "H") 'magit-reflog)
    (define-key map (kbd "d") 'magit-diff-working-tree)
    (define-key map (kbd "D") 'magit-diff)
    (define-key map (kbd "a") 'magit-apply-item)
    (define-key map (kbd "A") 'magit-cherry-pick)
    (define-key map (kbd "v") 'magit-revert-item)
    (define-key map (kbd "x") 'magit-reset-head)
    (define-key map (kbd "X") 'magit-reset-working-tree)
    (define-key map (kbd "k") 'magit-discard-item)
    (define-key map (kbd "RET") 'magit-visit-item)
    (define-key map (kbd "SPC") 'magit-show-item-or-scroll-up)
    (define-key map (kbd "DEL") 'magit-show-item-or-scroll-down)
    (define-key map (kbd "b") 'magit-checkout)
    (define-key map (kbd "B") 'magit-create-branch)
    (define-key map (kbd "m") 'magit-manual-merge)
    (define-key map (kbd "M") 'magit-automatic-merge)
    (define-key map (kbd "R") 'magit-rebase-step)
    (define-key map (kbd "P") 'magit-push)
    (define-key map (kbd "f") 'magit-remote-update)
    (define-key map (kbd "F") 'magit-pull)
    (define-key map (kbd "c") 'magit-log-edit)
    (define-key map (kbd "C") 'magit-add-log)
    (define-key map (kbd "$") 'magit-display-process)
    (define-key map (kbd "q") 'quit-window)
    map))

(defvar magit-mode-hook nil)

(put 'magit-mode 'mode-class 'special)

(defvar magit-marked-object nil)
(make-variable-buffer-local 'magit-marked-object)
(put 'magit-marked-object 'permanent-local t)

(defvar magit-submode nil)
(make-variable-buffer-local 'magit-submode)
(put 'magit-submode 'permanent-local t)

(defun magit-mode ()
  "Review the status of a git repository and act on it.

Please see the manual for a complete description of Magit.

\\{magit-mode-map}"
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq major-mode 'magit-mode
	mode-name "Magit"
	mode-line-process ""
	truncate-lines t)
  (add-hook 'post-command-hook #'magit-highlight-item nil t)
  (use-local-map magit-mode-map)
  (run-mode-hooks 'magit-mode-hook))

(defun magit-mode-init (dir submode)
  (setq default-directory dir
	magit-submode submode)
  (magit-mode))

;;; Status

(defun magit-wash-untracked-files (status)
  (goto-char (point-min))
  (let ((seq 0))
    (while (not (eobp))
      (let ((filename (buffer-substring-no-properties
		       (point) (line-end-position))))
	(cond ((not (string= filename ""))
	       (magit-markup-section (line-beginning-position) 
				     (line-beginning-position 2)
				     1 seq)
	       (magit-put-line-property 'face '(:foreground "red"))
	       (magit-markup-item (line-beginning-position)
				  (line-beginning-position 2)
				  'untracked-file filename)
	       (setq seq (+ seq 1)))))
      (forward-line)
      (beginning-of-line))))

(defun magit-wash-diff-markup-diff (head-seq head-beg head-end)
  (let ((head-end (or head-end (point))))
    (when head-beg
      (magit-markup-item head-beg (point)
			 'diff nil
			 head-beg head-end)
      (magit-markup-section head-beg head-end 1 head-seq))))

(defun magit-hunk-item-head-beg (item)
  (car (magit-item-info item)))

(defun magit-hunk-item-head-end (item)
  (cadr (magit-item-info item)))

(defun magit-wash-diff-markup-hunk (head-seq hunk-seq
				    head-beg head-end hunk-beg)
  (when hunk-beg
    (magit-markup-item hunk-beg (point)
		       'hunk (list head-beg head-end))
    (magit-markup-section hunk-beg (point) 1 head-seq)
    (magit-markup-section hunk-beg (point) 2 hunk-seq)))

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
		     (point) (min (+ (point) n-files) (point-max)))))
	(cond ((looking-at "^diff")
	       (magit-put-line-property 'face 'magit-diff-file-header)
	       (magit-wash-diff-markup-diff head-seq head-beg head-end)
	       (magit-wash-diff-markup-hunk head-seq hunk-seq
					    head-beg head-end hunk-beg)
	       (setq head-seq (+ head-seq 1))
	       (setq head-beg (point))
	       (setq head-end nil)
	       (setq hunk-seq 0)
	       (setq hunk-beg nil))
	      ((looking-at "^@+")
	       (magit-put-line-property 'face 'magit-diff-hunk-header)
	       (setq n-files (- (length (match-string 0)) 1))
	       (if (null head-end)
		   (setq head-end (point)))
	       (magit-wash-diff-markup-hunk head-seq hunk-seq
					    head-beg head-end hunk-beg)
	       (setq hunk-seq (+ hunk-seq 1))
	       (setq hunk-beg (point)))
	      ((looking-at "^ ")
	       (magit-put-line-property 'face 'magit-diff-none))
	      ((string-match "\\+" prefix)
	       (magit-put-line-property 'face 'magit-diff-add))
	      ((string-match "-" prefix)
	       (magit-put-line-property 'face 'magit-diff-del))))
      (forward-line)
      (beginning-of-line))
    (magit-wash-diff-markup-diff head-seq head-beg head-end)
    (magit-wash-diff-markup-hunk head-seq hunk-seq
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
			(propertize (or branch "(detached)")
				    'face 'magit-branch)
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
	(let ((rebase (magit-rebase-info)))
	  (if rebase
	      (insert (apply 'format "Rebasing: %s (%s of %s)\n" rebase))))
	(insert "\n")
	(magit-insert-section 'untracked
			      "Untracked files:" 'magit-wash-untracked-files
			      "git" "ls-files" "--others" "--exclude-standard")
	(let ((staged (magit-anything-staged-p)))
	  (magit-insert-section 'unstaged
				(if staged "Unstaged changes:" "Changes:")
				'magit-wash-diff
				"git" "diff")
	  (if staged
	      (magit-insert-section 'staged
				    "Staged changes:" 'magit-wash-diff
				    "git" "diff" "--cached")))
	(if remote
	    (magit-insert-section 'unpushed
				  "Unpushed commits:" 'magit-wash-log
				  "git" "log" "--graph" "--pretty=oneline"
				  (format "%s/%s..HEAD" remote branch))))
      (magit-goto-line old-line)
      (magit-goto-section old-section)
      (when (bobp)
	(magit-goto-section '(unstaged 1 1))))
    (magit-refresh-marks-in-buffer buf)
    (magit-highlight-item)))

(defun magit-find-buffer (submode &optional dir)
  (let ((topdir (magit-get-top-dir (or dir default-directory))))
    (dolist (buf (buffer-list))
      (if (save-excursion
	    (set-buffer buf)
	    (and (equal default-directory topdir)
		 (eq major-mode 'magit-mode)
		 (eq magit-submode submode)))
	  (return buf)))))

(defun magit-find-status-buffer (&optional dir)
  (magit-find-buffer 'status dir))

(defun magit-status (dir)
  (interactive (list (magit-read-top-dir current-prefix-arg)))
  (save-some-buffers)
  (let* ((topdir (magit-get-top-dir dir))
	 (buf (or (magit-find-status-buffer topdir)
		  (switch-to-buffer
		   (generate-new-buffer
		    (concat "*magit: "
			    (file-name-nondirectory
			     (directory-file-name topdir)) "*"))))))
    (switch-to-buffer buf)
    (magit-mode-init topdir 'status)
    (magit-update-status buf)))

;;; Staging

(defun magit-write-hunk-item-patch (item file)
  (write-region (magit-hunk-item-head-beg item)
		(magit-hunk-item-head-end item)
		file)
  (write-region (magit-item-beginning item)
		(magit-item-ending item)
		file
		t))

(defun magit-hunk-item-is-conflict-p (item)
  (save-excursion
    (goto-char (magit-hunk-item-head-beg item))
    (looking-at "^diff --cc")))

(defun magit-diff-item-conflict-file (item)
  (save-excursion
    (goto-char (magit-item-beginning item))
    (if (looking-at "^diff --cc +\\(.*\\)$")
	(match-string 1)
      nil)))

(defun magit-diff-or-hunk-item-file (item)
  (save-excursion
    (goto-char (if (eq (magit-item-type item) 'hunk)
		   (magit-hunk-item-head-beg item)
		 (magit-item-beginning item)))
    (cond ((looking-at "^diff --git a/\\(.*\\) b/\\(.*\\)$")
	   (match-string 2))
	  ((looking-at "^diff --cc +\\(.*\\)$")
	   (match-string 1))
	  (t
	   nil))))

(defun magit-hunk-item-target-line (item)
  (save-excursion
    (beginning-of-line)
    (let ((line (line-number-at-pos)))
      (if (looking-at "-")
	  (error "Can't visit removed lines."))
      (goto-char (magit-item-beginning item))
      (if (not (looking-at "@@+ .* \\+\\([0-9]+\\),[0-9]+ @@+"))
	  (error "Hunk header not found."))
      (let ((target (parse-integer (match-string 1))))
	(forward-line)
	(while (< (line-number-at-pos) line)
	  ;; XXX - deal with combined diffs
	  (if (not (looking-at "-"))
	      (setq target (+ target 1)))
	  (forward-line))
	target))))

(defun magit-apply-hunk-item (item &rest args)
  (magit-write-hunk-item-patch item ".git/magit-tmp")
  (apply #'magit-run "git" "apply" (append args (list ".git/magit-tmp"))))

(defun magit-must-be-unstaged (item)
  (if (eq (magit-item-context item) 'staged)
      (error "Already staged"))
  (if (not (eq (magit-item-context item) 'unstaged))
      (error "Can't stage this.")))

(defun magit-must-be-staged (item)
  (if (eq (magit-item-context item) 'unstaged)
      (error "Not staged"))
  (if (not (eq (magit-item-context item) 'staged))
      (error "Can't unstage this.")))

(defun magit-stage-item ()
  "Add the item at point to the staging area."
  (interactive)
  (magit-item-case (item info "stage")
    ((untracked-file)
     (magit-run "git" "add" info))
    ((hunk)
     (magit-must-be-unstaged item)
     (if (magit-hunk-item-is-conflict-p item)
	 (error "Can't stage individual resolution hunks.  Please stage the whole file."))
     (magit-apply-hunk-item item "--cached"))
    ((diff)
     (magit-must-be-unstaged item)
     (magit-run "git" "add" (magit-diff-or-hunk-item-file item)))))

(defun magit-unstage-item ()
  "Remove the item at point from the staging area."
  (interactive)
  (magit-item-case (item info "unstage")
    ((hunk)
     (magit-must-be-staged item)
     (magit-apply-hunk-item item "--cached" "--reverse"))
    ((diff)
     (magit-must-be-staged item)
     (magit-run "git" "reset" "HEAD"
		(magit-diff-or-hunk-item-file item)))))

(defun magit-stage-all ()
  (interactive)
  (magit-run "git" "add" "-u" "."))

(defun magit-unstage-all ()
  (interactive)
  (magit-run "git" "reset" "HEAD"))

;;; Branches

(defun magit-checkout (rev)
  (interactive (list (magit-read-rev "Switch to" (magit-default-rev))))
  (if rev
      (magit-run "git" "checkout" (magit-rev-to-git rev))))

(defun magit-read-create-branch-args ()
  (let* ((cur-branch (magit-get-current-branch))
	 (branch (read-string "Create branch: "))
	 (parent (magit-read-rev "Parent" cur-branch)))
    (list branch parent)))

(defun magit-create-branch (branch parent)
  (interactive (magit-read-create-branch-args))
  (if (and branch (not (string= branch ""))
	   parent)
      (magit-run "git" "checkout" "-b"
		 branch
		 (magit-rev-to-git parent))))

;;; Merging

(defun magit-manual-merge (rev)
  (interactive (list (magit-read-rev "Manually merge")))
  (if rev
      (magit-run "git" "merge" "--no-ff" "--no-commit"
		 (magit-rev-to-git rev))))

(defun magit-automatic-merge (rev)
  (interactive (list (magit-read-rev "Merge")))
  (if rev
      (magit-run "git" "merge" (magit-rev-to-git rev))))

;;; Rebasing

(defun magit-rebase-info ()
  (cond ((file-exists-p ".dotest")
	 (list (magit-name-rev (car (magit-file-lines ".dotest/onto")))
	       (car (magit-file-lines ".dotest/next"))
	       (car (magit-file-lines ".dotest/last"))))
	((file-exists-p ".git/.dotest-merge")
	 (list (car (magit-file-lines ".git/.dotest-merge/onto_name"))
	       (car (magit-file-lines ".git/.dotest-merge/msgnum"))
	       (car (magit-file-lines ".git/.dotest-merge/end"))))
	(t
	 nil)))

(defun magit-rebase-step ()
  (interactive)
  (let ((info (magit-rebase-info)))
    (if (not info)
	(let ((rev (magit-read-rev "Rebase to")))
	  (if rev
	      (magit-run "git" "rebase" (magit-rev-to-git rev))))
      (let ((cursor-in-echo-area t)
	    (message-log-max nil))
	(message "Rebase in progress.  Abort, Skip, or Continue? ")
	(let ((reply (read-event)))
	  (case reply
	    ((?A ?a)
	     (magit-run "git" "rebase" "--abort"))
	    ((?S ?s)
	     (magit-run "git" "rebase" "--skip"))
	    ((?C ?c)
	     (magit-run "git" "rebase" "--continue"))))))))

;;; Resetting

(defun magit-reset-head (rev)
  (interactive (list (magit-read-rev "Reset head to"
				     (or (magit-default-rev)
					 "HEAD^"))))
  (if rev
      (magit-run "git" "reset" "--soft" (magit-rev-to-git rev))))

(defun magit-reset-working-tree ()
  (interactive)
  (if (yes-or-no-p "Discard all uncommitted changes? ")
      (magit-run "git" "reset" "--hard")))

;;; Updating, pull, and push

(defun magit-remote-update ()
  (interactive)
  (magit-run "git" "remote" "update"))

(defun magit-pull ()
  (interactive)
  (magit-run "git" "pull" "-v"))

(defun magit-push ()
  (interactive)
  (magit-run "git" "push" "-v"))

;;; Committing

(defvar magit-log-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'magit-log-edit-commit)
    (define-key map (kbd "C-c C-a") 'magit-log-edit-toggle-amending)
    map))

(defvar magit-pre-log-edit-window-configuration nil)

(defun magit-log-edit-cleanup ()
  (save-excursion
    (goto-char (point-min))
    (flush-lines "^#")
    (goto-char (point-min))
    (if (re-search-forward "[ \t\n]*\\'" nil t)
	(replace-match "\n" nil nil))))

(defun magit-log-edit-append (str)
  (save-excursion
    (set-buffer (get-buffer-create "*magit-log-edit*"))
    (goto-char (point-max))
    (insert str "\n")))

(defun magit-log-edit-get-fields ()
  (let ((buf (get-buffer "*magit-log-edit*"))
	(result nil))
    (if buf
	(save-excursion
	  (set-buffer buf)
	  (goto-char (point-min))
	  (while (looking-at "^\\([A-Za-z0-9]+\\): *\\(.*\\)$")
	    (setq result (acons (intern (downcase (match-string 1)))
				(match-string 2)
				result))
	    (forward-line))))
    (nreverse result)))

(defun magit-log-edit-set-fields (fields)
  (let ((buf (get-buffer-create "*magit-log-edit*")))
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (if (search-forward-regexp "^\\([A-Za-z0-9]+:.*\n\\)+\n?" nil t)
	  (delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (when fields
	(while fields
	  (insert (capitalize (symbol-name (caar fields))) ": "
		  (cdar fields) "\n")
	  (setq fields (cdr fields)))
	(insert "\n")))))

(defun magit-log-edit-set-field (name value)
  (let* ((fields (magit-log-edit-get-fields))
	 (cell (assq name fields)))
    (cond (cell
	   (if value
	       (rplacd cell value)
	     (setq fields (delq cell fields))))
	  (t
	   (if value
	       (setq fields (append fields (cons name value))))))
    (magit-log-edit-set-fields fields)))

(defun magit-log-edit-commit ()
  (interactive)
  (let* ((fields (magit-log-edit-get-fields))
	 (amend (equal (cdr (assq 'amend fields)) "yes"))
	 (author (cdr (assq 'author fields))))
    (magit-log-edit-set-fields nil)
    (magit-log-edit-cleanup)
    (if (> (buffer-size) 0)
	(write-region (point-min) (point-max) ".git/magit-log")
      (write-region "(Empty description)" nil ".git/magit-log"))
    (erase-buffer)
    (apply #'magit-run "git" "commit" "-F" ".git/magit-log"
	   (append (if (not (magit-anything-staged-p)) '("--all") '())
		   (if amend '("--amend") '())))
    (bury-buffer)
    (when magit-pre-log-edit-window-configuration
      (set-window-configuration magit-pre-log-edit-window-configuration)
      (setq magit-pre-log-edit-window-configuration nil))))

(defun magit-log-edit-toggle-amending ()
  (interactive)
  (let* ((fields (magit-log-edit-get-fields))
	 (cell (assq 'amend fields)))
    (if cell
	(rplacd cell (if (equal (cdr cell) "yes") "no" "yes"))
      (setq fields (acons 'amend "yes" fields))
      (magit-log-edit-append
       (magit-format-commit "HEAD" "%s%n%n%b")))
    (magit-log-edit-set-fields fields)))

(defun magit-log-edit ()
  (interactive)
  (let ((dir default-directory)
	(buf (get-buffer-create "*magit-log-edit*")))
    (setq magit-pre-log-edit-window-configuration
	  (current-window-configuration))
    (pop-to-buffer buf)
    (setq default-directory dir)
    (use-local-map magit-log-edit-map)
    (message "Type C-c C-c to commit.")))

(defun magit-add-log ()
  (interactive)
  (let ((item (magit-get-item)))
    (if (not (and item (eq (magit-item-type item) 'hunk)))
	(error "No hunk at point."))
    (let ((fun (save-window-excursion
		 (save-excursion
		   (magit-visit-item)
		   (add-log-current-defun))))
	  (file (magit-diff-or-hunk-item-file item)))
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
		      (insert (format "(%s): " fun))))))))))

;;; Commits

(defun magit-commit-at-point (&optional nil-ok-p)
  (let* ((item (magit-get-item))
	 (commit (and item
		      (eq (magit-item-type item) 'commit)
		      (magit-item-info item))))
    (if nil-ok-p
	commit
      (or commit
	  (error "No commit at point.")))))

(defun magit-apply-item ()
  (interactive)
  (magit-item-case (item info "apply")
    ((commit)
     (magit-log-edit-append
      (magit-format-commit info "%s%n%n%b"))
     (magit-log-edit-set-field 
      'author
      (magit-format-commit info "%an <%ae>, %ai"))
     (magit-run-shell "git diff %s^ %s | git apply -" info info))))

(defun magit-cherry-pick ()
  (interactive)
  (magit-item-case (item info "apply")
    ((commit)
     (magit-run "git" "cherry-pick" info))))
  
(defun magit-revert-item ()
  (interactive)
  (magit-item-case (item info "revert")
    ((commit)
     (magit-append-to-log-edit
      (magit-format-commit info "Reverting \"%s\""))
     (magit-run-shell "git diff %s^ %s | git apply --reverse -" info info))))

(defvar magit-currently-shown-commit nil)

(defun magit-show-commit (commit &optional scroll)
  (let ((dir default-directory)
	(commit (magit-commit-at-point))
	(buf (get-buffer-create "*magit-commit*")))
    (cond ((equal magit-currently-shown-commit commit)
	   (let ((win (get-buffer-window buf)))
	     (cond ((not win)
		    (display-buffer buf))
		   (scroll
		    (with-selected-window win
		      (funcall scroll))))))
	  (t
	   (setq magit-currently-shown-commit commit)
	   (display-buffer buf)
	   (save-excursion
	     (set-buffer buf)
	     (magit-mode-init dir 'commit)
	     (let ((inhibit-read-only t))
	       (erase-buffer)
	       (magit-insert-section 'commit nil 'magit-wash-diff
				     "git" "log" "--max-count=1" "--cc" "-p"
				     commit)))))))

(defun magit-wash-log (status)
  (goto-char (point-min))
  (let ((seq 0))
    (while (not (eobp))
      (when (search-forward-regexp "[0-9a-fA-F]\\{40\\}" (line-end-position) t)
	(let ((commit (match-string-no-properties 0)))
	  (delete-region (match-beginning 0) (match-end 0))
	  (goto-char (match-beginning 0))
	  (fixup-whitespace)
	  (magit-markup-item (line-beginning-position)
			     (line-beginning-position 2)
			     'commit commit)
	  (magit-markup-section (line-beginning-position)
				(line-beginning-position 2)
				1 seq)
	  (setq seq (+ 1 seq))))
      (forward-line))))

(defun magit-log (range)
  (interactive (list (magit-read-rev-range "Log" (magit-get-current-branch))))
  (if range
      (let* ((topdir (magit-get-top-dir default-directory))
	     (args (magit-rev-range-to-git range)))
	(switch-to-buffer "*magit-log*")
	(magit-mode-init topdir 'log)
	(let ((inhibit-read-only t))
	  (save-excursion
	    (erase-buffer)
	    (magit-insert-section 'history
				  (magit-rev-range-describe range "Commits")
				  'magit-wash-log
				  "git" "log" "--graph" "--max-count=10000"
				  "--pretty=oneline" args)))
	(magit-refresh-marks-in-buffer (current-buffer)))))

(defun magit-log-head ()
  (interactive)
  (magit-log "HEAD"))

;;; Reflog

(defun magit-reflog (head)
  (interactive (list (magit-read-rev "Reflog of" "HEAD")))
  (if head
      (let* ((topdir (magit-get-top-dir default-directory))
	     (args (magit-rev-to-git head)))
	(switch-to-buffer "*magit-reflog*")
	(magit-mode-init topdir 'reflog)
	(let ((inhibit-read-only t))
	  (save-excursion
	    (erase-buffer)
	    (magit-insert-section 'reflog
				  (format "Local history of head %s" head)
				  'magit-wash-log
				  "git" "log" "--walk-reflogs"
				  "--max-count=10000"
				  "--pretty=oneline" args)))
	(magit-refresh-marks-in-buffer (current-buffer)))))

(defun magit-reflog-head ()
  (interactive)
  (magit-reflog "HEAD"))

;;; Diffing

(defun magit-diff (range)
  (interactive (list (magit-read-rev-range "Diff")))
  (if range
      (let* ((dir default-directory)
	     (args (magit-rev-range-to-git range))
	     (buf (get-buffer-create "*magit-diff*")))
	(display-buffer buf)
	(save-excursion
	  (set-buffer buf)
	  (magit-mode-init dir 'diff)
	  (let ((inhibit-read-only t))
	    (erase-buffer)
	    (magit-insert-section 'diff 
				  (magit-rev-range-describe range "Changes")
				  'magit-wash-diff
				  "git" "diff" args))))))

(defun magit-diff-working-tree (rev)
  (interactive (list (magit-read-rev "Diff with")))
  (if rev
      (magit-diff rev)))

(defun magit-diff-with-mark ()
  (interactive)
  (magit-diff (cons (magit-marked-object)
		    (magit-commit-at-point))))

;;; Markers

(defun magit-marked-object ()
  (save-excursion
    (set-buffer (magit-find-status-buffer))
    magit-marked-object))

(defun magit-set-marked-object (object)
  (save-excursion
    (set-buffer (magit-find-status-buffer))
    (setq magit-marked-object object)))

(defun magit-refresh-marks-in-buffer (buf)
  (let ((marked (magit-marked-object)))
    (save-excursion
      (set-buffer buf)
      (let ((inhibit-read-only t))
	(goto-char (point-min))
	(while (not (eobp))
	  (let ((item (get-text-property (point) 'magit-item))
		(next-change (or (next-single-property-change (point)
							      'magit-item)
				 (point-max))))
	    (if (and item (eq (magit-item-type item) 'commit))
		(put-text-property (point) next-change
				   'face (if (equal marked
						    (magit-item-info item))
					     '(:foreground "red")
					   nil)))
	    (goto-char next-change)))))))

(defun magit-refresh-marks ()
  (let ((status-buffer (magit-find-status-buffer)))
    (magit-refresh-marks-in-buffer (current-buffer))
    (if (not (eq (current-buffer) status-buffer))
	(magit-refresh-marks-in-buffer status-buffer))))

(defun magit-mark-item ()
  (interactive)
  (magit-set-marked-object (magit-commit-at-point))
  (magit-refresh-marks))

;;; Miscellaneous

(defun magit-ignore-item ()
  (interactive)
  (magit-item-case (item info "ignore")
    ((untracked-file)
     (append-to-file (concat "/" info "\n")
		     nil ".gitignore")
     (magit-update-status (magit-find-status-buffer)))))

(defun magit-discard-item ()
  (interactive)
  (magit-item-case (item info "discard")
    ((untracked-file)
     (if (yes-or-no-p (format "Delete %s? " info))
	 (magit-run "rm" info)))
    ((hunk)
     (case (magit-item-context item)
       ((unstaged)
	(when (yes-or-no-p "Discard hunk? ")
	  (magit-apply-hunk-item item "--reverse")))
       ((staged)
	(if (magit-file-uptodate-p (magit-diff-or-hunk-item-file item))
	    (when (yes-or-no-p "Discard hunk? ")
	      (magit-apply-hunk-item item "--reverse" "--index"))
	  (error "Can't discard this hunk.  Please unstage it first.")))
       (t
	(error "Can't discard this hunk."))))
    ((diff)
     (let ((file (magit-diff-or-hunk-item-file item)))
       (if (yes-or-no-p (format "Discard changes to %s? " file))
	   (magit-run "git" "checkout" "--" file))))))

(defun magit-visit-item ()
  (interactive)
  (magit-item-case (item info "visit")
    ((untracked-file)
     (find-file info))
    ((diff hunk)
     (let ((file (magit-diff-or-hunk-item-file item))
	   (line (if (eq (magit-item-type item) 'hunk)
		     (magit-hunk-item-target-line item)
		   nil)))
       (find-file file)
       (if line
	   (goto-line line))))
    ((commit)
     (magit-show-commit info)
     (pop-to-buffer "*magit-commit*"))))

(defun magit-show-item-or-scroll-up ()
  (interactive)
  (magit-item-case (item info)
    ((commit)
     (magit-show-commit info #'scroll-up))))

(defun magit-show-item-or-scroll-down ()
  (interactive)
  (magit-item-case (item info)
    ((commit)
     (magit-show-commit info #'scroll-down))))

(defun magit-describe-item ()
  (interactive)
  (let ((item (magit-get-item)))
    (if item
	(message "Item: %s %s-%s %S"
		 (magit-item-type item)
		 (magit-item-beginning item)
		 (magit-item-ending item)
		 (magit-item-info item))
      (message "No item here"))))

(provide 'magit)
