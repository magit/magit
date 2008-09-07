;;; Magit -- control Git from Emacs.

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

;; - Handle renames.
;; - Visiting from staged hunks doesn't always work since the line
;;   numbers don't refer to the working tree.  Fix that somehow.
;; - 'C' is very unreliable and often makes a mess.
;; - Update commit details when using n and p with commits.
;; - Tags
;; - Equivalent of interactive rebase
;; - Equivalent of git-wtf, http://git-wt-commit.rubyforge.org/#git-wtf
;; - 'Subsetting', only looking at a subset of all files.

(require 'cl)
(require 'parse-time)
(require 'log-edit)

(defgroup magit nil
  "Controlling Git from Emacs."
  :prefix "magit-"
  :group 'tools)

(defcustom magit-collapse-threshold 50
  "Sections with more lines than this are collapsed automatically."
  :group 'magit
  :type '(integer))

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
	 (magit-dir (magit-shell
		     "cd '%s' && git rev-parse --git-dir 2>/dev/null"
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

(defun magit-current-line ()
  (buffer-substring-no-properties (line-beginning-position)
				  (line-end-position)))

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
      (magit-marked-commit)
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
  (eq (magit-shell-exit-code "git diff --quiet -- %s" file) 0))

(defun magit-anything-staged-p ()
  (not (eq (magit-shell-exit-code "git diff --quiet --cached") 0)))

(defun magit-everything-clean-p ()
  (and (not (magit-anything-staged-p))
       (eq (magit-shell-exit-code "git diff --quiet") 0)))

;;; Sections

;; A buffer in magit-mode isorganized into hierarchical sections.
;; These sections are used for navigation and for hiding parts of the
;; buffer.
;;
;; Most sections also represent the objects that Magit works with,
;; such as files, diffs, hunks, commits, etc.  The 'type' of a section
;; identifies what kind of object it represents (if any), and the
;; parent and grand-parent, etc provide the context.

(defstruct magit-section
  parent title beginning end children hidden type info)

(defvar magit-top-section nil)
(make-variable-buffer-local 'magit-top-section)
(put 'magit-top-section 'permanent-local t)

(defvar magit-old-top-section nil)

(defun magit-new-section (title type)
  (let* ((s (make-magit-section :parent magit-top-section
			       :title title
			       :type type))
	 (old (and magit-old-top-section
		   (magit-find-section (magit-section-path s)
				       magit-old-top-section))))
    (if magit-top-section
	(setf (magit-section-children magit-top-section)
	      (append (magit-section-children magit-top-section)
		      (list s)))
      (setq magit-top-section s))
    (if old
	(setf (magit-section-hidden s) (magit-section-hidden old)))
    s))

(defun magit-cancel-section (section)
  (delete-region (magit-section-beginning section)
		 (magit-section-end section))
  (let ((parent (magit-section-parent section)))
    (if parent
	(setf (magit-section-children parent)
	      (delq section (magit-section-children parent)))
      (setq magit-top-section nil))))

(defmacro magit-with-section (title type &rest body)
  (declare (indent 2))
  (let ((s (gensym)))
    `(let* ((,s (magit-new-section ,title ,type))
	    (magit-top-section ,s))
       (setf (magit-section-beginning ,s) (point))
       ,@body
       (setf (magit-section-end ,s) (point))
       ,s)))

(defun magit-set-section-info (info &optional section)
  (setf (magit-section-info (or section magit-top-section)) info))

(defmacro magit-create-buffer-sections (&rest body)
  (declare (indent 0))
  `(let ((inhibit-read-only t))
     (erase-buffer)
     (let ((magit-old-top-section magit-top-section))
       (setq magit-top-section nil)
       ,@body
       (when (null magit-top-section)
	 (magit-with-section 'top nil
	   (insert "(empty)\n")))
       (magit-propertize-section magit-top-section)
       (magit-section-set-hidden magit-top-section
				 (magit-section-hidden magit-top-section)))))

(defun magit-propertize-section (section)
  (put-text-property (magit-section-beginning section)
		     (magit-section-end section)
		     'magit-section section)
  (dolist (s (magit-section-children section))
    (magit-propertize-section s)))

(defun magit-find-section (path top)
  (if (null path)
      top
    (let ((sec (find-if (lambda (s) (equal (car path)
					   (magit-section-title s)))
			(magit-section-children top))))
      (if sec
	  (magit-find-section (cdr path) sec)
	nil))))

(defun magit-section-path (section)
  (if (not (magit-section-parent section))
      '()
    (append (magit-section-path (magit-section-parent section))
	    (list (magit-section-title section)))))

(defun magit-find-section-at (pos secs)
  (while (and secs
	      (not (and (<= (magit-section-beginning (car secs)) pos)
			(<  pos (magit-section-end (car secs))))))
    (setq secs (cdr secs)))
  (if secs
      (or (magit-find-section-at pos (magit-section-children (car secs)))
	  (car secs))
    nil))

(defun magit-find-section-after (pos secs)
  (while (and secs
	      (not (> (magit-section-beginning (car secs)) pos)))
    (setq secs (cdr secs)))
  (car secs))

(defun magit-find-section-before (pos secs)
  (let ((prev nil))
    (while (and secs
		(not (> (magit-section-beginning (car secs)) pos)))
      (setq prev (car secs))
      (setq secs (cdr secs)))
    prev))

(defun magit-current-section ()
  (or (get-text-property (point) 'magit-section)
      magit-top-section))

(defun magit-insert-section (type title washer threshold cmd &rest args)
  (let* ((body-beg nil)
	 (section
	  (magit-with-section type nil
	    (if title
		(insert (propertize title 'face 'magit-section-title) "\n"))
	    (setq body-beg (point))
	    (apply 'call-process cmd nil t nil args)
	    (if washer
		(save-restriction
		  (narrow-to-region body-beg (point))
		  (goto-char (point-min))
		  (funcall washer)
		  (goto-char (point-max)))))))
    (if (= body-beg (point))
	(magit-cancel-section section)
      (insert "\n"))))
  
(defun magit-next-section (section)
  (let ((parent (magit-section-parent section)))
    (if parent
	(let ((next (cadr (memq section
				(magit-section-children parent)))))
	  (or next
	      (magit-next-section parent))))))

(defun magit-goto-next-section ()
  (interactive)
  (let* ((section (magit-current-section))
	 (next (or (and (magit-section-children section)
			(magit-find-section-after (point)
						  (magit-section-children
						   section)))
		   (magit-next-section section))))
    (if next
	(goto-char (magit-section-beginning next))
      (message "No next section"))))

(defun magit-prev-section (section)
  (let ((parent (magit-section-parent section)))
    (if parent
	(let ((prev (cadr (memq section
				(reverse (magit-section-children parent))))))
	  (cond (prev
		 (while (magit-section-children prev)
		   (setq prev (car (reverse (magit-section-children prev)))))
		 prev)
		(t
		 parent))))))

(defun magit-goto-previous-section ()
  (interactive)
  (let ((section (magit-current-section)))
    (cond ((= (point) (magit-section-beginning section))
	   (let ((prev (magit-prev-section (magit-current-section))))
	     (if prev
		 (goto-char (magit-section-beginning prev))
	       (message "No previous section"))))
	  (t
	   (let ((prev (magit-find-section-before (point)
						  (magit-section-children
						   section))))
	     (goto-char (magit-section-beginning (or prev section))))))))

(defun magit-goto-section (path)
  (let ((sec (magit-find-section path magit-top-section)))
    (if sec
	(goto-char (magit-section-beginning sec))
      (message "No such section"))))

(defun magit-for-all-sections (func &optional top)
  (let ((section (or top magit-top-section)))
    (when section
      (funcall func section)
      (dolist (c (magit-section-children section))
	(magit-for-all-sections func c)))))

(defun magit-section-set-hidden (section hidden)
  (setf (magit-section-hidden section) hidden)
  (let ((inhibit-read-only t)
	(beg (save-excursion
			 (goto-char (magit-section-beginning section))
			 (forward-line)
			 (point)))
	(end (magit-section-end section)))
    (put-text-property beg end 'invisible hidden)
    (put-text-property beg end 'rear-nonsticky t))
  (if (not hidden)
      (dolist (c (magit-section-children section))
	(magit-section-set-hidden c (magit-section-hidden c)))))

(defun magit-section-collapse (section)
  (dolist (c (magit-section-children section))
    (setf (magit-section-hidden c) t))
  (magit-section-set-hidden section nil))

(defun magit-section-expand (section)
  (dolist (c (magit-section-children section))
    (setf (magit-section-hidden c) nil))
  (magit-section-set-hidden section nil))

(defun magit-section-hideshow (flag-or-func)
  (let ((section (magit-current-section)))
    (cond ((magit-section-parent section)
	   (goto-char (magit-section-beginning section))
	   (if (functionp flag-or-func)
	       (funcall flag-or-func section)
	     (magit-section-set-hidden section flag-or-func))))))

(defun magit-show-section ()
  (interactive)
  (magit-section-hideshow nil))

(defun magit-hide-section ()
  (interactive)
  (magit-section-hideshow t))

(defun magit-collapse-section ()
  (interactive)
  (magit-section-hideshow #'magit-section-collapse))

(defun magit-expand-section ()
  (interactive)
  (magit-section-hideshow #'magit-section-expand))

(defun magit-toggle-section ()
  (interactive)
  (magit-section-hideshow
   (lambda (s)
     (magit-section-set-hidden (not (magit-section-hidden s))))))

(defun magit-cycle-section ()
  (interactive)
  (magit-section-hideshow
   (lambda (s)
     (cond ((magit-section-hidden s)
	    (magit-section-collapse s))
	   ((notany #'magit-section-hidden (magit-section-children s))
	    (magit-section-set-hidden s t))
	   (t
	    (magit-section-expand s))))))

(defmacro magit-define-section-jumper (sym title)
  (let ((fun (intern (format "magit-jump-to-%s" sym)))
	(doc (format "Jump to section `%s'." title)))
    `(defun ,fun ()
       (interactive)
       (magit-goto-section '(,sym)))))

(defvar magit-highlight-overlay nil)

(defvar magit-highlighted-section nil)

(defun magit-highlight-section ()
  (let ((section (magit-current-section)))
    (when (not (eq section magit-highlighted-section))
      (setq magit-highlighted-section section)
      (if (not magit-highlight-overlay)
	  (let ((ov (make-overlay 1 1)))
	    (overlay-put ov 'face 'magit-item-highlight)
	    (setq magit-highlight-overlay ov)))
      (if (and section (magit-section-type section))
	  (move-overlay magit-highlight-overlay
			(magit-section-beginning section)
			(magit-section-end section)
			(current-buffer))
	(delete-overlay magit-highlight-overlay)))))

(defun magit-section-context-type (section)
  (if (null section)
      '()
    (let ((c (or (magit-section-type section)
		 (if (symbolp (magit-section-title section))
		     (magit-section-title section)))))
      (if c
	  (cons c (magit-section-context-type
		   (magit-section-parent section)))
	'()))))

(defun magit-prefix-p (prefix list)
  ;;; Very schemish...
  (or (null prefix)
      (if (eq (car prefix) '*)
	  (or (magit-prefix-p (cdr prefix) list)
	      (and (not (null list))
		   (magit-prefix-p prefix (cdr list))))
	(and (not (null list))
	     (eq (car prefix) (car list))
	     (magit-prefix-p (cdr prefix) (cdr list))))))

(defmacro magit-section-case (head &rest clauses)
  (declare (indent 1))
  (let ((section (car head))
	(info (cadr head))
	(type (gensym))
	(context (gensym))
	(opname (caddr head)))
    `(let* ((,section (magit-current-section))
	    (,info (magit-section-info ,section))
	    (,type (magit-section-type ,section))
	    (,context (magit-section-context-type ,section)))
       (cond ,@(mapcar (lambda (clause)
			 (let ((prefix (reverse (car clause)))
			       (body (cdr clause)))
			   `((magit-prefix-p ',prefix ,context)
			     ,@body)))
		       clauses)
	     ,@(if opname
		   `(((not ,type)
		      (error "Nothing to %s here." ,opname))
		     (t
		      (error "Can't %s a %s."
			     ,opname
			     (or (get ,type 'magit-description)
				 ,type)))))))))

(defun magit-wash-sequence (func)
  (while (and (not (eobp))
	      (funcall func))))

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
      (set-process-sentinel magit-process 'magit-process-sentinel)
      (set-process-filter magit-process 'magit-process-filter))))

(defun magit-process-sentinel (process event)
  (with-current-buffer (process-buffer process)
    (let ((msg (format "Git %s." (substring event 0 -1))))
      (insert msg "\n")
      (message msg)))
  (setq magit-process nil)
  (magit-set-mode-line-process nil)
  (magit-revert-files)
  (magit-update-status (magit-find-status-buffer)))

(defun magit-process-filter (proc string)
  (save-excursion
    (set-buffer (process-buffer proc))
    (goto-char (process-mark proc))
    ;; Find last ^M in string.  If one was found, ignore everything
    ;; before it and delete the current line.
    (let ((ret-pos (position ?\r string :from-end t)))
      (cond (ret-pos
	     (goto-char (line-beginning-position))
	     (delete-region (point) (line-end-position))
	     (insert (substring string (+ ret-pos 1))))
	    (t
	     (insert string))))
    (set-marker (process-mark proc) (point))))

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

(defun magit-display-process ()
  (interactive)
  (display-buffer "*magit-process*"))

;;; Mode

;; We define individual functions (instead of using lambda etc) so
;; that the online help can show something meaningful.

(magit-define-section-jumper unpulled  "Unpulled commits")
(magit-define-section-jumper unstaged  "Unstaged changes")
(magit-define-section-jumper staged    "Staged changes")
(magit-define-section-jumper unpushed  "Unpushed commits")

(defvar magit-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "n") 'magit-goto-next-section)
    (define-key map (kbd "p") 'magit-goto-previous-section)
    (define-key map (kbd "TAB") 'magit-cycle-section)
    (define-key map (kbd "1") 'magit-jump-to-unpulled)
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
  (add-hook 'post-command-hook #'magit-highlight-section nil t)
  (use-local-map magit-mode-map)
  (run-mode-hooks 'magit-mode-hook))

(defun magit-mode-init (dir submode)
  (setq default-directory dir
	magit-submode submode)
  (magit-mode))

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

(defun magit-for-all-buffers (func)
  (dolist (buf (buffer-list))
    (save-excursion
      (set-buffer buf)
      (if (eq major-mode 'magit-mode)
	  (funcall func)))))

;;; Diffs and Hunks

(defun magit-diff-line-file ()
  (cond ((looking-at "^diff --git a/\\(.*\\) b/\\(.*\\)$")
	 (match-string-no-properties 2))
	((looking-at "^diff --cc +\\(.*\\)$")
	 (match-string-no-properties 1))
	(t
	 nil)))

(defun magit-wash-diffs ()
  (goto-char (point-min))
  (magit-wash-sequence #'magit-wash-diff-or-other-file))

(defun magit-wash-diff-or-other-file ()
  (or (magit-wash-diff)
      (magit-wash-other-file)))

(defun magit-wash-other-file ()
  (if (looking-at "^? \\(.*\\)$")
      (let ((file (match-string-no-properties 1)))
	(delete-region (point) (+ (line-end-position) 1))
	(magit-with-section file 'file
	  (magit-set-section-info file)
	  (insert "\tNew      " file "\n"))
	t)
    nil))

(defun magit-wash-diff ()
  (cond ((looking-at "^diff")
	 (magit-with-section (magit-current-line) 'diff
	   (let ((file (magit-diff-line-file))
		 (end (save-excursion
			(forward-line) ;; skip over "diff" line
			(if (search-forward-regexp "^diff\\|^@@" nil t)
			    (goto-char (match-beginning 0))
			  (goto-char (point-max)))
			(point-marker))))
	     (let* ((status (cond
			     ((looking-at "^diff --cc")
			      'unmerged)
			     ((save-excursion
				(search-forward-regexp "^new" end t))
			      'new)
			     ((save-excursion
				(search-forward-regexp "^deleted" end t))
			      'deleted)
			     ((save-excursion
				(search-forward-regexp "^rename" end t))
			      'renamed)
			     (t
			      'modified)))
		    (file2 (cond
			    ((save-excursion
			       (search-forward-regexp "^rename from \\(.*\\)"
						      end t))
			     (match-string-no-properties 1))))
		    (status-text (case status
				   ((unmerged)
				    (format "Unmerged %s" file))
				   ((new)
				    (format "New      %s" file))
				   ((deleted)
				    (format "Deleted  %s" file))
				   ((renamed)
				    (format "Renamed  %s   (from %s)"
					    file file2))
 				   ((modified)
				    (format "Modified %s" file))
				   (
				    (format "?        %s" file)))))
	       (magit-set-section-info (list status file file2))
	       (insert "\t" status-text "\n")
	       (goto-char end)
	       (magit-wash-sequence #'magit-wash-hunk)
	       t))))
	(t
	 nil)))

(defun magit-diff-item-kind (diff)
  (car (magit-section-info diff)))

(defun magit-diff-item-file (diff)
  (cadr (magit-section-info diff)))

(defun magit-diff-item-file2 (diff)
  (caddr (magit-section-info diff)))

(defun magit-wash-hunk ()
  (cond ((looking-at "\\(^@+\\)[^@]*@+")
	 (let ((n-files (length (match-string 1)))
	       (head (match-string 0)))
	   (magit-with-section head 'hunk
	     (magit-put-line-property 'face 'magit-diff-hunk-header)
	     (forward-line)
	     (while (not (or (eobp)
			     (looking-at "^diff\\|^@@")))
	       (let ((prefix (buffer-substring-no-properties
			      (point) (min (+ (point) n-files) (point-max)))))
		 (cond ((string-match "\\+" prefix)
			(magit-put-line-property 'face 'magit-diff-add))
		       ((string-match "-" prefix)
			(magit-put-line-property 'face 'magit-diff-del))
		       (t
			(magit-put-line-property 'face 'magit-diff-none))))
	       (forward-line))))
	 t)
	(t
	 nil)))

(defun magit-hunk-item-diff (hunk)
  (let ((diff (magit-section-parent hunk)))
    (or (eq (magit-section-type diff) 'diff)
	(error "Huh?  Parent of hunk not a diff."))
    diff))

(defun magit-diff-item-write-header (diff file &optional append-p)
  (let ((beg (save-excursion 
	       (goto-char (magit-section-beginning diff))
	       (forward-line)
	       (point)))
	(end (if (magit-section-children diff)
		 (magit-section-beginning (car (magit-section-children diff)))
	       (magit-section-end diff))))
    (write-region beg end file append-p)))

(defun magit-write-hunk-item-patch (hunk file)
  (magit-diff-item-write-header (magit-hunk-item-diff hunk) file)
  (write-region (magit-section-beginning hunk) (magit-section-end hunk)
		file t))

(defun magit-hunk-item-is-conflict-p (hunk)
  ;;; XXX - Using the title is a bit too clever...
  (string-match "^diff --cc"
		(magit-section-title (magit-hunk-item-diff hunk))))

(defun magit-hunk-item-target-line (hunk)
  (save-excursion
    (beginning-of-line)
    (let ((line (line-number-at-pos)))
      (if (looking-at "-")
	  (error "Can't visit removed lines."))
      (goto-char (magit-section-beginning hunk))
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

(defun magit-insert-unstaged-changes (title)
  (magit-insert-section 'unstaged title 'magit-wash-diffs
			magit-collapse-threshold
			"sh" "-c" "git ls-files -t --others --exclude-standard; git diff"))

(defun magit-insert-staged-changes ()
  (magit-insert-section 'staged	"Staged changes:" 'magit-wash-diffs
			magit-collapse-threshold
			"git" "diff" "--cached"))

;;; Logs and Commits

(defun magit-wash-log-line ()
  (if (search-forward-regexp "[0-9a-fA-F]\\{40\\}" (line-end-position) t)
      (let ((commit (match-string-no-properties 0)))
	(delete-region (match-beginning 0) (match-end 0))
	(goto-char (match-beginning 0))
	(fixup-whitespace)
	(goto-char (line-beginning-position))
	(magit-with-section commit 'commit
	  (magit-set-section-info commit)
	  (forward-line)))
    (forward-line))
  t)

(defun magit-wash-log ()
  (goto-char (point-min))
  (magit-wash-sequence #'magit-wash-log-line))

(defvar magit-currently-shown-commit nil)

(defun magit-wash-commit ()
  (goto-char (point-min))
  (cond ((search-forward-regexp "^diff" nil t)
	 (goto-char (match-beginning 0))
	 (magit-wash-diff))))

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
	     (magit-create-buffer-sections
	      (magit-insert-section 'commitbuf nil 'magit-wash-commit nil
				    "git" "log" "--max-count=1" "--cc" "-p"
				    commit)))))))

(defvar magit-marked-commit nil)

(defun magit-refresh-marked-commits ()
  (magit-for-all-buffers #'magit-refresh-marked-commits-in-buffer))

(defun magit-refresh-marked-commits-in-buffer ()
  (let ((inhibit-read-only t))
    (magit-for-all-sections
     (lambda (section)
       (if (not (magit-section-p section))
	   (message "%s" section))
       (when (eq (magit-section-type section) 'commit)
	 (put-text-property (magit-section-beginning section)
			    (magit-section-end section)
			    'face (if (equal (magit-section-info section)
					     magit-marked-commit)
				      '(:foreground "red")
				    nil)))))))

(defun magit-set-marked-commit (commit)
  (setq magit-marked-commit commit)
  (magit-refresh-marked-commits))

(defun magit-marked-commit ()
  (or magit-marked-commit
      (error "Not commit marked")))

(defun magit-insert-unpulled-commits (remote branch)
  (magit-insert-section 'unpulled
			"Unpulled commits:" 'magit-wash-log
			nil
			"git" "log" "--graph" "--pretty=oneline"
			(format "HEAD..%s/%s" remote branch)))

(defun magit-insert-unpushed-commits (remote branch)
  (magit-insert-section 'unpushed
			"Unpushed commits:" 'magit-wash-log
			nil
			"git" "log" "--graph" "--pretty=oneline"
			(format "%s/%s..HEAD" remote branch)))

;;; Status

(defun magit-update-status (buf)
  (with-current-buffer buf
    (let ((old-line (line-number-at-pos)))
      (magit-create-buffer-sections
       (magit-with-section 'status nil
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
	    (format
	     "Head:   %s\n"
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
	   (magit-insert-unrewritten-changes)
	   (magit-insert-pending-commits)
	   (when remote
	     (magit-insert-unpulled-commits remote branch))
	   (let ((staged (magit-anything-staged-p)))
	     (magit-insert-unstaged-changes
	      (if staged "Unstaged changes:" "Changes:"))
	     (if staged
		 (magit-insert-staged-changes)))
	   (when remote
	     (magit-insert-unpushed-commits remote branch)))))
      (magit-goto-line old-line)
      (when (bobp)
	(magit-goto-section '(unstaged)))
      (magit-highlight-section))))

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
    (magit-update-status buf)
    (magit-refresh-marked-commits-in-buffer)))

;;; Staging and Unstaging

(defun magit-apply-hunk-item (hunk &rest args)
  (magit-write-hunk-item-patch hunk ".git/magit-tmp")
  (apply #'magit-run "git" "apply" (append args (list ".git/magit-tmp"))))

(defun magit-stage-item ()
  "Add the item at point to the staging area."
  (interactive)
  (magit-section-case (item info "stage")
    ((unstaged file)
     (magit-run "git" "add" info))
    ((unstaged diff hunk)
     (if (magit-hunk-item-is-conflict-p item)
	 (error (concat "Can't stage individual resolution hunks.  "
			"Please stage the whole file.")))
     (magit-apply-hunk-item item "--cached"))
    ((unstaged diff)
     (magit-run "git" "add" "-u" (magit-diff-item-file item)))
    ((staged *)
     (error "Already staged"))
    ((hunk)
     (error "Can't stage this hunk"))
    ((diff)
     (error "Can't stage this diff"))))

(defun magit-unstage-item ()
  "Remove the item at point from the staging area."
  (interactive)
  (magit-section-case (item info "unstage")
    ((staged diff hunk)
     (magit-apply-hunk-item item "--cached" "--reverse"))
    ((staged diff)
     (magit-run "git" "reset" "-q" "HEAD" "--" (magit-diff-item-file item)))
    ((unstaged *)
     (error "Already unstaged"))
    ((hunk)
     (error "Can't unstage this hunk"))
    ((diff)
     (error "Can't unstage this diff"))))

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

;;; Rewriting

(defun magit-read-rewrite-info ()
  (when (file-exists-p ".git/magit-rewrite-info")
    (with-temp-buffer
      (insert-file-contents ".git/magit-rewrite-info")
      (goto-char (point-min))
      (read (current-buffer)))))

(defun magit-write-rewrite-info (info)
  (with-temp-file ".git/magit-rewrite-info"
    (prin1 info (current-buffer))
    (princ "\n" (current-buffer))))

(defun magit-insert-pending-commits ()
  (let* ((info (magit-read-rewrite-info))
	 (pending (cdr (assq 'pending info))))
    (when pending
      (magit-with-section 'pending nil
	(insert (propertize "Pending commits:\n"
			    'face 'magit-section-title))
	(dolist (p pending)
	  (magit-with-section p 'commit
	    (magit-set-section-info p)
	    (insert (magit-shell
		     "git log --max-count=1 --pretty=format:%s %s --"
		     (magit-escape-for-shell "* %s") p)
		    "\n"))))
      (insert "\n"))))

(defun magit-insert-unrewritten-changes ()
  (let* ((info (magit-read-rewrite-info))
	 (orig (cadr (assq 'orig info))))
    (when orig
      (magit-insert-section 'unrewritten
			    "Unrewritten changes"
			    'magit-wash-diff nil
			    "git" "diff" "-R" orig))))

(defun magit-rewrite-start (from &optional onto)
  (interactive (list (magit-read-rev "Rewrite from" (magit-default-rev))))
  (or (magit-everything-clean-p)
      (error "You have uncommitted changes."))
  (or (not (magit-read-rewrite-info))
      (error "Rewrite in progress."))
  (let* ((orig (magit-shell "git rev-parse HEAD"))
	 (base (magit-shell "git rev-parse %s^" from))
	 (pending (magit-shell-lines "git rev-list %s.." base)))
    (magit-write-rewrite-info `((orig ,orig)
				(pending ,@pending)))
    (magit-run "git" "reset" "--hard" base)))

(defun magit-rewrite-stop ()
  (interactive)
  (let* ((info (magit-read-rewrite-info)))
    (or info
	(error "No rewrite in progress."))
    (when (yes-or-no-p "Stop rewrite? ")
      (magit-write-rewrite-info nil)
      (magit-update-status (magit-find-status-buffer)))))

(defun magit-rewrite-abort ()
  (interactive)
  (let* ((info (magit-read-rewrite-info))
	 (orig (cadr (assq 'orig info))))
    (or info
	(error "No rewrite in progress."))
    (or (magit-everything-clean-p)
	(error "You have uncommitted changes."))
    (when (yes-or-no-p "Abort rewrite? ")
      (magit-write-rewrite-info nil)
      (magit-run "git" "reset" "--hard" orig))))

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
    (define-key map (kbd "M-p") 'log-edit-previous-comment)
    (define-key map (kbd "M-n") 'log-edit-next-comment)
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
	       (setq fields (append fields (list (cons name value)))))))
    (magit-log-edit-set-fields fields)))

(defun magit-log-edit-setup-author-env (author)
  (cond (author
	 ;; XXX - this is a bit strict, probably.
	 (or (string-match "\\(.*\\) <\\(.*\\)>, \\(.*\\)" author)
	     (error "Can't parse author string."))
	 ;; Shucks, setenv destroys the match data.
	 (let ((name (match-string 1 author))
	       (email (match-string 2 author))
	       (date  (match-string 3 author)))
	   (setenv "GIT_AUTHOR_NAME" name)
	   (setenv "GIT_AUTHOR_EMAIL" email)
	   (setenv "GIT_AUTHOR_DATE" date)))
	(t
	 (setenv "GIT_AUTHOR_NAME")
	 (setenv "GIT_AUTHOR_EMAIL")
	 (setenv "GIT_AUTHOR_DATE"))))

(defun magit-log-edit-push-to-comment-ring (comment)
  (when (or (ring-empty-p log-edit-comment-ring)
	    (not (equal comment (ring-ref log-edit-comment-ring 0))))
    (ring-insert log-edit-comment-ring comment)))

(defun magit-log-edit-commit ()
  (interactive)
  (let* ((fields (magit-log-edit-get-fields))
	 (amend (equal (cdr (assq 'amend fields)) "yes"))
	 (author (cdr (assq 'author fields))))
    (magit-log-edit-push-to-comment-ring (buffer-string))
    (magit-log-edit-setup-author-env author)
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
  (let ((section (magit-current-section)))
    (if (not (eq (magit-section-type section) 'hunk))
	(error "No hunk at point."))
    (let ((fun (save-window-excursion
		 (save-excursion
		   (magit-visit-item)
		   (add-log-current-defun))))
	  (file (magit-diff-item-file (magit-hunk-item-diff section))))
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
  (let* ((section (magit-current-section))
	 (commit (and (eq (magit-section-type section) 'commit)
		      (magit-section-info section))))
    (if nil-ok-p
	commit
      (or commit
	  (error "No commit at point.")))))

(defun magit-apply-item ()
  (interactive)
  (magit-section-case (item info "apply")
    ((commit)
     (magit-log-edit-append
      (magit-format-commit info "%s%n%n%b"))
     (magit-log-edit-set-field 
      'author
      (magit-format-commit info "%an <%ae>, %ai"))
     (magit-run-shell "git diff %s^ %s | git apply -" info info))))

(defun magit-cherry-pick ()
  (interactive)
  (magit-section-case (item info "apply")
    ((commit)
     (magit-run "git" "cherry-pick" info))))
  
(defun magit-revert-item ()
  (interactive)
  (magit-section-case (item info "revert")
    ((commit)
     (magit-log-edit-append
      (magit-format-commit info "Reverting \"%s\""))
     (magit-run-shell "git diff %s^ %s | git apply --reverse -" info info))))

(defun magit-log (range)
  (interactive (list (magit-read-rev-range "Log" (magit-get-current-branch))))
  (if range
      (let* ((topdir (magit-get-top-dir default-directory))
	     (args (magit-rev-range-to-git range)))
	(switch-to-buffer "*magit-log*")
	(magit-mode-init topdir 'log)
	(magit-create-buffer-sections
	 (magit-insert-section 'log
			       (magit-rev-range-describe range "Commits")
			       'magit-wash-log nil
			       "git" "log" "--graph" "--max-count=10000"
			       "--pretty=oneline" args))
	(magit-refresh-marked-commits-in-buffer)
	(goto-char (point-min)))))

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
	(magit-create-buffer-sections
	 (magit-insert-section 'reflog
			       (format "Local history of head %s" head)
			       'magit-wash-log nil
			       "git" "log" "--walk-reflogs"
			       "--max-count=10000"
			       "--pretty=oneline" args))
	(magit-refresh-marked-commits-in-buffer)
	(goto-char (point-min)))))

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
	  (magit-create-buffer-sections
	   (magit-insert-section 'diffbuf
				 (magit-rev-range-describe range "Changes")
				 'magit-wash-diffs nil
				 "git" "diff" args))))))

(defun magit-diff-working-tree (rev)
  (interactive (list (magit-read-rev "Diff with")))
  (if rev
      (magit-diff rev)))

(defun magit-diff-with-mark ()
  (interactive)
  (magit-diff (cons (magit-marked-commit)
		    (magit-commit-at-point))))

;;; Miscellaneous

(defun magit-ignore-item ()
  (interactive)
  (magit-section-case (item info "ignore")
    ((unstaged file)
     (append-to-file (concat "/" info "\n")
		     nil ".gitignore")
     (magit-update-status (magit-find-status-buffer)))))

(defun magit-discard-item ()
  (interactive)
  (magit-section-case (item info "discard")
    ((unstaged file)
     (if (yes-or-no-p (format "Delete %s? " info))
	 (magit-run "rm" info)))
    ((unstaged diff hunk)
     (when (yes-or-no-p "Discard hunk? ")
       (magit-apply-hunk-item item "--reverse")))
    ((staged diff hunk)
     (if (magit-file-uptodate-p (magit-diff-item-file
				 (magit-hunk-item-diff item)))
	 (when (yes-or-no-p "Discard hunk? ")
	   (magit-apply-hunk-item item "--reverse" "--index"))
       (error "Can't discard this hunk.  Please unstage it first.")))
    ((diff)
     (let ((kind (magit-diff-item-kind item))
	   (file (magit-diff-item-file item)))
       (cond ((eq kind 'deleted)
	      (when (yes-or-no-p (format "Resurrect %s? " file))
		(magit-shell "git reset -q -- %s" 
			     (magit-escape-for-shell file))
		(magit-run "git" "checkout" "--" file)))
	     ((eq kind 'new)
	      (if (yes-or-no-p (format "Delete %s? " file))
		  (magit-run "git" "rm" "-f" "--" file)))
	     (t
	      (if (yes-or-no-p (format "Discard changes to %s? " file))
		  (magit-run "git" "checkout" "--" file))))))))

(defun magit-visit-item ()
  (interactive)
  (magit-section-case (item info "visit")
    ((unstaged file)
     (find-file info))
    ((diff)
     (find-file (magit-diff-item-file item)))
    ((hunk)
     (let ((file (magit-diff-item-file (magit-hunk-item-diff item)))
	   (line (magit-hunk-item-target-line item)))
       (find-file file)
       (goto-line line)))
    ((commit)
     (magit-show-commit info)
     (pop-to-buffer "*magit-commit*"))))

(defun magit-show-item-or-scroll-up ()
  (interactive)
  (magit-section-case (item info)
    ((commit)
     (magit-show-commit info #'scroll-up))))

(defun magit-show-item-or-scroll-down ()
  (interactive)
  (magit-section-case (item info)
    ((commit)
     (magit-show-commit info #'scroll-down))))

(defun magit-mark-item ()
  (interactive)
  (magit-section-case (item info "mark")
    ((commit)
     (magit-set-marked-commit info))))

(defun magit-describe-item ()
  (interactive)
  (let ((section (magit-current-section)))
    (message "Section: %s %s-%s %S %S"
	     (magit-section-type section)
	     (magit-section-beginning section)
	     (magit-section-end section)
	     (magit-section-title section)
	     (magit-section-info section))))

(provide 'magit)
