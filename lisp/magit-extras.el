;;; magit-extras.el --- additional functionality for Magit  -*- lexical-binding: t -*-

;; Copyright (C) 2008-2018  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

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
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Additional functionality for Magit.

;;; Code:

(require 'magit)

(declare-function dired-read-shell-command "dired-aux" (prompt arg files))

(defgroup magit-extras nil
  "Additional functionality for Magit."
  :group 'magit-extensions)

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
  (magit-with-toplevel
    (magit-process-file magit-git-executable nil 0 nil "gui")))

;;;###autoload
(defun magit-run-git-gui-blame (commit filename &optional linenum)
  "Run `git gui blame' on the given FILENAME and COMMIT.
Interactively run it for the current file and the `HEAD', with a
prefix or when the current file cannot be determined let the user
choose.  When the current buffer is visiting FILENAME instruct
blame to center around the line point is on."
  (interactive
   (let (revision filename)
     (when (or current-prefix-arg
               (not (setq revision "HEAD"
                          filename (magit-file-relative-name nil 'tracked))))
       (setq revision (magit-read-branch-or-commit "Blame from revision"))
       (setq filename (magit-read-file-from-rev revision "Blame file")))
     (list revision filename
           (and (equal filename
                       (ignore-errors
                         (magit-file-relative-name buffer-file-name)))
                (line-number-at-pos)))))
  (magit-with-toplevel
    (apply #'magit-process-file magit-git-executable nil 0 nil "gui" "blame"
           `(,@(and linenum (list (format "--line=%d" linenum)))
             ,commit
             ,filename))))

;;;###autoload
(defun magit-run-gitk ()
  "Run `gitk' in the current repository."
  (interactive)
  (magit-process-file magit-gitk-executable nil 0))

;;;###autoload
(defun magit-run-gitk-branches ()
  "Run `gitk --branches' in the current repository."
  (interactive)
  (magit-process-file magit-gitk-executable nil 0 nil "--branches"))

;;;###autoload
(defun magit-run-gitk-all ()
  "Run `gitk --all' in the current repository."
  (interactive)
  (magit-process-file magit-gitk-executable nil 0 nil "--all"))

;;; Emacs Tools

;;;###autoload
(defun ido-enter-magit-status ()
  "Drop into `magit-status' from file switching.

To make this command available use something like:

  (add-hook \\='ido-setup-hook
            (lambda ()
              (define-key ido-completion-map
                (kbd \"C-x g\") \\='ido-enter-magit-status)))

Starting with Emacs 25.1 the Ido keymaps are defined just once
instead of every time Ido is invoked, so now you can modify it
like pretty much every other keymap:

  (define-key ido-common-completion-map
    (kbd \"C-x g\") 'ido-enter-magit-status)"
  (interactive)
  (with-no-warnings ; FIXME these are internal variables
    (setq ido-exit 'fallback fallback 'magit-status))
  (exit-minibuffer))

;;;###autoload
(defun magit-dired-jump (&optional other-window)
  "Visit file at point using Dired.
With a prefix argument, visit in another window.  If there
is no file at point, then instead visit `default-directory'."
  (interactive "P")
  (dired-jump other-window (-if-let (file (magit-file-at-point))
                               (progn (setq file (expand-file-name file))
                                      (if (file-directory-p file)
                                          (concat file "/.")
                                        file))
                             (concat default-directory "/."))))

;;;###autoload
(defun magit-dired-log (&optional follow)
  "Show log for all marked files, or the current file."
  (interactive "P")
  (-if-let (topdir (magit-toplevel default-directory))
      (let ((args (car (magit-log-arguments)))
            (files (dired-get-marked-files nil nil #'magit-file-tracked-p)))
        (unless files
          (user-error "No marked file is being tracked by Git"))
        (when (and follow
                   (not (member "--follow" args))
                   (not (cdr files)))
          (push "--follow" args))
        (magit-mode-setup-internal
         #'magit-log-mode
         (list (list (or (magit-get-current-branch) "HEAD"))
               args
               (let ((default-directory topdir))
                 (mapcar #'file-relative-name files)))
         magit-log-buffer-file-locked))
    (magit--not-inside-repository-error)))

;;;###autoload
(defun magit-do-async-shell-command (file)
  "Open FILE with `dired-do-async-shell-command'.
Interactively, open the file at point."
  (interactive (list (or (magit-file-at-point)
                         (completing-read "Act on file: "
                                          (magit-list-files)))))
  (require 'dired-aux)
  (dired-do-async-shell-command
   (dired-read-shell-command "& on %s: " current-prefix-arg (list file))
   nil (list file)))

;;; Shift Selection

(defun magit--turn-on-shift-select-mode-p ()
  (and shift-select-mode
       this-command-keys-shift-translated
       (not mark-active)
       (not (eq (car-safe transient-mark-mode) 'only))))

;;;###autoload
(defun magit-previous-line (&optional arg try-vscroll)
  "Like `previous-line' but with Magit-specific shift-selection.

Magit's selection mechanism is based on the region but selects an
area that is larger than the region.  This causes `previous-line'
when invoked while holding the shift key to move up one line and
thereby select two lines.  When invoked inside a hunk body this
command does not move point on the first invocation and thereby
it only selects a single line.  Which inconsistency you prefer
is a matter of preference."
  (declare (interactive-only
            "use `forward-line' with negative argument instead."))
  (interactive "p\np")
  (unless arg (setq arg 1))
  (let ((stay (or (magit-diff-inside-hunk-body-p)
                  (magit-section-position-in-heading-p))))
    (if (and stay (= arg 1) (magit--turn-on-shift-select-mode-p))
        (push-mark nil nil t)
      (with-no-warnings
        (handle-shift-selection)
        (previous-line (if stay (max (1- arg) 1) arg) try-vscroll)))))

;;;###autoload
(defun magit-next-line (&optional arg try-vscroll)
  "Like `next-line' but with Magit-specific shift-selection.

Magit's selection mechanism is based on the region but selects
an area that is larger than the region.  This causes `next-line'
when invoked while holding the shift key to move down one line
and thereby select two lines.  When invoked inside a hunk body
this command does not move point on the first invocation and
thereby it only selects a single line.  Which inconsistency you
prefer is a matter of preference."
  (declare (interactive-only forward-line))
  (interactive "p\np")
  (unless arg (setq arg 1))
  (let ((stay (or (magit-diff-inside-hunk-body-p)
                  (magit-section-position-in-heading-p))))
    (if (and stay (= arg 1) (magit--turn-on-shift-select-mode-p))
        (push-mark nil nil t)
      (with-no-warnings
        (handle-shift-selection)
        (next-line (if stay (max (1- arg) 1) arg) try-vscroll)))))

;;; Clean

;;;###autoload
(defun magit-clean (&optional arg)
  "Remove untracked files from the working tree.
With a prefix argument also remove ignored files,
with two prefix arguments remove ignored files only.
\n(git clean -f -d [-x|-X])"
  (interactive "p")
  (when (yes-or-no-p (format "Remove %s files? "
                             (pcase arg
                               (1 "untracked")
                               (4 "untracked and ignored")
                               (_ "ignored"))))
    (magit-wip-commit-before-change)
    (magit-run-git "clean" "-f" "-d" (pcase arg (4 "-x") (16 "-X")))))

(put 'magit-clean 'disabled t)

;;; Gitignore

;;;###autoload
(defun magit-gitignore (file-or-pattern &optional local)
  "Instruct Git to ignore FILE-OR-PATTERN.
With a prefix argument only ignore locally."
  (interactive (list (magit-gitignore-read-pattern current-prefix-arg)
                     current-prefix-arg))
  (let ((gitignore
         (if local
             (magit-git-dir (convert-standard-filename "info/exclude"))
           (expand-file-name ".gitignore" (magit-toplevel)))))
    (make-directory (file-name-directory gitignore) t)
    (with-temp-buffer
      (when (file-exists-p gitignore)
        (insert-file-contents gitignore))
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert (replace-regexp-in-string "\\(\\\\*\\)" "\\1\\1" file-or-pattern))
      (insert "\n")
      (write-region nil nil gitignore))
    (if local
        (magit-refresh)
      (magit-run-git "add" ".gitignore"))))

;;;###autoload
(defun magit-gitignore-locally (file-or-pattern)
  "Instruct Git to locally ignore FILE-OR-PATTERN."
  (interactive (list (magit-gitignore-read-pattern t)))
  (magit-gitignore file-or-pattern t))

(defun magit-gitignore-read-pattern (local)
  (let* ((default (magit-current-file))
         (choices
          (delete-dups
           (--mapcat
            (cons (concat "/" it)
                  (-when-let (ext (file-name-extension it))
                    (list (concat "/" (file-name-directory "foo") "*." ext)
                          (concat "*." ext))))
            (magit-untracked-files)))))
    (when default
      (setq default (concat "/" default))
      (unless (member default choices)
        (setq default (concat "*." (file-name-extension default)))
        (unless (member default choices)
          (setq default nil))))
    (magit-completing-read (concat "File or pattern to ignore"
                                   (and local " locally"))
                           choices nil nil nil nil default)))

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
      (call-interactively #'magit-diff-visit-file)
      (setq buf (current-buffer))
      (setq pos (point)))
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

;;; Reshelve

;;;###autoload
(defun magit-reshelve-since (rev)
  "Change the author and committer dates of the commits since REV.

Ask the user for the first reachable commit whose dates should
be changed.  The read the new date for that commit.  The initial
minibuffer input and the previous history element offer good
values.  The next commit will be created one minute later and so
on.

This command is only intended for interactive use and should only
be used on highly rearranged and unpublished history."
  (interactive (list nil))
  (cond
   ((not rev)
    (let ((backup (concat "refs/original/refs/heads/"
                          (magit-get-current-branch))))
      (when (and (magit-ref-p backup)
                 (not (magit-y-or-n-p
                       "Backup ref %s already exists.  Override? " backup)))
        (user-error "Abort")))
    (magit-log-select 'magit-reshelve-since
      "Type %p on a commit to reshelve it and the commits above it,"))
   (t
    (cl-flet ((adjust (time offset)
                      (format-time-string
                       "%F %T %z"
                       (+ (floor time)
                          (* offset 60)
                          (- (car (decode-time time)))))))
      (let* ((start (concat rev "^"))
             (range (concat start ".." (magit-get-current-branch)))
             (time-rev (adjust (float-time (string-to-number
                                            (magit-rev-format "%at" start)))
                               1))
             (time-now (adjust (float-time)
                               (- (string-to-number
                                   (magit-git-string "rev-list" "--count"
                                                     range))))))
        (push time-rev magit--reselve-history)
        (let ((date (floor
                     (float-time
                      (date-to-time
                       (read-string "Date for first commit: "
                                    time-now 'magit--reselve-history))))))
          (magit-with-toplevel
            (magit-run-git-async
             "filter-branch" "--force" "--env-filter"
             (format "case $GIT_COMMIT in %s\nesac"
                     (mapconcat (lambda (rev)
                                  (prog1 (format "%s) \
export GIT_AUTHOR_DATE=\"%s\"; \
export GIT_COMMITTER_DATE=\"%s\";;" rev date date)
                                    (cl-incf date 60)))
                                (magit-git-lines "rev-list" "--reverse"
                                                 range)
                                " "))
             range "--")
            (set-process-sentinel
             magit-this-process
             (lambda (process event)
               (when (memq (process-status process) '(exit signal))
                 (if (> (process-exit-status process) 0)
                     (magit-process-sentinel process event)
                   (process-put process 'inhibit-refresh t)
                   (magit-process-sentinel process event)
                   (magit-run-git "update-ref" "-d"
                                  (concat "refs/original/refs/heads/"
                                          (magit-get-current-branch))))))))))))))

;;; Revision Stack

(defvar magit-revision-stack nil)

(defcustom magit-pop-revision-stack-format
  '("[%N: %h] " "%N: %H\n   %s\n" "\\[\\([0-9]+\\)[]:]")
  "Control how `magit-pop-revision-stack' inserts a revision.

The command `magit-pop-revision-stack' inserts a representation
of the revision last pushed to the `magit-revision-stack' into
the current buffer.  It inserts text at point and/or near the end
of the buffer, and removes the consumed revision from the stack.

The entries on the stack have the format (HASH TOPLEVEL) and this
option has the format (POINT-FORMAT EOB-FORMAT INDEX-REGEXP), all
of which may be nil or a string (though either one of EOB-FORMAT
or POINT-FORMAT should be a string, and if INDEX-REGEXP is
non-nil, then the two formats should be too).

First INDEX-REGEXP is used to find the previously inserted entry,
by searching backward from point.  The first submatch must match
the index number.  That number is incremented by one, and becomes
the index number of the entry to be inserted.  If you don't want
to number the inserted revisions, then use nil for INDEX-REGEXP.

If INDEX-REGEXP is non-nil, then both POINT-FORMAT and EOB-FORMAT
should contain \"%N\", which is replaced with the number that was
determined in the previous step.

Both formats, if non-nil and after removing %N, are then expanded
using `git show --format=FORMAT ...' inside TOPLEVEL.

The expansion of POINT-FORMAT is inserted at point, and the
expansion of EOB-FORMAT is inserted at the end of the buffer (if
the buffer ends with a comment, then it is inserted right before
that)."
  :package-version '(magit . "2.3.0")
  :group 'magit-commands
  :type '(list (choice (string :tag "Insert at point format")
                       (cons (string :tag "Insert at point format")
                             (repeat (string :tag "Argument to git show")))
                       (const :tag "Don't insert at point" nil))
               (choice (string :tag "Insert at eob format")
                       (cons (string :tag "Insert at eob format")
                             (repeat (string :tag "Argument to git show")))
                       (const :tag "Don't insert at eob" nil))
               (choice (regexp :tag "Find index regexp")
                       (const :tag "Don't number entries" nil))))

(defun magit-pop-revision-stack (rev toplevel)
  "Insert a representation of a revision into the current buffer.

Pop a revision from the `magit-revision-stack' and insert it into
the current buffer according to `magit-pop-revision-stack-format'.
Revisions can be put on the stack using `magit-copy-section-value'
and `magit-copy-buffer-revision'.

If the stack is empty or with a prefix argument, instead read a
revision in the minibuffer.  By using the minibuffer history this
allows selecting an item which was popped earlier or to insert an
arbitrary reference or revision without first pushing it onto the
stack.

When reading the revision from the minibuffer, then it might not
be possible to guess the correct repository.  When this command
is called inside a repository (e.g. while composing a commit
message), then that repository is used.  Otherwise (e.g. while
composing an email) then the repository recorded for the top
element of the stack is used (even though we insert another
revision).  If not called inside a repository and with an empty
stack, or with two prefix arguments, then read the repository in
the minibuffer too."
  (interactive
   (if (or current-prefix-arg (not magit-revision-stack))
       (let ((default-directory
               (or (and (not (= (prefix-numeric-value current-prefix-arg) 16))
                        (or (magit-toplevel)
                            (cadr (car magit-revision-stack))))
                   (magit-read-repository))))
         (list (magit-read-branch-or-commit "Insert revision")
               default-directory))
     (push (caar magit-revision-stack) magit-revision-history)
     (pop magit-revision-stack)))
  (if rev
      (-let [(pnt-format eob-format idx-format) magit-pop-revision-stack-format]
        (let ((default-directory toplevel)
              (idx (and idx-format
                        (save-excursion
                          (if (re-search-backward idx-format nil t)
                              (number-to-string
                               (1+ (string-to-number (match-string 1))))
                            "1"))))
              pnt-args eob-args)
          (when (listp pnt-format)
            (setq pnt-args (cdr pnt-format))
            (setq pnt-format (car pnt-format)))
          (when (listp eob-format)
            (setq eob-args (cdr eob-format))
            (setq eob-format (car eob-format)))
          (when pnt-format
            (when idx-format
              (setq pnt-format
                    (replace-regexp-in-string "%N" idx pnt-format t t)))
            (magit-rev-insert-format pnt-format rev pnt-args)
            (backward-delete-char 1))
          (when eob-format
            (when idx-format
              (setq eob-format
                    (replace-regexp-in-string "%N" idx eob-format t t)))
            (save-excursion
              (goto-char (point-max))
              (skip-syntax-backward ">s-")
              (beginning-of-line)
              (if (and comment-start (looking-at comment-start))
                  (while (looking-at comment-start)
                    (forward-line -1))
                (forward-line)
                (unless (= (current-column) 0)
                  (insert ?\n)))
              (insert ?\n)
              (magit-rev-insert-format eob-format rev eob-args)
              (backward-delete-char 1)))))
    (user-error "Revision stack is empty")))

(define-key git-commit-mode-map
  (kbd "C-c C-w") 'magit-pop-revision-stack)

;;;###autoload
(defun magit-copy-section-value ()
  "Save the value of the current section for later use.

Save the section value to the `kill-ring', and, provided that
the current section is a commit, branch, or tag section, push
the (referenced) revision to the `magit-revision-stack' for use
with `magit-pop-revision-stack'.

When the current section is a branch or a tag, and a prefix
argument is used, then save the revision at its tip to the
`kill-ring' instead of the reference name.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above."
  (interactive)
  (if (use-region-p)
      (copy-region-as-kill nil nil 'region)
    (-when-let* ((section (magit-current-section))
                 (value (oref section value)))
      (magit-section-case
        ((branch commit module-commit tag)
         (let ((default-directory default-directory) ref)
           (magit-section-case
             ((branch tag)
              (setq ref value))
             (module-commit
              (setq default-directory
                    (file-name-as-directory
                     (expand-file-name (magit-section-parent-value section)
                                       (magit-toplevel))))))
           (setq value (magit-rev-parse value))
           (push (list value default-directory) magit-revision-stack)
           (kill-new (message "%s" (or (and current-prefix-arg ref)
                                       value)))))
        (t (kill-new (message "%s" value)))))))

;;;###autoload
(defun magit-copy-buffer-revision ()
  "Save the revision of the current buffer for later use.

Save the revision shown in the current buffer to the `kill-ring'
and push it to the `magit-revision-stack'.

This command is mainly intended for use in `magit-revision-mode'
buffers, the only buffers where it is always unambiguous exactly
which revision should be saved.

Most other Magit buffers usually show more than one revision, in
some way or another, so this command has to select one of them,
and that choice might not always be the one you think would have
been the best pick.

In such buffers it is often more useful to save the value of
the current section instead, using `magit-copy-section-value'.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above."
  (interactive)
  (if (use-region-p)
      (copy-region-as-kill nil nil 'region)
    (-when-let (rev (cond ((memq major-mode '(magit-cherry-mode
                                              magit-log-select-mode
                                              magit-reflog-mode
                                              magit-refs-mode
                                              magit-revision-mode
                                              magit-stash-mode
                                              magit-stashes-mode))
                           (car magit-refresh-args))
                          ((memq major-mode '(magit-diff-mode
                                              magit-log-mode))
                           (let ((r (caar magit-refresh-args)))
                             (if (string-match "\\.\\.\\.?\\(.+\\)" r)
                                 (match-string 1 r)
                               r)))
                          ((eq major-mode 'magit-status-mode) "HEAD")))
      (when (magit-rev-verify-commit rev)
        (setq rev (magit-rev-parse rev))
        (push (list rev default-directory) magit-revision-stack)
        (kill-new (message "%s" rev))))))

;;; Miscellaneous

;;;###autoload
(defun magit-abort-dwim ()
  "Abort current operation.
Depending on the context, this will abort a merge, a rebase, a
patch application, a cherry-pick, a revert, or a bisect."
  (interactive)
  (cond ((magit-merge-in-progress-p)     (magit-merge-abort))
        ((magit-rebase-in-progress-p)    (magit-rebase-abort))
        ((magit-am-in-progress-p)        (magit-am-abort))
        ((magit-sequencer-in-progress-p) (magit-sequencer-abort))
        ((magit-bisect-in-progress-p)    (magit-bisect-reset))))

(provide 'magit-extras)
;;; magit-extras.el ends here
