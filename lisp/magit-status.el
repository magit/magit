;;; magit-status.el --- the grand overview  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2018  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

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

;; This library implements the status buffer.

;;; Code:

(require 'magit)

(eval-when-compile (require 'subr-x))

(defvar bookmark-make-record-function)

;;; Options

(defgroup magit-status nil
  "Inspect and manipulate Git repositories."
  :link '(info-link "(magit)Status Buffer")
  :group 'magit-modes)

(defcustom magit-status-mode-hook nil
  "Hook run after entering Magit-Status mode."
  :group 'magit-status
  :type 'hook)

(defcustom magit-status-headers-hook
  '(magit-insert-error-header
    magit-insert-diff-filter-header
    magit-insert-head-branch-header
    magit-insert-upstream-branch-header
    magit-insert-push-branch-header
    magit-insert-tags-header)
  "Hook run to insert headers into the status buffer.

This hook is run by `magit-insert-status-headers', which in turn
has to be a member of `magit-status-sections-hook' to be used at
all."
  :package-version '(magit . "2.1.0")
  :group 'magit-status
  :type 'hook
  :options '(magit-insert-error-header
             magit-insert-diff-filter-header
             magit-insert-repo-header
             magit-insert-remote-header
             magit-insert-head-branch-header
             magit-insert-upstream-branch-header
             magit-insert-push-branch-header
             magit-insert-tags-header))

(defcustom magit-status-sections-hook
  '(magit-insert-status-headers
    magit-insert-merge-log
    magit-insert-rebase-sequence
    magit-insert-am-sequence
    magit-insert-sequencer-sequence
    magit-insert-bisect-output
    magit-insert-bisect-rest
    magit-insert-bisect-log
    magit-insert-untracked-files
    magit-insert-unstaged-changes
    magit-insert-staged-changes
    magit-insert-stashes
    magit-insert-unpulled-from-upstream
    magit-insert-unpulled-from-pushremote
    magit-insert-unpushed-to-upstream-or-recent
    magit-insert-unpushed-to-pushremote)
  "Hook run to insert sections into a status buffer."
  :package-version '(magit . "2.12.0")
  :group 'magit-status
  :type 'hook)

(defcustom magit-status-expand-stashes t
  "OBSOLETE.  Whether the list of stashes is expanded initially.
Use `magit-section-visibility-alist' instead."
  :package-version '(magit . "2.3.0")
  :group 'magit-status
  :type 'boolean)

(defcustom magit-status-show-hashes-in-headers nil
  "Whether headers in the status buffer show hashes.
The functions which respect this option are
`magit-insert-head-branch-header',
`magit-insert-upstream-branch-header', and
`magit-insert-push-branch-header'."
  :package-version '(magit . "2.4.0")
  :group 'magit-status
  :type 'boolean)

(defcustom magit-status-margin
  (list nil
        (nth 1 magit-log-margin)
        'magit-log-margin-width nil
        (nth 4 magit-log-margin))
  "Format of the margin in `magit-status-mode' buffers.

The value has the form (INIT STYLE WIDTH AUTHOR AUTHOR-WIDTH).

If INIT is non-nil, then the margin is shown initially.
STYLE controls how to format the committer date.  It can be one
  of `age' (to show the age of the commit), `age-abbreviated' (to
  abbreviate the time unit to a character), or a string (suitable
  for `format-time-string') to show the actual date.
WIDTH controls the width of the margin.  This exists for forward
  compatibility and currently the value should not be changed.
AUTHOR controls whether the name of the author is also shown by
  default.
AUTHOR-WIDTH has to be an integer.  When the name of the author
  is shown, then this specifies how much space is used to do so."
  :package-version '(magit . "2.9.0")
  :group 'magit-status
  :group 'magit-margin
  :type magit-log-margin--custom-type
  :initialize 'magit-custom-initialize-reset
  :set-after '(magit-log-margin)
  :set (apply-partially #'magit-margin-set-variable 'magit-status-mode))

;;; Commands

;;;###autoload
(defun magit-init (directory)
  "Initialize a Git repository, then show its status.

If the directory is below an existing repository, then the user
has to confirm that a new one should be created inside.  If the
directory is the root of the existing repository, then the user
has to confirm that it should be reinitialized.

Non-interactively DIRECTORY is (re-)initialized unconditionally."
  (interactive
   (let ((directory (file-name-as-directory
                     (expand-file-name
                      (read-directory-name "Create repository in: ")))))
     (-when-let (toplevel (magit-toplevel directory))
       (setq toplevel (expand-file-name toplevel))
       (unless (y-or-n-p (if (file-equal-p toplevel directory)
                             (format "Reinitialize existing repository %s? "
                                     directory)
                           (format "%s is a repository.  Create another in %s? "
                                   toplevel directory)))
         (user-error "Abort")))
     (list directory)))
  ;; `git init' does not understand the meaning of "~"!
  (magit-call-git "init" (magit-convert-filename-for-git
                          (expand-file-name directory)))
  (magit-status-internal directory))

;;;###autoload
(defun magit-status (&optional directory cache)
  "Show the status of the current Git repository in a buffer.
With a prefix argument prompt for a repository to be shown.
With two prefix arguments prompt for an arbitrary directory.
If that directory isn't the root of an existing repository,
then offer to initialize it as a new repository."
  (interactive
   (let ((magit--refresh-cache (list (cons 0 0))))
     (list (and (or current-prefix-arg (not (magit-toplevel)))
                (magit-read-repository
                 (>= (prefix-numeric-value current-prefix-arg) 16)))
           magit--refresh-cache)))
  (let ((magit--refresh-cache (or cache (list (cons 0 0)))))
    (if directory
        (let ((toplevel (magit-toplevel directory)))
          (setq directory (file-name-as-directory
                           (expand-file-name directory)))
          (if (and toplevel (file-equal-p directory toplevel))
              (magit-status-internal directory)
            (when (y-or-n-p
                   (if toplevel
                       (format "%s is a repository.  Create another in %s? "
                               toplevel directory)
                     (format "Create repository in %s? " directory)))
              ;; Creating a new repository will invalidate cached
              ;; values.
              (setq magit--refresh-cache nil)
              (magit-init directory))))
      (magit-status-internal default-directory))))

(put 'magit-status 'interactive-only 'magit-status-internal)

(defalias 'magit 'magit-status
  "An alias for `magit-status' for better discoverability.

Instead of invoking this alias for `magit-status' using
\"M-x magit RET\", you should bind a key to `magit-status'
and read the info node `(magit)Getting Started', which
also contains other useful hints.")

;;;###autoload
(defun magit-status-internal (directory)
  (magit--tramp-asserts directory)
  (let ((default-directory directory))
    (magit-mode-setup #'magit-status-mode)))

(defvar magit--remotes-using-recent-git nil)

(defun magit--tramp-asserts (directory)
  (-when-let (remote (file-remote-p directory))
    (unless (member remote magit--remotes-using-recent-git)
      (-if-let (version (let ((default-directory directory))
                          (magit-git-version)))
          (if (version<= magit--minimal-git version)
              (push version magit--remotes-using-recent-git)
            (display-warning 'magit (format "\
Magit requires Git >= %s, but on %s the version is %s.

If multiple Git versions are installed on the host, then the
problem might be that TRAMP uses the wrong executable.

First check the value of `magit-git-executable'.  Its value is
used when running git locally as well as when running it on a
remote host.  The default value is \"git\", except on Windows
where an absolute path is used for performance reasons.

If the value already is just \"git\" but TRAMP never-the-less
doesn't use the correct executable, then consult the info node
`(tramp)Remote programs'.\n" magit--minimal-git remote version) :error))
        (display-warning 'magit (format "\
Magit cannot find Git on %s.

First check the value of `magit-git-executable'.  Its value is
used when running git locally as well as when running it on a
remote host.  The default value is \"git\", except on Windows
where an absolute path is used for performance reasons.

If the value already is just \"git\" but TRAMP never-the-less
doesn't find the executable, then consult the info node
`(tramp)Remote programs'.\n" remote) :error)))))

;;; Mode

(defvar magit-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "jz" 'magit-jump-to-stashes)
    (define-key map "jt" 'magit-jump-to-tracked)
    (define-key map "jn" 'magit-jump-to-untracked)
    (define-key map "ju" 'magit-jump-to-unstaged)
    (define-key map "js" 'magit-jump-to-staged)
    (define-key map "jfu" 'magit-jump-to-unpulled-from-upstream)
    (define-key map "jfp" 'magit-jump-to-unpulled-from-pushremote)
    (define-key map "jpu" 'magit-jump-to-unpushed-to-upstream)
    (define-key map "jpp" 'magit-jump-to-unpushed-to-pushremote)
    (define-key map [remap dired-jump] 'magit-dired-jump)
    map)
  "Keymap for `magit-status-mode'.")

(define-derived-mode magit-status-mode magit-mode "Magit"
  "Mode for looking at Git status.

This mode is documented in info node `(magit)Status Buffer'.

\\<magit-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-section-toggle] to expand or hide the section at point.
Type \\[magit-visit-thing] to visit the change or commit at point.

Type \\[magit-dispatch-popup] to see available prefix popups.

Staging and applying changes is documented in info node
`(magit)Staging and Unstaging' and info node `(magit)Applying'.

\\<magit-hunk-section-map>Type \
\\[magit-apply] to apply the change at point, \
\\[magit-stage] to stage,
\\[magit-unstage] to unstage, \
\\[magit-discard] to discard, or \
\\[magit-reverse] to reverse it.

\\<magit-status-mode-map>\
Type \\[magit-commit-popup] to create a commit.

\\{magit-status-mode-map}"
  :group 'magit-status
  (hack-dir-local-variables-non-file-buffer)
  (setq imenu-create-index-function
        'magit-imenu--status-create-index-function)
  (setq-local bookmark-make-record-function
              #'magit-bookmark--status-make-record))

(defun magit-status-refresh-buffer ()
  (magit-git-exit-code "update-index" "--refresh")
  (magit-insert-section (status)
    (magit-run-section-hook 'magit-status-sections-hook))
  (run-hooks 'magit-status-refresh-hook))

(defun magit-status-maybe-update-revision-buffer (&optional _)
  "When moving in the status buffer, update the revision buffer.
If there is no revision buffer in the same frame, then do nothing."
  (when (derived-mode-p 'magit-status-mode)
    (magit-log-maybe-update-revision-buffer-1)))

(defun magit-status-maybe-update-blob-buffer (&optional _)
  "When moving in the status buffer, update the blob buffer.
If there is no blob buffer in the same frame, then do nothing."
  (when (derived-mode-p 'magit-status-mode)
    (magit-log-maybe-update-blob-buffer-1)))

;;; Sections
;;;; Special Headers

(defun magit-insert-status-headers ()
  "Insert header sections appropriate for `magit-status-mode' buffers.
The sections are inserted by running the functions on the hook
`magit-status-headers-hook'."
  (if (magit-rev-verify "HEAD")
      (magit-insert-headers magit-status-headers-hook)
    (insert "In the beginning there was darkness\n\n")))

(defvar magit-error-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] 'magit-process-buffer)
    map)
  "Keymap for `error' sections.")

(defun magit-insert-error-header ()
  "Insert the message about the Git error that just occured.

This function is only aware of the last error that occur when Git
was run for side-effects.  If, for example, an error occurs while
generating a diff, then that error won't be inserted.  Refreshing
the status buffer causes this section to disappear again."
  (when magit-this-error
    (magit-insert-section (error 'git)
      (insert (propertize (format "%-10s" "GitError! ")
                          'face 'magit-section-heading))
      (insert (propertize magit-this-error 'face 'font-lock-warning-face))
      (-when-let (key (car (where-is-internal 'magit-process-buffer)))
        (insert (format "  [Type `%s' for details]" (key-description key))))
      (insert ?\n))
    (setq magit-this-error nil)))

(defun magit-insert-diff-filter-header ()
  "Insert a header line showing the effective diff filters."
  (when magit-diff-section-file-args
    (magit-insert-section (filter 'diff)
      (insert (propertize (format "%-10s" "Filter! ")
                          'face 'magit-section-heading))
      (insert (mapconcat #'identity magit-diff-section-file-args " "))
      (insert ?\n))))

;;;; Reference Headers

(cl-defun magit-insert-head-branch-header
    (&optional (branch (magit-get-current-branch)))
  "Insert a header line about BRANCH.
When BRANCH is nil, use the current branch or, if none, the
detached `HEAD'."
  (let ((output (magit-rev-format "%h %s" (or branch "HEAD"))))
    (string-match "^\\([^ ]+\\) \\(.*\\)" output)
    (magit-bind-match-strings (commit summary) output
      (if branch
          (magit-insert-section (branch branch)
            (insert (format "%-10s" "Head: "))
            (when magit-status-show-hashes-in-headers
              (insert (propertize commit 'face 'magit-hash) ?\s))
            (insert (propertize branch 'face 'magit-branch-local))
            (insert ?\s)
            (insert (funcall magit-log-format-message-function branch summary))
            (insert ?\n))
        (magit-insert-section (commit commit)
          (insert (format "%-10s" "Head: "))
          (insert (propertize commit 'face 'magit-hash))
          (insert ?\s summary ?\n))))))

(cl-defun magit-insert-upstream-branch-header
    (&optional (branch (magit-get-current-branch))
               (pull   (magit-get-upstream-branch branch))
               keyword)
  "Insert a header line about branch usually pulled into current branch."
  (when pull
    (magit-insert-section (branch pull)
      (let ((rebase (magit-get "branch" branch "rebase")))
        (pcase rebase
          ("true")
          ("false" (setq rebase nil))
          (_       (setq rebase (magit-get-boolean "pull.rebase"))))
        (insert (format "%-10s" (or keyword (if rebase "Rebase: " "Merge: ")))))
      (--when-let (and magit-status-show-hashes-in-headers
                       (not (string-match-p " " pull))
                       (magit-rev-format "%h" pull))
        (insert (propertize it 'face 'magit-hash) " "))
      (if (string-match-p " " pull)
          (pcase-let ((`(,url ,branch) (split-string pull " ")))
            (insert branch " from " url " "))
        (insert pull " ")
        (--if-let (and (magit-rev-verify pull)
                       (magit-rev-format "%s" pull))
            (insert (funcall magit-log-format-message-function pull it))
          (insert (propertize "is missing" 'face 'font-lock-warning-face))))
      (insert ?\n))))

(cl-defun magit-insert-push-branch-header
    (&optional (branch (magit-get-current-branch))
               (push   (magit-get-push-branch branch)))
  "Insert a header line about the branch the current branch is pushed to."
  (when push
    (magit-insert-section (branch push)
      (insert (format "%-10s" "Push: "))
      (--when-let (and magit-status-show-hashes-in-headers
                       (magit-rev-format "%h" push))
        (insert (propertize it 'face 'magit-hash) ?\s))
      (insert (propertize push 'face 'magit-branch-remote) ?\s)
      (--if-let (and (magit-rev-verify push)
                     (magit-rev-format "%s" push))
          (insert (funcall magit-log-format-message-function push it))
        (insert (propertize "is missing" 'face 'font-lock-warning-face)))
      (insert ?\n))))

(defun magit-insert-tags-header ()
  "Insert a header line about the current and/or next tag."
  (let* ((this-tag (magit-get-current-tag nil t))
         (next-tag (magit-get-next-tag nil t))
         (this-cnt (cadr this-tag))
         (next-cnt (cadr next-tag))
         (this-tag (car this-tag))
         (next-tag (car next-tag))
         (both-tags (and this-tag next-tag t)))
    (when (or this-tag next-tag)
      (magit-insert-section (tag (or this-tag next-tag))
        (insert (format "%-10s" (if both-tags "Tags: " "Tag: ")))
        (cl-flet ((insert-count
                   (tag count face)
                   (insert (concat (propertize tag 'face 'magit-tag)
                                   (and (> count 0)
                                        (format " (%s)"
                                                (propertize (format "%s" count)
                                                            'face face)))))))
          (when this-tag  (insert-count this-tag this-cnt 'magit-branch-local))
          (when both-tags (insert ", "))
          (when next-tag  (insert-count next-tag next-cnt 'magit-tag)))
        (insert ?\n)))))

;;;; Auxiliary Headers

(defun magit-insert-user-header ()
  "Insert a header line about the current user."
  (let ((name  (magit-get "user.name"))
        (email (magit-get "user.email")))
    (when (and name email)
      (magit-insert-section (user name)
        (insert (format "%-10s" "User: "))
        (insert (propertize name 'face 'magit-log-author))
        (insert " <" email ">\n")))))

(defun magit-insert-repo-header ()
  "Insert a header line showing the path to the repository top-level."
  (let ((topdir (magit-toplevel)))
    (magit-insert-section (repo topdir)
      (insert (format "%-10s%s\n" "Repo: " (abbreviate-file-name topdir))))))

(defun magit-insert-remote-header ()
  "Insert a header line about the remote of the current branch.

If no remote is configured for the current branch, then fall back
showing the \"origin\" remote, or if that does not exist the first
remote in alphabetic order."
  (-when-let* ((name (magit-get-some-remote))
               ;; Under certain configurations it's possible for url
               ;; to be nil, when name is not, see #2858.
               (url (magit-get "remote" name "url")))
    (magit-insert-section (remote name)
      (insert (format "%-10s" "Remote: "))
      (insert (propertize name 'face 'magit-branch-remote) ?\s)
      (insert url ?\n))))

;;;; File Sections

(defvar magit-untracked-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-delete-thing] 'magit-discard)
    (define-key map "s" 'magit-stage)
    map)
  "Keymap for the `untracked' section.")

(magit-define-section-jumper magit-jump-to-untracked "Untracked files" untracked)

(defun magit-insert-untracked-files ()
  "Maybe insert a list or tree of untracked files.

Do so depending on the value of `status.showUntrackedFiles'.
Note that even if the value is `all', Magit still initially
only shows directories.  But the directory sections can then
be expanded using \"TAB\".

If the first element of `magit-diff-section-arguments' is a
directory, then limit the list to files below that.  The value
value of that variable can be set using \"D = f DIRECTORY RET g\"."
  (let* ((show (or (magit-get "status.showUntrackedFiles") "normal"))
         (base (car magit-diff-section-file-args))
         (base (and base (file-directory-p base) base)))
    (unless (equal show "no")
      (if (equal show "all")
          (-when-let (files (magit-untracked-files nil base))
            (magit-insert-section (untracked)
              (magit-insert-heading "Untracked files:")
              (magit-insert-files files base)
              (insert ?\n)))
        (-when-let
            (files (--mapcat (and (eq (aref it 0) ??)
                                  (list (substring it 3)))
                             (magit-git-items "status" "-z" "--porcelain"
                                              "--" base)))
          (magit-insert-section (untracked)
            (magit-insert-heading "Untracked files:")
            (dolist (file files)
              (magit-insert-section (file file)
                (insert (propertize file 'face 'magit-filename) ?\n)))
            (insert ?\n)))))))

(magit-define-section-jumper magit-jump-to-tracked "Tracked files" tracked)

(defun magit-insert-tracked-files ()
  "Insert a tree of tracked files.

If the first element of `magit-diff-section-arguments' is a
directory, then limit the list to files below that.  The value
value of that variable can be set using \"D = f DIRECTORY RET g\"."
  (-when-let (files (magit-list-files))
    (let* ((base (car magit-diff-section-file-args))
           (base (and base (file-directory-p base) base)))
      (magit-insert-section (tracked nil t)
        (magit-insert-heading "Tracked files:")
        (magit-insert-files files base)
        (insert ?\n)))))

(defun magit-insert-ignored-files ()
  "Insert a tree of ignored files.

If the first element of `magit-diff-section-arguments' is a
directory, then limit the list to files below that.  The value
of that variable can be set using \"D = f DIRECTORY RET g\"."
  (-when-let (files (magit-ignored-files))
    (let* ((base (car magit-diff-section-file-args))
           (base (and base (file-directory-p base) base)))
      (magit-insert-section (tracked nil t)
        (magit-insert-heading "Ignored files:")
        (magit-insert-files files base)
        (insert ?\n)))))

(defun magit-insert-files (files directory)
  (while (and files (string-prefix-p (or directory "") (car files)))
    (let ((dir (file-name-directory (car files))))
      (if (equal dir directory)
          (let ((file (pop files)))
            (magit-insert-section (file file)
              (insert (propertize file 'face 'magit-filename) ?\n)))
        (magit-insert-section (file dir t)
          (insert (propertize dir 'file 'magit-filename) ?\n)
          (magit-insert-heading)
          (setq files (magit-insert-files files dir))))))
  files)

(provide 'magit-status)
;;; magit-status.el ends here
