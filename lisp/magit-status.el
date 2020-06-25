;;; magit-status.el --- the grand overview  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2020  The Magit Project Contributors
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

(eval-when-compile
  (require 'subr-x))

(require 'magit)

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
    magit-insert-unpushed-to-pushremote
    magit-insert-unpushed-to-upstream-or-recent
    magit-insert-unpulled-from-pushremote
    magit-insert-unpulled-from-upstream)
  "Hook run to insert sections into a status buffer."
  :package-version '(magit . "2.12.0")
  :group 'magit-status
  :type 'hook)

(defcustom magit-status-initial-section '(1)
  "The section point is placed on when a status buffer is created.

When such a buffer is merely being refreshed or being shown again
after it was merely buried, then this option has no effect.

If this is nil, then point remains on the very first section as
usual.  Otherwise it has to be a list of integers and section
identity lists.  The members of that list are tried in order
until a matching section is found.

An integer means to jump to the nth section, 1 for example
jumps over the headings.  To get a section's \"identity list\"
use \\[universal-argument] \\[magit-describe-section-briefly].

If, for example, you want to jump to the commits that haven't
been pulled from the upstream, or else the second section, then
use: (((unpulled . \"..@{upstream}\") (status)) 1).

See option `magit-section-initial-visibility-alist' for how to
control the initial visibility of the jumped to section."
  :package-version '(magit . "2.90.0")
  :group 'magit-status
  :type '(choice (const :tag "as usual" nil)
                 (repeat (choice (number :tag "nth top-level section")
                                 (sexp   :tag "section identity")))))

(defcustom magit-status-goto-file-position nil
  "Whether to go to position corresponding to file position.

If this is non-nil and the current buffer is visiting a file,
then `magit-status' tries to go to the position in the status
buffer that corresponds to the position in the file-visiting
buffer.  This jumps into either the diff of unstaged changes
or the diff of staged changes.

If the previously current buffer does not visit a file, or if
the file has neither unstaged nor staged changes then this has
no effect.

The command `magit-status-here' tries to go to that position,
regardless of the value of this option."
  :package-version '(magit . "3.0.0")
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
STYLE controls how to format the author or committer date.
  It can be one of `age' (to show the age of the commit),
  `age-abbreviated' (to abbreviate the time unit to a character),
  or a string (suitable for `format-time-string') to show the
  actual date.  Option `magit-log-margin-show-committer-date'
  controls which date is being displayed.
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

(defcustom magit-status-use-buffer-arguments 'selected
  "Whether `magit-status' reuses arguments when the buffer already exists.

This option has no effect when merely refreshing the status
buffer using `magit-refresh'.

Valid values are:

`always': Always use the set of arguments that is currently
  active in the status buffer, provided that buffer exists
  of course.
`selected': Use the set of arguments from the status
  buffer, but only if it is displayed in a window of the
  current frame.  This is the default.
`current': Use the set of arguments from the status buffer,
  but only if it is the current buffer.
`never': Never use the set of arguments from the status
  buffer."
  :package-version '(magit . "3.0.0")
  :group 'magit-buffers
  :group 'magit-commands
  :type '(choice
          (const :tag "always use args from buffer" always)
          (const :tag "use args from buffer if displayed in frame" selected)
          (const :tag "use args from buffer if it is current" current)
          (const :tag "never use args from buffer" never)))

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
     (when-let ((toplevel (magit-toplevel directory)))
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
  (magit-status-setup-buffer directory))

;;;###autoload
(defun magit-status (&optional directory cache)
  "Show the status of the current Git repository in a buffer.

If the current directory isn't located within a Git repository,
then prompt for an existing repository or an arbitrary directory,
depending on option `magit-repository-directories', and show the
status of the selected repository instead.

* If that option specifies any existing repositories, then offer
  those for completion and show the status buffer for the
  selected one.

* Otherwise read an arbitrary directory using regular file-name
  completion.  If the selected directory is the top-level of an
  existing working tree, then show the status buffer for that.

* Otherwise offer to initialize the selected directory as a new
  repository.  After creating the repository show its status
  buffer.

These fallback behaviors can also be forced using one or more
prefix arguments:

* With two prefix arguments (or more precisely a numeric prefix
  value of 16 or greater) read an arbitrary directory and act on
  it as described above.  The same could be accomplished using
  the command `magit-init'.

* With a single prefix argument read an existing repository, or
  if none can be found based on `magit-repository-directories',
  then fall back to the same behavior as with two prefix
  arguments."
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
              (magit-status-setup-buffer directory)
            (when (y-or-n-p
                   (if toplevel
                       (format "%s is a repository.  Create another in %s? "
                               toplevel directory)
                     (format "Create repository in %s? " directory)))
              ;; Creating a new repository invalidates cached values.
              (setq magit--refresh-cache nil)
              (magit-init directory))))
      (magit-status-setup-buffer default-directory))))

(put 'magit-status 'interactive-only 'magit-status-setup-buffer)

;;;###autoload
(defalias 'magit 'magit-status
  "An alias for `magit-status' for better discoverability.

Instead of invoking this alias for `magit-status' using
\"M-x magit RET\", you should bind a key to `magit-status'
and read the info node `(magit)Getting Started', which
also contains other useful hints.")

;;;###autoload
(defun magit-status-here ()
  "Like `magit-status' but with non-nil `magit-status-goto-file-position'."
  (interactive)
  (let ((magit-status-goto-file-position t))
    (call-interactively #'magit-status)))

(put 'magit-status-here 'interactive-only 'magit-status-setup-buffer)

(defvar magit--remotes-using-recent-git nil)

(defun magit--tramp-asserts (directory)
  (when-let ((remote (file-remote-p directory)))
    (unless (member remote magit--remotes-using-recent-git)
      (if-let ((version (let ((default-directory directory))
                          (magit-git-version))))
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
    (define-key map "j" 'magit-status-jump)
    (define-key map [remap dired-jump] 'magit-dired-jump)
    map)
  "Keymap for `magit-status-mode'.")

(transient-define-prefix magit-status-jump ()
  "In a Magit-Status buffer, jump to a section."
  ["Jump to"
   [("z " "Stashes" magit-jump-to-stashes
     :if (lambda () (memq 'magit-insert-stashes magit-status-sections-hook)))
    ("t " "Tracked" magit-jump-to-tracked
     :if (lambda () (memq 'magit-insert-tracked-files magit-status-sections-hook)))
    ("n " "Untracked" magit-jump-to-untracked
     :if (lambda () (memq 'magit-insert-untracked-files magit-status-sections-hook)))
    ("u " "Unstaged" magit-jump-to-unstaged
     :if (lambda () (memq 'magit-insert-unstaged-changes magit-status-sections-hook)))
    ("s " "Staged" magit-jump-to-staged
     :if (lambda () (memq 'magit-insert-staged-changes magit-status-sections-hook)))]
   [("fu" "Unpulled from upstream" magit-jump-to-unpulled-from-upstream
     :if (lambda () (memq 'magit-insert-unpulled-from-upstream magit-status-sections-hook)))
    ("fp" "Unpulled from pushremote" magit-jump-to-unpulled-from-pushremote
     :if (lambda () (memq 'magit-insert-unpulled-from-pushremote magit-status-sections-hook)))
    ("pu" magit-jump-to-unpushed-to-upstream
     :if (lambda ()
           (or (memq 'magit-insert-unpushed-to-upstream-or-recent magit-status-sections-hook)
               (memq 'magit-insert-unpushed-to-upstream magit-status-sections-hook)))
     :description (lambda ()
                    (let ((upstream (magit-get-upstream-branch)))
                      (if (or (not upstream)
                              (magit-rev-ancestor-p "HEAD" upstream))
                          "Recent commits"
                        "Unmerged into upstream"))))
    ("pp" "Unpushed to pushremote" magit-jump-to-unpushed-to-pushremote
     :if (lambda () (memq 'magit-insert-unpushed-to-pushremote magit-status-sections-hook)))
    ("a " "Assumed unstaged" magit-jump-to-assume-unchanged
     :if (lambda () (memq 'magit-insert-assume-unchanged-files magit-status-sections-hook)))
    ("w " "Skip worktree" magit-jump-to-skip-worktree
     :if (lambda () (memq 'magit-insert-skip-worktree-files magit-status-sections-hook)))]])

(define-derived-mode magit-status-mode magit-mode "Magit"
  "Mode for looking at Git status.

This mode is documented in info node `(magit)Status Buffer'.

\\<magit-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-section-toggle] to expand or hide the section at point.
Type \\[magit-visit-thing] to visit the change or commit at point.

Type \\[magit-dispatch] to invoke major commands.

Staging and applying changes is documented in info node
`(magit)Staging and Unstaging' and info node `(magit)Applying'.

\\<magit-hunk-section-map>Type \
\\[magit-apply] to apply the change at point, \
\\[magit-stage] to stage,
\\[magit-unstage] to unstage, \
\\[magit-discard] to discard, or \
\\[magit-reverse] to reverse it.

\\<magit-status-mode-map>\
Type \\[magit-commit] to create a commit.

\\{magit-status-mode-map}"
  :group 'magit-status
  (hack-dir-local-variables-non-file-buffer)
  (setq imenu-create-index-function
        'magit-imenu--status-create-index-function))

(put 'magit-status-mode 'magit-diff-default-arguments
     '("--no-ext-diff"))
(put 'magit-status-mode 'magit-log-default-arguments
     '("-n256" "--decorate"))

;;;###autoload
(defun magit-status-setup-buffer (&optional directory)
  (unless directory
    (setq directory default-directory))
  (magit--tramp-asserts directory)
  (let* ((default-directory directory)
         (d (magit-diff--get-value 'magit-status-mode
                                   magit-status-use-buffer-arguments))
         (l (magit-log--get-value 'magit-status-mode
                                  magit-status-use-buffer-arguments))
         (file (and magit-status-goto-file-position
                    (magit-file-relative-name)))
         (line (and file (line-number-at-pos)))
         (col  (and file (current-column)))
         (buf  (magit-setup-buffer #'magit-status-mode nil
                 (magit-buffer-diff-args  (nth 0 d))
                 (magit-buffer-diff-files (nth 1 d))
                 (magit-buffer-log-args   (nth 0 l))
                 (magit-buffer-log-files  (nth 1 l)))))
    (when file
      (with-current-buffer buf
        (let ((staged (magit-get-section '((staged) (status)))))
          (if (and staged
                   (cadr (magit-diff--locate-hunk file line staged)))
              (magit-diff--goto-position file line col staged)
            (let ((unstaged (magit-get-section '((unstaged) (status)))))
              (unless (and unstaged
                           (magit-diff--goto-position file line col unstaged))
                (when staged
                  (magit-diff--goto-position file line col staged))))))))
    buf))

(defun magit-status-refresh-buffer ()
  (magit-git-exit-code "update-index" "--refresh")
  (magit-insert-section (status)
    (magit-run-section-hook 'magit-status-sections-hook)))

(defun magit-status-goto-initial-section ()
  "In a `magit-status-mode' buffer, jump `magit-status-initial-section'.
Actually doing so is deferred until `magit-refresh-buffer-hook'
runs `magit-status-goto-initial-section-1'.  That function then
removes itself from the hook, so that this only happens when the
status buffer is first created."
  (when (and magit-status-initial-section
             (derived-mode-p 'magit-status-mode))
    (add-hook 'magit-refresh-buffer-hook
              'magit-status-goto-initial-section-1 nil t)))

(defun magit-status-goto-initial-section-1 ()
  "In a `magit-status-mode' buffer, jump `magit-status-initial-section'.
This function removes itself from `magit-refresh-buffer-hook'."
  (when-let ((section
              (--some (if (integerp it)
                          (nth (1- it)
                               (magit-section-siblings (magit-current-section)
                                                       'next))
                        (magit-get-section it))
                      magit-status-initial-section)))
    (goto-char (oref section start))
    (when-let ((vis (cdr (assq 'magit-status-initial-section
                               magit-section-initial-visibility-alist))))
      (if (eq vis 'hide)
          (magit-section-hide section)
        (magit-section-show section))))
  (remove-hook 'magit-refresh-buffer-hook
               'magit-status-goto-initial-section-1 t))

(defun magit-status-maybe-update-revision-buffer (&optional _)
  "When moving in the status buffer, update the revision buffer.
If there is no revision buffer in the same frame, then do nothing."
  (when (derived-mode-p 'magit-status-mode)
    (magit--maybe-update-revision-buffer)))

(defun magit-status-maybe-update-stash-buffer (&optional _)
  "When moving in the status buffer, update the stash buffer.
If there is no stash buffer in the same frame, then do nothing."
  (when (derived-mode-p 'magit-status-mode)
    (magit--maybe-update-stash-buffer)))

(defun magit-status-maybe-update-blob-buffer (&optional _)
  "When moving in the status buffer, update the blob buffer.
If there is no blob buffer in the same frame, then do nothing."
  (when (derived-mode-p 'magit-status-mode)
    (magit--maybe-update-blob-buffer)))

;;; Sections
;;;; Special Headers

(defun magit-insert-status-headers ()
  "Insert header sections appropriate for `magit-status-mode' buffers.
The sections are inserted by running the functions on the hook
`magit-status-headers-hook'."
  (if (magit-rev-verify "HEAD")
      (magit-insert-headers 'magit-status-headers-hook)
    (insert "In the beginning there was darkness\n\n")))

(defvar magit-error-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] 'magit-process-buffer)
    map)
  "Keymap for `error' sections.")

(defun magit-insert-error-header ()
  "Insert the message about the Git error that just occurred.

This function is only aware of the last error that occur when Git
was run for side-effects.  If, for example, an error occurs while
generating a diff, then that error won't be inserted.  Refreshing
the status buffer causes this section to disappear again."
  (when magit-this-error
    (magit-insert-section (error 'git)
      (insert (propertize (format "%-10s" "GitError! ")
                          'font-lock-face 'magit-section-heading))
      (insert (propertize magit-this-error
                          'font-lock-face 'font-lock-warning-face))
      (when-let ((key (car (where-is-internal 'magit-process-buffer))))
        (insert (format "  [Type `%s' for details]" (key-description key))))
      (insert ?\n))
    (setq magit-this-error nil)))

(defun magit-insert-diff-filter-header ()
  "Insert a header line showing the effective diff filters."
  (let ((ignore-modules (magit-ignore-submodules-p)))
    (when (or ignore-modules
              magit-buffer-diff-files)
      (insert (propertize (format "%-10s" "Filter! ")
                          'font-lock-face 'magit-section-heading))
      (when ignore-modules
        (insert ignore-modules)
        (when magit-buffer-diff-files
          (insert " -- ")))
      (when magit-buffer-diff-files
        (insert (mapconcat #'identity magit-buffer-diff-files " ")))
      (insert ?\n))))

;;;; Reference Headers

(defun magit-insert-head-branch-header (&optional branch)
  "Insert a header line about the current branch.
If `HEAD' is detached, then insert information about that commit
instead.  The optional BRANCH argument is for internal use only."
  (let ((branch (or branch (magit-get-current-branch)))
        (output (magit-rev-format "%h %s" (or branch "HEAD"))))
    (string-match "^\\([^ ]+\\) \\(.*\\)" output)
    (magit-bind-match-strings (commit summary) output
      (when (equal summary "")
        (setq summary "(no commit message)"))
      (if branch
          (magit-insert-section (branch branch)
            (insert (format "%-10s" "Head: "))
            (when magit-status-show-hashes-in-headers
              (insert (propertize commit 'font-lock-face 'magit-hash) ?\s))
            (insert (propertize branch 'font-lock-face 'magit-branch-local))
            (insert ?\s)
            (insert (funcall magit-log-format-message-function branch summary))
            (insert ?\n))
        (magit-insert-section (commit commit)
          (insert (format "%-10s" "Head: "))
          (insert (propertize commit 'font-lock-face 'magit-hash))
          (insert ?\s)
          (insert (funcall magit-log-format-message-function nil summary))
          (insert ?\n))))))

(defun magit-insert-upstream-branch-header (&optional branch upstream keyword)
  "Insert a header line about the upstream of the current branch.
If no branch is checked out, then insert nothing.  The optional
arguments are for internal use only."
  (when-let ((branch (or branch (magit-get-current-branch))))
    (let ((remote (magit-get "branch" branch "remote"))
          (merge  (magit-get "branch" branch "merge"))
          (rebase (magit-get "branch" branch "rebase")))
      (when (or remote merge)
        (unless upstream
          (setq upstream (magit-get-upstream-branch branch)))
        (magit-insert-section (branch upstream)
          (pcase rebase
            ("true")
            ("false" (setq rebase nil))
            (_       (setq rebase (magit-get-boolean "pull.rebase"))))
          (insert (format "%-10s" (or keyword (if rebase "Rebase: " "Merge: "))))
          (insert
           (if upstream
               (concat (and magit-status-show-hashes-in-headers
                            (concat (propertize (magit-rev-format "%h" upstream)
                                                'font-lock-face 'magit-hash)
                                    " "))
                       upstream " "
                       (funcall magit-log-format-message-function upstream
                                (funcall magit-log-format-message-function nil
                                         (or (magit-rev-format "%s" upstream)
                                             "(no commit message)"))))
             (cond
              ((magit--unnamed-upstream-p remote merge)
               (concat (propertize merge  'font-lock-face 'magit-branch-remote)
                       " from "
                       (propertize remote 'font-lock-face 'bold)))
              ((magit--valid-upstream-p remote merge)
               (if (equal remote ".")
                   (concat
                    (propertize merge 'font-lock-face 'magit-branch-local)
                    (propertize " does not exist"
                                'font-lock-face 'font-lock-warning-face))
                 (concat
                  (propertize merge 'font-lock-face 'magit-branch-remote)
                  (propertize " does not exist on "
                              'font-lock-face 'font-lock-warning-face)
                  (propertize remote 'font-lock-face 'magit-branch-remote))))
              (t
               (propertize "invalid upstream configuration"
                           'font-lock-face 'font-lock-warning-face)))))
          (insert ?\n))))))

(defun magit-insert-push-branch-header ()
  "Insert a header line about the branch the current branch is pushed to."
  (when-let ((branch (magit-get-current-branch))
             (target (magit-get-push-branch branch)))
    (magit-insert-section (branch target)
      (insert (format "%-10s" "Push: "))
      (insert
       (if (magit-rev-verify target)
           (concat target " "
                   (and magit-status-show-hashes-in-headers
                        (concat (propertize (magit-rev-format "%h" target)
                                            'font-lock-face 'magit-hash)
                                " "))
                   (funcall magit-log-format-message-function target
                            (funcall magit-log-format-message-function nil
                                     (or (magit-rev-format "%s" target)
                                         "(no commit message)"))))
         (let ((remote (magit-get-push-remote branch)))
           (if (magit-remote-p remote)
               (concat target
                       (propertize " does not exist"
                                   'font-lock-face 'font-lock-warning-face))
             (concat remote
                     (propertize " remote does not exist"
                                 'font-lock-face 'font-lock-warning-face))))))
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
                   (insert (concat (propertize tag 'font-lock-face 'magit-tag)
                                   (and (> count 0)
                                        (format " (%s)"
                                                (propertize
                                                 (format "%s" count)
                                                 'font-lock-face face)))))))
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
        (insert (propertize name 'font-lock-face 'magit-log-author))
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
  (when-let ((name (magit-get-some-remote))
             ;; Under certain configurations it's possible for url
             ;; to be nil, when name is not, see #2858.
             (url (magit-get "remote" name "url")))
    (magit-insert-section (remote name)
      (insert (format "%-10s" "Remote: "))
      (insert (propertize name 'font-lock-face 'magit-branch-remote) ?\s)
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

If the first element of `magit-buffer-diff-files' is a
directory, then limit the list to files below that.  The value
value of that variable can be set using \"D -- DIRECTORY RET g\"."
  (let* ((show (or (magit-get "status.showUntrackedFiles") "normal"))
         (base (car magit-buffer-diff-files))
         (base (and base (file-directory-p base) base)))
    (unless (equal show "no")
      (if (equal show "all")
          (when-let ((files (magit-untracked-files nil base)))
            (magit-insert-section (untracked)
              (magit-insert-heading "Untracked files:")
              (magit-insert-files files base)
              (insert ?\n)))
        (when-let ((files
                    (--mapcat (and (eq (aref it 0) ??)
                                   (list (substring it 3)))
                              (magit-git-items "status" "-z" "--porcelain"
                                               (magit-ignore-submodules-p t)
                                               "--" base))))
          (magit-insert-section (untracked)
            (magit-insert-heading "Untracked files:")
            (dolist (file files)
              (magit-insert-section (file file)
                (insert (propertize file 'font-lock-face 'magit-filename) ?\n)))
            (insert ?\n)))))))

(magit-define-section-jumper magit-jump-to-tracked "Tracked files" tracked)

(defun magit-insert-tracked-files ()
  "Insert a tree of tracked files.

If the first element of `magit-buffer-diff-files' is a
directory, then limit the list to files below that.  The value
value of that variable can be set using \"D -- DIRECTORY RET g\"."
  (when-let ((files (magit-list-files)))
    (let* ((base (car magit-buffer-diff-files))
           (base (and base (file-directory-p base) base)))
      (magit-insert-section (tracked nil t)
        (magit-insert-heading "Tracked files:")
        (magit-insert-files files base)
        (insert ?\n)))))

(defun magit-insert-ignored-files ()
  "Insert a tree of ignored files.

If the first element of `magit-buffer-diff-files' is a
directory, then limit the list to files below that.  The value
of that variable can be set using \"D -- DIRECTORY RET g\"."
  (when-let ((files (magit-ignored-files)))
    (let* ((base (car magit-buffer-diff-files))
           (base (and base (file-directory-p base) base)))
      (magit-insert-section (tracked nil t)
        (magit-insert-heading "Ignored files:")
        (magit-insert-files files base)
        (insert ?\n)))))

(magit-define-section-jumper magit-jump-to-skip-worktree "Skip-worktree files" skip-worktree)

(defun magit-insert-skip-worktree-files ()
  "Insert a tree of skip-worktree files.

If the first element of `magit-buffer-diff-files' is a
directory, then limit the list to files below that.  The value
of that variable can be set using \"D -- DIRECTORY RET g\"."
  (when-let ((files (magit-skip-worktree-files)))
    (let* ((base (car magit-buffer-diff-files))
           (base (and base (file-directory-p base) base)))
      (magit-insert-section (skip-worktree nil t)
        (magit-insert-heading "Skip-worktree files:")
        (magit-insert-files files base)
        (insert ?\n)))))

(magit-define-section-jumper magit-jump-to-assume-unchanged "Assume-unchanged files" assume-unchanged)

(defun magit-insert-assume-unchanged-files ()
  "Insert a tree of files that are assumed to be unchanged.

If the first element of `magit-buffer-diff-files' is a
directory, then limit the list to files below that.  The value
of that variable can be set using \"D -- DIRECTORY RET g\"."
  (when-let ((files (magit-assume-unchanged-files)))
    (let* ((base (car magit-buffer-diff-files))
           (base (and base (file-directory-p base) base)))
      (magit-insert-section (assume-unchanged nil t)
        (magit-insert-heading "Assume-unchanged files:")
        (magit-insert-files files base)
        (insert ?\n)))))

(defun magit-insert-files (files directory)
  (while (and files (string-prefix-p (or directory "") (car files)))
    (let ((dir (file-name-directory (car files))))
      (if (equal dir directory)
          (let ((file (pop files)))
            (magit-insert-section (file file)
              (insert (propertize file 'font-lock-face 'magit-filename) ?\n)))
        (magit-insert-section (file dir t)
          (insert (propertize dir 'file 'magit-filename) ?\n)
          (magit-insert-heading)
          (setq files (magit-insert-files files dir))))))
  files)

;;; _
(provide 'magit-status)
;;; magit-status.el ends here
