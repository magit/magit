;;; magit.el --- A Git porcelain inside Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2008-2016  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Marius Vollmer <marius.vollmer@gmail.com>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;;	Kyle Meyer        <kyle@kyleam.com>
;;	Noam Postavsky    <npostavs@users.sourceforge.net>
;; Former-Maintainers:
;;	Nicolas Dudebout  <nicolas.dudebout@gatech.edu>
;;	Peter J. Weisberg <pj@irregularexpressions.net>
;;	Phil Jackson      <phil@shellarchive.co.uk>
;;	Rémi Vanicat      <vanicat@debian.org>
;;	Yann Hodique      <yann.hodique@gmail.com>

;; Package-Requires: ((emacs "24.4") (async "20150909.2257") (dash "20151021.113") (with-editor "20160408.201") (git-commit "20160425.430") (magit-popup "20160512.328"))
;; Keywords: git tools vc
;; Homepage: https://github.com/magit/magit

;; Magit requires at least GNU Emacs 24.4 and Git 1.9.4.

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

;; Magit is an interface to the version control system Git,
;; implemented as an Emacs package.  Magit aspires to be a complete
;; Git porcelain.  While we cannot (yet) claim, that Magit wraps and
;; improves upon each and every Git command, it is complete enough to
;; allow even experienced Git users to perform almost all of their
;; daily version control tasks directly from within Emacs.  While many
;; fine Git clients exist, only Magit and Git itself deserve to be
;; called porcelains.

;;; Code:

(require 'cl-lib)
(require 'dash)

(require 'with-editor)
(require 'git-commit)
(require 'magit-core)
(require 'magit-diff)
(require 'magit-apply)
(require 'magit-log)

(require 'format-spec)
(require 'package nil t) ; used in `magit-version'

(eval-when-compile (require 'dired-x))
(declare-function dired-jump 'dired-x)
(eval-when-compile (require 'eshell))
(declare-function eshell-parse-arguments 'eshell)
(eval-when-compile (require 'message))
(declare-function message-goto-body 'message)

(defconst magit--minimal-git "1.9.4")
(defconst magit--minimal-emacs "24.4")

;;; Options
;;;; Status Mode

(defgroup magit-status nil
  "Inspect and manipulate Git repositories."
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
    magit-insert-unpushed-to-upstream
    magit-insert-unpushed-to-pushremote)
  "Hook run to insert sections into a status buffer."
  :package-version '(magit . "2.4.0")
  :group 'magit-status
  :type 'hook)

(defvar magit-status-refresh-hook nil
  "Hook run after a status buffer has been refreshed.")

(make-obsolete-variable 'magit-status-refresh-hook "\
use `magit-pre-refresh-hook', `magit-post-refresh-hook',
  `magit-refresh-buffer-hook', or `magit-status-mode-hook' instead.

  If you want to run a function every time the status buffer is
  refreshed, in order to do something with that buffer, then use:

    (add-hook 'magit-refresh-buffer-hook
              (lambda ()
                (when (derived-mode-p 'magit-status-mode)
                  ...)))

  If your hook function should run regardless of whether the
  status buffer exists or not, then use `magit-pre-refresh-hook'
  or `magit-post-refresh-hook'.

  If your hook function only has to be run once, when the buffer
  is first created, then `magit-status-mode-hook' instead.
" "Magit 2.4.0")

(defcustom magit-status-expand-stashes t
  "Whether the list of stashes is expanded initially."
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

;;;; Refs Mode

(defgroup magit-refs nil
  "Inspect and manipulate Git branches and tags."
  :group 'magit-modes)

(defcustom magit-refs-mode-hook nil
  "Hook run after entering Magit-Refs mode."
  :package-version '(magit . "2.1.0")
  :group 'magit-refs
  :type 'hook)

(defcustom magit-refs-sections-hook
  '(magit-insert-error-header
    magit-insert-branch-description
    magit-insert-local-branches
    magit-insert-remote-branches
    magit-insert-tags)
  "Hook run to insert sections into a references buffer."
  :package-version '(magit . "2.1.0")
  :group 'magit-refs
  :type 'hook)

(defcustom magit-refs-show-commit-count nil
  "Whether to show commit counts in Magit-Refs mode buffers.

all    Show counts for branches and tags.
branch Show counts for branches only.
nil    Never show counts.

To change the value in an existing buffer use the command
`magit-refs-show-commit-count'"
  :package-version '(magit . "2.1.0")
  :group 'magit-refs
  :safe (lambda (val) (memq val '(all branch nil)))
  :type '(choice (const all    :tag "For branches and tags")
                 (const branch :tag "For branches only")
                 (const nil    :tag "Never")))
(put 'magit-refs-show-commit-count 'safe-local-variable 'symbolp)
(put 'magit-refs-show-commit-count 'permanent-local t)

(defcustom magit-refs-show-margin 'branch
  "Whether to initially show the margin in refs buffers.

When non-nil the committer name and date are initially displayed
in the margin of refs buffers.  The margin can be shown or hidden
in the current buffer using the command `magit-toggle-margin'."
  :package-version '(magit . "2.1.0")
  :group 'magit-refs
  :safe (lambda (val) (memq val '(all branch nil)))
  :type '(choice (const all    :tag "For branches and tags")
                 (const branch :tag "For branches only")
                 (const nil    :tag "Never")))

(defcustom magit-visit-ref-create nil
  "Whether `magit-visit-ref' may create new branches.

When this is non-nil, then \"visiting\" a remote branch in a
refs buffer works by creating a new local branch which tracks
the remote branch and then checking out the new local branch."
  :package-version '(magit . "2.1.0")
  :group 'magit-refs
  :group 'magit-commands
  :type 'boolean)

;;;; Miscellaneous

(defcustom magit-branch-read-upstream-first t
  "When creating a branch, read upstream before name of new branch."
  :package-version '(magit . "2.2.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-branch-prefer-remote-upstream nil
  "Whether to favor remote upstreams when creating new branches.

When a new branch is created, Magit offers the branch, commit, or
stash as the default starting point of the new branch.  If there
is no such thing at point, then it falls back to offer the
current branch as starting-point.  The user may then accept that
default or pick something else.

If the chosen starting-point is a branch, then it may also be set
as the upstream of the new branch, depending on the value of the
Git variable `branch.autoSetupMerge'.  By default this is done
for remote branches, but not for local branches.

You might prefer to always use some remote branch as upstream.
If the chosen starting-point is (1) a local branch, (2) whose
name is a member of the value of this option, (3) the upstream of
that local branch is a remote branch with the same name, and (4)
that remote branch can be fast-forwarded to the local branch,
then the chosen branch is used as starting-point, but its own
upstream is used as the upstream of the new branch.

Assuming the chosen branch matches these conditions you would end
up with with e.g.:

  feature --upstream--> origin/master

instead of

  feature --upstream--> master --upstream--> origin/master

Which you prefer is a matter of personal preference.  If you do
prefer the former, then you should add branches such as \"master\",
\"next\", and \"maint\" to the value of this options."
  :package-version '(magit . "2.4.0")
  :group 'magit-commands
  :type '(repeat string))

(defcustom magit-branch-popup-show-variables t
  "Whether the `magit-branch-popup' shows Git variables.
This defaults to t to avoid changing key bindings.  When set to
nil, no variables are displayed directly in this popup, instead
the sub-popup `magit-branch-config-popup' has to be used to view
and change branch related variables."
  :package-version '(magit . "2.7.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-repository-directories nil
  "List of directories that are or contain Git repositories.
Each element has the form (DIRECTORY . DEPTH) or, for backward
compatibility, just DIRECTORY.  DIRECTORY has to be a directory
or a directory file-name, a string.  DEPTH, an integer, specifies
the maximum depth to look for Git repositories.  If it is 0, then
only add DIRECTORY itself.  For elements that are strings, the
value of option `magit-repository-directories-depth' specifies
the depth."
  :package-version '(magit . "2.7.1")
  :group 'magit
  :type '(repeat (choice (cons directory (integer :tag "Depth")) directory)))

(defcustom magit-repository-directories-depth 3
  "The maximum depth to look for Git repositories.
This option is obsolete and only used for elements of the option
`magit-repository-directories' (which see) that don't specify the
depth directly."
  :group 'magit
  :type 'integer)

(defcustom magit-repolist-columns
  '(("Name"    25 magit-repolist-column-ident                  nil)
    ("Version" 25 magit-repolist-column-version                nil)
    ("L<U"      3 magit-repolist-column-unpulled-from-upstream (:right-align t))
    ("L>U"      3 magit-repolist-column-unpushed-to-upstream   (:right-align t))
    ("Path"    99 magit-repolist-column-path))
  "List of columns displayed by `magit-list-repositories'.

Each element has the form (HEADER WIDTH FORMAT PROPS).

HEADER is the string displayed in the header.  WIDTH is the width
of the column.  FORMAT is a function that is called with one
argument, the repository identification (usually its basename),
and with `default-directory' bound to the toplevel of its working
tree.  It has to return a string to be inserted or nil.  PROPS is
an alist that supports the keys ~:right-align~ and ~:pad-right~."
  :package-version '(magit . "2.7.1")
  :group 'magit-commands
  :type `(repeat (list :tag "Column"
                       (string   :tag "Header Label")
                       (integer  :tag "Column Width")
                       (function :tag "Inserter Function")
                       (repeat   :tag "Properties"
                                 (list (choice :tag "Property"
                                               (const :right-align)
                                               (const :pad-right)
                                               (symbol))
                                       (sexp   :tag "Value"))))))

;;;; Faces

(defface magit-header-line
  '((t :inherit magit-section-heading))
  "Face for the `header-line'."
  :group 'magit-faces)

(defface magit-dimmed
  '((((class color) (background light)) :foreground "grey50")
    (((class color) (background  dark)) :foreground "grey50"))
  "Face for text that shouldn't stand out."
  :group 'magit-faces)

(defface magit-hash
  '((((class color) (background light)) :foreground "grey60")
    (((class color) (background  dark)) :foreground "grey40"))
  "Face for the sha1 part of the log output."
  :group 'magit-faces)

(defface magit-tag
  '((((class color) (background light)) :foreground "Goldenrod4")
    (((class color) (background  dark)) :foreground "LightGoldenrod2"))
  "Face for tag labels shown in log buffer."
  :group 'magit-faces)

(defface magit-branch-remote
  '((((class color) (background light)) :foreground "DarkOliveGreen4")
    (((class color) (background  dark)) :foreground "DarkSeaGreen2"))
  "Face for remote branch head labels shown in log buffer."
  :group 'magit-faces)

(defface magit-branch-local
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background  dark)) :foreground "LightSkyBlue1"))
  "Face for local branches."
  :group 'magit-faces)

(defface magit-branch-current
  '((((class color) (background light)) :inherit magit-branch-local :box t)
    (((class color) (background  dark)) :inherit magit-branch-local :box t))
  "Face for current branch."
  :group 'magit-faces)

(defface magit-head
  '((((class color) (background light)) :inherit magit-branch-local)
    (((class color) (background  dark)) :inherit magit-branch-local))
  "Face for the symbolic ref \"HEAD\"."
  :group 'magit-faces)

(defface magit-refname
  '((((class color) (background light)) :foreground "grey30")
    (((class color) (background  dark)) :foreground "grey80"))
  "Face for refnames without a dedicated face."
  :group 'magit-faces)

(defface magit-refname-stash
  '((t :inherit magit-refname))
  "Face for wip refnames."
  :group 'magit-faces)

(defface magit-refname-wip
  '((t :inherit magit-refname))
  "Face for wip refnames."
  :group 'magit-faces)

(defface magit-signature-good
  '((t :foreground "green"))
  "Face for good signatures."
  :group 'magit-faces)

(defface magit-signature-bad
  '((t :foreground "red"))
  "Face for bad signatures."
  :group 'magit-faces)

(defface magit-signature-untrusted
  '((t :foreground "cyan"))
  "Face for good untrusted signatures."
  :group 'magit-faces)

(defface magit-cherry-unmatched
  '((t :foreground "cyan"))
  "Face for unmatched cherry commits."
  :group 'magit-faces)

(defface magit-cherry-equivalent
  '((t :foreground "magenta"))
  "Face for equivalent cherry commits."
  :group 'magit-faces)

(defface magit-filename
  '((t :weight normal))
  "Face for filenames."
  :group 'magit-faces)

;;; Inspect
;;;; Status Mode
;;;;; Status Core

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
    map)
  "Keymap for `magit-status-mode'.")

(eval-after-load 'dired-x
  '(define-key magit-status-mode-map [remap dired-jump] 'magit-dired-jump))

(define-derived-mode magit-status-mode magit-mode "Magit"
  "Mode for looking at Git status.

This mode is documented in info node `(magit)Status buffer'.

\\<magit-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-section-toggle] to expand or hide the section at point.
Type \\[magit-visit-thing] to visit the change or commit at point.

Type \\[magit-dispatch-popup] to see available prefix popups.

Staging and applying changes is documented in info node
`(magit)Staging and unstaging' and info node `(magit)Applying'.

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
  (hack-dir-local-variables-non-file-buffer))

;;;###autoload
(defun magit-status (&optional directory)
  "Show the status of the current Git repository in a buffer.
With a prefix argument prompt for a repository to be shown.
With two prefix arguments prompt for an arbitrary directory.
If that directory isn't the root of an existing repository
then offer to initialize it as a new repository."
  (interactive
   (list (and (or current-prefix-arg (not (magit-toplevel)))
              (magit-read-repository
               (>= (prefix-numeric-value current-prefix-arg) 16)))))
  (if directory
      (let ((toplevel (magit-toplevel directory)))
        (setq directory (file-name-as-directory (expand-file-name directory)))
        (if (and toplevel (string-equal directory toplevel))
            (magit-status-internal directory)
          (when (y-or-n-p
                 (if toplevel
                     (format "%s is a repository.  Create another in %s? "
                             toplevel directory)
                   (format "Create repository in %s? " directory)))
            (magit-init directory))))
    (magit-status-internal default-directory)))

(put 'magit-status 'interactive-only 'magit-status-internal)

;;;###autoload
(defun magit-status-internal (directory)
  (magit-tramp-asserts directory)
  (let ((default-directory directory))
    (magit-mode-setup #'magit-status-mode)))

;;;;; Standard Status Sections

(defvar magit-status-sections-hook-1 nil)

(defun magit-status-refresh-buffer ()
  (magit-git-exit-code "update-index" "--refresh")
  (magit-insert-section (status)
    (if (-all-p #'functionp magit-status-sections-hook)
        (run-hooks 'magit-status-sections-hook)
      (message "`magit-status-sections-hook' contains entries that are \
no longer valid.\nUsing standard value instead.  Please re-configure")
      (sit-for 5)
      (let ((magit-status-sections-hook-1
             (eval (car (get 'magit-status-sections-hook 'standard-value)))))
        (run-hooks 'magit-status-sections-hook-1))))
  (run-hooks 'magit-status-refresh-hook))

(defun magit-insert-status-headers ()
  "Insert header sections appropriate for `magit-status-mode' buffers.
The sections are inserted by running the functions on the hook
`magit-status-headers-hook'."
  (if (magit-rev-verify "HEAD")
      (magit-insert-headers magit-status-headers-hook)
    (insert "In the beginning there was darkness\n\n")))

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
            (insert ?\s summary ?\n))
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
      (insert (format "%-10s"
                      (or keyword
                          (if (magit-get-boolean "branch" branch "rebase")
                              "Rebase: "
                            "Merge: "))))
      (--when-let (and magit-status-show-hashes-in-headers
                       (magit-rev-format "%h" pull))
        (insert (propertize it 'face 'magit-hash) ?\s))
      (insert (propertize pull 'face
                          (if (string= (magit-get "branch" branch "remote") ".")
                              'magit-branch-local
                            'magit-branch-remote)))
      (insert ?\s)
      (if (magit-rev-verify pull)
          (insert (or (magit-rev-format "%s" pull) ""))
        (insert (propertize "is missing" 'face 'font-lock-warning-face)))
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
      (if (magit-rev-verify push)
          (insert (or (magit-rev-format "%s" push) ""))
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
        (when this-tag
          (insert (magit-format-status-tag-sentence this-tag this-cnt nil)))
        (when both-tags
          (insert ", "))
        (when next-tag
          (insert (magit-format-status-tag-sentence next-tag next-cnt t)))
        (insert ?\n)))))

(defun magit-format-status-tag-sentence (tag count next)
  (concat (propertize tag 'face 'magit-tag)
          (and (> count 0)
               (format " (%s)"
                       (propertize (format "%s" count) 'face
                                   (if next 'magit-tag 'magit-branch-local))))))

(defun magit-insert-diff-filter-header ()
  "Insert a header line showing the effective diff filters."
  (when magit-diff-section-file-args
    (magit-insert-section (filter 'diff)
      (insert (propertize (format "%-10s" "Filter! ")
                          'face 'magit-section-heading))
      (insert (mapconcat #'identity magit-diff-section-file-args " "))
      (insert ?\n))))

(magit-define-section-jumper magit-jump-to-untracked "Untracked files" untracked)

(defvar magit-untracked-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-delete-thing] 'magit-discard)
    (define-key map "s" 'magit-stage)
    map)
  "Keymap for the `untracked' section.")

(defun magit-insert-untracked-files ()
  "Maybe insert a list or tree of untracked files.
Do so depending on the value of `status.showUntrackedFiles'."
  (let ((show (or (magit-get "status.showUntrackedFiles") "normal")))
    (unless (equal show "no")
      (if (equal show "all")
          (-when-let (files (magit-untracked-files))
            (magit-insert-section (untracked)
              (magit-insert-heading "Untracked files:")
              (magit-insert-un/tracked-files-1 files nil)
              (insert ?\n)))
        (-when-let
            (files (--mapcat (and (eq (aref it 0) ??)
                                  (list (substring it 3)))
                             (magit-git-items "status" "-z" "--porcelain")))
          (magit-insert-section (untracked)
            (magit-insert-heading "Untracked files:")
            (dolist (file files)
              (magit-insert-section (file file)
                (insert (propertize file 'face 'magit-filename) ?\n)))
            (insert ?\n)))))))

(defun magit-insert-un/tracked-files-1 (files directory)
  (while (and files (string-prefix-p (or directory "") (car files)))
    (let ((dir (file-name-directory (car files))))
      (if (equal dir directory)
          (let ((file (pop files)))
            (magit-insert-section (file file)
              (insert (propertize file 'face 'magit-filename) ?\n)))
        (magit-insert-section (file dir t)
          (insert (propertize dir 'file 'magit-filename) ?\n)
          (magit-insert-heading)
          (setq files (magit-insert-un/tracked-files-1 files dir))))))
  files)

;;;;; Auxiliary Status Sections

(magit-define-section-jumper magit-jump-to-tracked "Tracked files" tracked)

(defun magit-insert-tracked-files ()
  "Insert a tree of tracked files."
  (-when-let (files (magit-list-files))
    (magit-insert-section (tracked nil t)
      (magit-insert-heading "Tracked files:")
      (magit-insert-un/tracked-files-1 files nil)
      (insert ?\n))))

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
  (--when-let (or (magit-get-remote)
                  (let ((remotes (magit-list-remotes)))
                    (or (car (member "origin" remotes))
                        (car remotes))))
    (magit-insert-section (remote it)
      (insert (format "%-10s" "Remote: "))
      (insert (propertize it 'face 'magit-branch-remote) ?\s)
      (insert (magit-get "remote" it "url") ?\n))))

;;;;; Status Miscellaneous

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

;;;; Refs Mode

(defvar magit-refs-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "\C-y" 'magit-refs-set-show-commit-count)
    map)
  "Keymap for `magit-refs-mode'.")

(define-derived-mode magit-refs-mode magit-mode "Magit Refs"
  "Mode which lists and compares references.

This mode is documented in info node `(magit)References buffer'.

\\<magit-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-section-toggle] to expand or hide the section at point.
Type \\[magit-visit-thing] or \\[magit-diff-show-or-scroll-up] \
to visit the commit or branch at point.

Type \\[magit-branch-popup] to see available branch commands.
Type \\[magit-merge-popup] to merge the branch or commit at point.
Type \\[magit-cherry-pick-popup] to apply the commit at point.
Type \\[magit-reset] to reset HEAD to the commit at point.

\\{magit-refs-mode-map}"
  :group 'magit-refs
  (hack-dir-local-variables-non-file-buffer))

;;;###autoload (autoload 'magit-show-refs-popup "magit" nil t)
(magit-define-popup magit-show-refs-popup
  "Popup console for `magit-show-refs'."
  'magit-refs
  :man-page "git-branch"
  :switches '((?m "Merged to HEAD"            "--merged")
              (?M "Merged to master"          "--merged=master")
              (?n "Not merged to HEAD"        "--no-merged")
              (?N "Not merged to master"      "--no-merged=master"))
  :options  '((?c "Contains"   "--contains="  magit-read-branch-or-commit)
              (?m "Merged"     "--merged="    magit-read-branch-or-commit)
              (?n "Not merged" "--no-merged=" magit-read-branch-or-commit))
  :actions  '((?y "Show refs, comparing them with HEAD"
                  magit-show-refs-head)
              (?c "Show refs, comparing them with current branch"
                  magit-show-refs-current)
              (?o "Show refs, comparing them with other branch"
                  magit-show-refs))
  :default-action 'magit-show-refs-head
  :use-prefix 'popup)

;;;###autoload
(defun magit-show-refs-head (&optional args)
  "List and compare references in a dedicated buffer.
Refs are compared with `HEAD'."
  (interactive (list (magit-show-refs-arguments)))
  (magit-show-refs nil args))

;;;###autoload
(defun magit-show-refs-current (&optional args)
  "List and compare references in a dedicated buffer.
Refs are compared with the current branch or `HEAD' if
it is detached."
  (interactive (list (magit-show-refs-arguments)))
  (magit-show-refs (magit-get-current-branch) args))

;;;###autoload
(defun magit-show-refs (&optional ref args)
  "List and compare references in a dedicated buffer.
Refs are compared with a branch read form the user."
  (interactive (list (magit-read-other-branch "Compare with")
                     (magit-show-refs-arguments)))
  (magit-mode-setup #'magit-refs-mode ref args))

(defun magit-refs-refresh-buffer (&rest _ignore)
  (setq magit-set-buffer-margin-refresh (not magit-show-margin))
  (unless (magit-rev-verify (or (car magit-refresh-args) "HEAD"))
    (setq magit-refs-show-commit-count nil))
  (magit-insert-section (branchbuf)
    (run-hooks 'magit-refs-sections-hook)))

(defun magit-insert-branch-description ()
  "Insert header containing the description of the current branch.
Insert a header line with the name and description of the
current branch.  The description is taken from the Git variable
`branch.<NAME>.description'; if that is undefined then no header
line is inserted at all."
  (-when-let* ((branch (magit-get-current-branch))
               (desc (magit-get "branch" branch "description"))
               (desc-lines (split-string desc "\n")))
    (magit-insert-section (branchdesc branch t)
      (magit-insert-heading branch ": " (car desc-lines))
      (insert (mapconcat 'identity (cdr desc-lines) "\n"))
      (insert "\n\n"))))

(defconst magit-refs-branch-line-re
  (concat "^"
          "\\(?:[ \\*]\\) "
          "\\(?1:([^)]+)\\|[^ ]+?\\)"       ; branch
          "\\(?: +\\)"
          "\\(?2:[0-9a-fA-F]+\\) "          ; sha1
          "\\(?:\\["
          "\\(?4:[^:]+\\)"                  ; upstream
          "\\(?:: \\(?:"
          "\\(?7:gone\\)\\|"                ; gone
          "\\(?:ahead \\(?5:[0-9]+\\)\\)?"  ; ahead
          "\\(?:, \\)?"
          "\\(?:behind \\(?6:[0-9]+\\)\\)?" ; behind
          "\\)\\)?"
          "\\] \\)?"
          "\\(?3:.*\\)"))                   ; message

(defconst magit-refs-symref-line-re "^  \\([^ ]+\\) +-> \\(.+\\)")

(defvar magit-refs-local-branch-format "%4c %-25n %U%m\n"
  "Format used for local branches in refs buffers.")
(defvar magit-refs-remote-branch-format "%4c %-25n %m\n"
  "Format used for remote branches in refs buffers.")
(defvar magit-refs-symref-format "%4c %-25n -> %m\n"
  "Format used for symrefs in refs buffers.")
(defvar magit-refs-tags-format "%4c %-25n %m\n"
  "Format used for tags in refs buffers.")
(defvar magit-refs-indent-cherry-lines 3
  "Indentation of cherries in refs buffers.")

(defvar magit-branch-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing]  'magit-visit-ref)
    (define-key map [remap magit-delete-thing] 'magit-branch-delete)
    (define-key map "R" 'magit-branch-rename)
    map)
  "Keymap for `branch' sections.")

(defvar magit-remote-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-delete-thing] 'magit-remote-remove)
    (define-key map "R" 'magit-remote-rename)
    map)
  "Keymap for `remote' sections.")

(defun magit-refs-set-show-commit-count ()
  "Change for which refs the commit count is shown."
  (interactive)
  (setq-local magit-refs-show-commit-count
              (magit-read-char-case "Show commit counts for " nil
                (?a "[a]ll refs" 'all)
                (?b "[b]ranches only" t)
                (?n "[n]othing" nil)))
  (magit-refresh))

(defun magit-visit-ref ()
  "Visit the reference or revision at point.

In most places use `magit-show-commit' to visit the reference or
revision at point.

In `magit-refs-mode', when there is a reference at point, instead
checkout that reference.  When option `magit-visit-ref-create' is
non-nil and point is on remote branch, then create a local branch
with the same name and check it out.

With a prefix argument only focus on the reference at point, i.e.
the commit counts and cherries are updated to be relative to that
reference, but it is not checked out."
  (interactive)
  (if (derived-mode-p 'magit-refs-mode)
      (magit-section-case
        (([branch * branchbuf]
          [tag    * branchbuf])
         (let ((ref (magit-section-value (magit-current-section))))
           (if current-prefix-arg
               (magit-show-refs ref)
             (if (magit-section-when [branch remote])
                 (let ((start ref)
                       (arg "-b"))
                   (string-match "^[^/]+/\\(.+\\)" ref)
                   (setq ref (match-string 1 ref))
                   (when (magit-branch-p ref)
                     (if (yes-or-no-p
                          (format "Branch %s already exists.  Recreate it?" ref))
                         (setq arg "-B")
                       (user-error "Abort")))
                   (magit-run-git "checkout" arg ref start))
               (magit-run-git "checkout" ref))
             (setcar magit-refresh-args ref)
             (magit-refresh))))
        ([commit * branchbuf]
         (call-interactively #'magit-show-commit)))
    (call-interactively #'magit-show-commit)))

(defun magit-insert-local-branches ()
  "Insert sections showing all local branches."
  (magit-insert-section (local nil)
    (magit-insert-heading "Branches:")
    (let ((current  (magit-get-current-branch))
          (branches (magit-list-local-branch-names)))
      (dolist (line (magit-git-lines "branch" "-vv"
                                     (cadr magit-refresh-args)))
        (cond
         ((string-match magit-refs-branch-line-re line)
          (magit-bind-match-strings
              (branch hash message upstream ahead behind gone) line
            (when (string-match-p "(HEAD detached" branch)
              (setq branch nil))
            (magit-insert-branch
             branch magit-refs-local-branch-format current branches
             'magit-branch-local hash message upstream ahead behind gone)))
         ((string-match magit-refs-symref-line-re line)
          (magit-bind-match-strings (symref ref) line
            (magit-insert-symref symref ref 'magit-branch-local))))))
    (insert ?\n)))

(defun magit-insert-remote-branches ()
  "Insert sections showing all remote-tracking branches."
  (dolist (remote (magit-list-remotes))
    (magit-insert-section (remote remote)
      (magit-insert-heading
        (let ((pull (magit-get "remote" remote "url"))
              (push (magit-get "remote" remote "pushurl")))
          (format "%s (%s):" (capitalize remote)
                  (concat pull (and pull push ", ") push))))
      (let ((current  (magit-get-current-branch))
            (branches (magit-list-local-branch-names)))
        (dolist (line (magit-git-lines "branch" "-vvr"
                                       (cadr magit-refresh-args)))
          (cond
           ((string-match magit-refs-branch-line-re line)
            (magit-bind-match-strings (branch hash message) line
              (when (string-match-p (format "^%s/" remote) branch)
                (magit-insert-branch
                 branch magit-refs-remote-branch-format current branches
                 'magit-branch-remote hash message))))
           ((string-match magit-refs-symref-line-re line)
            (magit-bind-match-strings (symref ref) line
              (magit-insert-symref symref ref 'magit-branch-remote))))))
      (insert ?\n))))

(defun magit-insert-branch (branch format &rest args)
  "For internal use, don't add to a hook."
  (unless magit-refs-show-commit-count
    (setq format (replace-regexp-in-string "%[0-9]\\([cC]\\)" "%1\\1" format t)))
  (if (equal branch "HEAD")
      (magit-insert-section it (commit (magit-rev-parse "HEAD") t)
        (apply #'magit-insert-branch-1 it nil format args))
    (magit-insert-section it (branch branch t)
      (apply #'magit-insert-branch-1 it branch format args))))

(defun magit-insert-branch-1
    (section branch format current branches face
             &optional hash message upstream ahead behind gone)
  "For internal use, don't add to a hook."
  (let* ((head  (or (car magit-refresh-args) current "HEAD"))
         (count (and branch
                     (magit-refs-format-commit-count branch head format)))
         (mark  (cond ((or (equal branch head)
                           (and (not branch) (equal head "HEAD")))
                       (if (equal branch current)
                           (propertize "@" 'face 'magit-head)
                         (propertize "#" 'face 'magit-tag)))
                      ((equal branch current)
                       (propertize "." 'face 'magit-head)))))
    (when upstream
      (setq upstream (propertize upstream 'face
                                 (if (member upstream branches)
                                     'magit-branch-local
                                   'magit-branch-remote))))
    (magit-insert-heading
      (format-spec
       format
       `((?a . ,(or ahead ""))
         (?b . ,(or behind ""))
         (?c . ,(or mark count ""))
         (?C . ,(or mark " "))
         (?h . ,(or (propertize hash 'face 'magit-hash) ""))
         (?m . ,(or message ""))
         (?n . ,(propertize (or branch "(detached)") 'face face))
         (?u . ,(or upstream ""))
         (?U . ,(if upstream
                    (format (propertize "[%s%s] " 'face 'magit-dimmed)
                            upstream
                            (cond
                             (gone
                              (concat ": " (propertize gone 'face 'error)))
                             ((or ahead behind)
                              (concat ": "
                                      (and ahead (format "ahead %s" ahead))
                                      (and ahead behind ", ")
                                      (and behind (format "behind %s" behind))))
                             (t "")))
                  "")))))
    (when magit-show-margin
      (magit-refs-format-margin branch))
    (magit-refs-insert-cherry-commits head branch section)))

(defun magit-insert-symref (symref ref face)
  "For internal use, don't add to a hook."
  (magit-insert-section (commit symref)
    (insert
     (format-spec (if magit-refs-show-commit-count
                      magit-refs-symref-format
                    (replace-regexp-in-string "%[0-9]\\([cC]\\)" "%1\\1"
                                              magit-refs-symref-format t))
                  `((?c . "")
                    (?n . ,(propertize symref 'face face))
                    (?m . ,(propertize ref    'face face)))))))

(defvar magit-tag-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing]  'magit-visit-ref)
    (define-key map [remap magit-delete-thing] 'magit-tag-delete)
    map)
  "Keymap for `tag' sections.")

(defun magit-insert-tags ()
  "Insert sections showing all tags."
  (-when-let (tags (magit-git-lines "tag" "-l" "-n"))
    (magit-insert-section (tags)
      (magit-insert-heading "Tags:")
      (let ((head (or (car magit-refresh-args)
                      (magit-get-current-branch)
                      "HEAD"))
            (format (if magit-refs-show-commit-count
                        magit-refs-tags-format
                      (replace-regexp-in-string
                       "%[0-9]\\([cC]\\)" "%1\\1" magit-refs-tags-format t))))
        (dolist (tag (nreverse tags))
          (string-match "^\\([^ \t]+\\)[ \t]+\\([^ \t\n].*\\)?" tag)
          (let* ((message (match-string 2 tag))
                 (tag     (match-string 1 tag))
                 (count   (magit-refs-format-commit-count tag head format t))
                 (mark    (and (equal tag head)
                               (propertize "#" 'face 'magit-tag))))
            (magit-insert-section section (tag tag t)
              (magit-insert-heading
               (format-spec format
                            `((?n . ,(propertize tag 'face 'magit-tag))
                              (?c . ,(or mark count ""))
                              (?m . ,(or message "")))))
              (when (and magit-show-margin
                         (eq magit-refs-show-margin 'all))
                (magit-refs-format-margin (concat tag "^{commit}")))
              (magit-refs-insert-cherry-commits head tag section)))))
      (insert ?\n))))

(defun magit-refs-insert-cherry-commits (head ref section)
  (if (magit-section-hidden section)
      (setf (magit-section-washer section)
            (apply-partially #'magit-refs-insert-cherry-commits-1
                             head ref section))
    (magit-refs-insert-cherry-commits-1 head ref section)))

(defun magit-refs-insert-cherry-commits-1 (head ref section)
  (let ((start (point)))
    (magit-git-wash (apply-partially 'magit-log-wash-log 'cherry)
      "cherry" "-v" "--abbrev" head ref magit-refresh-args)
    (unless (= (point) start)
      (insert (propertize "\n" 'magit-section section)))))

(defun magit-refs-format-commit-count (ref head format &optional tag-p)
  (and (string-match-p "%-?[0-9]+c" format)
       (if tag-p
           (eq magit-refs-show-commit-count 'all)
         magit-refs-show-commit-count)
       (let ((count (cadr (magit-rev-diff-count head ref))))
         (and (> count 0)
              (propertize (number-to-string count) 'face 'magit-dimmed)))))

(defun magit-refs-format-margin (commit)
  (save-excursion
    (goto-char (line-beginning-position 0))
    (let ((line (magit-rev-format "%ct%cN" commit)))
      (magit-format-log-margin (substring line 10)
                               (substring line 0 10)))))

;;;; Files

;;;###autoload
(defun magit-find-file (rev file)
  "View FILE from REV.
Switch to a buffer visiting blob REV:FILE,
creating one if none already exists."
  (interactive (magit-find-file-read-args "Find file"))
  (switch-to-buffer (magit-find-file-noselect rev file)))

;;;###autoload
(defun magit-find-file-other-window (rev file)
  "View FILE from REV, in another window.
Like `magit-find-file', but create a new window or reuse an
existing one."
  (interactive (magit-find-file-read-args "Find file in other window"))
  (switch-to-buffer-other-window (magit-find-file-noselect rev file)))

(defun magit-find-file-read-args (prompt)
  (let  ((rev (magit-read-branch-or-commit "Find file from revision")))
    (list rev (magit-read-file-from-rev rev prompt))))

(defvar magit-read-file-hist nil)

(defun magit-read-file-from-rev (rev prompt &optional default)
  (let ((files (magit-revision-files rev)))
    (magit-completing-read
     prompt files nil t nil 'magit-read-file-hist
     (car (member (or default (magit-current-file)) files)))))

(defun magit-read-changed-file (rev-or-range prompt &optional default)
  (magit-read-file-choice
   prompt
   (magit-changed-files rev-or-range)
   default
   (concat "No file changed in " rev-or-range)))

(defun magit-get-revision-buffer (rev file &optional create)
  (funcall (if create 'get-buffer-create 'get-buffer)
           (format "%s.~%s~" file (subst-char-in-string ?/ ?_ rev))))

(defun magit-get-revision-buffer-create (rev file)
  (magit-get-revision-buffer rev file t))

(defvar magit-find-file-hook nil)

(defun magit-find-file-noselect (rev file)
  "Read FILE from REV into a buffer and return the buffer.
FILE must be relative to the top directory of the repository."
  (let ((topdir (magit-toplevel)))
    (when (file-name-absolute-p file)
      (setq file (file-relative-name file topdir)))
    (or (magit-get-revision-buffer rev file)
        (with-current-buffer (magit-get-revision-buffer-create rev file)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (magit-git-insert "cat-file" "-p" (concat rev ":" file)))
          (setq magit-buffer-revision  (magit-rev-format "%H" rev)
                magit-buffer-refname   rev
                magit-buffer-file-name (expand-file-name file topdir))
          (let ((buffer-file-name magit-buffer-file-name)
                (after-change-major-mode-hook
                 (remq 'global-diff-hl-mode-enable-in-buffers
                       after-change-major-mode-hook)))
            (normal-mode t))
          (setq buffer-read-only t)
          (set-buffer-modified-p nil)
          (goto-char (point-min))
          (magit-blob-mode 1)
          (run-hooks 'magit-find-file-hook)
          (current-buffer)))))

(defvar magit-find-index-hook nil)

(defun magit-find-file-index-noselect (file &optional revert)
  "Read FILE from the index into a buffer and return the buffer.
FILE must to be relative to the top directory of the repository."
  (let* ((bufname (concat file ".~{index}~"))
         (origbuf (get-buffer bufname))
         (default-directory (magit-toplevel)))
    (with-current-buffer (get-buffer-create bufname)
      (when (or (not origbuf) revert
                (y-or-n-p (format "%s already exists; revert it? " bufname)))
        (let ((inhibit-read-only t)
              (temp (car (split-string
                          (or (magit-git-string "checkout-index" "--temp" file)
                              (error "Error making temp file"))
                          "\t"))))
          (erase-buffer)
          (insert-file-contents temp nil nil nil t)
          (delete-file temp)))
      (setq magit-buffer-revision  "{index}"
            magit-buffer-refname   "{index}"
            magit-buffer-file-name (expand-file-name file))
      (let ((buffer-file-name magit-buffer-file-name))
        (normal-mode t))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (run-hooks 'magit-find-index-hook)
      (current-buffer))))

(defun magit-update-index ()
  "Update the index with the contents of the current buffer.
The current buffer has to be visiting a file in the index, which
is done using `magit-find-index-noselect'."
  (interactive)
  (let ((file (magit-file-relative-name)))
    (unless (equal magit-buffer-refname "{index}")
      (user-error "%s isn't visiting the index" file))
    (if (y-or-n-p (format "Update index with contents of %s" (buffer-name)))
        (let ((index (make-temp-file "index"))
              (buffer (current-buffer)))
          (when magit-wip-before-change-mode
            (magit-wip-commit-before-change (list file) " before un-/stage"))
          (with-temp-file index
            (insert-buffer-substring buffer))
          (magit-call-git "update-index" "--cacheinfo"
                          (substring (magit-git-string "ls-files" "-s" file) 0 6)
                          (magit-git-string "hash-object" "-t" "blob" "-w"
                                            (concat "--path=" file)
                                            "--" index)
                          file)
          (set-buffer-modified-p nil)
          (when magit-wip-after-apply-mode
            (magit-wip-commit-after-apply (list file) " after un-/stage")))
      (message "Abort")))
  (--when-let (magit-mode-get-buffer 'magit-status-mode)
    (with-current-buffer it (magit-refresh)))
  t)

;;;###autoload
(defun magit-dired-jump (&optional other-window)
  "Visit file at point using Dired.
With a prefix argument, visit in other window.  If there
is no file at point then instead visit `default-directory'."
  (interactive "P")
  (dired-jump other-window (--if-let (magit-file-at-point)
                               (expand-file-name it)
                             default-directory)))

;;;###autoload
(defun magit-checkout-file (rev file)
  "Checkout FILE from REV."
  (interactive
   (let ((rev (magit-read-branch-or-commit
               "Checkout from revision" magit-buffer-revision)))
     (list rev (magit-read-file-from-rev rev "Checkout file"))))
  (magit-with-toplevel
    (magit-run-git "checkout" rev "--" file)))

;;; Manipulate
;;;; Init

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
       (unless (y-or-n-p (if (string-equal toplevel directory)
                             (format "Reinitialize existing repository %s? "
                                     directory)
                           (format "%s is a repository.  Create another in %s? "
                                   toplevel directory)))
         (user-error "Abort")))
     (list directory)))
  ;; `git init' does not understand the meaning of "~"!
  (magit-call-git "init" (magit-convert-git-filename
                          (expand-file-name directory)))
  (magit-status-internal directory))

;;;; Branch
;;;;; Branch Popup

;;;###autoload (autoload 'magit-branch-popup "magit" nil t)
(magit-define-popup magit-branch-popup
  "Popup console for branch commands."
  'magit-commands
  :man-page "git-branch"
  :actions '((?b "Checkout"              magit-checkout)
             (?n "Create new branch"     magit-branch)
             (?C "Configure..."          magit-branch-config-popup)
             (?c "Checkout new branch"   magit-branch-and-checkout)
             (?s "Create new spin-off"   magit-branch-spinoff)
             (?m "Rename"                magit-branch-rename)
             (?w "Checkout new worktree" magit-worktree-checkout)
             (?W "Create new worktree"   magit-worktree-branch)
             (?x "Reset"                 magit-branch-reset) nil nil
             (?k "Delete"                magit-branch-delete))
  :default-action 'magit-checkout
  :max-action-columns 3
  :setup-function 'magit-branch-popup-setup)

(defvar magit-branch-config-variables)

(defun magit-branch-popup-setup (val def)
  (magit-popup-default-setup val def)
  (when magit-branch-popup-show-variables
    (magit-popup-put :variables (magit-popup-convert-variables
                                 val magit-branch-config-variables))))

;;;###autoload
(defun magit-checkout (revision)
  "Checkout REVISION, updating the index and the working tree.
If REVISION is a local branch then that becomes the current
branch.  If it is something else then `HEAD' becomes detached.
Checkout fails if the working tree or the staging area contain
changes.
\n(git checkout REVISION)."
  (interactive (list (magit-read-other-branch-or-commit "Checkout")))
  (magit-run-git "checkout" revision))

;;;###autoload
(defun magit-branch (branch start-point &optional args)
  "Create BRANCH at branch or revision START-POINT.
\n(git branch [ARGS] BRANCH START-POINT)."
  (interactive (magit-branch-read-args "Create branch"))
  (magit-call-git "branch" args branch start-point)
  (--when-let (and (magit-get-upstream-branch branch)
                   (magit-get-indirect-upstream-branch start-point))
    (magit-call-git "branch" (concat "--set-upstream-to=" it) branch))
  (magit-refresh))

;;;###autoload
(defun magit-branch-and-checkout (branch start-point &optional args)
  "Create and checkout BRANCH at branch or revision START-POINT.
\n(git checkout [ARGS] -b BRANCH START-POINT)."
  (interactive (magit-branch-read-args "Create and checkout branch"))
  (if (string-match-p "^stash@{[0-9]+}$" start-point)
      (magit-run-git "stash" "branch" branch start-point)
    (magit-call-git "checkout" args "-b" branch start-point)
    (--when-let (and (magit-get-upstream-branch branch)
                     (magit-get-indirect-upstream-branch start-point))
      (magit-call-git "branch" (concat "--set-upstream-to=" it) branch))
    (magit-refresh)))

;;;###autoload
(defun magit-branch-orphan (branch start-point &optional args)
  "Create and checkout an orphan BRANCH with contents from revision START-POINT.
\n(git checkout --orphan [ARGS] BRANCH START-POINT)."
  (interactive (magit-branch-read-args "Create and checkout orphan branch"))
  (magit-run-git "checkout" "--orphan" args branch start-point))

(defun magit-branch-read-args (prompt)
  (let ((args (magit-branch-arguments)) start branch)
    (cond (magit-branch-read-upstream-first
           (setq start  (magit-read-starting-point prompt))
           (setq branch (magit-read-string-ns
                         "Branch name"
                         (let ((def (mapconcat #'identity
                                               (cdr (split-string start "/"))
                                               "/")))
                           (and (member start (magit-list-remote-branch-names))
                                (not (member def (magit-list-local-branch-names)))
                                def)))))
          (t
           (setq branch (magit-read-string-ns "Branch name"))
           (setq start  (magit-read-starting-point prompt))))
    (list branch start args)))

;;;###autoload
(defun magit-branch-spinoff (branch &rest args)
  "Create new branch from the unpushed commits.

Create and checkout a new branch starting at and tracking the
current branch.  That branch in turn is reset to the last commit
it shares with its upstream.  If the current branch has no
upstream or no unpushed commits, then the new branch is created
anyway and the previously current branch is not touched.

This is useful to create a feature branch after work has already
began on the old branch (likely but not necessarily \"master\").

If the current branch is a member of the value of option
`magit-branch-prefer-remote-upstream' (which see), then the
current branch will be used as the starting point as usual, but
the upstream of the starting-point may be used as the upstream
of the new branch, instead of the starting-point itself."
  (interactive (list (magit-read-string "Spin off branch")
                     (magit-branch-arguments)))
  (when (magit-branch-p branch)
    (user-error "Branch %s already exists" branch))
  (-if-let (current (magit-get-current-branch))
      (let (tracked base)
        (magit-call-git "checkout" args "-b" branch current)
        (--when-let (magit-get-indirect-upstream-branch current)
          (magit-call-git "branch" "--set-upstream-to" it branch))
        (when (and (setq tracked (magit-get-upstream-branch current))
                   (setq base (magit-git-string "merge-base" current tracked))
                   (not (magit-rev-eq base current)))
          (magit-call-git "update-ref" "-m"
                          (format "reset: moving to %s" base)
                          (concat "refs/heads/" current) base))
        (magit-refresh))
    (magit-run-git "checkout" "-b" branch)))

;;;###autoload
(defun magit-branch-reset (branch to &optional args set-upstream)
  "Reset a branch to the tip of another branch or any other commit.

When the branch being reset is the current branch, then do a
hard reset.  If there are any uncommitted changes, then the user
has to confirming the reset because those changes would be lost.

This is useful when you have started work on a feature branch but
realize it's all crap and want to start over.

When resetting to another branch and a prefix argument is used,
then also set the target branch as the upstream of the branch
that is being reset."
  (interactive
   (let* ((atpoint (magit-branch-at-point))
          (branch  (magit-read-local-branch "Reset branch" atpoint)))
     (list branch
           (magit-completing-read (format "Reset %s to" branch)
                                  (delete branch (magit-list-branch-names))
                                  nil nil nil 'magit-revision-history
                                  (or (and (not (equal branch atpoint)) atpoint)
                                      (magit-get-upstream-branch branch)))
           (magit-branch-arguments)
           current-prefix-arg)))
  (unless (member "--force" args)
    (setq args (cons "--force" args)))
  (if (equal branch (magit-get-current-branch))
      (if (and (magit-anything-modified-p)
               (not (yes-or-no-p "Uncommitted changes will be lost.  Proceed?")))
          (user-error "Abort")
        (magit-reset-hard to)
        (when (and set-upstream (magit-branch-p to))
          (magit-set-branch*merge/remote branch to)))
    (magit-branch branch to args)))

;;;###autoload
(defun magit-branch-delete (branches &optional force)
  "Delete one or multiple branches.
If the region marks multiple branches, then offer to delete
those, otherwise prompt for a single branch to be deleted,
defaulting to the branch at point."
  (interactive
   (let ((branches (magit-region-values 'branch))
         (force current-prefix-arg))
     (if (if (> (length branches) 1)
             (magit-confirm t nil "Delete %i branches" branches)
           (setq branches
                 (list (magit-read-branch (if current-prefix-arg
                                              "Force delete branch"
                                            "Delete branch")
                                          (magit-get-previous-branch)))))
         (unless force
           (--when-let (-intersection
                        (-union (magit-list-unmerged-branches)
                                (magit-list-unmerged-to-upstream-branches))
                        branches)
             (if (magit-confirm 'delete-unmerged-branch
                   "Delete unmerged branch %s"
                   "Delete %i unmerged branches" it)
                 (setq force t)
               (or (setq branches (-difference branches it))
                   (user-error "Abort")))))
       (user-error "Abort"))
     (list branches force)))
  (let* ((refs (-map #'magit-ref-fullname branches))
         (ambiguous (--filter (not it) refs)))
    (when ambiguous
      (user-error
       "%s ambiguous.  Please cleanup using git directly."
       (let ((len (length ambiguous)))
         (cond
          ((= len 1)
           (format "%s is" (--first (not (magit-ref-fullname it)) branches)))
          ((= len (length refs))
           (format "These %s names are" len))
          (t
           (format "%s of these names are" len))))))
    (cond
     ((string-match "^refs/remotes/\\([^/]+\\)" (car refs))
      (let* ((remote (match-string 1 (car refs)))
             (offset (1+ (length remote))))
        (magit-run-git-async
         "push" remote (--map (concat ":" (substring it offset)) branches))))
     ((> (length branches) 1)
      (magit-run-git "branch" (if force "-D" "-d")
                     (delete (magit-get-current-branch) branches)))
     (t ; And now for something completely different.
      (let* ((branch (car branches))
             (prompt (format "Branch %s is checked out.  " branch)))
        (when (equal branch (magit-get-current-branch))
          (pcase (if (or (equal branch "master")
                         (not (magit-rev-verify "master")))
                     (magit-read-char-case prompt nil
                       (?d "[d]etach HEAD & delete" 'detach)
                       (?a "[a]bort"                'abort))
                   (magit-read-char-case prompt nil
                     (?d "[d]etach HEAD & delete"     'detach)
                     (?c "[c]heckout master & delete" 'master)
                     (?a "[a]bort"                    'abort)))
            (`detach (magit-call-git "checkout" "--detach"))
            (`master (magit-call-git "checkout" "master"))
            (`abort  (user-error "Abort")))
          (setq force t))
        (magit-run-git "branch" (if force "-D" "-d") branch))))))

(put 'magit-branch-delete 'interactive-only t)

;;;###autoload
(defun magit-branch-rename (old new &optional force)
  "Rename branch OLD to NEW.
With prefix, forces the rename even if NEW already exists.
\n(git branch -m|-M OLD NEW)."
  (interactive
   (let ((branch (magit-read-local-branch "Rename branch")))
     (list branch
           (magit-read-string-ns (format "Rename branch '%s' to" branch)
                                 nil 'magit-revision-history)
           current-prefix-arg)))
  (unless (string= old new)
    (magit-run-git "branch" (if force "-M" "-m") old new)))

;;;;; Branch Config Popup

(defvar magit-branch-config-branch nil)

;;;###autoload
(defun magit-branch-config-popup (branch)
  "Popup console for setting branch variables."
  (interactive
   (list (if (or current-prefix-arg
                 (and (eq magit-current-popup 'magit-branch-popup)
                      magit-branch-popup-show-variables))
             (magit-read-local-branch "Configure branch")
           (magit-get-current-branch))))
  (let ((magit-branch-config-branch branch))
    (magit-invoke-popup 'magit-branch-config-popup nil nil)))

(defvar magit-branch-config-variables
  '((lambda ()
      (concat
       (propertize "Configure " 'face 'magit-popup-heading)
       (propertize (magit-branch-config-branch) 'face 'magit-branch-local)))
    (?d "branch.%s.description"
        magit-edit-branch*description
        magit-format-branch*description)
    (?u "branch.%s.merge"
        magit-set-branch*merge/remote
        magit-format-branch*merge/remote)
    (?r "branch.%s.rebase"
        magit-cycle-branch*rebase
        magit-format-branch*rebase)
    (?p "branch.%s.pushRemote"
        magit-cycle-branch*pushRemote
        magit-format-branch*pushRemote)
    "Configure repository defaults"
    (?\M-r "pull.rebase"
           magit-cycle-pull.rebase
           magit-format-pull.rebase)
    (?\M-p "remote.pushDefault"
           magit-cycle-remote.pushDefault
           magit-format-remote.pushDefault)
    "Configure branch creation"
    (?U "branch.autoSetupMerge"
        magit-cycle-branch*autoSetupMerge
        magit-format-branch*autoSetupMerge)
    (?R "branch.autoSetupRebase"
        magit-cycle-branch*autoSetupRebase
        magit-format-branch*autoSetupRebase)))

(defvar magit-branch-config-popup
  `(:man-page "git-branch"
    :variables ,magit-branch-config-variables
    :default-action magit-checkout
    :setup-function magit-branch-config-popup-setup))

(defun magit-branch-config-popup-setup (val def)
  (magit-popup-default-setup val def)
  (setq-local magit-branch-config-branch magit-branch-config-branch))

(defun magit-branch-config-branch (&optional prompt)
  (if prompt
      (or (and (not current-prefix-arg)
               (or magit-branch-config-branch
                   (magit-get-current-branch)))
          (magit-read-local-branch prompt))
    (or magit-branch-config-branch
        (magit-get-current-branch)
        "<name>")))

;;;###autoload
(defun magit-edit-branch*description (branch)
  "Edit the description of the current branch.
With a prefix argument edit the description of another branch.

The description for the branch named NAME is stored in the Git
variable `branch.<name>.description'."
  (interactive (list (magit-branch-config-branch "Edit branch description")))
  (magit-run-git-with-editor "branch" "--edit-description" branch))

(defun magit-edit-branch*description-check-buffers ()
  (and buffer-file-name
       (string-match-p "/BRANCH_DESCRIPTION\\'" buffer-file-name)
       (add-hook 'with-editor-post-finish-hook
                 (lambda ()
                   (when (derived-mode-p 'magit-popup-mode)
                     (magit-refresh-popup-buffer)))
                 nil t)))

(add-hook 'find-file-hook 'magit-edit-branch*description-check-buffers)

(defun magit-format-branch*description ()
  (let* ((branch (magit-branch-config-branch))
         (width (+ (length branch) 19))
         (var (format "branch.%s.description" branch)))
    (concat var " " (make-string (- width (length var)) ?\s)
            (-if-let (value (magit-get var))
                (propertize value 'face 'magit-popup-option-value)
              (propertize "unset" 'face 'magit-popup-disabled-argument)))))

;;;###autoload
(defun magit-set-branch*merge/remote (branch upstream)
  "Set or unset the upstream of the current branch.
With a prefix argument do so for another branch.

When the branch in question already has an upstream then simply
unsets it.  Invoke this command again to set another upstream.

Together the Git variables `branch.<name>.remote' and
`branch.<name>.merge' define the upstream branch of the local
branch named NAME.  The value of `branch.<name>.remote' is the
name of the upstream remote.  The value of `branch.<name>.merge'
is the full reference of the upstream branch, on the remote.

Non-interactively, when UPSTREAM is non-nil, then always set it
as the new upstream, regardless of whether another upstream was
already set.  When nil, then always unset."
  (interactive
   (let ((branch (magit-branch-config-branch "Change upstream of branch")))
     (list branch (and (not (magit-get-upstream-branch branch))
                       (magit-read-upstream-branch)))))
  (if upstream
      (-let (((remote . merge) (magit-split-branch-name upstream)))
        (setf (magit-get (format "branch.%s.remote" branch)) remote)
        (setf (magit-get (format "branch.%s.merge"  branch))
              (concat "refs/heads/" merge)))
    (magit-call-git "branch" "--unset-upstream" branch))
  (when (called-interactively-p 'any)
    (magit-refresh)))

(defun magit-format-branch*merge/remote ()
  (let* ((branch (magit-branch-config-branch))
         (width (+ (length branch) 20))
         (varM (format "branch.%s.merge" branch))
         (varR (format "branch.%s.remote" branch))
         (face (if (equal (magit-get varR) ".")
                   'magit-branch-local
                 'magit-branch-remote)))
    (concat varM (make-string (- width (length varM)) ?\s)
            (-if-let (value (magit-get varM))
                (propertize value 'face face)
              (propertize "unset" 'face 'magit-popup-disabled-argument))
            "\n   " varR (make-string (- width (length varR)) ?\s)
            (-if-let (value (magit-get varR))
                (propertize value 'face face)
              (propertize "unset" 'face 'magit-popup-disabled-argument)))))

;;;###autoload
(defun magit-cycle-branch*rebase (branch)
  "Cycle the value of `branch.<name>.rebase' for the current branch.
With a prefix argument cycle the value for another branch.

The Git variables `branch.<name>.rebase' controls whether pulling
into the branch named NAME is done by rebasing that branch onto
the fetched branch or by merging that branch.

When `true' then pulling is done by rebasing.
When `false' then pulling is done by merging.

When that variable is undefined then the value of `pull.rebase'
is used instead.  It defaults to `false'."
  (interactive (list (magit-branch-config-branch
                      "Cycle branch.<name>.rebase for")))
  (magit-popup-set-variable (format "branch.%s.rebase" branch)
                            '("true" "false")
                            "false" "pull.rebase"))

(defun magit-format-branch*rebase ()
  (let ((branch (magit-branch-config-branch)))
    (magit-popup-format-variable (format "branch.%s.rebase" branch)
                                 '("true" "false")
                                 "false" "pull.rebase"
                                 (+ (length branch) 20))))

;;;###autoload
(defun magit-cycle-branch*pushRemote (branch)
  "Cycle the value of `branch.<name>.pushRemote' for the current branch.
With a prefix argument cycle the value for another branch.

The Git variable `branch.<name>.pushRemote' specifies the remote
that the branch named NAME is usually pushed to.  The value has
to be the name of an existing remote.

If that variable is undefined, then the value of the Git variable
`remote.pushDefault' is used instead, provided that it is defined,
which by default it is not."
  (interactive (list (magit-branch-config-branch
                      "Cycle branch.<name>.pushRemote for")))
  (magit-popup-set-variable (format "branch.%s.pushRemote" branch)
                            (magit-list-remotes)
                            "remote.pushDefault"))

(defun magit-format-branch*pushRemote ()
  (let ((branch (magit-branch-config-branch)))
    (magit-popup-format-variable (format "branch.%s.pushRemote" branch)
                                 (magit-list-remotes)
                                 nil "remote.pushDefault"
                                 (+ (length branch) 20))))

;;;###autoload
(defun magit-cycle-pull.rebase ()
  "Cycle the repository-local value of `pull.rebase'.

The Git variable `pull.rebase' specifies whether pulling is done
by rebasing or by merging.  It can be overwritten using the Git
variable `branch.<name>.rebase'.

When `true' then pulling is done by rebasing.
When `false' (the default) then pulling is done by merging."
  (interactive)
  (magit-popup-set-variable "pull.rebase" '("true" "false") "false"))

(defun magit-format-pull.rebase ()
  (magit-popup-format-variable "pull.rebase" '("true" "false") "false" nil 19))

;;;###autoload
(defun magit-cycle-remote.pushDefault ()
  "Cycle the repository-local value of `remote.pushDefault'.

The Git variable `remote.pushDefault' specifies the remote that
local branches are usually pushed to.  It can be overwritten
using the Git variable `branch.<name>.pushRemote'."
  (interactive)
  (magit-popup-set-variable "remote.pushDefault" (magit-list-remotes)))

(defun magit-format-remote.pushDefault ()
  (magit-popup-format-variable "remote.pushDefault"
                               (magit-list-remotes) nil nil 19))

;;;###autoload
(defun magit-cycle-branch*autoSetupMerge ()
  "Cycle the repository-local value of `branch.autoSetupMerge'.

The Git variable `branch.autoSetupMerge' under what circumstances
creating a branch (named NAME) should result in the variables
`branch.<name>.merge' and `branch.<name>.remote' being set
according to the starting point used to create the branch.  If
the starting point isn't a branch, then these variables are never
set.

When `always' then the variables are set regardless of whether
the starting point is a local or a remote branch.

When `true' (the default) then the variable are set when the
starting point is a remote branch, but not when it is a local
branch.

When `false' then the variables are never set."
  (interactive)
  (magit-popup-set-variable "branch.autoSetupMerge"
                            '("always" "true" "false") "true"))

(defun magit-format-branch*autoSetupMerge ()
  (magit-popup-format-variable "branch.autoSetupMerge"
                               '("always" "true" "false") "true" nil 23))

;;;###autoload
(defun magit-cycle-branch*autoSetupRebase ()
  "Cycle the repository-local value of `branch.autoSetupRebase'.

The Git variable `branch.autoSetupRebase' specifies whether
creating a branch (named NAME) should result in the variable
`branch.<name>.rebase' being set to `true'.

When `always' then the variable is set regardless of whether the
starting point is a local or a remote branch.

When `local' then the variable are set when the starting point
is a local branch, but not when it is a remote branch.

When `remote' then the variable are set when the starting point
is a remote branch, but not when it is a local branch.

When `never' (the default) then the variable is never set."
  (interactive)
  (magit-popup-set-variable "branch.autoSetupRebase"
                            '("always" "local" "remote" "never") "never"))

(defun magit-format-branch*autoSetupRebase ()
  (magit-popup-format-variable "branch.autoSetupRebase"
                               '("always" "local" "remote" "never")
                               "never" nil 23))

;;;; Merge

;;;###autoload (autoload 'magit-merge-popup "magit" nil t)
(magit-define-popup magit-merge-popup
  "Popup console for merge commands."
  'magit-commands
  :man-page "git-merge"
  :switches '((?f "Fast-forward only" "--ff-only")
              (?n "No fast-forward"   "--no-ff")
              (?s "Squash"            "--squash"))
  :options  '((?s "Strategy" "--strategy="))
  :actions  '((?m "Merge"                  magit-merge)
              (?e "Merge and edit message" magit-merge-editmsg)
              (?p "Preview merge"          magit-merge-preview)
              (?n "Merge but don't commit" magit-merge-nocommit))
  :sequence-actions   '((?m "Commit merge" magit-commit)
                        (?a "Abort merge"  magit-merge-abort))
  :sequence-predicate 'magit-merge-state
  :default-action 'magit-merge
  :max-action-columns 2)

;;;###autoload
(defun magit-merge (rev &optional args nocommit)
  "Merge commit REV into the current branch; using default message.

Unless there are conflicts or a prefix argument is used create a
merge commit using a generic commit message and without letting
the user inspect the result.  With a prefix argument pretend the
merge failed to give the user the opportunity to inspect the
merge.

\(git merge --no-edit|--no-commit [ARGS] REV)"
  (interactive (list (magit-read-other-branch-or-commit "Merge")
                     (magit-merge-arguments)
                     current-prefix-arg))
  (magit-merge-assert)
  (magit-run-git "merge" (if nocommit "--no-commit" "--no-edit") args rev))

;;;###autoload
(defun magit-merge-editmsg (rev &optional args)
  "Merge commit REV into the current branch; and edit message.
Perform the merge and prepare a commit message but let the user
edit it.
\n(git merge --edit --no-ff [ARGS] rev)"
  (interactive (list (magit-read-other-branch-or-commit "Merge")
                     (magit-merge-arguments)))
  (magit-merge-assert)
  (cl-pushnew "--no-ff" args :test #'equal)
  (with-editor "GIT_EDITOR"
    (let ((magit-process-popup-time -1))
      (magit-run-git-async "merge" "--edit" args rev))))

;;;###autoload
(defun magit-merge-nocommit (rev &optional args)
  "Merge commit REV into the current branch; pretending it failed.
Pretend the merge failed to give the user the opportunity to
inspect the merge and change the commit message.
\n(git merge --no-commit --no-ff [ARGS] rev)"
  (interactive (list (magit-read-other-branch-or-commit "Merge")
                     (magit-merge-arguments)))
  (magit-merge-assert)
  (cl-pushnew "--no-ff" args :test #'equal)
  (magit-run-git "merge" "--no-commit" args rev))

;;;###autoload
(defun magit-merge-preview (rev)
  "Preview result of merging REV into the current branch."
  (interactive (list (magit-read-other-branch-or-commit "Preview merge")))
  (magit-mode-setup #'magit-merge-preview-mode rev))

(define-derived-mode magit-merge-preview-mode magit-diff-mode "Magit Merge"
  "Mode for previewing a merge."
  :group 'magit-diff
  (hack-dir-local-variables-non-file-buffer))

(defun magit-merge-preview-refresh-buffer (rev)
  (let* ((branch (magit-get-current-branch))
         (head (or branch (magit-rev-verify "HEAD"))))
    (setq header-line-format
          (propertize (format "Preview merge of %s into %s"
                              rev (or branch "HEAD"))
                      'face 'magit-header-line))
    (magit-insert-section (diffbuf)
      (magit-git-wash #'magit-diff-wash-diffs
        "merge-tree" (magit-git-string "merge-base" head rev) head rev))))

;;;###autoload
(defun magit-merge-abort ()
  "Abort the current merge operation.
\n(git merge --abort)"
  (interactive)
  (if (file-exists-p (magit-git-dir "MERGE_HEAD"))
      (when (magit-confirm 'abort-merge)
        (magit-run-git-async "merge" "--abort"))
    (user-error "No merge in progress")))

(defun magit-checkout-stage (file arg)
  "During a conflict checkout and stage side, or restore conflict."
  (interactive
   (let ((file (magit-completing-read "Checkout file"
                                      (magit-tracked-files) nil nil nil
                                      'magit-read-file-hist
                                      (magit-current-file))))
     (cond ((member file (magit-unmerged-files))
            (list file (magit-checkout-read-stage file)))
           ((yes-or-no-p (format "Restore conflicts in %s? " file))
            (list file "--merge"))
           (t
            (user-error "Quit")))))
  (pcase (cons arg (cddr (car (magit-file-status file))))
    ((or `("--ours"   ?D ,_)
         `("--theirs" ,_ ?D))
     (magit-run-git "rm" "--" file))
    (_ (if (equal arg "--merge")
           ;; This fails if the file was deleted on one
           ;; side.  And we cannot do anything about it.
           (magit-run-git "checkout" "--merge" "--" file)
         (magit-call-git "checkout" arg "--" file)
         (magit-run-git "add" "-u" "--" file)))))

(defun magit-merge-state ()
  (file-exists-p (magit-git-dir "MERGE_HEAD")))

(defun magit-merge-assert ()
  (or (not (magit-anything-modified-p))
      (magit-confirm 'merge-dirty
        "Merging with dirty worktree is risky.  Continue")
      (user-error "Abort")))

(defun magit-checkout-read-stage (file)
  (magit-read-char-case (format "For %s checkout: " file) t
    (?o "[o]ur stage"   "--ours")
    (?t "[t]heir stage" "--theirs")
    (?c "[c]onflict"    "--merge")))

(defun magit-insert-merge-log ()
  "Insert section for the on-going merge.
Display the heads that are being merged.
If no merge is in progress, do nothing."
  (-when-let (heads (mapcar 'magit-get-shortname
                            (magit-file-lines (magit-git-dir "MERGE_HEAD"))))
    (magit-insert-section (commit (car heads))
      (magit-insert-heading
        (format "Merging %s:" (mapconcat 'identity heads ", ")))
      (magit-insert-log
       (concat (magit-git-string "merge-base" "--octopus" "HEAD" (car heads))
               ".." (car heads))
       (let ((args magit-log-section-arguments))
         (unless (member "--decorate=full" magit-log-section-arguments)
           (push "--decorate=full" args))
         args)))))

;;;; Reset

;;;###autoload (autoload 'magit-reset-popup "magit" nil t)
(magit-define-popup magit-reset-popup
  "Popup console for reset commands."
  'magit-commands
  :man-page "git-reset"
  :actions '((?m "reset mixed  (HEAD and index)"         magit-reset-head)
             (?s "reset soft   (HEAD only)"              magit-reset-soft)
             (?h "reset hard   (HEAD, index, and files)" magit-reset-hard)
             (?i "reset index  (index only)"             magit-reset-index))
  :max-action-columns 1)

;;;###autoload
(defun magit-reset-index (commit)
  "Reset the index to COMMIT.
Keep the head and working tree as-is, so if COMMIT refers to the
head this effectively unstages all changes.
\n(git reset COMMIT .)"
  (interactive (list (magit-read-branch-or-commit "Reset index to")))
  (magit-reset-internal nil commit "."))

;;;###autoload
(defun magit-reset (commit &optional hard)
  "Reset the head and index to COMMIT, but not the working tree.
With a prefix argument also reset the working tree.
\n(git reset --mixed|--hard COMMIT)"
  (interactive (list (magit-read-branch-or-commit
                      (if current-prefix-arg
                          "Hard reset to"
                        "Reset head to"))
                     current-prefix-arg))
  (magit-reset-internal (if hard "--hard" "--mixed") commit))

;;;###autoload
(defun magit-reset-head (commit)
  "Reset the head and index to COMMIT, but not the working tree.
\n(git reset --mixed COMMIT)"
  (interactive (list (magit-read-branch-or-commit "Reset head to")))
  (magit-reset-internal "--mixed" commit))

;;;###autoload
(defun magit-reset-soft (commit)
  "Reset the head to COMMIT, but not the index and working tree.
\n(git reset --soft REVISION)"
  (interactive (list (magit-read-branch-or-commit "Soft reset to")))
  (magit-reset-internal "--soft" commit))

;;;###autoload
(defun magit-reset-hard (commit)
  "Reset the head, index, and working tree to COMMIT.
\n(git reset --hard REVISION)"
  (interactive (list (magit-read-branch-or-commit "Hard reset to")))
  (magit-reset-internal "--hard" commit))

(defun magit-reset-internal (arg commit &optional path)
  (when (and (not (member arg '("--hard" nil)))
             (equal (magit-rev-parse commit)
                    (magit-rev-parse "HEAD~")))
    (with-temp-buffer
      (magit-git-insert "show" "-s" "--format=%B" "HEAD")
      (when git-commit-major-mode
        (funcall git-commit-major-mode))
      (git-commit-setup-font-lock)
      (git-commit-save-message)))
  (let ((cmd (if (and (equal commit "HEAD") (not arg)) "unstage" "reset")))
    (magit-wip-commit-before-change nil (concat " before " cmd))
    (magit-run-git "reset" arg commit "--" path)
    (when (equal cmd "unstage")
      (magit-wip-commit-after-apply nil " after unstage"))))

;;;; Files

(defun magit-file-rename (file newname)
  "Rename the FILE to NEWNAME.
If FILE isn't tracked in Git fallback to using `rename-file'."
  (interactive
   (let* ((file (magit-read-file "Rename file"))
          (newname (read-file-name (format "Rename %s to file: " file))))
     (list (expand-file-name file (magit-toplevel))
           (expand-file-name newname))))
  (if (magit-file-tracked-p file)
      (let ((oldbuf (get-file-buffer file)))
        (when (and oldbuf (buffer-modified-p oldbuf))
          (user-error "Save %s before moving it" file))
        (when (file-exists-p newname)
          (user-error "%s already exists" newname))
        (magit-run-git "mv" file newname)
        (when oldbuf
          (with-current-buffer oldbuf
            (let ((buffer-read-only buffer-read-only))
              (set-visited-file-name newname))
            (if (fboundp 'vc-refresh-state)
                (vc-refresh-state)
              (with-no-warnings
                (vc-find-file-hook))))))
    (rename-file file newname current-prefix-arg)
    (magit-refresh)))

(defun magit-file-untrack (file)
  "Untrack FILE.
Stop tracking FILE in Git but do not remove it from the working
tree."
  (interactive (list (magit-read-tracked-file "Untrack file")))
  (magit-run-git "rm" "--cached" "--" file))

(defun magit-file-delete (file &optional force)
  "Delete FILE.
With a prefix argument FORCE do so even when FILE has uncommitted
changes.

If FILE isn't tracked in Git fallback to using `delete-file'."
  (interactive (list (magit-read-file "Delete file")))
  (if (magit-file-tracked-p file)
      (magit-run-git "rm" (and force "--force") "--" file)
    (delete-file (expand-file-name file (magit-toplevel)) t)
    (magit-refresh)))

(defun magit-read-tracked-file (prompt)
  (magit-read-file prompt t))

(defun magit-read-file (prompt &optional tracked-only)
  (let ((choices (nconc (magit-list-files)
                        (unless tracked-only (magit-untracked-files)))))
    (magit-completing-read prompt choices nil t nil nil
                           (car (member (or (magit-section-when (file))
                                            (magit-file-relative-name
                                             nil tracked-only))
                                        choices)))))

(defun magit-read-files (prompt initial-contents)
  (mapconcat 'identity
             (completing-read-multiple (or prompt "File,s: ")
                                       (magit-list-files)
                                       nil nil initial-contents) ","))

(defun magit-read-file-choice (prompt files &optional error default)
  "Read file from FILES.

If FILES has only one member, return that instead of prompting.
If FILES has no members, give a user error.  ERROR can be given
to provide a more informative error.

If DEFAULT is non-nil, use this as the default value instead of
`magit-current-file'."
  (pcase (length files)
    (0 (user-error (or error "No file choices")))
    (1 (car files))
    (_ (magit-completing-read
        prompt files nil t nil 'magit-read-file-hist
        (car (member (or default (magit-current-file)) files))))))

;;; Miscellaneous
;;;; Worktree

;;;###autoload
(defun magit-worktree-checkout (path branch)
  (interactive
   (let ((branch (magit-read-local-branch "Checkout")))
     (list (read-directory-name (format "Checkout %s in new worktree: " branch))
           branch)))
  "Checkout BRANCH in a new worktree at PATH."
  (magit-run-git "worktree" "add" path branch)
  (magit-diff-visit-directory path))

;;;###autoload
(defun magit-worktree-branch (path branch start-point &optional force)
  "Create a new BRANCH and check it out in a new worktree at PATH."
  (interactive
   `(,(read-directory-name "Create worktree: ")
     ,@(butlast (magit-branch-read-args "Create and checkout branch"))
     ,current-prefix-arg))
  (magit-run-git "worktree" "add" (if force "-B" "-b") branch path start-point)
  (magit-diff-visit-directory path))

(defun magit-worktree-delete (worktree)
  "Delete a worktree, defaulting to the worktree at point.
The primary worktree cannot be deleted."
  (interactive
   (list (magit-completing-read "Delete worktree"
                                (cdr (magit-list-worktrees))
                                nil t nil nil
                                (magit-section-when (worktree)))))
  (if (file-directory-p (expand-file-name ".git" worktree))
      (user-error "Deleting %s would delete the shared .git directory" worktree)
    (let ((primary (file-name-as-directory (caar (magit-list-worktrees)))))
      (when (if magit-delete-by-moving-to-trash
                (magit-confirm-files 'trash (list "worktree"))
              (magit-confirm-files 'delete (list "worktree")))
        (let ((delete-by-moving-to-trash magit-delete-by-moving-to-trash))
          (delete-directory worktree t magit-delete-by-moving-to-trash))
        (if (file-exists-p default-directory)
            (magit-run-git "worktree" "prune")
          (let ((default-directory primary))
            (magit-run-git "worktree" "prune")))))))

(defun magit-worktree-status (worktree)
  "Show the status for the worktree at point.
If there is no worktree at point, then read one in the
minibuffer.  If the worktree at point is the one whose
status is already being displayed in the current buffer,
then show it in Dired instead."
  (interactive
   (list (or (magit-section-when (worktree))
             (magit-completing-read
              "Show status for worktree"
              (cl-delete (directory-file-name (magit-toplevel))
                         (magit-list-worktrees)
                         :test #'equal :key #'car)))))
  (magit-diff-visit-directory worktree))

(defvar magit-worktree-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing]  'magit-worktree-status)
    (define-key map [remap magit-delete-thing] 'magit-worktree-delete)
    map)
  "Keymap for `worktree' sections.")

(defun magit-insert-worktrees ()
  "Insert sections for all worktrees.
If there is only one worktree, then insert nothing."
  (let ((worktrees (magit-list-worktrees)))
    (when (> (length worktrees) 1)
      (magit-insert-section (worktrees)
        (magit-insert-heading "Worktrees:")
        (let* ((cols
                (mapcar (-lambda ((path barep commit branch))
                          (cons (cond
                                 (branch (propertize branch
                                                     'face 'magit-branch-local))
                                 (commit (propertize (magit-rev-abbrev commit)
                                                     'face 'magit-hash))
                                 (barep  "(bare)"))
                                path))
                        worktrees))
               (align (1+ (-max (--map (string-width (car it)) cols)))))
          (pcase-dolist (`(,head . ,path) cols)
            (magit-insert-section (worktree path)
              (insert head)
              (indent-to align)
              (insert (let ((r (file-relative-name path))
                            (a (abbreviate-file-name path)))
                        (if (< (string-width r) (string-width a)) r a)))
              (insert ?\n))))
        (insert ?\n)))))

;;;; Tag

;;;###autoload (autoload 'magit-tag-popup "magit" nil t)
(magit-define-popup magit-tag-popup
  "Popup console for tag commands."
  'magit-commands
  :man-page "git-tag"
  :switches '((?a "Annotate" "--annotate")
              (?s "Sign"     "--sign")
              (?f "Force"    "--force"))
  :actions  '((?t "Create"   magit-tag)
              (?k "Delete"   magit-tag-delete)
              (?p "Prune"    magit-tag-prune))
  :default-action 'magit-tag)

;;;###autoload
(defun magit-tag (name rev &optional args)
  "Create a new tag with the given NAME at REV.
With a prefix argument annotate the tag.
\n(git tag [--annotate] NAME REV)"
  (interactive (list (magit-read-tag "Tag name")
                     (magit-read-branch-or-commit "Place tag on")
                     (let ((args (magit-tag-arguments)))
                       (when current-prefix-arg
                         (cl-pushnew "--annotate" args))
                       args)))
  (magit-run-git-with-editor "tag" args name rev))

;;;###autoload
(defun magit-tag-delete (tags)
  "Delete one or more tags.
If the region marks multiple tags (and nothing else), then offer
to delete those, otherwise prompt for a single tag to be deleted,
defaulting to the tag at point.
\n(git tag -d TAGS)"
  (interactive (list (--if-let (magit-region-values 'tag)
                         (magit-confirm t nil "Delete %i tags" it)
                       (magit-read-tag "Delete tag" t))))
  (magit-run-git "tag" "-d" tags))

(defun magit-tag-prune (tags remote-tags remote)
  "Offer to delete tags missing locally from REMOTE, and vice versa."
  (interactive
   (let* ((remote (magit-read-remote "Prune tags using remote"))
          (tags   (magit-list-tags))
          (rtags  (prog2 (message "Determining remote tags...")
                      (magit-remote-list-tags remote)
                    (message "Determining remote tags...done")))
          (ltags  (-difference tags rtags))
          (rtags  (-difference rtags tags)))
     (unless (or ltags rtags)
       (message "Same tags exist locally and remotely"))
     (unless (magit-confirm t "Delete %s locally"
               "Delete %i tags locally" ltags)
       (setq ltags nil))
     (unless (magit-confirm t "Delete %s from remote"
               "Delete %i tags from remote" rtags)
       (setq rtags nil))
     (list ltags rtags remote)))
  (when tags
    (magit-call-git "tag" "-d" tags))
  (when remote-tags
    (magit-run-git-async "push" remote (--map (concat ":" it) remote-tags))))

;;;; Notes

;;;###autoload (autoload 'magit-notes-popup "magit" nil t)
(magit-define-popup magit-notes-popup
  "Popup console for notes commands."
  'magit-commands
  :man-page "git-tag"
  :switches '("Switch for prune"
              (?n "Dry run"          "--dry-run"))
  :options  '("Option for edit and remove"
              (?r "Manipulate ref"   "--ref="      magit-notes-popup-read-ref)
              "Option for merge"
              (?s "Merge strategy"   "--strategy="))
  :actions  '((?T "Edit"             magit-notes-edit)
              (?r "Remove"           magit-notes-remove)
              (?m "Merge"            magit-notes-merge)
              (?p "Prune"            magit-notes-prune)
              (?s "Set ref"          magit-notes-set-ref)
              (?S "Set display refs" magit-notes-set-display-refs))
  :sequence-actions '((?c "Commit merge" magit-notes-merge-commit)
                      (?a "Abort merge"  magit-notes-merge-abort))
  :sequence-predicate 'magit-notes-merging-p
  :default-action 'magit-notes-edit)

(defun magit-notes-edit (commit &optional ref)
  "Edit the note attached to COMMIT.
REF is the notes ref used to store the notes.

Interactively or when optional REF is nil use the value of Git
variable `core.notesRef' or \"refs/notes/commits\" if that is
undefined."
  (interactive (magit-notes-read-args "Edit notes"))
  (magit-run-git-with-editor "notes" (and ref (concat "--ref=" ref))
                             "edit" commit))

(defun magit-notes-remove (commit &optional ref)
  "Remove the note attached to COMMIT.
REF is the notes ref from which the note is removed.

Interactively or when optional REF is nil use the value of Git
variable `core.notesRef' or \"refs/notes/commits\" if that is
undefined."
  (interactive (magit-notes-read-args "Remove notes"))
  (magit-run-git-with-editor "notes" (and ref (concat "--ref=" ref))
                             "remove" commit))

(defun magit-notes-merge (ref)
  "Merge the notes ref REF into the current notes ref.

The current notes ref is the value of Git variable
`core.notesRef' or \"refs/notes/commits\" if that is undefined.

When there are conflict, then they have to resolved in the
temporary worktree \".git/NOTES_MERGE_WORKTREE\".  When
done use `magit-notes-merge-commit' to finish.  To abort
use `magit-notes-merge-abort'."
  (interactive (list (magit-read-string-ns "Merge reference")))
  (magit-run-git-with-editor "notes" "merge" ref))

(defun magit-notes-merge-commit ()
  "Commit the current notes ref merge.
Also see `magit-notes-merge'."
  (interactive)
  (magit-run-git-with-editor "notes" "merge" "--commit"))

(defun magit-notes-merge-abort ()
  "Abort the current notes ref merge.
Also see `magit-notes-merge'."
  (interactive)
  (magit-run-git-with-editor "notes" "merge" "--abort"))

(defun magit-notes-prune (&optional dry-run)
  "Remove notes about unreachable commits."
  (interactive (list (and (member "--dry-run" (magit-notes-arguments)) t)))
  (when dry-run
    (magit-process-buffer))
  (magit-run-git-with-editor "notes" "prune" (and dry-run "--dry-run")))

(defun magit-notes-set-ref (ref &optional global)
  "Set the current notes ref to REF.
The ref is made current by setting the value of the Git variable
`core.notesRef'.  With a prefix argument GLOBAL change the global
value, else the value in the current repository.  When this is
undefined, then \"refs/notes/commit\" is used.

Other `magit-notes-*' commands, as well as the sub-commands
of Git's `note' command, default to operate on that ref."
  (interactive
   (list (magit-completing-read "Set notes ref"
                                (nconc (list "refs/" "refs/notes/")
                                       (magit-list-notes-refnames))
                                nil nil
                                (--when-let (magit-get "core.notesRef")
                                  (if (string-match "^refs/notes/\\(.+\\)" it)
                                      (match-string 1 it)
                                    it)))
         current-prefix-arg))
  (if ref
      (magit-run-git "config" (and global "--global") "core.notesRef"
                     (if (string-prefix-p "refs/" ref)
                         ref
                       (concat "refs/notes/" ref)))
    (magit-run-git "config" (and global "--global")
                   "--unset" "core.notesRef")))

(defun magit-notes-set-display-refs (refs &optional global)
  "Set notes refs to be display in addition to \"core.notesRef\".
REFS is a colon separated list of notes refs.  The values are
stored in the Git variable `notes.displayRef'.  With a prefix
argument GLOBAL change the global values, else the values in
the current repository."
  (interactive
   (list (magit-completing-read "Set additional notes ref(s)"
                                (nconc (list "refs/" "refs/notes/")
                                       (magit-list-notes-refnames))
                                nil nil
                                (mapconcat #'identity
                                           (magit-get-all "notes.displayRef")
                                           ":"))
         current-prefix-arg))
  (when (and refs (atom refs))
    (setq refs (split-string refs ":")))
  (when global
    (setq global "--global"))
  (magit-git-success "config" "--unset-all" global "notes.displayRef")
  (dolist (ref refs)
    (magit-call-git "config" "--add" global "notes.displayRef" ref))
  (magit-refresh))

(defun magit-notes-read-args (prompt)
 (list (magit-read-branch-or-commit prompt)
       (--when-let (--first (string-match "^--ref=\\(.+\\)" it)
                            (magit-notes-arguments))
         (match-string 1 it))))

(defun magit-notes-popup-read-ref (prompt &optional initial-input)
  (magit-completing-read prompt (nconc (list "refs/" "refs/notes/")
                                       (magit-list-notes-refnames))
                         nil nil initial-input))

(defun magit-notes-merging-p ()
  (let ((dir (magit-git-dir "NOTES_MERGE_WORKTREE")))
    (and (file-directory-p dir)
         (directory-files dir nil "^[^.]"))))

;;;; File Mode

(defvar magit-file-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-xg"    'magit-status)
    (define-key map "\C-x\M-g" 'magit-dispatch-popup)
    (define-key map "\C-c\M-g" 'magit-file-popup)
    map)
  "Keymap for `magit-file-mode'.")

;;;###autoload (autoload 'magit-file-popup "magit" nil t)
(magit-define-popup magit-file-popup
  "Popup console for Magit commands in file-visiting buffers."
  :actions '((?s "Stage"     magit-stage-file)
             (?d "Diff"      magit-diff-buffer-file)
             (?l "Log"       magit-log-buffer-file)
             (?b "Blame"     magit-blame-popup)
             (?u "Unstage"   magit-unstage-file)
             (?D "Diff..."   magit-diff-buffer-file-popup)
             (?L "Log..."    magit-log-buffer-file-popup)
             (?p "Find blob" magit-blob-previous)
             (?c "Commit"    magit-commit-popup))
  :max-action-columns 4)

(defvar magit-file-mode-lighter "")

(define-minor-mode magit-file-mode
  "Enable some Magit features in file-visiting buffers.

Currently this only adds the following key bindings.
\n\\{magit-file-mode-map}"
  :package-version '(magit . "2.2.0")
  :lighter magit-file-mode-lighter
  :keymap  magit-file-mode-map)

(defun magit-file-mode-turn-on ()
  (and buffer-file-name
       (ignore-errors (magit-inside-worktree-p))
       (magit-file-mode)))

;;;###autoload
(define-globalized-minor-mode global-magit-file-mode
  magit-file-mode magit-file-mode-turn-on
  :package-version '(magit . "2.2.0")
  :group 'magit-modes)

;;;; Blob Mode

(defvar magit-blob-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'magit-blob-next)
    (define-key map "p" 'magit-blob-previous)
    (define-key map "q" 'magit-kill-this-buffer)
    map)
  "Keymap for `magit-blob-mode'.")

(define-minor-mode magit-blob-mode
  "Enable some Magit features in blob-visiting buffers.

Currently this only adds the following key bindings.
\n\\{magit-blob-mode-map}"
  :package-version '(magit . "2.3.0"))

(defun magit-blob-next ()
  "Visit the next blob which modified the current file."
  (interactive)
  (if magit-buffer-file-name
      (magit-blob-visit (or (magit-blob-successor magit-buffer-revision
                                                  magit-buffer-file-name)
                            magit-buffer-file-name)
                        (line-number-at-pos))
    (if (buffer-file-name (buffer-base-buffer))
        (user-error "You have reached the end of time")
      (user-error "Buffer isn't visiting a file or blob"))))

(defun magit-blob-previous ()
  "Visit the previous blob which modified the current file."
  (interactive)
  (-if-let (file (or magit-buffer-file-name
                     (buffer-file-name (buffer-base-buffer))))
      (--if-let (magit-blob-ancestor magit-buffer-revision file)
          (magit-blob-visit it (line-number-at-pos))
        (user-error "You have reached the beginning of time"))
    (user-error "Buffer isn't visiting a file or blob")))

(defun magit-blob-visit (blob-or-file line)
  (if (stringp blob-or-file)
      (find-file blob-or-file)
    (-let [(rev file) blob-or-file]
      (magit-find-file rev file)
      (let ((str (magit-rev-format "%ct%s" rev)))
        (message "%s (%s ago)" (substring str 10)
                 (magit-format-duration
                  (abs (truncate (- (float-time)
                                    (string-to-number
                                     (substring str 0 10)))))
                  magit-duration-spec)))))
  (goto-char (point-min))
  (forward-line (1- line)))

(defun magit-blob-ancestor (rev file)
  (let ((lines (magit-with-toplevel
                 (magit-git-lines "log" "-2" "--format=%H" "--name-only"
                                  "--follow" (or rev "HEAD") "--" file))))
    (if rev (cddr lines) (butlast lines 2))))

(defun magit-blob-successor (rev file)
  (let ((lines (magit-with-toplevel
                 (magit-git-lines "log" "--format=%H" "--name-only" "--follow"
                                  "HEAD" "--" file))))
    (catch 'found
      (while lines
        (if (equal (nth 2 lines) rev)
            (throw 'found (list (nth 0 lines) (nth 1 lines)))
          (setq lines (nthcdr 2 lines)))))))

(defun magit-kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;;;; Dispatch Popup

;;;###autoload (autoload 'magit-dispatch-popup "magit" nil t)
(magit-define-popup magit-dispatch-popup
  "Popup console for dispatching other popups."
  'magit-commands nil nil
  :actions '("Popup and dwim commands"
             (?A "Cherry-picking"  magit-cherry-pick-popup)
             (?b "Branching"       magit-branch-popup)
             (?B "Bisecting"       magit-bisect-popup)
             (?c "Committing"      magit-commit-popup)
             (?d "Diffing"         magit-diff-popup)
             (?D "Change diffs"    magit-diff-refresh-popup)
             (?e "Ediff dwimming"  magit-ediff-dwim)
             (?E "Ediffing"        magit-ediff-popup)
             (?f "Fetching"        magit-fetch-popup)
             (?F "Pulling"         magit-pull-popup)
             (?l "Logging"         magit-log-popup)
             (?L "Change logs"     magit-log-refresh-popup)
             (?m "Merging"         magit-merge-popup)
             (?M "Remoting"        magit-remote-popup)
             (?o "Submodules"      magit-submodule-popup)
             (?P "Pushing"         magit-push-popup)
             (?r "Rebasing"        magit-rebase-popup)
             (?t "Tagging"         magit-tag-popup)
             (?T "Notes"           magit-notes-popup)
             (?V "Reverting"       magit-revert-popup)
             (?w "Apply patches"   magit-am-popup)
             (?W "Format patches"  magit-patch-popup)
             (?X "Resetting"       magit-reset-popup)
             (?y "Show Refs"       magit-show-refs-popup)
             (?z "Stashing"        magit-stash-popup)
             (?! "Running"         magit-run-popup)
             "Applying changes"
             (?a "Apply"           magit-apply)
             (?s "Stage"           magit-stage)
             (?u "Unstage"         magit-unstage)
             nil
             (?v "Reverse"         magit-reverse)
             (?S "Stage all"       magit-stage-modified)
             (?U "Unstage all"     magit-unstage-all)
             nil
             (?k "Discard"         magit-discard)
             "\
 g      refresh current buffer
 TAB    toggle section at point
 RET    visit thing at point

 C-h m  show all key bindings" nil)
  :max-action-columns 4)

;;;; Git Popup

(defvar magit-git-command-history nil)

;;;###autoload (autoload 'magit-run-popup "magit" nil t)
(magit-define-popup magit-run-popup
  "Popup console for running raw Git commands."
  'magit-commands nil nil
  :actions '((?! "Git Subcommand (in topdir)" magit-git-command-topdir)
             (?k "Gitk"                       magit-run-gitk)
             (?p "Git Subcommand (in pwd)"    magit-git-command)
             (?a "Gitk --all"                 magit-run-gitk-all)
             (?s "Shell command (in topdir)"  magit-shell-command-topdir)
             (?b "Gitk --branches"            magit-run-gitk-branches)
             (?S "Shell command (in pwd)"     magit-shell-command)
             (?g "Git Gui"                    magit-run-git-gui))
  :default-action 'magit-git-command
  :max-action-columns 2)

;;;###autoload
(defun magit-git-command (args directory)
  "Execute a Git subcommand asynchronously, displaying the output.
With a prefix argument run Git in the root of the current
repository, otherwise in `default-directory'."
  (interactive (magit-read-shell-command "Git subcommand (pwd: %s)"))
  (require 'eshell)
  (with-temp-buffer
    (insert args)
    (setq args (mapcar 'eval (eshell-parse-arguments (point-min)
                                                     (point-max))))
    (setq default-directory directory)
    (let ((magit-git-global-arguments
           ;; A human will want globbing by default.
           (remove "--literal-pathspecs"
                   magit-git-global-arguments)))
     (magit-run-git-async args)))
  (magit-process-buffer))

;;;###autoload
(defun magit-git-command-topdir (args directory)
  "Execute a Git subcommand asynchronously, displaying the output.
Run Git in the top-level directory of the current repository.
\n(fn)" ; arguments are for internal use
  (interactive (magit-read-shell-command "Git subcommand (pwd: %s)" t))
  (magit-git-command args directory))

;;;###autoload
(defun magit-shell-command (args directory)
  "Execute a shell command asynchronously, displaying the output.
With a prefix argument run the command in the root of the current
repository, otherwise in `default-directory'."
  (interactive (magit-read-shell-command "Shell command (pwd: %s)"))
  (require 'eshell)
  (with-temp-buffer
    (insert args)
    (setq args (mapcar 'eval (eshell-parse-arguments (point-min)
                                                     (point-max))))
    (setq default-directory directory)
    (apply #'magit-start-process (car args) nil (cdr args)))
  (magit-process-buffer))

;;;###autoload
(defun magit-shell-command-topdir (args directory)
  "Execute a shell command asynchronously, displaying the output.
Run the command in the top-level directory of the current repository.
\n(fn)" ; arguments are for internal use
  (interactive (magit-read-shell-command "Shell command (pwd: %s)" t))
  (magit-shell-command args directory))

(defun magit-read-shell-command (prompt &optional root)
  (let ((dir (if (or root current-prefix-arg)
                 (or (magit-toplevel)
                     (user-error "Not inside a Git repository"))
               default-directory)))
    (list (magit-read-string (format prompt (abbreviate-file-name dir))
                             nil 'magit-git-command-history)
          dir)))

;;;; Repository List

;;;###autoload
(defun magit-list-repositories ()
  "Display a list of repositories.

Use the options `magit-repository-directories'
and `magit-repository-directories-depth' to
control which repositories are displayed."
  (interactive)
  (with-current-buffer (get-buffer-create "*Magit Repositories*")
    (magit-repolist-mode)
    (setq tabulated-list-entries
          (mapcar (-lambda ((id . path))
                    (let ((default-directory path))
                      (list path
                            (vconcat (--map (or (funcall (nth 2 it) id) "")
                                            magit-repolist-columns)))))
                  (magit-list-repos-uniquify
                   (--map (cons (file-name-nondirectory (directory-file-name it))
                                it)
                          (magit-list-repos)))))
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))

(defvar magit-repolist-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "g"  'magit-list-repositories)
    (define-key map "\r" 'magit-repolist-status)
    map)
  "Local keymap for Magit-Repolist mode buffers.")

(defun magit-repolist-status (&optional _button)
  "Show the status for the repository at point."
  (interactive)
  (--if-let (tabulated-list-get-id)
      (magit-status-internal it)
    (user-error "There is no repository at point")))

(define-derived-mode magit-repolist-mode tabulated-list-mode "Repos"
  "Major mode for browsing a list of Git repositories."
  (setq x-stretch-cursor        nil)
  (setq tabulated-list-padding  0)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (setq tabulated-list-format
        (vconcat (mapcar (-lambda ((title width _fn props))
                           (nconc (list title width t)
                                  (-flatten props)))
                         magit-repolist-columns)))
  (tabulated-list-init-header))

(defun magit-repolist-column-ident (id)
  "Insert the identification of the repository.
Usually this is just its basename."
  id)

(defun magit-repolist-column-path (_id)
  "Insert the absolute path of the repository."
  (abbreviate-file-name default-directory))

(defun magit-repolist-column-version (_id)
  "Insert a description of the repository's `HEAD' revision."
  (let ((v (or (magit-git-string "describe" "--tags")
               ;; If there are no tags, use the date in MELPA format.
               (magit-git-string "show" "--no-patch" "--format=%cd-g%h"
                                 "--date=format:%Y%m%d.%H%M"))))
    (if (string-match-p "\\`[0-9]" v)
        (concat " " v)
      v)))

(defun magit-repolist-column-unpulled-from-upstream (_id)
  "Insert number of upstream commits not in the current branch."
  (--when-let (magit-get-upstream-branch)
    (let ((n (cadr (magit-rev-diff-count "HEAD" it))))
      (propertize (number-to-string n) 'face (if (> n 0) 'bold 'shadow)))))

(defun magit-repolist-column-unpulled-from-pushremote (_id)
  "Insert number of commits in the push branch but not the current branch."
  (--when-let (magit-get-push-branch)
    (when (magit-rev-parse-p it)
      (let ((n (cadr (magit-rev-diff-count "HEAD" it))))
        (propertize (number-to-string n) 'face (if (> n 0) 'bold 'shadow))))))

(defun magit-repolist-column-unpushed-to-upstream (_id)
  "Insert number of commits in the current branch but not its upstream."
  (--when-let (magit-get-upstream-branch)
    (let ((n (car (magit-rev-diff-count "HEAD" it))))
      (propertize (number-to-string n) 'face (if (> n 0) 'bold 'shadow)))))

(defun magit-repolist-column-unpushed-to-pushremote (_id)
  "Insert number of commits in the current branch but not its push branch."
  (--when-let (magit-get-push-branch)
    (when (magit-rev-parse-p it)
      (let ((n (car (magit-rev-diff-count "HEAD" it))))
        (propertize (number-to-string n) 'face (if (> n 0) 'bold 'shadow))))))

(defun magit-read-repository (&optional read-directory-name)
  "Read a Git repository in the minibuffer, with completion.

The completion choices are the basenames of top-levels of
repositories found in the directories specified by option
`magit-repository-directories'.  In case of name conflicts
the basenames are prefixed with the name of the respective
parent directories.  The returned value is the actual path
to the selected repository.

With prefix argument simply read a directory name using
`read-directory-name'."
  (if (and (not read-directory-name) magit-repository-directories)
      (let* ((repos (magit-list-repos-uniquify
                     (--map (cons (file-name-nondirectory
                                   (directory-file-name it))
                                  it)
                            (magit-list-repos))))
             (reply (magit-completing-read "Git repository" repos)))
        (file-name-as-directory
         (or (cdr (assoc reply repos))
             (if (file-directory-p reply)
                 (expand-file-name reply)
               (user-error "Not a repository or a directory: %s" reply)))))
    (file-name-as-directory
     (read-directory-name "Git repository: "
                          (or (magit-toplevel) default-directory)))))

(defun magit-list-repos ()
  (--mapcat (if (consp it)
                (magit-list-repos-1 (car it) (cdr it))
              (magit-list-repos-1 it magit-repository-directories-depth))
            magit-repository-directories))

(defun magit-list-repos-1 (directory depth)
  (cond ((file-readable-p (expand-file-name ".git" directory))
         (list directory))
        ((and (> depth 0) (magit-file-accessible-directory-p directory))
         (--mapcat (when (file-directory-p it)
                     (magit-list-repos-1 it (1- depth)))
                   (directory-files directory t "^[^.]" t)))))

(defun magit-list-repos-uniquify (alist)
  (let (result (dict (make-hash-table :test 'equal)))
    (dolist (a (delete-dups alist))
      (puthash (car a) (cons (cdr a) (gethash (car a) dict)) dict))
    (maphash
     (lambda (key value)
       (if (= (length value) 1)
           (push (cons key (car value)) result)
         (setq result
               (append result
                       (magit-list-repos-uniquify
                        (--map (cons (concat
                                      key "\\"
                                      (file-name-nondirectory
                                       (directory-file-name
                                        (substring it 0 (- (1+ (length key)))))))
                                     it)
                               value))))))
     dict)
    result))

;;;; Revision Stack

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

If INDEX-REGEXP is non-nil then both POINT-FORMAT and EOB-FORMAT
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

If the stack is empty or with a prefix argument instead read a
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
            (setq pnt-args (cdr pnt-format)
                  pnt-format (car pnt-format)))
          (when (listp eob-format)
            (setq eob-args (cdr eob-format)
                  eob-format (car eob-format)))
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
      (copy-region-as-kill (mark) (point) 'region)
    (-when-let* ((section (magit-current-section))
                 (value (magit-section-value section)))
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
      (copy-region-as-kill (mark) (point) 'region)
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

;;; magit.el ends soon

(defconst magit-font-lock-keywords
  (eval-when-compile
    `((,(concat "(\\(magit-define-section-jumper\\)\\_>"
                "[ \t'\(]*"
                "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
       (1 'font-lock-keyword-face)
       (2 'font-lock-function-name-face nil t))
      (,(concat "(" (regexp-opt '("magit-insert-section"
                                  "magit-section-case"
                                  "magit-section-when"
                                  "magit-bind-match-strings"
                                  "magit-with-temp-index"
                                  "magit-with-blob"
                                  "magit-with-toplevel") t)
                "\\_>")
       . 1))))

(font-lock-add-keywords 'emacs-lisp-mode magit-font-lock-keywords)

(defvar magit-version 'undefined
  "The version of Magit that you're using.
Use the function by the same name instead of this variable.")

;;;###autoload
(defun magit-version ()
  "Return the version of Magit currently in use.
When called interactive also show the used versions of Magit,
Git, and Emacs in the echo area."
  (interactive)
  (let ((magit-git-global-arguments nil)
        (toplib (or load-file-name buffer-file-name))
        debug)
    (unless (and toplib
                 (equal (file-name-nondirectory toplib) "magit.el"))
      (setq toplib (locate-library "magit.el")))
    (push toplib debug)
    (when toplib
      (let* ((topdir (file-name-directory toplib))
             (gitdir (expand-file-name
                      ".git" (file-name-directory
                              (directory-file-name topdir))))
             (static (locate-library "magit-version.el" nil (list topdir))))
        (or (progn
              (push 'repo debug)
              (when (and (file-exists-p gitdir)
                         ;; It is a repo, but is it the Magit repo?
                         (file-exists-p
                          (expand-file-name "../lisp/magit.el" gitdir)))
                (push t debug)
                ;; Inside the repo the version file should only exist
                ;; while running make.
                (when (and static (not noninteractive))
                  (ignore-errors (delete-file static)))
                (setq magit-version
                      (let ((default-directory topdir))
                        (magit-git-string "describe" "--tags" "--dirty")))))
            (progn
              (push 'static debug)
              (when (and static (file-exists-p static))
                (push t debug)
                (load-file static)
                magit-version))
            (when (featurep 'package)
              (push 'elpa debug)
              (ignore-errors
                (--when-let (assq 'magit package-alist)
                  (push t debug)
                  (setq magit-version
                        (and (fboundp 'package-desc-version)
                             (package-version-join
                              (package-desc-version (cadr it)))))))))))
    (if (stringp magit-version)
        (when (called-interactively-p 'any)
          (message "Magit %s, Git %s, Emacs %s, %s"
                   (or magit-version "(unknown)")
                   (or (magit-git-version t) "(unknown)")
                   emacs-version
                   system-type))
      (setq debug (reverse debug))
      (setq magit-version 'error)
      (when magit-version
        (push magit-version debug))
      (message "Cannot determine Magit's version %S" debug))
    magit-version))

(defun magit-startup-asserts ()
  (let ((version (magit-git-version)))
    (when (and version
               (version< version magit--minimal-git)
               (not (equal (getenv "TRAVIS") "true")))
      (display-warning 'magit (format "\
Magit requires Git >= %s, you are using %s.

If this comes as a surprise to you, because you do actually have
a newer version installed, then that probably means that the
older version happens to appear earlier on the `$PATH'.  If you
always start Emacs from a shell, then that can be fixed in the
shell's init file.  If you start Emacs by clicking on an icon,
or using some sort of application launcher, then you probably
have to adjust the environment as seen by graphical interface.
For X11 something like ~/.xinitrc should work.

If you use Tramp to work inside remote Git repositories, then you
have to make sure a suitable Git is used on the remote machines
too.\n" magit--minimal-git version) :error)))
  (when (version< emacs-version magit--minimal-emacs)
    (display-warning 'magit (format "\
Magit requires Emacs >= %s, you are using %s.

If this comes as a surprise to you, because you do actually have
a newer version installed, then that probably means that the
older version happens to appear earlier on the `$PATH'.  If you
always start Emacs from a shell, then that can be fixed in the
shell's init file.  If you start Emacs by clicking on an icon,
or using some sort of application launcher, then you probably
have to adjust the environment as seen by graphical interface.
For X11 something like ~/.xinitrc should work.\n"
                                    magit--minimal-emacs emacs-version)
                     :error))
  (--each '((magit-log-edit  . git-commit)
            (git-commit-mode . git-commit)
            (git-rebase-mode . git-rebase))
    (when (or (featurep (car it)) (locate-library (symbol-name (car it))))
      (display-warning 'magit (format "%s has to be removed

Magit is no longer compatible with the library `%s',
which was used in earlier releases.  Please remove it, so that
Magit can use the successor `%s' without the obsolete
library getting in the way.  Then restart Emacs.\n"
                                      (car it)  (car it) (cdr it)) :error))))

(defvar magit--remotes-using-recent-git nil)

(defun magit-tramp-asserts (directory)
  (-when-let (remote (file-remote-p directory))
    (unless (member remote magit--remotes-using-recent-git)
      (-if-let (version (let ((default-directory directory))
                          (magit-git-version)))
          (if (version<= magit--minimal-git version)
              (push version magit--remotes-using-recent-git)
            (display-warning 'magit (format "\
Magit requires Git >= %s, but on %s the version is %s.

If multiple Git versions are installed on the host then the
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

(define-obsolete-function-alias 'global-magit-file-buffer-mode
  'global-magit-file-mode "Magit 2.3.0")

(define-obsolete-function-alias 'magit-insert-head-header
  'magit-insert-head-branch-header "Magit 2.4.0")

(define-obsolete-function-alias 'magit-insert-upstream-header
  'magit-insert-upstream-branch-header "Magit 2.4.0")

(define-obsolete-function-alias 'magit-insert-pull-branch-header
  'magit-insert-upstream-branch-header "Magit 2.4.0")

(provide 'magit)

(cl-eval-when (load eval)
  (require 'magit-sequence)
  (require 'magit-commit)
  (require 'magit-remote)
  (require 'magit-bisect)
  (require 'magit-stash)
  (require 'magit-blame)
  (unless (load "magit-autoloads" t t)
    (require 'magit-submodule)
    (require 'magit-subtree)
    (require 'magit-ediff)
    (require 'magit-extras)
    (require 'git-rebase)))

(if after-init-time
    (progn (magit-startup-asserts)
           (magit-version))
  (add-hook 'after-init-hook #'magit-startup-asserts t)
  (add-hook 'after-init-hook #'magit-version t))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; magit.el ends here
