;;; magit.el --- control Git from Emacs

;; Copyright (C) 2008-2014  The Magit Project Developers
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

;; Author: Marius Vollmer <marius.vollmer@gmail.com>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Former-Maintainers:
;;	Nicolas Dudebout  <nicolas.dudebout@gatech.edu>
;;	Peter J. Weisberg <pj@irregularexpressions.net>
;;	Phil Jackson      <phil@shellarchive.co.uk>
;;	Rémi Vanicat      <vanicat@debian.org>
;;	Yann Hodique      <yann.hodique@gmail.com>

;; Keywords: vc tools
;; Package: magit
;; Package-Requires: ((cl-lib "0.3") (dash "2.6.0") (git-commit-mode "0.14.0") (git-rebase-mode "0.14.0") (with-editor "0"))

;; Magit requires at least GNU Emacs 23.2 and Git 1.7.2.5.
;; These are the versions shipped by Debian oldstable (6.0, Squeeze).

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
;; along with Magit.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Invoking the magit-status function will show a buffer with the
;; status of the current git repository and its working tree.  That
;; buffer offers key bindings for manipulating the status in simple
;; ways.
;;
;; The status buffer mainly shows the difference between the working
;; tree and the index, and the difference between the index and the
;; current HEAD.  You can add individual hunks from the working tree
;; to the index, and you can commit the index.
;;
;; See the Magit User Manual for more information.

;;; Code:
;;;; Dependencies

(when (version< emacs-version "23.2")
  (error "Magit requires at least GNU Emacs 23.2"))

(require 'git-commit-mode)
(require 'git-rebase-mode)
(require 'with-editor)

(require 'magit-popup)

(require 'ansi-color)
(require 'autorevert)
(require 'cl-lib)
(require 'dash)
(require 'diff-mode)
(require 'epa)
(require 'format-spec)
(require 'grep)
(require 'help-mode)
(require 'ring)
(require 'server)
(require 'tramp)
(require 'view)

(eval-when-compile
  (require 'dired)
  (require 'dired-x)
  (require 'ediff)
  (require 'eshell)
  (require 'ido)
  (require 'package nil t)
  (require 'view))

;;;; Declarations

(declare-function dired-jump 'dired-x)
(declare-function dired-uncache 'dired)
(declare-function ediff-cleanup-mess 'ediff)
(declare-function eshell-parse-arguments 'eshell)
(declare-function ido-completing-read 'ido)
(declare-function iswitchb-read-buffer 'iswitchb)
(declare-function package-desc-vers 'package)
(declare-function package-desc-version 'package)
(declare-function package-version-join 'package)

(defvar iswitchb-temp-buflist)
(defvar package-alist)

(defvar magit-log-buffer-name)
(defvar magit-reflog-buffer-name)
(defvar magit-refresh-args)
(defvar magit-stash-buffer-name)
(defvar magit-status-buffer-name)
(defvar magit-this-process)

;;;; Compatibility

(eval-and-compile
  ;; Added in Emacs 24.1
  (unless (fboundp 'run-hook-wrapped)
    (defun run-hook-wrapped  (hook wrap-function &rest args)
      "Run HOOK, passing each function through WRAP-FUNCTION.
I.e. instead of calling each function FUN directly with arguments ARGS,
it calls WRAP-FUNCTION with arguments FUN and ARGS.
As soon as a call to WRAP-FUNCTION returns non-nil, `run-hook-wrapped'
aborts and returns that value."
      (when (boundp hook)
        (let ((fns (symbol-value hook)))
          (apply 'run-hook-wrapped-1 hook
                 (if (functionp fns) (list fns) fns)
                 wrap-function args)))))

    (defun run-hook-wrapped-1 (hook fns wrap-function &rest args)
      (cl-loop for fn in fns
               if (and (eq fn t)
                       (local-variable-p hook)
                       (default-boundp hook)
                       (apply 'run-hook-wrapped-1 nil
                              (default-value hook) wrap-function args))
               return it
               else if (and (functionp fn) (apply wrap-function fn args))
               return it))
  )


;;; Settings
;;;; Custom Groups

(defgroup magit nil
  "Controlling Git from Emacs."
  :group 'tools)

(defgroup magit-process nil
  "Git and other external processes used by Magit."
  :group 'magit)

(defgroup magit-popups nil
  "Command console popups provided by Magit."
  :group 'magit)

(defgroup magit-modes nil
  "Modes used or provided by Magit."
  :group 'magit)

(defgroup magit-status nil
  "Inspect and manipulate Git repositories."
  :group 'magit-modes)

(defgroup magit-diff nil
  "Inspect and manipulate Git diffs."
  :group 'magit-modes)

(defgroup magit-commit nil
  "Inspect and manipulate Git commits."
  :group 'magit-modes)

(defgroup magit-log nil
  "Inspect and manipulate Git history."
  :group 'magit-modes)

(defgroup magit-extensions nil
  "Extensions to Magit."
  :group 'magit)

(defgroup magit-wip nil
  "Git-Wip support for Magit."
  :group 'magit-extensions)

(defgroup magit-faces nil
  "Faces used by Magit."
  :group 'magit
  :group 'faces)

(custom-add-to-group 'magit-popup  'magit-popups      'custom-group)
(custom-add-to-group 'magit-popups 'magit-popup       'custom-group)
(custom-add-to-group 'magit-modes  'magit-popup       'custom-group)
(custom-add-to-group 'magit-faces  'magit-popup-faces 'custom-group)

(when (featurep 'gitattributes-mode)
  (custom-add-to-group 'magit-modes 'gitattributes-mode 'custom-group))
(when (featurep 'gitconfig-mode)
  (custom-add-to-group 'magit-modes 'gitconfig-mode 'custom-group))
(when (featurep 'gitignore-mode)
  (custom-add-to-group 'magit-modes 'gitignore-mode 'custom-group))

(custom-add-to-group 'magit-modes   'git-commit       'custom-group)
(custom-add-to-group 'magit-faces   'git-commit-faces 'custom-group)
(custom-add-to-group 'magit-modes   'git-rebase       'custom-group)
(custom-add-to-group 'magit-faces   'git-rebase-faces 'custom-group)
(custom-add-to-group 'magit-process 'with-editor      'custom-group)

(custom-add-to-group 'magit 'vc-follow-symlinks 'custom-variable)

;;;; Custom Options
;;;;; Processes

(defcustom magit-git-executable
  (or (and (eq system-type 'windows-nt)
           ;; On Windows asking for "git" from $PATH might also return
           ;; a "git.exe" or "git.cmd".  Using "bin/git.exe" directly
           ;; is faster than using one of the wrappers "cmd/git.exe"
           ;; or "cmd/git.cmd".  The wrappers are likely to come
           ;; earlier on $PATH, and so we have to exlicitly use
           ;; the former.
           (let ((exe (executable-find "git.exe")))
             (when exe
               (let ((alt (directory-file-name (file-name-directory exe))))
                 (if (and (equal (file-name-nondirectory alt) "cmd")
                          (setq alt (expand-file-name
                                     (convert-standard-filename "bin/git.exe")
                                     (file-name-directory alt)))
                          (file-executable-p alt))
                     alt
                   exe)))))
      (executable-find "git") "git")
  "The Git executable used by Magit."
  :group 'magit-process
  :type 'string)

(defcustom magit-git-standard-options
  '("--no-pager" "-c" "core.preloadindex=true")
  "Standard options when running Git.
Be careful what you add here, especially if you are using
tramp to connect to servers with ancient Git versions."
  :group 'magit-process
  :type '(repeat string))

(defcustom magit-success-executable "true"
  "The executable which always succeeds.
An executable, such as \"true\", which does
nothing but return with a zero exit status."
  :package-version '(magit . "2.1.0")
  :group 'magit-process
  :type 'string)

(defcustom magit-process-connection-type (not (eq system-type 'cygwin))
  "Connection type used for the git process.

If nil, use pipes: this is usually more efficient, and works on Cygwin.
If t, use ptys: this enables magit to prompt for passphrases when needed."
  :group 'magit-process
  :type '(choice (const :tag "pipe" nil)
                 (const :tag "pty" t)))

(defcustom magit-process-popup-time -1
  "Popup the process buffer if a command takes longer than this many seconds."
  :group 'magit-process
  :type '(choice (const :tag "Never" -1)
                 (const :tag "Immediately" 0)
                 (integer :tag "After this many seconds")))

(defcustom magit-process-log-max 32
  "Maximum number of sections to keep in a process log buffer.
When adding a new section would go beyond the limit set here,
then the older half of the sections are remove.  Sections that
belong to processes that are still running are never removed."
  :package-version '(magit . "2.1.0")
  :group 'magit-process
  :type 'integer)

(defcustom magit-process-quote-curly-braces
  (and (eq system-type 'windows-nt)
       (let ((case-fold-search t))
         (string-match-p "cygwin" magit-git-executable))
       t)
  "Whether curly braces should be quoted when calling git.
This may be necessary when using Windows.  On all other system
types this must always be nil.

We are not certain when quoting is needed, but it appears it is
needed when using Cygwin Git but not when using stand-alone Git.
The default value is set based on that assumptions.  If this
turns out to be wrong you can customize this option but please
also comment on issue #816."
  :package-version '(magit . "2.1.0")
  :group 'magit-process
  :set-after '(magit-git-executable)
  :type 'boolean)

(defcustom magit-process-yes-or-no-prompt-regexp
  " [\[(]\\([Yy]\\(?:es\\)?\\)[/|]\\([Nn]o?\\)[\])] ?[?:] ?$"
  "Regexp matching Yes-or-No prompts of git and its subprocesses."
  :package-version '(magit . "2.1.0")
  :group 'magit-process
  :type 'regexp)

(defcustom magit-process-password-prompt-regexps
  '("^\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$"
    "^\\(Enter \\)?[Pp]assword\\( for '.*'\\)?: ?$"
    "^.*'s password: ?$"
    "^Yubikey for .*: ?$")
  "List of regexps matching password prompts of git and its subprocesses."
  :package-version '(magit . "2.1.0")
  :group 'magit-process
  :type '(repeat (regexp)))

(defcustom magit-process-username-prompt-regexps
  '("^Username for '.*': ?$")
  "List of regexps matching username prompts of git and its subprocesses."
  :package-version '(magit . "2.1.0")
  :group 'magit-process
  :type '(repeat (regexp)))

;;;;; Staging

(defcustom magit-stage-all-confirm t
  "Whether to require confirmation before staging all changes.
This reduces the risk of accidentally losing the index.  If
nothing at all is staged yet, then always stage without requiring
confirmation, because it can be undone without the risk of losing
a carefully crafted index."
  :package-version '(magit . "2.1.0")
  :group 'magit
  :type 'boolean)

(defcustom magit-unstage-all-confirm t
  "Whether to require confirmation before unstaging all changes.
This reduces the risk of accidentally losing of the index.  If
there are no staged changes at all, then always unstage without
confirmation, because it can be undone without the risk of losing
a carefully crafted index."
  :package-version '(magit . "2.1.0")
  :group 'magit
  :type 'boolean)

(defcustom magit-revert-item-confirm t
  "Whether to require confirmation before reverting hunks.
If you disable this, consider enabling `magit-revert-backup'
instead."
  :group 'magit
  :type 'boolean)

(defcustom magit-revert-backup nil
  "Whether to backup a hunk before reverting it.
The hunk is stored in \".git/magit/reverted.diff\" and can be
applied using `magit-revert-undo'.  Older hunks are available
in the same directory as numbered backup files and have to be
applied manually.  Only individual hunks are backed up; when
a complete file is reverted (which requires confirmation) no
backup is created."
  :package-version '(magit . "2.1.0")
  :group 'magit
  :type 'boolean)

(defcustom magit-save-some-buffers t
  "Whether certain commands save modified buffers before running.

nil        don't save buffers.
t          ask which buffers to save.
`dontask'  save all buffers without asking."
  :group 'magit
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Ask" t)
                 (const :tag "Save without asking" dontask)))

(defcustom magit-save-some-buffers-predicate
  'magit-save-buffers-predicate-tree-only
  "A predicate function to decide whether to save a buffer.

Used by function `magit-save-some-buffers' when the variable of
the same name is non-nil."
  :group 'magit
  :type '(radio (function-item magit-save-buffers-predicate-tree-only)
                (function-item magit-save-buffers-predicate-all)
                (function :tag "Other")))

(defcustom magit-rewrite-inclusive t
  "Whether magit includes the selected base commit in a rewrite operation.

t means both the selected commit as well as any subsequent
commits will be rewritten.  This is magit's default behaviour,
equivalent to 'git rebase -i ${REV}~1'

  A'---B'---C'---D'
  ^

nil means the selected commit will be literally used as 'base',
so only subsequent commits will be rewritten.  This is consistent
with git-rebase, equivalent to 'git rebase -i ${REV}', yet more
cumbersome to use from the status buffer.

  A---B'---C'---D'
  ^"
  :group 'magit
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (const :tag "Ask"   ask)))

;;;;; Highlighting

(defun magit-set-variable-and-refresh (symbol value)
  "Set SYMBOL to VALUE and call `magit-refresh-all'."
  (set-default symbol value)
  ;; If magit isn't fully loaded yet no buffer that might
  ;; need refreshing can exist and we can take a shortcut.
  ;; We also don't want everything to repeatedly refresh
  ;; when evaluating this file.
  (when (and (featurep 'magit) (not buffer-file-name))
    (magit-refresh-all)))

(defcustom magit-highlight-whitespace t
  "Specify where to highlight whitespace errors.
See `magit-highlight-trailing-whitespace',
`magit-highlight-indentation'.  The symbol t means in all diffs,
`status' means only in the status buffer, and nil means nowhere."
  :group 'magit
  :set 'magit-set-variable-and-refresh
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (const :tag "In status buffer" status)))

(defcustom magit-highlight-trailing-whitespace t
  "Whether to highlight whitespace at the end of a line in diffs.
Used only when `magit-highlight-whitespace' is non-nil."
  :group 'magit
  :set 'magit-set-variable-and-refresh
  :type 'boolean)

(defcustom magit-highlight-indentation nil
  "Highlight the \"wrong\" indentation style.
Used only when `magit-highlight-whitespace' is non-nil.

The value is a list of cons cells.  The car is a regular
expression, and the cdr is the value that applies to repositories
whose directory matches the regular expression.  If more than one
item matches, then the *last* item in the list applies.  So, the
default value should come first in the list.

If the value is `tabs', highlight indentation with tabs.  If the
value is an integer, highlight indentation with at least that
many spaces.  Otherwise, highlight neither."
  :group 'magit
  :set 'magit-set-variable-and-refresh
  :type `(repeat (cons (string :tag "Directory regexp")
                       (choice (const :tag "Tabs" tabs)
                               (integer :tag "Spaces" :value ,tab-width)
                               (const :tag "Neither" nil))))) ;^FIXME

(defcustom magit-item-highlight-face 'magit-item-highlight
  "The face used to highlight the current section.

By default the highlighting of the current section is done using
the background color specified by face `magit-item-highlight'.

If you don't want to use the background to do the highlighting,
this *might* by as easy as customizing that face.  However if you
are using a theme, which in turn sets the background color of
that face then, due to limitations in face inheritance when using
themes, you might be forced to use another face.

Unfortunately it is only possible to override a face attribute,
set by a theme, but not to drop it entirely.  This means that one
has to explicitly use the `default' background color, to make it
appear *as if* the background wasn't used.

One reason you might want to *not* use the background, is that
doing so forces the use of overlays for parts of diffs and for
refnames.  Using overlays potentially degrades performance when
generating large diffs.  Also see option `magit-use-overlays'."
  :package-version '(magit . "2.1.0")
  :group 'magit
  :group 'magit-faces
  :type '(choice (const magit-item-highlight)
                 (const bold)
                 (face  :tag "Other face")
                 (const :tag "Don't highlight" nil)))

(defcustom magit-use-overlays
  (not (eq magit-item-highlight-face 'bold))
  "Whether to use overlays to highlight various diff components.

This has to be non-nil if the current section is highlighted by
changing the background color.  Otherwise background colors that
hold semantic meaning, like that of the added and removed lines
in diffs, as well as section headings, would be shadowed by the
highlighting.

To select the face used for highlighting customize the option
`magit-item-highlight-face'.  If you set that to `bold' or some
other face that does not use the background then you can set this
option to nil.  Doing so could potentially improve performance
when generating large diffs."
  :package-version '(magit . "2.1.0")
  :group 'magit
  :group 'magit-faces
  :set-after '(magit-item-highlight-face)
  :type 'boolean)

(define-obsolete-variable-alias 'magit-diff-use-overlays
  'magit-use-overlays "2.1.0")

;;;;; Completion

(defcustom magit-completing-read-function 'magit-builtin-completing-read
  "Function to be called when requesting input from the user."
  :group 'magit
  :type '(radio (function-item magit-builtin-completing-read)
                (function-item magit-ido-completing-read)
                (function-item magit-iswitchb-completing-read)
                (function :tag "Other")))

(defcustom magit-repo-dirs nil
  "Directories containing Git repositories.
Magit will look into these directories for Git repositories and
offer them as choices for `magit-status'."
  :group 'magit
  :type '(repeat string))

(defcustom magit-repo-dirs-depth 3
  "The maximum depth to look for Git repos.
When looking for a Git repository below the directories in
`magit-repo-dirs', Magit will only descend this many levels
deep."
  :group 'magit
  :type 'integer)

;;;;; Modes
;;;;;; Common

(defcustom magit-mode-hook '(magit-load-config-extensions)
  "Hook run when entering a Magit mode derived mode."
  :group 'magit-modes
  :type 'hook
  :options '(magit-load-config-extensions))

(defcustom magit-show-xref-buttons '(magit-diff-mode magit-commit-mode)
  "List of modes whose buffers should contain history buttons.
Currently only `magit-diff-mode' and `magit-commit-mode' are
supported."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type '(repeat (choice (const magit-diff-mode)
                         (const magit-commit-mode))))

(defcustom magit-show-child-count nil
  "Whether to append the number of childen to section headings."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type 'boolean)

(defvar magit-status-line-align-to 9)

(defcustom magit-restore-window-configuration nil
  "Whether quitting a Magit buffer restores previous window configuration.

Function `magit-mode-display-buffer' is used to display and
select Magit buffers.  Unless the buffer was already displayed in
a window of the selected frame it also stores the previous window
configuration.  If this option is non-nil that configuration will
later be restored by `magit-mode-quit-window', provided the
buffer has not since been displayed in another frame.

This works best when only two windows are usually displayed in a
frame.  If this isn't the case setting this to t might often lead
to undesirable behaviour.  Also quitting a Magit buffer while
another Magit buffer that was created earlier is still displayed
will cause that buffer to be hidden, which might or might not be
what you want."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type 'boolean)

(defcustom magit-refs-namespaces
  '(("^\\(HEAD\\)$"              magit-log-head-label-head nil)
    ("^refs/tags/\\(.+\\)"       magit-log-head-label-tags nil)
    ("^refs/heads/\\(.+\\)"      magit-log-head-label-local nil)
    ("^refs/remotes/\\(.+\\)"    magit-log-head-label-remote nil)
    ("^refs/bisect/\\(bad\\)"    magit-log-head-label-bisect-bad nil)
    ("^refs/bisect/\\(skip.*\\)" magit-log-head-label-bisect-skip nil)
    ("^refs/bisect/\\(good.*\\)" magit-log-head-label-bisect-good nil)
    ("^refs/wip/\\(.+\\)"        magit-log-head-label-wip nil)
    ("^refs/patches/\\(.+\\)"    magit-log-head-label-patches nil)
    ("^\\(bad\\):"               magit-log-head-label-bisect-bad nil)
    ("^\\(skip\\):"              magit-log-head-label-bisect-skip nil)
    ("^\\(good\\):"              magit-log-head-label-bisect-good nil)
    ("\\(.+\\)"                  magit-log-head-label-default nil))
  "How different refs should be formatted for display.

Each entry controls how a certain type of ref is displayed, and
has the form (REGEXP FACE FORMATTER).  REGEXP is a regular
expression used to match full refs.  The first entry whose REGEXP
matches the reference is used.  The first regexp submatch becomes
the \"label\" that represents the ref and is propertized with
font FONT.  If FORMATTER is non-nil it should be a function that
takes two arguments, the full ref and the face.  It is supposed
to return a propertized label that represents the ref.

Currently this variable is only used in logs and the branch
manager but it will be used in more places in the future."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type '(repeat
          (list regexp
                face
                (choice (const :tag "first submatch is label" nil)
                        (function :tag "format using function")))))

;;;;;; Status

(defcustom magit-status-sections-hook
  '(magit-insert-status-local-line
    magit-insert-status-remote-line
    magit-insert-status-head-line
    magit-insert-status-tags-line
    magit-insert-status-merge-line
    magit-insert-status-rebase-lines
    magit-insert-empty-line
    magit-insert-rebase-sequence
    magit-insert-bisect-output
    magit-insert-bisect-rest
    magit-insert-bisect-log
    magit-insert-stashes
    magit-insert-untracked-files
    magit-insert-unstaged-changes
    magit-insert-staged-changes
    magit-insert-unpulled-commits
    magit-insert-unpushed-commits)
  "Hook run to insert sections into the status buffer.

This option allows reordering the sections and adding sections
that are by default displayed in other Magit buffers.  Doing the
latter is currently not recommended because not all functions
that insert sections have been adapted yet.  Only inserters that
take no argument can be used and some functions exist that begin
with the `magit-insert-' prefix but do not insert a section.

Note that there are already plans to improve this and to add
similar hooks for other Magit modes."
  :package-version '(magit . "2.1.0")
  :group 'magit-status
  :type 'hook)

(defcustom magit-status-buffer-switch-function 'pop-to-buffer
  "Function for `magit-status' to use for switching to the status buffer.

The function is given one argument, the status buffer."
  :group 'magit-status
  :type '(radio (function-item switch-to-buffer)
                (function-item pop-to-buffer)
                (function :tag "Other")))

(defcustom magit-status-show-sequence-help t
  "Whether to show instructions on how to proceed a stopped action.
When this is non-nil and a commit failed to apply during a merge
or rebase, then show instructions on how to continue."
  :package-version '(magit . "2.1.0")
  :group 'magit-status
  :type 'boolean)

(defcustom magit-status-tags-line-subject 'head
  "Whether tag or head is the subject on tags line in status buffer.

This controls how the words \"ahead\" and \"behind\" are used on
the tags line in the status buffer.  The tags line does not
actually display complete sentences, but when thinking about when
to use which term, it helps imagining it did.  This option
controls whether the tag names should be considered the subjects
or objects in these sentences.

`tag'   The previous tag is *behind* HEAD by N commits.
        The next tag is *ahead* of HEAD by N commits.
`head'  HEAD is *ahead* of the previous tag by N commits.
        HEAD is *behind* the next tag by N commits.

If the value is `tag' the commit counts are fontified; otherwise
they are not (due to semantic considerations)."
  :package-version '(magit . "2.1.0")
  :group 'magit-status
  :type '(choice (const :tag "tags are the subjects" tag)
                 (const :tag "head is the subject" head)))

;;;;;; Diff

(defcustom magit-show-diffstat t
  "Whether to show diffstat in diff and commit buffers."
  :package-version '(magit . "2.1.0")
  :group 'magit-diff
  :group 'magit-commit
  :type 'boolean)

(defcustom magit-diff-options nil
  ""
  :group 'magit
  :type 'sexp)

(put 'magit-diff-options 'permanent-local t)

;; This variable is only a temporary hack.  Eventually it
;; will be possible to set these arguments in the diff popup.
(defvar magit-diff-extra-options '("-M" "-C"))

(defcustom magit-diff-auto-show
  '(commit stage-all log-oneline log-select)
  "Whether to automatically show relevant diff.

When this option is non-nil certain operations cause the relevant
changes to be displayed automatically.

`commit'
`stage-all'
`log-oneline'
`log-follow'
`log-select'

In the event that expanding very large patches takes a long time
\\<global-map>\\[keyboard-quit] can be used to abort that step.
This is especially useful when you would normally not look at the
changes, e.g. because you are committing some binary files."
  :package-version '(magit . "2.1.0")
  :group 'magit-diff
  :type 'sexp)

(defcustom magit-diff-refine-hunk nil
  "Show fine (word-granularity) differences within diff hunks.

There are three possible settings:

nil    never show fine differences
t      show fine differences for the selected diff hunk only
`all'  show fine differences for all displayed diff hunks"
  :group 'magit-diff
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Selected only" t)
                 (const :tag "All" all))
  :set 'magit-set-variable-and-refresh)

;;;;;; Commit

(defcustom magit-commit-ask-to-stage t
  "Whether to ask to stage everything when committing and nothing is staged."
  :package-version '(magit . "2.1.0")
  :group 'magit-commit
  :type 'boolean)

(defcustom magit-commit-extend-override-date nil
  "Whether using `magit-commit-extend' changes the committer date."
  :package-version '(magit . "2.1.0")
  :group 'magit-commit
  :type 'boolean)

(defcustom magit-commit-reword-override-date nil
  "Whether using `magit-commit-reword' changes the committer date."
  :package-version '(magit . "2.1.0")
  :group 'magit-commit
  :type 'boolean)

(defcustom magit-commit-squash-confirm t
  "Whether the commit targeted by squash and fixup has to be confirmed.
When non-nil then the commit at point (if any) is used as default
choice, otherwise it has to be confirmed.  This option only
affects `magit-commit-squash' and `magit-commit-fixup'.  The
\"instant\" variants always require confirmation because making
an error while using those is harder to recover from."
  :package-version '(magit . "2.1.0")
  :group 'magit-commit
  :type 'boolean)

;;;;;; Log

(defcustom magit-log-auto-more nil
  "Insert more log entries automatically when moving past the last entry.

Only considered when moving past the last entry with
`magit-goto-*-section' commands."
  :group 'magit-log
  :type 'boolean)

(defcustom magit-log-cutoff-length 100
  "The maximum number of commits to show in the log and whazzup buffers."
  :group 'magit-log
  :type 'integer)

(defcustom magit-log-infinite-length 99999
  "Number of log used to show as maximum for `magit-log-cutoff-length'."
  :group 'magit-log
  :type 'integer)

(defcustom magit-log-format-graph-function nil
  "Function used to format graphs in log buffers.
The function is called with one argument, the propertized graph
of a single line in as a string.  It has to return the formatted
string.  This option can also be nil, in which case the graph is
inserted as is."
  :package-version '(magit . "2.1.0")
  :group 'magit-log
  :type '(choice (const :tag "insert as is" nil)
                 function))

(defcustom magit-log-show-margin t
  "Whether to use a margin when showing `oneline' logs.
When non-nil the author name and date are displayed in the margin
of the log buffer if that contains a `oneline' log.  This can be
toggled temporarily using the command `magit-log-toggle-margin'."
  :package-version '(magit . "2.1.0")
  :group 'magit-log
  :type 'boolean)

(put 'magit-log-show-margin 'permanent-local t)

(defcustom magit-log-margin-spec '(25 nil magit-duration-spec)
  "How to format the margin for `oneline' logs.

When the log buffer contains a `oneline' log, then it optionally
uses the right margin to display the author name and author date.
This is also supported in the reflog buffer.

Logs that are shown together with other non-log information (e.g.
in the status buffer) are never accompanied by a margin.  The
same applies to `long' logs, in this case because that would be
redundant.

This option controls how that margin is formatted, the other
option affecting this is `magit-log-show-margin'; if that is nil
then no margin is displayed at all.  To toggle this temporarily
use the command `magit-log-show-margin'.

The value has the form (WIDTH CHARACTERP DURATION-SPEC).  The
width of the margin is controlled using WIDTH, an integer.  When
CHARACTERP is non-nil time units are shown as single characters,
otherwise the full name of the unit is displayed.  DURATION-SPEC
has to be a variable, its value controls which time units are
used, how many seconds they contain, and what their names are."
  :package-version '(magit . "2.1.0")
  :group 'magit-log
  :type '(list (integer  :tag "Margin width")
               (choice   :tag "Time unit style"
                         (const :tag "Character" t)
                         (const :tag "Word" nil))
               (variable :tag "Duration spec variable")))

(defcustom magit-duration-spec
  `((?Y "year"   "years"   ,(round (* 60 60 24 365.2425)))
    (?M "month"  "months"  ,(round (* 60 60 24 30.436875)))
    (?w "week"   "weeks"   ,(* 60 60 24 7))
    (?d "day"    "days"    ,(* 60 60 24))
    (?h "hour"   "hours"   ,(* 60 60))
    (?m "minute" "minutes" 60)
    (?s "second" "seconds" 1))
  "Units used to display durations in a human format.
The value is a list of time units, beginning with the longest.
Each element has the form ((CHAR UNIT UNITS SECONDS)..).  UNIT
is the time unit, UNITS is the plural of that unit.  CHAR is a
character that can be used as abbreviation and must be unique
amoung all elements.  SECONDS is the number of seconds in one
UNIT.  Also see option `magit-log-margin-spec'."
  :package-version '(magit . "2.1.0")
  :group 'magit-log
  :type '(repeat (list (character :tag "Unit character")
                       (string    :tag "Unit singular string")
                       (string    :tag "Unit plural string")
                       (integer   :tag "Seconds in unit"))))

(defcustom magit-ellipsis #x2026 ; "horizontal ellipsis"
  "Character appended to abreviated text.
Currently this is used only in the log margin, but might later
be used elsewhere too.  Filenames that were abbreviated by Git
are left as-is."
  :package-version '(magit . "2.1.0")
  :group 'magit-log
  :type 'character)

;;;;;; Others

(defcustom magit-auto-revert-mode-lighter " MRev"
  "String to display when Magit-Auto-Revert mode is active."
  :group 'magit-modes)

(define-minor-mode magit-auto-revert-mode
  "Toggle global Magit-Auto-Revert mode.
With prefix ARG, enable Magit-Auto-Revert mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Magit-Auto-Revert mode is a global minor mode that, after Magit
has run a Git command, reverts buffers associated with files that
have changed on disk and are tracked in the current Git repository."
  :group 'magit-modes
  :lighter magit-auto-revert-mode-lighter
  :global t
  :init-value t)

(defcustom magit-merge-warn-dirty-worktree t
  "Whether to issue a warning when attempting to start a merge in a dirty worktree."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type 'boolean)

(defcustom magit-push-hook '(magit-push-dwim)
  "Hook run by `magit-push' to actually do the work.
See `magit-push' and `magit-push-dwim' for more information."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type 'hook)

(defcustom magit-set-upstream-on-push nil
  "Whether `magit-push' may set upstream when pushing a branch.
This only applies if the branch does not have an upstream set yet.

nil        don't use --set-upstream.
t          ask if --set-upstream should be used.
`dontask'  always use --set-upstream.
`refuse'   refuse to push unless a remote branch has already been set."
  :group 'magit-modes
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Ask" t)
                 (const :tag "Ask if not set" askifnotset)
                 (const :tag "Refuse" refuse)
                 (const :tag "Always" dontask)))

(defcustom magit-stash-snapshot-message-format
  "Snapshot taken at %Y-%m-%d %H:%M:%S"
  "Format for messages of snapshot stashes.
`format-time-string' to create the message from this format."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type 'string)

(defcustom magit-branch-manager-sections-hook
  '(magit-insert-branch-description
    magit-insert-local-branches
    magit-insert-remote-branches
    magit-insert-tags)
  "Hook run to insert sections into the branch manager buffer."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type 'hook)

(defcustom magit-wazzup-sections-hook
  '(magit-insert-wazzup-branches)
  "Hook run to insert sections into the wazzup buffer."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type 'hook)

(defcustom magit-cherry-sections-hook
  '(magit-insert-cherry-head-line
    magit-insert-cherry-upstream-line
    magit-insert-cherry-help-lines
    magit-insert-empty-line
    magit-insert-cherry-commits)
  "Hook run to insert sections into the cherry buffer."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type 'hook)

(defcustom magit-wip-commit-message "autosave %r"
  "Commit message for git-wip commits.

The following `format'-like specs are supported:
%r the relative filename of the file being saved, and
%a the absolute filename of the file being saved,
%t the absolute filename of the repository toplevel."
  :group 'magit-wip
  :type 'string)

(defcustom magit-wip-save-message "Wrote %a (wip)"
  "Message shown after updating a work-in-progress ref.

The following `format'-like specs are supported:
%r the relative filename of the file being saved, and
%a the absolute filename of the file being saved,
%t the absolute filename of the repository toplevel."
  :group 'magit-wip
  :type '(choice (const :tag "No message" nil)
                 (string :tag "Format")))

(defcustom magit-wip-save-mode-lighter " MWip"
  "Lighter for Magit-Wip-Save mode."
  :group 'magit-wip
  :type 'string)

(defcustom magit-wip-ref-format "refs/wip/%b"
  "Format of work-in-progress refs.

The format string has to begin with \"refs/PREFIX/\"
and end with a `format'-like spec, one of:
%b the short branch name, e.g. \"master\", or
%r the full refname, e.g. \"refs/heads/master\".

When `HEAD' is detached then \"HEAD\" is used for both %b and %r.
The use of %r is recommended but %b is used in the default value
for compatibilty with git-wip (https://github.com/bartman/git-wip)."
  :group 'magit-modes
  :type 'string)

;;;; Custom Faces

(defface magit-header
  '((t :inherit header-line))
  "Face for generic header lines.

Many Magit faces inherit from this one by default."
  :group 'magit-faces)

(defface magit-section-title
  '((t :inherit magit-header))
  "Face for section titles."
  :group 'magit-faces)

(defface magit-branch
  '((t :inherit magit-header))
  "Face for branches."
  :group 'magit-faces)

(defface magit-tag
  '((t :inherit magit-header))
  "Face for tags."
  :group 'magit-faces)

(defface magit-diff-file-header
  '((t :bold t))
  "Face for diff file header lines."
  :group 'magit-faces)

(defface magit-diff-hunk-header
  '((t :inherit diff-hunk-header))
  "Face for diff hunk header lines."
  :group 'magit-faces)

(defface magit-diff-add
  '((t :inherit diff-added))
  "Face for lines in a diff that have been added."
  :group 'magit-faces)

(defface magit-diff-del
  '((t :inherit diff-removed))
  "Face for lines in a diff that have been deleted."
  :group 'magit-faces)

(defface magit-diff-none
  '((t :inherit diff-context))
  "Face for lines in a diff that are unchanged."
  :group 'magit-faces)

(defface magit-diff-merge-current
  '((t :inherit font-lock-preprocessor-face))
  "Face for merge conflict marker 'current' line."
  :group 'magit-faces)

(defface magit-diff-merge-separator
  '((t :inherit font-lock-preprocessor-face))
  "Face for merge conflict marker seperator."
  :group 'magit-faces)

(defface magit-diff-merge-diff3-separator
  '((t :inherit font-lock-preprocessor-face))
  "Face for merge conflict marker seperator."
  :group 'magit-faces)

(defface magit-diff-merge-proposed
  '((t :inherit font-lock-preprocessor-face))
  "Face for merge conflict marker 'proposed' line."
  :group 'magit-faces)

(defface magit-log-graph
  '((((class color) (background light))
     :foreground "grey11")
    (((class color) (background dark))
     :foreground "grey80"))
  "Face for the graph element of the log output."
  :group 'magit-faces)

(defface magit-log-sha1
  '((((class color) (background light))
     :foreground "firebrick")
    (((class color) (background dark))
     :foreground "tomato"))
  "Face for the sha1 element of the log output."
  :group 'magit-faces)

(defface magit-log-author
  '((((class color) (background light))
     :foreground "firebrick")
    (((class color) (background dark))
     :foreground "tomato"))
  "Face for the author element of the log output."
  :group 'magit-faces)

(defface magit-log-date
  '((t))
  "Face for the date element of the log output."
  :group 'magit-faces)

(defface magit-log-message
  '((t))
  "Face for the message element of the log output."
  :group 'magit-faces)

(defface magit-cherry-unmatched
  '((t :foreground "cyan"))
  "Face for unmatched cherry commits.")

(defface magit-cherry-equivalent
  '((t :foreground "magenta"))
  "Face for equivalent cherry commits.")

(defface magit-item-highlight
  '((t :inherit secondary-selection))
  "Face for highlighting the current item."
  :group 'magit-faces)

(defface magit-log-head-label-bisect-good
  '((((class color) (background light))
     :box t
     :background "light green"
     :foreground "dark olive green")
    (((class color) (background dark))
     :box t
     :background "light green"
     :foreground "dark olive green"))
  "Face for good bisect refs."
  :group 'magit-faces)

(defface magit-log-head-label-bisect-skip
  '((((class color) (background light))
     :box t
     :background "light goldenrod"
     :foreground "dark goldenrod")
    (((class color) (background dark))
     :box t
     :background "light goldenrod"
     :foreground "dark goldenrod"))
  "Face for skipped bisect refs."
  :group 'magit-faces)

(defface magit-log-head-label-bisect-bad
  '((((class color) (background light))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4")
    (((class color) (background dark))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4"))
  "Face for bad bisect refs."
  :group 'magit-faces)

(defface magit-log-head-label-remote
  '((((class color) (background light))
     :box t
     :background "Grey85"
     :foreground "OliveDrab4")
    (((class color) (background dark))
     :box t
     :background "Grey11"
     :foreground "DarkSeaGreen2"))
  "Face for remote branch head labels shown in log buffer."
  :group 'magit-faces)

(defface magit-log-head-label-tags
  '((((class color) (background light))
     :box t
     :background "LemonChiffon1"
     :foreground "goldenrod4")
    (((class color) (background dark))
     :box t
     :background "LemonChiffon1"
     :foreground "goldenrod4"))
  "Face for tag labels shown in log buffer."
  :group 'magit-faces)

(defface magit-log-head-label-patches
  '((((class color) (background light))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4")
    (((class color) (background dark))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4"))
  "Face for Stacked Git patches."
  :group 'magit-faces)

(defface magit-whitespace-warning-face
  '((t :inherit trailing-whitespace))
  "Face for highlighting whitespace errors in Magit diffs."
  :group 'magit-faces)

(defface magit-log-head-label-local
  '((((class color) (background light))
     :box t
     :background "Grey85"
     :foreground "LightSkyBlue4")
    (((class color) (background dark))
     :box t
     :background "Grey13"
     :foreground "LightSkyBlue1"))
  "Face for local branch head labels shown in log buffer."
  :group 'magit-faces)

(defface magit-log-head-label-head
  '((((class color) (background light))
     :box t
     :background "Grey70"
     :foreground "Black")
    (((class color) (background dark))
     :box t
     :background "Grey20"
     :foreground "White"))
  "Face for working branch head labels shown in log buffer."
  :group 'magit-faces)

(defface magit-log-head-label-default
  '((((class color) (background light))
     :box t
     :background "Grey50")
    (((class color) (background dark))
     :box t
     :background "Grey50"))
  "Face for unknown ref labels shown in log buffer."
  :group 'magit-faces)

(defface magit-log-head-label-wip
  '((((class color) (background light))
     :box t
     :background "Grey95"
     :foreground "LightSkyBlue3")
    (((class color) (background dark))
     :box t
     :background "Grey07"
     :foreground "LightSkyBlue4"))
  "Face for git-wip labels shown in log buffer."
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

(defface magit-signature-none
  '((t :inherit magit-log-message))
  "Face for unsigned commits."
  :group 'magit-faces)

(defface magit-log-reflog-label-commit
  '((((class color) (background light))
     :box t
     :background "LemonChiffon1"
     :foreground "goldenrod4")
    (((class color) (background dark))
     :box t
     :background "LemonChiffon1"
     :foreground "goldenrod4"))
  "Face for reflog subject labels shown in reflog buffer."
  :group 'magit-faces)

(defface magit-log-reflog-label-amend
  '((t :inherit magit-log-reflog-label-commit))
  "Face for reflog subject labels shown in reflog buffer."
  :group 'magit-faces)

(defface magit-log-reflog-label-merge
  '((t :inherit magit-log-reflog-label-commit))
  "Face for reflog subject labels shown in reflog buffer."
  :group 'magit-faces)

(defface magit-log-reflog-label-checkout
  '((((class color) (background light))
     :box t
     :background "Grey85"
     :foreground "LightSkyBlue4")
    (((class color) (background dark))
     :box t
     :background "Grey13"
     :foreground "LightSkyBlue1"))
  "Face for reflog subject labels shown in reflog buffer."
  :group 'magit-faces)

(defface magit-log-reflog-label-reset
  '((((class color) (background light))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4")
    (((class color) (background dark))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4"))
  "Face for reflog subject labels shown in reflog buffer."
  :group 'magit-faces)

(defface magit-log-reflog-label-rebase
  '((((class color) (background light))
     :box t
     :background "Grey85"
     :foreground "OliveDrab4")
    (((class color) (background dark))
     :box t
     :background "Grey11"
     :foreground "DarkSeaGreen2"))
  "Face for reflog subject labels shown in reflog buffer."
  :group 'magit-faces)

(defface magit-log-reflog-label-cherry-pick
  '((((class color) (background light))
     :box t
     :background "light green"
     :foreground "dark olive green")
    (((class color) (background dark))
     :box t
     :background "light green"
     :foreground "dark olive green"))
  "Face for reflog subject labels shown in reflog buffer."
  :group 'magit-faces)

(defface magit-log-reflog-label-remote
  '((((class color) (background light))
     :box t
     :background "Grey50")
    (((class color) (background dark))
     :box t
     :background "Grey50"))
  "Face for reflog subject labels shown in reflog buffer."
  :group 'magit-faces)

(defface magit-log-reflog-label-other
  '((((class color) (background light))
     :box t
     :background "Grey50")
    (((class color) (background dark))
     :box t
     :background "Grey50"))
  "Face for reflog subject labels shown in reflog buffer."
  :group 'magit-faces)

(defface magit-process-ok
  '((t :inherit magit-header
       :foreground "green"))
  "Face for zero exit-status."
  :group 'magit-faces)

(defface magit-process-ng
  '((t :inherit magit-header
       :foreground "red"))
  "Face for non-zero exit-status."
  :group 'magit-faces)

;;;; Keymaps

(when (boundp 'git-commit-mode-map)
  (define-key git-commit-mode-map
    (kbd "C-c C-d") 'magit-diff-while-committing))

(defvar magit-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "^"    'magit-goto-parent-section)
    (define-key map "n"    'magit-goto-next-section)
    (define-key map "p"    'magit-goto-previous-section)
    (define-key map "M-n"  'magit-goto-next-sibling-section)
    (define-key map "M-p"  'magit-goto-previous-sibling-section)
    (define-key map [backtab] 'magit-expand-collapse-section)
    (define-key map "\t"   'magit-toggle-section)
    (define-key map "1"    'magit-show-level-1)
    (define-key map "2"    'magit-show-level-2)
    (define-key map "3"    'magit-show-level-3)
    (define-key map "4"    'magit-show-level-4)
    (define-key map "\M-1" 'magit-show-level-1-all)
    (define-key map "\M-2" 'magit-show-level-2-all)
    (define-key map "\M-3" 'magit-show-level-3-all)
    (define-key map "\M-4" 'magit-show-level-4-all)
    (define-key map "\M-h" 'magit-show-level-1)
    (define-key map "\M-H" 'magit-show-level-1-all)
    (define-key map "\M-s" 'magit-show-level-4)
    (define-key map "\M-S" 'magit-show-level-4-all)
    (define-key map "g" 'magit-refresh)
    (define-key map "G" 'magit-refresh-all)
    (define-key map "q" 'magit-mode-quit-window)
    (define-key map "$" 'magit-process)
    (define-key map "\C-c\C-c" 'magit-dispatch-popup)
    (define-key map "\C-c\C-e" 'magit-dispatch-popup)
    (define-key map "?"        'magit-dispatch-popup)
    (define-key map "b" 'magit-branch-popup)
    (define-key map "B" 'magit-bisect-popup)
    (define-key map "c" 'magit-commit-popup)
    (define-key map "d" 'magit-diff-popup)
    (define-key map "H" 'magit-diff-toggle-refine-hunk)
    (define-key map "+" 'magit-diff-more-context)
    (define-key map "-" 'magit-diff-less-context)
    (define-key map "0" 'magit-diff-default-context)
    (define-key map "e" 'magit-rebase-popup)
    (define-key map "f" 'magit-fetch-popup)
    (define-key map "F" 'magit-pull-popup)
    (define-key map "J" 'magit-am-popup)
    (define-key map "l" 'magit-log-popup)
    (define-key map "m" 'magit-merge-popup)
    (define-key map "M" 'magit-remote-popup)
    (define-key map "o" 'magit-submodule-popup)
    (define-key map "P" 'magit-push-popup)
    (define-key map "r" 'magit-rebase-popup)
    (define-key map "t" 'magit-tag-popup)
    (define-key map "w" 'magit-wazzup)
    (define-key map "\r"       'magit-visit-item)
    (define-key map [  return] 'magit-visit-item)
    (define-key map [C-return] 'magit-dired-jump)
    (define-key map "\s"       'magit-show-item-or-scroll-up)
    (define-key map "\d"       'magit-show-item-or-scroll-down)
    (define-key map "a" 'magit-apply-item)
    (define-key map "A" 'magit-cherry-pick-item)
    (define-key map "S" 'magit-stage-all)
    (define-key map "U" 'magit-unstage-all)
    (define-key map "v" 'magit-revert-item)
    (define-key map "x" 'magit-reset-head)
    (define-key map "X" 'magit-clean)
    (define-key map "y" 'magit-cherry)
    (define-key map "z" 'magit-stash-popup)
    (define-key map ":" 'magit-git-command)
    (define-key map "!" 'magit-run-popup)
    (define-key map "L"      'magit-add-change-log-entry)
    (define-key map "\C-x4a" 'magit-add-change-log-entry-other-window)
    (define-key map "\C-w"   'magit-copy-item-as-kill)
    map)
  "Parent keymap for all keymaps of modes derived from `magit-mode'.")

(defvar magit-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "i" 'magit-ignore-item)
    (define-key map "I" 'magit-ignore-item-locally)
    (define-key map "k" 'magit-discard-item)
    (define-key map "s" 'magit-stage-item)
    (define-key map "u" 'magit-unstage-item)
    (define-key map "C" 'magit-commit-add-log)
    (define-key map "jz" 'magit-jump-to-stashes)
    (define-key map "jn" 'magit-jump-to-untracked)
    (define-key map "ju" 'magit-jump-to-unstaged)
    (define-key map "js" 'magit-jump-to-staged)
    (define-key map "jf" 'magit-jump-to-unpulled)
    (define-key map "jp" 'magit-jump-to-unpushed)
    (define-key map "jr" 'magit-jump-to-pending)
    (define-key map "\M-h" 'magit-show-level-2)
    (define-key map "\M-H" 'magit-show-level-2-all)
    map)
  "Keymap for `magit-status-mode'.")

(defvar magit-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "\C-c\C-d" 'magit-diff-while-committing)
    (define-key map "\C-c\C-b" 'magit-go-backward)
    (define-key map "\C-c\C-f" 'magit-go-forward)
    (define-key map "\s" 'scroll-up)
    (define-key map "\d" 'scroll-down)
    (define-key map "\M-g" 'magit-jump-to-diffstats)
    (define-key map "jd"   'magit-jump-to-diffstats)
    map)
  "Keymap for `magit-diff-mode'.")

(defvar magit-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-diff-mode-map)
    map)
  "Keymap for `magit-commit-mode'.")

(eval-after-load 'dired-x
  '(define-key magit-status-mode-map [remap dired-jump] 'magit-dired-jump))

(defvar magit-log-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "+" 'magit-log-show-more-entries)
    (define-key map "h" 'magit-log-toggle-margin)
    map)
  "Keymap for `magit-log-mode'.")

(defvar magit-log-select-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-log-mode-map)
    (define-key map "."        'magit-log-select-pick)
    (define-key map "\C-c\C-c" 'magit-log-select-pick)
    (define-key map "q"        'magit-log-select-quit)
    (define-key map "\C-c\C-k" 'magit-log-select-quit)
    map)
  "Keymap for `magit-log-select-mode'.")
(put 'magit-log-select-pick :advertised-binding [?\C-c ?\C-c])
(put 'magit-log-select-quit :advertised-binding [?\C-c ?\C-k])

(defvar magit-cherry-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    map)
  "Keymap for `magit-cherry-mode'.")

(defvar magit-reflog-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-log-mode-map)
    map)
  "Keymap for `magit-reflog-mode'.")

(defvar magit-wazzup-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "i" 'magit-ignore-item)
    map)
  "Keymap for `magit-wazzup-mode'.")

(defvar magit-branch-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "c" 'magit-branch-and-checkout)
    (define-key map "a" 'magit-remote-add)
    (define-key map "r" 'magit-rename-item)
    (define-key map "k" 'magit-discard-item)
    map)
  "Keymap for `magit-branch-manager-mode'.")

(defvar magit-process-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    map)
  "Keymap for `magit-process-mode'.")

(easy-menu-define magit-mode-menu magit-mode-map
  "Magit menu"
  '("Magit"
    ["Refresh" magit-refresh t]
    ["Refresh all" magit-refresh-all t]
    "---"
    ["Stage" magit-stage-item t]
    ["Stage all" magit-stage-all t]
    ["Unstage" magit-unstage-item t]
    ["Unstage all" magit-unstage-all t]
    ["Commit" magit-commit-popup t]
    ["Add log entry" magit-commit-add-log t]
    ["Tag" magit-tag t]
    "---"
    ["Diff working tree" magit-diff-working-tree t]
    ["Diff" magit-diff t]
    ("Log"
     ["Oneline Log" magit-log t]
     ["Verbose Log" magit-log-verbose t]
     ["Reflog" magit-reflog t]
     ["Extended..." magit-log-popup t])
    "---"
    ["Cherry pick" magit-cherry-pick-item t]
    ["Apply" magit-apply-item t]
    ["Revert" magit-revert-item t]
    "---"
    ["Ignore" magit-ignore-item t]
    ["Ignore locally" magit-ignore-item-locally t]
    ["Discard" magit-discard-item t]
    ["Reset head" magit-reset-head t]
    ["Clean working tree" magit-clean t]
    ["Stash" magit-stash t]
    ["Snapshot" magit-stash-snapshot t]
    "---"
    ["Branch..." magit-checkout t]
    ["Merge" magit-merge t]
    ["Interactive resolve" magit-interactive-resolve t]
    ["Rebase..." magit-rebase-popup t]
    "---"
    ["Push" magit-push t]
    ["Pull" magit-pull t]
    ["Remote update" magit-remote-update t]
    ("Submodule"
     ["Submodule update" magit-submodule-update t]
     ["Submodule update and init" magit-submodule-update-init t]
     ["Submodule init" magit-submodule-init t]
     ["Submodule sync" magit-submodule-sync t])
    "---"
    ("Extensions")
    "---"
    ["Display Git output" magit-process t]
    ["Quit Magit" magit-mode-quit-window t]))

(magit-define-popup magit-dispatch-popup
  "Popup console for dispatching other popups."
  'magit-popups
  :actions '((?b "Branching"       magit-branch-popup)
             (?B "Bisecting"       magit-bisect-popup)
             (?c "Committing"      magit-commit-popup)
             (?d "Diffing"         magit-diff-popup)
             (?f "Fetching"        magit-fetch-popup)
             (?F "Pulling"         magit-pull-popup)
             (?g "Refresh Buffers" magit-refresh-all)
             (?l "Logging"         magit-log-popup)
             (?m "Merging"         magit-merge-popup)
             (?M "Remoting"        magit-remote-popup)
             (?P "Pushing"         magit-push-popup)
             (?o "Submoduling"     magit-submodule-popup)
             (?r "Rebasing"        magit-rebase-popup)
             (?s "Show Status"     magit-status)
             (?S "Stage all"       magit-stage-all)
             (?t "Tagging"         magit-tag-popup)
             (?U "Unstage all"     magit-unstage-all)
             (?v "Show Commit"     magit-show-commit)
             (?V "Show File"       magit-show)
             (?w "Wazzup"          magit-wazzup)
             (?y "Cherry"          magit-cherry)
             (?z "Stashing"        magit-stash-popup)
             (?! "Running"         magit-run-popup)
             (?$ "Show Process"    magit-display-process)))

;;; Utilities
;;;; Various Utilities

(defmacro magit-bind-match-strings (varlist string &rest body)
  (declare (indent 2))
  (let ((s (cl-gensym "string"))
        (i 0))
    `(let ((,s ,string))
       (let ,(save-match-data
               (mapcar (lambda (var)
                         (list var (list 'match-string (cl-incf i) s)))
                       varlist))
         ,@body))))

(defmacro magit-read-char-case (prompt abort &rest clauses)
  (declare (indent 2))
  (let ((ng (cl-gensym "ng-"))
        (p0 (cl-gensym "p0-"))
        (p1 (cl-gensym "p1-"))
        (p2 (cl-gensym "p2-")))
    `(let* ((,ng 0)
            (,p0 ,prompt)
            (,p1 (concat ,p0 (mapconcat 'cadr ',clauses ", ")))
            (,p2 (concat (unless ,p0 "Choose one of ") ,p1
                         (and ,abort ", or [C-g] to abort")))
            (cursor-in-echo-area t))
       (catch 'choice
         (while (< ,ng 5) ; prevent user panic
           (cl-case (read-event (concat (if (> ,ng 0) ,p2 ,p1) " "))
             ,@(--map `(,(car it) (throw 'choice (progn ,@(cddr it))))
                      clauses)
             (t (ding) (cl-incf ,ng))))))))

(defun magit-file-line (file)
  "Return the first line of FILE as a string."
  (when (file-regular-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-substring-no-properties (point-min)
                                      (line-end-position)))))

(defun magit-file-lines (file &optional keep-empty-lines)
  "Return a list of strings containing one element per line in FILE.
Unless optional argument KEEP-EMPTY-LINES is t, trim all empty lines."
  (when (file-regular-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (split-string (buffer-string) "\n" (not keep-empty-lines)))))

(defvar-local magit-file-name ()
  "Name of file the buffer shows a different version of.")

(defun magit-buffer-file-name (&optional relative)
  (let ((topdir (magit-get-top-dir)))
    (--when-let (or buffer-file-name
                    (-when-let (base (buffer-base-buffer))
                      (with-current-buffer base
                        buffer-file-name))
                    (-when-let (name magit-file-name)
                      (expand-file-name name topdir)))
      (if relative
          (file-relative-name (file-truename it) topdir)
        (file-truename it)))))

(defun magit-commit-log-buffer ()
  (cl-find-if (lambda (buf)
                (equal (magit-get-top-dir)
                       (with-current-buffer buf
                         (and git-commit-mode (magit-get-top-dir)))))
              (append (buffer-list (selected-frame))
                      (buffer-list))))

(defun magit-format-duration (duration spec width)
  (cl-destructuring-bind (char unit units weight)
      (car spec)
    (let ((cnt (round (/ duration weight 1.0))))
      (if (or (not (cdr spec))
              (>= (/ duration weight) 1))
          (if (= width 1)
              (format "%3i%c" cnt char)
            (format (format "%%3i %%-%is" width) cnt
                    (if (= cnt 1) unit units)))
        (magit-format-duration duration (cdr spec) width)))))

(defun magit-flatten-onelevel (list)
  (cl-mapcan (lambda (elt)
               (cond ((consp elt) (copy-sequence elt))
                     (elt (list elt))))
             list))

(defun magit-insert (string &optional face &rest args)
  (if magit-use-overlays
      (if face
          (let ((start (point)))
            (insert string)
            (let ((ov (make-overlay start (point) nil t)))
              (overlay-put ov 'face face)
              (overlay-put ov 'evaporate t)))
        (let ((buf (current-buffer))
              (offset (1- (point))))
          (with-temp-buffer
            (save-excursion (insert string))
            (while (not (eobp))
              (let* ((beg (point))
                     (end (or (next-single-property-change beg 'face)
                              (point-max)))
                     (face (get-text-property beg 'face))
                     (text (buffer-substring-no-properties beg end)))
                (with-current-buffer buf
                  (insert text)
                  (when face
                    (let ((ov (make-overlay (+ beg offset)
                                            (+ end offset) nil t)))
                      (overlay-put ov 'face face)
                      (overlay-put ov 'evaporate t))))
                (goto-char end))))))
    (insert (propertize string 'face face)))
  (apply #'insert args))

(defun magit-put-face-property (start end face)
  (if magit-use-overlays
      (let ((ov (make-overlay start end nil t)))
        (overlay-put ov 'face face)
        ;; (overlay-put ov 'priority 10)
        (overlay-put ov 'evaporate t))
    (put-text-property start end 'face face)))

(defun magit-load-config-extensions ()
  "Load Magit extensions that are defined at the Git config layer."
  (dolist (ext (magit-get-all "magit.extension"))
    (let ((sym (intern (format "magit-%s-mode" ext))))
      (when (and (fboundp sym)
                 (not (eq sym 'magit-wip-save-mode)))
        (funcall sym 1)))))

;;;; Buffer Margins

(defun magit-set-buffer-margin (width enable)
  (-when-let (window (get-buffer-window))
    (with-selected-window window
      (set-window-margins nil (car (window-margins)) (if enable width 0))
      (let ((fn (apply-partially
                 (lambda (width)
                   (-when-let (window (get-buffer-window))
                     (with-selected-window window
                       (set-window-margins nil (car (window-margins))
                                           width))))
                 width)))
        (if enable
            (add-hook  'window-configuration-change-hook fn nil t)
          (remove-hook 'window-configuration-change-hook fn t))))))

(defun magit-make-margin-overlay (&rest strings)
  (let ((o (make-overlay (point) (line-end-position) nil t)))
    (overlay-put o 'evaporate t)
    (overlay-put o 'before-string
                 (propertize "o" 'display
                             (list '(margin right-margin)
                                   (apply #'concat strings))))))

(defvar-local magit-log-margin-timeunit-width nil)

(defun magit-log-margin-set-timeunit-width ()
  (cl-destructuring-bind (width characterp duration-spec)
      magit-log-margin-spec
    (setq magit-log-margin-timeunit-width
          (if characterp
              1
            (apply 'max (mapcar (lambda (e)
                                  (max (length (nth 1 e))
                                       (length (nth 2 e))))
                                (symbol-value duration-spec)))))))

;;; Magit Api
;;;; Section Api
;;;;; Section Core

(cl-defstruct magit-section
  type info
  beginning content-beginning end
  hidden washer
  diff-status diff-file2
  process
  parent children)

(defvar-local magit-root-section nil
  "The root section in the current buffer.
All other sections are descendants of this section.  The value
of this variable is set by `magit-with-section' and you should
never modify it.")
(put 'magit-root-section 'permanent-local t)

;;;;; Section Creation

(defvar magit-with-section--parent nil
  "For use by `magit-with-section' only.")

(defvar magit-with-section--oldroot nil
  "For use by `magit-with-section' only.")

(defmacro magit-with-section (arglist &rest body)
  "\n\n(fn (NAME TYPE &optional INFO HEADING COLLAPSE) &rest ARGS)"
  (declare (indent 1) (debug ((form form &optional form form form) body)))
  (let ((s (car arglist)))
    `(let ((,s (make-magit-section
                :type ',(nth 1 arglist)
                :info  ,(nth 2 arglist)
                :beginning (point-marker)
                :content-beginning (point-marker)
                :parent magit-with-section--parent)))
       (setf (magit-section-hidden ,s)
             (-if-let (old (and magit-with-section--oldroot
                                (magit-find-section
                                 (magit-section-path ,s)
                                 magit-with-section--oldroot)))
                 (magit-section-hidden old)
               ,(nth 4 arglist)))
       (let ((magit-with-section--parent ,s)
             (magit-with-section--oldroot
              (or magit-with-section--oldroot
                  (unless magit-with-section--parent
                    (prog1 magit-root-section
                      (setq magit-root-section ,s))))))
         ,@body)
       (when ,s
         (set-marker-insertion-type (magit-section-content-beginning ,s) t)
         (-when-let (heading ,(nth 3 arglist))
           (save-excursion
             (goto-char (magit-section-beginning ,s))
             (insert
              (if (string-match-p "\n$" heading)
                  (substring heading 0 -1)
                (propertize
                 (let (c)
                   (if (and magit-show-child-count
                            (string-match-p ":$" heading)
                            (> (setq c (length (magit-section-children ,s))) 0))
                       (format "%s (%s):" (substring heading 0 -1) c)
                     heading))
                 'face 'magit-section-title)))
             (insert "\n")))
         (set-marker-insertion-type (magit-section-beginning ,s) t)
         (goto-char (max (point) ; smaller if there is no content
                         (magit-section-content-beginning ,s)))
         (setf (magit-section-end ,s) (point-marker))
         (save-excursion
           (goto-char (magit-section-beginning ,s))
           (let ((end (magit-section-end ,s)))
             (while (< (point) end)
               (let ((next (or (next-single-property-change
                                (point) 'magit-section)
                               end)))
                 (unless (get-text-property (point) 'magit-section)
                   (put-text-property (point) next 'magit-section ,s))
                 (goto-char next)))))
         (if (eq ,s magit-root-section)
             (magit-section-set-hidden magit-root-section nil)
           (setf (magit-section-children (magit-section-parent ,s))
                 (nconc (magit-section-children (magit-section-parent ,s))
                        (list ,s)))))
       ,s)))

(defmacro magit-cmd-insert-section (arglist washer program &rest args)
  "\n\n(fn (TYPE &optional HEADING) WASHER PROGRAM &rest ARGS)"
  (declare (indent 2))
  `(magit-with-section (section ,(car arglist)
                                ',(car arglist)
                                ,(cadr arglist))
     (apply #'process-file ,program nil (list t nil) nil
            (magit-flatten-onelevel (list ,@args)))
     (unless (eq (char-before) ?\n)
       (insert "\n"))
     (save-restriction
       (narrow-to-region (magit-section-content-beginning section) (point))
       (goto-char (point-min))
       (funcall ,washer)
       (goto-char (point-max)))
     (let ((parent   (magit-section-parent section))
           (head-beg (magit-section-beginning section))
           (body-beg (magit-section-content-beginning section)))
       (if (= (point) body-beg)
           (if (not parent)
               (insert "(empty)\n")
             (delete-region head-beg body-beg)
             (setq section nil))
         (insert "\n")))))

(defmacro magit-git-insert-section (arglist washer &rest args)
  "\n\n(fn (TYPE &optional HEADING) WASHER &rest ARGS)"
  (declare (indent 2))
  `(magit-cmd-insert-section ,arglist
       ,washer
     magit-git-executable
     magit-git-standard-options ,@args))

(defmacro magit-insert-line-section (arglist line)
  "\n\n(fn (TYPE &optional INFO) line)"
  (declare (indent 1))
  (let ((l (cl-gensym "line")))
    `(let ((,l (concat ,line "\n")))
       (when (string-match "^\\([^:]+\\):\\( \\)" ,l)
         (setq ,l (replace-match
                   (make-string (max 1 (- magit-status-line-align-to
                                          (length (match-string 1 ,l))))
                                ?\s)
                   t t ,l 2)))
       (magit-with-section (section ,(car arglist) ',(car arglist) ,l)
         (setf (magit-section-info section) ,(cadr arglist))))))

;;;;; Section Searching

(defun magit-find-section (path parent)
  "Find the section at the path PATH in subsection of section parent."
  (if (null path)
      parent
    (--when-let (car (--drop-while
                      (not (equal (magit-section-info it) (car path)))
                      (magit-section-children parent)))
      (magit-find-section (cdr path) it))))

(defun magit-section-path (section)
  "Return the path of SECTION."
  (-when-let (parent (magit-section-parent section))
    (append (magit-section-path parent)
            (list (magit-section-info section)))))

(defun magit-find-section-after (pos)
  "Find the first section that begins after POS."
  (magit-find-section-after* pos (list magit-root-section)))

(defun magit-find-section-after* (pos secs)
  "Find the first section that begins after POS in the list SECS
\(including children of sections in SECS)."
  (while (and secs
              (<= (magit-section-beginning (car secs)) pos))
    (setq secs (if (magit-section-hidden (car secs))
                   (cdr secs)
                 (append (magit-section-children (car secs))
                         (cdr secs)))))
  (car secs))

(defun magit-find-section-before (pos)
  "Return the last section that begins before POS."
  (let ((section (magit-find-section-at pos)))
    (cl-do* ((current (or (magit-section-parent section)
                          section)
                      next)
             (next (unless (magit-section-hidden current)
                     (magit-find-section-before*
                      pos (magit-section-children current)))
                   (unless (magit-section-hidden current)
                     (magit-find-section-before*
                      pos (magit-section-children current)))))
        ((null next) current))))

(defun magit-find-section-before* (pos secs)
  "Find the last section that begins before POS in the list SECS."
  (let ((prev nil))
    (while (and secs
                (< (magit-section-beginning (car secs)) pos))
      (setq prev (car secs))
      (setq secs (cdr secs)))
    prev))

(defun magit-current-section ()
  "Return the Magit section at point."
  (magit-find-section-at (point)))

(defun magit-find-section-at (pos)
  "Return the Magit section at POS."
  (or (get-text-property pos 'magit-section)
      magit-root-section))

;;;;; Section Jumping

(defun magit-goto-next-section ()
  "Go to the next section."
  (interactive)
  (--if-let (magit-find-section-after (point))
      (magit-goto-section it)
    (message "No next section")))

(defun magit-goto-previous-section ()
  "Go to the previous section."
  (interactive)
  (if (eq (point) 1)
      (message "No previous section")
    (magit-goto-section (magit-find-section-before (point)))))

(defun magit-goto-parent-section ()
  "Go to the parent section."
  (interactive)
  (--when-let (magit-section-parent (magit-current-section))
    (goto-char (magit-section-beginning it))))

(defun magit-goto-next-sibling-section ()
  "Go to the next sibling section."
  (interactive)
  (let* ((section (magit-current-section))
         (parent  (magit-section-parent section)))
    (--if-let (and parent (magit-find-section-after*
                           (1- (magit-section-end section))
                           (magit-section-children parent)))
        (magit-goto-section it)
      (magit-goto-next-section))))

(defun magit-goto-previous-sibling-section ()
  "Go to the previous sibling section."
  (interactive)
  (let* ((section (magit-current-section))
         (parent  (magit-section-parent section)))
    (--if-let (and parent (magit-find-section-before*
                           (magit-section-beginning section)
                           (magit-section-children parent)))
        (magit-goto-section it)
      (magit-goto-previous-section))))

(defun magit-goto-section (section)
  (goto-char (magit-section-beginning section))
  (magit-log-maybe-show-commit section)
  (magit-log-maybe-show-more-entries section))

(defun magit-goto-section-at-path (path)
  "Go to the section described by PATH."
  (--if-let (magit-find-section path magit-root-section)
      (goto-char (magit-section-beginning it))
    (message "No such section")))

(defmacro magit-define-section-jumper (sym title)
  "Define an interactive function to go to section SYM.
TITLE is the displayed title of the section."
  (let ((fun (intern (format "magit-jump-to-%s" sym))))
    `(progn
       (defun ,fun (&optional expand) ,(format "\
Jump to section '%s'.
With a prefix argument also expand it." title)
         (interactive "P")
         (if (magit-goto-section-at-path '(,sym))
             (when expand
               (with-local-quit (magit-expand-section))
               (recenter 0))
           (message ,(format "Section '%s' wasn't found" title))))
       (put ',fun 'definition-name ',sym))))

(magit-define-section-jumper stashes   "Stashes")
(magit-define-section-jumper untracked "Untracked files")
(magit-define-section-jumper unstaged  "Unstaged changes")
(magit-define-section-jumper staged    "Staged changes")
(magit-define-section-jumper unpulled  "Unpulled commits")
(magit-define-section-jumper unpushed  "Unpushed commits")
(magit-define-section-jumper pending   "Pending commits")
(magit-define-section-jumper diffstats "Diffstats")

;;;;; Section Hooks

(defun magit-add-section-hook (hook function &optional at append local)
  "Add to the value of section hook HOOK the function FUNCTION.

Add FUNCTION at the beginning of the hook list unless optional
APPEND is non-nil, in which case FUNCTION is added at the end.
If FUNCTION already is a member then move it to the new location.

If optional AT is non-nil and a member of the hook list, then add
FUNCTION next to that instead.  Add before or after AT depending
on APPEND.  If only FUNCTION is a member of the list, then leave
it where ever it already is.

If optional LOCAL is non-nil, then modify the hook's buffer-local
value rather than its global value.  This makes the hook local by
copying the default value.  That copy is then modified.

HOOK should be a symbol.  If HOOK is void, it is first set to nil.
HOOK's value must not be a single hook function.  FUNCTION should
be a function that takes no arguments and inserts one or multiple
sections at point, moving point forward.  FUNCTION may choose not
to insert its section(s), when doing so would not make sense.  It
should not be abused for other side-effects.  To remove FUNCTION
again use `remove-hook'."
  (or (boundp hook) (set hook nil))
  (or (default-boundp hook) (set-default hook nil))
  (let ((value (if local
                   (if (local-variable-p hook)
                       (symbol-value hook)
                     (unless (local-variable-if-set-p hook)
                       (make-local-variable hook))
                     (copy-sequence (default-value hook)))
                 (default-value hook))))
    (if at
        (when (setq at (member at value))
          (setq value (delq function value))
          (if append
              (push function (cdr at))
            (push (car at) (cdr at))
            (setcar at function)))
      (setq value (delq function value)))
    (unless (member function value)
      (setq value (if append
                      (append value (list function))
                    (cons function value))))
    (if local
        (set hook value)
      (set-default hook value))))

;;;;; Section Utilities

(defun magit-map-sections (function section)
  "Apply FUNCTION to SECTION and recursively its subsections."
  (funcall function section)
  (mapc (apply-partially 'magit-map-sections function)
        (magit-section-children section)))

(defun magit-wash-sequence (function)
  "Repeatedly call FUNCTION until it returns nil or eob is reached.
FUNCTION has to move point forward or return nil."
  (while (and (not (eobp)) (funcall function))))

(defun magit-section-parent-info (section)
  (setq section (magit-section-parent section))
  (when section (magit-section-info   section)))

(defun magit-section-lineage (section)
  (when section
    (cons section (magit-section-lineage (magit-section-parent section)))))

(defun magit-section-siblings (section &optional direction)
  (-when-let (parent (magit-section-parent section))
    (let ((siblings  (magit-section-children parent)))
      (cl-ecase direction
        (prev (member section (reverse siblings)))
        (next (member section siblings))
        (nil  siblings)))))

(defun magit-section-region-siblings (&optional key)
  (let ((beg (magit-find-section-at (region-beginning)))
        (end (magit-find-section-at (region-end))))
    (if (eq beg end)
        (list (if key (funcall key beg) beg))
      (goto-char (region-end))
      (when (bolp)
        (setq end (magit-find-section-at (1- (point)))))
      (while (> (length (magit-section-path beg))
                (length (magit-section-path end)))
        (setq beg (magit-section-parent beg)))
      (while (> (length (magit-section-path end))
                (length (magit-section-path beg)))
        (setq end (magit-section-parent end)))
      (let* ((parent   (magit-section-parent beg))
             (siblings (magit-section-children parent)))
        (if (eq parent (magit-section-parent end))
            (mapcar (or key #'identity)
                    (cl-intersection (memq beg siblings)
                                     (memq end (reverse siblings))))
          (user-error "Ambitious cross-section region"))))))

(defun magit-diff-section-for-diffstat (section)
  (let ((file (magit-section-info section)))
    (cl-find-if (lambda (s)
                  (and (eq (magit-section-type s) 'diff)
                       (string-equal (magit-section-info s) file)))
                (magit-section-children magit-root-section))))

;;;;; Section Visibility

(defun magit-section-set-hidden (section hidden)
  "Hide SECTION if HIDDEN is not nil, show it otherwise."
  (setf (magit-section-hidden section) hidden)
  (let ((washer (magit-section-washer section)))
    (when (and washer (not hidden))
      (let ((magit-with-section--parent section)
            (inhibit-read-only t))
        (save-excursion
          (goto-char (magit-section-end section))
          (setf (magit-section-content-beginning section) (point-marker))
          (funcall washer)
          (setf (magit-section-end section) (point-marker))))
      (setf (magit-section-washer section) nil)))
  (let ((inhibit-read-only t)
        (beg (save-excursion
               (goto-char (magit-section-beginning section))
               (forward-line)
               (point)))
        (end (magit-section-end section)))
    (when (< beg end)
      (put-text-property beg end 'invisible hidden)))
  (unless hidden
    (dolist (c (magit-section-children section))
      (magit-section-set-hidden c (magit-section-hidden c)))))

(defun magit-section-any-hidden (section)
  "Return true if SECTION or any of its children is hidden."
  (or (magit-section-hidden section)
      (-first 'magit-section-any-hidden
              (magit-section-children section))))

(defun magit-section-collapse (section)
  "Show SECTION and hide all its children."
  (dolist (c (magit-section-children section))
    (setf (magit-section-hidden c) t))
  (magit-section-set-hidden section nil))

(defun magit-section-expand (section)
  "Show SECTION and all its children."
  (dolist (c (magit-section-children section))
    (setf (magit-section-hidden c) nil))
  (magit-section-set-hidden section nil))

(defun magit-section-expand-all-aux (section)
  "Show recursively all SECTION's children."
  (dolist (c (magit-section-children section))
    (setf (magit-section-hidden c) nil)
    (magit-section-expand-all-aux c)))

(defun magit-section-expand-all (section)
  "Show SECTION and all its children."
  (magit-section-expand-all-aux section)
  (magit-section-set-hidden section nil))

(defun magit-section-hideshow (flag-or-func)
  "Show or hide current section depending on FLAG-OR-FUNC.

If FLAG-OR-FUNC is a function, it will be ran on current section.
IF FLAG-OR-FUNC is a boolean, the section will be hidden if it is
true, shown otherwise."
  (let ((section (magit-current-section)))
    (when (magit-section-parent section)
      (goto-char (magit-section-beginning section))
      (if (functionp flag-or-func)
          (funcall flag-or-func section)
        (magit-section-set-hidden section flag-or-func)))))

(defun magit-show-section ()
  "Show current section."
  (interactive)
  (magit-section-hideshow nil))

(defun magit-hide-section ()
  "Hide current section."
  (interactive)
  (magit-section-hideshow t))

(defun magit-collapse-section ()
  "Hide all subsection of current section."
  (interactive)
  (magit-section-hideshow #'magit-section-collapse))

(defun magit-expand-section ()
  "Show all subsection of current section."
  (interactive)
  (magit-section-hideshow #'magit-section-expand))

(defun magit-toggle-file-section ()
  "Like `magit-toggle-section' but toggle at file granularity."
  (interactive)
  (when (eq (magit-section-type (magit-current-section)) 'hunk)
    (magit-goto-parent-section))
  (magit-toggle-section))

(defun magit-toggle-section ()
  "Toggle hidden status of current section."
  (interactive)
  (magit-section-hideshow
   (lambda (s)
     (magit-section-set-hidden s (not (magit-section-hidden s))))))

(defun magit-expand-collapse-section ()
  "Toggle hidden status of subsections of current section."
  (interactive)
  (magit-section-hideshow
   (lambda (s)
     (cond ((magit-section-any-hidden s)
            (magit-section-expand-all s))
           (t
            (magit-section-collapse s))))))

(defun magit-cycle-section ()
  "Cycle between expanded, hidden and collapsed state for current section.

Hidden: only the first line of the section is shown
Collapsed: only the first line of the subsection is shown
Expanded: everything is shown."
  (interactive)
  (magit-section-hideshow
   (lambda (s)
     (cond ((magit-section-hidden s)
            (magit-section-collapse s))
           ((with-no-warnings
              (cl-notany #'magit-section-hidden (magit-section-children s)))
            (magit-section-set-hidden s t))
           (t
            (magit-section-expand s))))))

(defun magit-show-level-1 ()
  "Show surrounding sections on first level."
  (interactive)
  (magit-show-level 1))

(defun magit-show-level-1-all ()
  "Show all sections on first level."
  (interactive)
  (magit-show-level 1 t))

(defun magit-show-level-2 ()
  "Show surrounding sections up to second level."
  (interactive)
  (magit-show-level 2))

(defun magit-show-level-2-all ()
  "Show all sections up to second level."
  (interactive)
  (magit-show-level 2 t))

(defun magit-show-level-3 ()
  "Show surrounding sections up to third level."
  (interactive)
  (magit-show-level 3))

(defun magit-show-level-3-all ()
  "Show all sections up to third level."
  (interactive)
  (magit-show-level 3 t))

(defun magit-show-level-4 ()
  "Show surrounding sections up to fourth level."
  (interactive)
  (magit-show-level 4))

(defun magit-show-level-4-all ()
  "Show all sections up to fourth level."
  (interactive)
  (magit-show-level 4 t))

(defun magit-show-level (level &optional all)
  (if all
      (magit-show-level* magit-root-section 0 level nil)
    (let ((path (reverse (magit-section-lineage (magit-current-section)))))
      (magit-show-level* (car path) 0 level (cdr path)))))

(defun magit-show-level* (section level threshold path)
  (magit-section-set-hidden section (>= level threshold))
  (when (< level threshold)
    (if path
        (magit-show-level* (car path) (1+ level) threshold (cdr path))
      (dolist (c (magit-section-children section))
        (magit-show-level* c (1+ level) threshold nil)))))

;;;;; Section Highlighting

(defvar-local magit-highlighted-section nil)
(defvar-local magit-highlight-overlay nil)

(defun magit-highlight-section ()
  "Highlight current section.
If its HIGHLIGHT slot is nil, then don't highlight it."
  (let ((section (magit-current-section))
        (refinep (lambda ()
                   (and magit-highlighted-section
                        (eq magit-diff-refine-hunk t)
                        (eq (magit-section-type magit-highlighted-section)
                            'hunk)))))
    (unless (eq section magit-highlighted-section)
      (when (funcall refinep)
        (magit-diff-unrefine-hunk magit-highlighted-section))
      (setq magit-highlighted-section section)
      (unless magit-highlight-overlay
        (overlay-put (setq magit-highlight-overlay (make-overlay 1 1))
                     'face magit-item-highlight-face))
      (cond ((and section (magit-section-parent section))
             (when (funcall refinep)
               (magit-diff-refine-hunk section))
             (move-overlay magit-highlight-overlay
                           (magit-section-beginning section)
                           (magit-section-end section)
                           (current-buffer)))
            (t
             (delete-overlay magit-highlight-overlay))))))

;;;;; Section Actions

(defun magit-section-context-type (section)
  (cons (magit-section-type section)
        (-when-let (parent (magit-section-parent section))
          (magit-section-context-type parent))))

(defun magit-section-match-1 (l1 l2)
  (or (null l1)
      (if (eq (car l1) '*)
          (or (magit-section-match-1 (cdr l1) l2)
              (and l2
                   (magit-section-match-1 l1 (cdr l2))))
        (and l2
             (equal (car l1) (car l2))
             (magit-section-match-1 (cdr l1) (cdr l2))))))

(defun magit-section-match (condition &optional section)
  (unless section
    (setq section (magit-current-section)))
  (cond ((eq condition t) t)
        ((not section)  nil)
        ((listp condition)
         (--first (magit-section-match it section) condition))
        (t
         (magit-section-match-1 (if (symbolp condition)
                                    (list condition)
                                  (append condition nil))
                                (magit-section-context-type section)))))

(defmacro magit-section-case (slots &rest clauses)
  (declare (indent 1))
  `(let* ((it (magit-current-section))
          ,@(mapcar
             (lambda (slot)
               `(,slot
                 (and it (,(intern (format "magit-section-%s" slot)) it))))
             slots))
     (cond ,@(mapcar (lambda (clause)
                       `((magit-section-match ',(car clause) it)
                         ,@(cdr clause)))
                     clauses))))

(defconst magit-section-action-success
  (make-symbol "magit-section-action-success"))

(defmacro magit-section-action (opname slots &rest clauses)
  (declare (indent 2) (debug (sexp &rest (sexp body))))
  (let ((value (cl-gensym "value")))
    `(let ((,value
            (or (run-hook-wrapped
                 ',(intern (format "magit-%s-hook" opname))
                 (lambda (fn section)
                   (when (magit-section-match
                          (or (get fn 'magit-section-action-context)
                              (error "%s undefined for %s"
                                     'magit-section-action-context fn))
                          section)
                     (funcall fn (magit-section-info section))))
                 (magit-current-section))
                (magit-section-case ,slots
                  ,@clauses
                  (t (user-error
                      (if (magit-current-section)
                          ,(format "Cannot %s this section" opname)
                        ,(format "Nothing to %s here" opname))))))))
       (unless (eq ,value magit-section-action-success)
         ,value))))

(defun magit-current-commit ()
  (magit-section-case (info) (commit info)))

;;;; Process Api
;;;;; Process Commands

(defun magit-process ()
  "Display Magit process buffer."
  (interactive)
  (let ((buf (magit-process-buffer)))
    (if (buffer-live-p buf)
        (pop-to-buffer buf)
      (user-error "Process buffer doesn't exist"))))

(defun magit-process-kill ()
  "Kill the process at point."
  (interactive)
  (magit-section-case (info)
    (process (if (eq (process-status info) 'run)
                 (when (yes-or-no-p "Kill this process? ")
                   (kill-process info))
               (user-error "Process isn't running")))))

(defvar magit-git-command-history nil)

(magit-define-popup magit-run-popup
  "Popup console for running raw Git commands."
  'magit-popups
  :actions '((?! "Git Subcommand (from root)" magit-git-command-topdir)
             (?: "Git Subcommand (from pwd)" magit-git-command)
             (?g "Git Gui" magit-run-git-gui)
             (?k "Gitk" magit-run-gitk))
  :default-action 'magit-git-command)

;;;###autoload
(defun magit-git-command (args directory)
  "Execute a Git subcommand asynchronously, displaying the output.
With a prefix argument run Git in the root of the current
repository.  Non-interactively run Git in DIRECTORY with ARGS."
  (interactive (magit-git-command-read-args))
  (require 'eshell)
  (magit-mode-display-buffer (magit-process-buffer nil t)
                             'magit-process-mode 'pop-to-buffer)
  (goto-char (point-max))
  (let ((default-directory directory))
    (magit-run-git-async
     (with-temp-buffer
       (insert args)
       (mapcar 'eval (eshell-parse-arguments (point-min)
                                             (point-max)))))))

(defun magit-git-command-topdir (args directory)
  "Execute a Git subcommand asynchronously, displaying the output.
Run Git in the root of the current repository.
\n(fn)" ; arguments are for internal use
  (interactive (magit-git-command-read-args t))
  (magit-git-command args directory))

(defun magit-git-command-read-args (&optional root)
  (let ((dir (if (or root current-prefix-arg)
                 (or (magit-get-top-dir)
                     (user-error "Not inside a Git repository"))
               default-directory)))
    (list (read-string (format "Git subcommand (in %s): "
                               (abbreviate-file-name dir))
                       nil 'magit-git-command-history)
          dir)))

;;;;; Process Mode

(define-derived-mode magit-process-mode magit-mode "Magit Process"
  "Mode for looking at git process output.")

(defvar magit-process-buffer-name "*magit-process*"
  "Name of buffer where output of processes is put.")

(defun magit-process-buffer (&optional topdir create)
  (or (magit-mode-get-buffer magit-process-buffer-name
                             'magit-process-mode topdir)
      (with-current-buffer (magit-mode-get-buffer-create
                            magit-process-buffer-name
                            'magit-process-mode topdir)
        (magit-process-mode)
        (let* ((inhibit-read-only t)
               (s (magit-with-section (section processbuf)
                    (insert "\n"))))
          (set-marker-insertion-type (magit-section-beginning s) nil)
          (set-marker-insertion-type (magit-section-content-beginning s) nil)
          (current-buffer)))))

;;;;; Synchronous Processes

(defun magit-git-exit-code (&rest args)
  "Execute Git with ARGS, returning its exit code."
  (apply #'process-file magit-git-executable nil nil nil
         (append magit-git-standard-options
                 (magit-flatten-onelevel args))))

(defun magit-git-success (&rest args)
  "Execute Git with ARGS, returning t if its exit code is 0."
  (= (apply #'magit-git-exit-code args) 0))

(defun magit-git-failure (&rest args)
  "Execute Git with ARGS, returning t if its exit code is 1."
  (= (apply #'magit-git-exit-code args) 1))

(defun magit-git-string (&rest args)
  "Execute Git with ARGS, returning the first line of its output.
If there is no output return nil.  If the output begins with a
newline return an empty string."
  (with-temp-buffer
    (apply #'process-file magit-git-executable nil (list t nil) nil
           (append magit-git-standard-options
                   (magit-flatten-onelevel args)))
    (unless (= (point-min) (point-max))
      (goto-char (point-min))
      (buffer-substring-no-properties
       (line-beginning-position)
       (line-end-position)))))

(defun magit-git-true (&rest args)
  "Execute Git with ARGS, returning t if it prints \"true\".
Return t if the first (and usually only) output line is the
string \"true\", otherwise return nil."
  (equal (apply #'magit-git-string args) "true"))

(defun magit-git-false (&rest args)
  "Execute Git with ARGS, returning t if it prints \"false\".
Return t if the first (and usually only) output line is the
string \"false\", otherwise return nil."
  (equal (apply #'magit-git-string args) "false"))

(defun magit-git-insert (&rest args)
  "Execute Git with ARGS, inserting its output at point."
  (apply #'process-file magit-git-executable nil (list t nil) nil
         (append magit-git-standard-options
                 (magit-flatten-onelevel args))))

(defun magit-git-lines (&rest args)
  "Execute Git with ARGS, returning its output as a list of lines.
Empty lines anywhere in the output are omitted."
  (with-temp-buffer
    (apply #'process-file magit-git-executable nil (list t nil) nil
           (append magit-git-standard-options
                   (magit-flatten-onelevel args)))
    (split-string (buffer-string) "\n" 'omit-nulls)))

(defun magit-run-git (&rest args)
  "Call Git synchronously in a separate process, and refresh.

The arguments ARGS specify command line arguments.  The first
level of ARGS is flattened, so each member of ARGS has to be a
string or a list of strings.

Option `magit-git-executable' specifies which Git executable is
used.  The arguments in option `magit-git-standard-options' are
prepended to ARGS.

After Git returns, the current buffer (if it is a Magit buffer)
as well as the current repository's status buffer are refreshed.
Unmodified buffers visiting files that are tracked in the current
repository are reverted if `magit-auto-revert-mode' is active.

Process output goes into a new section in a buffer specified by
variable `magit-process-buffer-name'."
  (apply #'magit-call-git (magit-process-quote-arguments args))
  (magit-refresh))

(defun magit-call-git (&rest args)
  "Call Git synchronously in a separate process.

The arguments ARGS specify command line arguments.  The first
level of ARGS is flattened, so each member of ARGS has to be a
string or a list of strings.

Option `magit-git-executable' specifies which Git executable is
used.  The arguments in option `magit-git-standard-options' are
prepended to ARGS.

Process output goes into a new section in a buffer specified by
variable `magit-process-buffer-name'."
  (apply #'magit-call-process magit-git-executable
         (append magit-git-standard-options args)))

(defun magit-call-process (program &rest args)
  "Call PROGRAM synchronously in a separate process.

The arguments ARGS specify command line arguments.  The first
level of ARGS is flattened, so each member of ARGS has to be a
string or a list of strings.

Process output goes into a new section in a buffer specified by
variable `magit-process-buffer-name'."
  (setq args (magit-flatten-onelevel args))
  (cl-destructuring-bind (process-buf . section)
      (magit-process-setup program args)
    (magit-process-finish
     (let ((inhibit-read-only t))
       (apply #'process-file program nil process-buf nil args))
     process-buf (current-buffer) default-directory section)))

(defun magit-run-git-with-input (input &rest args)
  "Call Git in a separate process.

The first argument, INPUT, has to be a buffer or the name of an
existing buffer.  The content of that buffer is used as the
process' standard input.

The remaining arguments, ARGS, specify command line arguments.
The first level of ARGS is flattened, so each member of ARGS has
to be a string or a list of strings.

Option `magit-git-executable' specifies which Git executable is
used.  The arguments in option `magit-git-standard-options' are
prepended to ARGS.

After Git returns, the current buffer (if it is a Magit buffer)
as well as the current repository's status buffer are refreshed.
Unmodified buffers visiting files that are tracked in the current
repository are reverted if `magit-auto-revert-mode' is active.

This function actually starts a asynchronous process, but it then
waits for that process to return."
  (apply #'magit-start-git input args)
  (magit-process-wait)
  (magit-refresh))

(defun magit-run-git-with-logfile (file &rest args)
  "Call Git in a separate process and log its output.
And log the output to FILE.  This function might have a
short halflive.  See `magit-run-git' for more information."
  (apply #'magit-start-git nil args)
  (process-put magit-this-process 'logfile file)
  (set-process-filter magit-this-process 'magit-process-logfile-filter)
  (magit-process-wait)
  (magit-refresh))

;;;;; Asynchronous Processes

(defvar magit-this-process nil)

(defun magit-run-git-with-editor (&rest args)
  (with-git-editor (apply #'magit-run-git-async args)))

(defun magit-run-git-async (&rest args)
  "Start Git, prepare for refresh, and return the process object.

If optional argument INPUT is non-nil, it has to be a buffer or
the name of an existing buffer.  The content of that buffer is
used as the process' standard input.

The remaining arguments, ARGS, specify command line arguments.
The first level of ARGS is flattened, so each member of ARGS has
to be a string or a list of strings.

Display the command line arguments in the echo area.

After Git returns some buffers are refreshed: the buffer that was
current when `magit-start-process' was called (if it is a Magit
buffer and still alive), as well as the respective Magit status
buffer.  Unmodified buffers visiting files that are tracked in
the current repository are reverted if `magit-auto-revert-mode'
is active.

See `magit-start-process' for more information."
  (message "Running %s %s" magit-git-executable
           (mapconcat 'identity (magit-flatten-onelevel args) " "))
  (apply #'magit-start-git nil args))

(defun magit-start-git (&optional input &rest args)
  "Start Git, prepare for refresh, and return the process object.

If optional argument INPUT is non-nil, it has to be a buffer or
the name of an existing buffer.  The buffer content becomes the
processes standard input.

The remaining arguments, ARGS, specify command line arguments.
The first level of ARGS is flattened, so each member of ARGS has
to be a string or a list of strings.

After Git returns some buffers are refreshed: the buffer that was
current when `magit-start-process' was called (if it is a Magit
buffer and still alive), as well as the respective Magit status
buffer.  Unmodified buffers visiting files that are tracked in
the current repository are reverted if `magit-auto-revert-mode'
is active.

See `magit-start-process' for more information."
  (apply #'magit-start-process magit-git-executable input
         (append magit-git-standard-options
                 (magit-process-quote-arguments args))))

(defun magit-start-process (program &optional input &rest args)
  "Start PROGRAM, prepare for refresh, and return the process object.

If optional argument INPUT is non-nil, it has to be a buffer or
the name of an existing buffer.  The buffer content becomes the
processes standard input.

The remaining arguments, ARGS, specify command line arguments.
The first level of ARGS is flattened, so each member of ARGS has
to be a string or a list of strings.

The process is started using `start-file-process' and then setup
to use the sentinel `magit-process-sentinel' and the filter
`magit-process-filter'.  Information required by these functions
is stored in the process object.  When this function returns the
process has not started to run yet so it is possible to override
the sentinel and filter.

After the process returns, `magit-process-sentinel' refreshes the
buffer that was current when `magit-start-process' was called (if
it is a Magit buffer and still alive), as well as the respective
Magit status buffer.  Unmodified buffers visiting files that are
tracked in the current repository are reverted if
`magit-auto-revert-mode' is active."
  (setq args (magit-flatten-onelevel args))
  (cl-destructuring-bind (process-buf . section)
      (magit-process-setup program args)
    (let* ((process-connection-type
            ;; Don't use a pty, because it would set icrnl
            ;; which would modify the input (issue #20).
            (and (not input) magit-process-connection-type))
           (process (apply 'start-file-process
                           (file-name-nondirectory program)
                           process-buf program args)))
      (set-process-sentinel process #'magit-process-sentinel)
      (set-process-filter   process #'magit-process-filter)
      (set-process-buffer   process process-buf)
      (process-put process 'section section)
      (process-put process 'command-buf (current-buffer))
      (process-put process 'default-dir default-directory)
      (setf (magit-section-process section) process)
      (with-current-buffer process-buf
        (set-marker (process-mark process) (point)))
      (when input
        (with-current-buffer input
          (process-send-region process (point-min) (point-max))
          (process-send-eof    process)))
      (setq magit-this-process process)
      (setf (magit-section-info section) process)
      (magit-process-display-buffer process)
      process)))

;;;;; Process Internals

(defun magit-process-setup (program args)
  (magit-process-set-mode-line program args)
  (let ((buf (magit-process-buffer)))
    (if  buf
        (magit-process-truncate-log buf)
      (setq buf (magit-process-buffer nil t)))
    (with-current-buffer buf
      (goto-char (1- (point-max)))
      (let* ((inhibit-read-only t)
             (magit-with-section--parent magit-root-section)
             ;; Kids, don't do this ^^^^ at home.
             (s (magit-with-section
                    (section process nil
                             (mapconcat 'identity (cons program args) " "))
                  (insert "\n"))))
        (set-marker-insertion-type (magit-section-content-beginning s) nil)
        (unless (get-buffer-window (current-buffer) t)
          (magit-section-set-hidden s t))
        (insert "\n")
        (backward-char 2)
        (cons (current-buffer) s)))))

(defun magit-process-truncate-log (buffer)
  (with-current-buffer buffer
    (let* ((head nil)
           (tail (magit-section-children magit-root-section))
           (count (length tail)))
      (when (> (1+ count) magit-process-log-max)
        (while (and (cdr tail)
                    (> count (/ magit-process-log-max 2)))
          (let* ((inhibit-read-only t)
                 (section (car tail))
                 (process (magit-section-process section)))
            (cond ((not process))
                  ((memq (process-status process) '(exit signal))
                   (delete-region (magit-section-beginning section)
                                  (1+ (magit-section-end section)))
                   (cl-decf count))
                  (t
                   (push section head))))
          (pop tail))
        (setf (magit-section-children magit-root-section)
              (nconc (reverse head) tail))))))

(defun magit-process-sentinel (process event)
  "Default sentinel used by `magit-start-process'."
  (when (memq (process-status process) '(exit signal))
    (setq event (substring event 0 -1))
    (magit-process-unset-mode-line)
    (when (string-match "^finished" event)
      (message (concat (capitalize (process-name process)) " finished")))
    (magit-process-finish process)
    (when (eq process magit-this-process)
      (setq magit-this-process nil))
    (magit-refresh (and (buffer-live-p (process-get process 'command-buf))
                        (process-get process 'command-buf)))))

(defun magit-process-filter (proc string)
  "Default filter used by `magit-start-process'."
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (magit-process-yes-or-no-prompt proc string)
      (magit-process-username-prompt  proc string)
      (magit-process-password-prompt  proc string)
      (goto-char (process-mark proc))
      (setq string (propertize string 'invisible
                               (magit-section-hidden
                                (process-get proc 'section))))
      ;; Find last ^M in string.  If one was found, ignore everything
      ;; before it and delete the current line.
      (let ((ret-pos (length string)))
        (while (and (>= (setq ret-pos (1- ret-pos)) 0)
                    (/= ?\r (aref string ret-pos))))
        (cond ((>= ret-pos 0)
               (goto-char (line-beginning-position))
               (delete-region (point) (line-end-position))
               (insert-and-inherit (substring string (+ ret-pos 1))))
              (t
               (insert-and-inherit string))))
      (set-marker (process-mark proc) (point)))))

(defun magit-process-logfile-filter (process string)
  "Special filter used by `magit-run-git-with-logfile'."
  (magit-process-filter process string)
  (let ((file (process-get process 'logfile)))
    (with-temp-file file
      (when (file-exists-p file)
        (insert-file-contents file)
        (goto-char (point-max)))
      (insert string)
      (write-region (point-min) (point-max) file))))

(defun magit-process-yes-or-no-prompt (proc string)
  "Forward yes-or-no prompts to the user."
  (-when-let (beg (string-match magit-process-yes-or-no-prompt-regexp string))
    (let ((max-mini-window-height 30))
      (process-send-string
       proc
       (downcase
        (concat (match-string (if (yes-or-no-p (substring string 0 beg)) 1 2)
                              string)
                "\n"))))))

(defun magit-process-password-prompt (proc string)
  "Forward password prompts to the user."
  (--when-let (magit-process-match-prompt
               magit-process-password-prompt-regexps string)
    (process-send-string proc (concat (read-passwd it) "\n"))))

(defun magit-process-username-prompt (proc string)
  "Forward username prompts to the user."
  (--when-let (magit-process-match-prompt
               magit-process-username-prompt-regexps string)
    (process-send-string
     proc (concat (read-string it nil nil (user-login-name)) "\n"))))

(defun magit-process-match-prompt (prompts string)
  (when (--any? (string-match it string) prompts)
    (let ((prompt (match-string 0 string)))
      (cond ((string-match ": $" prompt) prompt)
            ((string-match ":$"  prompt) (concat prompt " "))
            (t                           (concat prompt ": "))))))

(defun magit-process-wait ()
  (while (and magit-this-process
              (eq (process-status magit-this-process) 'run))
    (sit-for 0.1 t)))

(defun magit-process-set-mode-line (program args)
  (when (equal program magit-git-executable)
    (setq args (nthcdr (1+ (length magit-git-standard-options)) args)))
  (magit-map-magit-buffers
   (apply-partially (lambda (s)
                      (setq mode-line-process s))
                    (concat " " program
                            (and args (concat " " (car args)))))))

(defun magit-process-unset-mode-line ()
  (magit-map-magit-buffers (lambda () (setq mode-line-process nil))))

(defvar magit-process-error-message-re
  (concat "^\\(?:error\\|fatal\\|git\\): \\(.*\\)" paragraph-separate))

(defun magit-process-finish (arg &optional process-buf command-buf
                                 default-dir section)
  (unless (integerp arg)
    (setq process-buf (process-buffer arg)
          command-buf (process-get arg 'command-buf)
          default-dir (process-get arg 'default-dir)
          section     (process-get arg 'section)
          arg         (process-exit-status arg)))
  (when (featurep 'dired)
    (dired-uncache default-dir))
  (when (buffer-live-p process-buf)
    (with-current-buffer process-buf
      (let ((inhibit-read-only t)
            (mark (magit-section-beginning section)))
        (set-marker-insertion-type mark nil)
        (goto-char mark)
        (insert (propertize (format "%3s " arg)
                            'magit-section section
                            'face (if (= arg 0)
                                      'magit-process-ok
                                    'magit-process-ng))))))
  (unless (= arg 0)
    (message ; `error' would prevent refresh
     "%s ... [%s buffer %s for details]"
     (or (and (buffer-live-p process-buf)
              (with-current-buffer process-buf
                (save-excursion
                  (goto-char (magit-section-end section))
                  (when (re-search-backward
                         magit-process-error-message-re nil
                         (magit-section-content-beginning section))
                    (match-string 1)))))
         "Git failed")
     (-if-let (key (and (buffer-live-p command-buf)
                        (with-current-buffer command-buf
                          (car (where-is-internal
                                'magit-process-display-buffer)))))
         (format "Hit %s to see" (key-description key))
       "See")
     (buffer-name process-buf)))
  arg)

(defun magit-process-display-buffer (process)
  (when (process-live-p process)
    (let ((buf (process-buffer process)))
      (cond ((not (buffer-live-p buf)))
            ((= magit-process-popup-time 0)
             (pop-to-buffer buf))
            ((> magit-process-popup-time 0)
             (run-with-timer magit-process-popup-time nil
                             (lambda (p)
                               (when (eq (process-status p) 'run)
                                 (let ((buf (process-buffer p)))
                                   (when (buffer-live-p buf)
                                     (pop-to-buffer buf)))))
                             process))))))

(defun magit-process-quote-arguments (args)
  "Quote each argument in list ARGS as an argument to Git.
Except when `magit-process-quote-curly-braces' is non-nil ARGS is
returned unchanged.  This is required to works around strangeness
of the Windows \"Powershell\"."
  (if magit-process-quote-curly-braces
      (mapcar (apply-partially 'replace-regexp-in-string
                               "{\\([0-9]+\\)}" "\\\\{\\1\\\\}")
              args)
    args))

(defvar magit-server-visit-args nil)
(defun  magit-server-visit-args (action &optional other-window args)
  (setq magit-server-visit-args
        (list action other-window (magit-get-top-dir)
              (current-window-configuration) args)))

(defun magit-server-visit ()
  (when (or (string-match-p git-commit-filename-regexp buffer-file-name)
            (string-match-p git-rebase-filename-regexp buffer-file-name))
    (let ((type     (nth 0 magit-server-visit-args))
          (otherwin (nth 1 magit-server-visit-args))
          (topdir   (nth 2 magit-server-visit-args))
          (winconf  (nth 3 magit-server-visit-args))
          (args     (nth 4 magit-server-visit-args)))
      (setq-local server-window (if otherwin 'pop-to-buffer 'switch-to-buffer))
      (when (equal (magit-get-top-dir) topdir)
        (setq with-editor-previous-winconf winconf)
        (setq magit-refresh-args args)))))

(add-hook 'server-visit-hook 'magit-server-visit t)

;;;; Mode Api
;;;;; Mode Foundation

(define-derived-mode magit-mode special-mode "Magit"
  "Parent major mode from which Magit major modes inherit.

Please see the manual for a complete description of Magit.

\\{magit-mode-map}"
  (buffer-disable-undo)
  (setq truncate-lines t)
  (add-hook 'pre-command-hook  #'magit-remember-point nil t)
  (add-hook 'post-command-hook #'magit-correct-point-after-command t t)
  (add-hook 'post-command-hook #'magit-highlight-section t t)
  ;; Emacs' normal method of showing trailing whitespace gives weird
  ;; results when `magit-whitespace-warning-face' is different from
  ;; `trailing-whitespace'.
  (when (and magit-highlight-whitespace
             magit-highlight-trailing-whitespace)
    (setq show-trailing-whitespace nil)))

(defvar-local magit-refresh-function nil)
(put 'magit-refresh-function 'permanent-local t)

(defvar-local magit-refresh-args nil)
(put 'magit-refresh-args 'permanent-local t)

(defmacro magit-mode-setup
  (buffer switch-func mode refresh-func &rest refresh-args)
  "Display and select BUFFER, turn on MODE, and refresh a first time.
Display BUFFER using `magit-mode-display-buffer', then turn on
MODE in BUFFER, set the local value of `magit-refresh-function'
to REFRESH-FUNC and that of `magit-refresh-args' to REFRESH-ARGS
and finally \"refresh\" a first time.  All arguments are
evaluated before switching to BUFFER."
  (let ((mode-symb (cl-gensym "mode-symb"))
        (toplevel  (cl-gensym "toplevel"))
        (init-args (cl-gensym "init-args"))
        (buf-symb  (cl-gensym "buf-symb")))
    `(let* ((,mode-symb ,mode)
            (,toplevel  (magit-get-top-dir))
            (,init-args (list ,mode-symb ,refresh-func ,@refresh-args))
            (,buf-symb  (magit-mode-display-buffer
                         ,buffer ,mode-symb ,switch-func)))
       (if ,toplevel
           (with-current-buffer ,buf-symb
             (apply #'magit-mode-init ,toplevel ,init-args))
         (user-error "Not inside a Git repository")))))

(defun magit-mode-init (dir mode refresh-func &rest refresh-args)
  "Turn on MODE and refresh in the current buffer.
Turn on MODE, set the local value of `magit-refresh-function' to
REFRESH-FUNC and that of `magit-refresh-args' to REFRESH-ARGS and
finally \"refresh\" a first time.

Also see `magit-mode-setup', a more convenient variant."
  (cl-case mode
    (magit-commit-mode
     (magit-setup-xref (cons #'magit-show-commit refresh-args))
     (goto-char (point-min)))
    (magit-diff-mode
     (magit-setup-xref (cons #'magit-diff refresh-args))
     (goto-char (point-min))))
  (setq default-directory dir
        magit-refresh-function refresh-func
        magit-refresh-args refresh-args)
  (funcall mode)
  (magit-mode-refresh-buffer))

(defvar magit-inhibit-save-previous-winconf nil)

(defvar-local magit-previous-window-configuration nil)
(put 'magit-previous-window-configuration 'permanent-local t)

(defvar-local magit-previous-section nil)
(put 'magit-previous-section 'permanent-local t)

(defun magit-mode-display-buffer (buffer mode &optional switch-function)
  "Display BUFFER in some window and select it.
BUFFER may be a buffer or a string, the name of a buffer.  Return
the buffer.

Unless BUFFER is already displayed in the selected frame store the
previous window configuration as a buffer local value, so that it
can later be restored by `magit-mode-quit-window'.

Then display and select BUFFER using SWITCH-FUNCTION.  If that is
nil either use `pop-to-buffer' if the current buffer's major mode
derives from Magit mode; or else use `switch-to-buffer'.

This is only intended for buffers whose major modes derive from
Magit mode."
  (cond ((stringp buffer)
         (setq buffer (magit-mode-get-buffer-create buffer mode)))
        ((not (bufferp buffer))
         (signal 'wrong-type-argument (list 'bufferp nil))))
  (let ((section (magit-current-section)))
    (with-current-buffer (get-buffer-create buffer)
      (setq magit-previous-section section)
      (unless (or (get-buffer-window buffer (selected-frame))
                  magit-inhibit-save-previous-winconf)
        (setq magit-previous-window-configuration
              (current-window-configuration)))))
  (funcall (or switch-function
               (if (derived-mode-p 'magit-mode)
                   'switch-to-buffer
                 'pop-to-buffer))
           buffer)
  buffer)

(defun magit-mode-get-buffer (format mode &optional topdir create)
  (if (not (string-match-p "%[Tt]" format))
      (funcall (if create #'get-buffer-create #'get-buffer) format)
    (unless topdir
      (setq topdir (magit-get-top-dir)))
    (let ((name (format-spec
                 format `((?T . ,topdir)
                          (?t . ,(file-name-nondirectory
                                  (directory-file-name topdir)))))))
      (or (--first (with-current-buffer it
                     (and (or (not mode) (eq major-mode mode))
                          (equal (expand-file-name default-directory) topdir)
                          (string-match-p (format "^%s\\(?:<[0-9]+>\\)?$"
                                                  (regexp-quote name))
                                          (buffer-name))))
                   (buffer-list))
          (and create (generate-new-buffer name))))))

(defun magit-mode-get-buffer-create (format mode &optional topdir)
  (magit-mode-get-buffer format mode topdir t))

(cl-defun magit-mode-refresh-buffer (&optional (buffer (current-buffer)))
  (with-current-buffer buffer
    (when magit-refresh-function
      (let* ((old-line (line-number-at-pos))
             (old-point (point))
             (old-window (selected-window))
             (old-window-start (window-start))
             (old-section (magit-current-section))
             (old-path (and old-section
                            (magit-section-path (magit-current-section)))))
        (beginning-of-line)
        (let ((inhibit-read-only t)
              (section-line (and old-section
                                 (count-lines
                                  (magit-section-beginning old-section)
                                  (point))))
              (line-char (- old-point (point))))
          (erase-buffer)
          (apply magit-refresh-function
                 magit-refresh-args)
          (--if-let (and old-path
                         (magit-find-section old-path magit-root-section))
              (progn (goto-char (magit-section-beginning it))
                     (forward-line section-line)
                     (forward-char line-char))
            (save-restriction
              (widen)
              (goto-char (point-min))
              (forward-line (1- old-line)))))
        (when (fboundp 'unrecord-window-buffer)
          (unrecord-window-buffer old-window buffer))
        (dolist (w (get-buffer-window-list buffer nil t))
          (set-window-point w (point))
          (set-window-start w old-window-start t))
        (magit-highlight-section)
        (run-hooks 'magit-mode-refresh-buffer-hook)))))

(defun magit-mode-quit-window (&optional kill-buffer)
  "Bury the current buffer and delete its window.
With a prefix argument, kill the buffer instead.

If `magit-restore-window-configuration' is non-nil and the last
configuration stored by `magit-mode-display-buffer' originates
from the selected frame then restore it after burrying/killing
the buffer.  Finally reset the window configuration to nil."
  (interactive "P")
  (let ((winconf magit-previous-window-configuration)
        (buffer (current-buffer))
        (frame (selected-frame)))
    (quit-window kill-buffer (selected-window))
    (when winconf
      (when (and magit-restore-window-configuration
                 (equal frame (window-configuration-frame winconf)))
        (set-window-configuration winconf)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq magit-previous-window-configuration nil)))))
    (run-hook-with-args 'magit-mode-quit-window-hook buffer)))

;;;;; Mode Utilities

(defun magit-map-magit-buffers (func &optional dir)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (derived-mode-p 'magit-mode)
                 (or (null dir)
                     (equal default-directory dir)))
        (funcall func)))))

;;;;; (section kludges)

(defvar-local magit-last-point nil)
(put 'magit-last-point 'permanent-local t)

(defun magit-remember-point ()
  (setq magit-last-point (point)))

(defun magit-invisible-region-end (pos)
  (while (and (not (= pos (point-max))) (invisible-p pos))
    (setq pos (next-char-property-change pos)))
  pos)

(defun magit-invisible-region-start (pos)
  (while (and (not (= pos (point-min))) (invisible-p pos))
    (setq pos (1- (previous-char-property-change pos))))
  pos)

(defun magit-correct-point-after-command ()
  "Move point outside of invisible regions.

Emacs often leaves point in invisible regions, it seems.  To fix
this, we move point ourselves and never let Emacs do its own
adjustments.

When point has to be moved out of an invisible region, it can be
moved to its end or its beginning.  We usually move it to its
end, except when that would move point back to where it was
before the last command."
  (when (invisible-p (point))
    (let ((end (magit-invisible-region-end (point))))
      (goto-char (if (= end magit-last-point)
                     (magit-invisible-region-start (point))
                   end))))
  (setq disable-point-adjustment t))

;;;;; Buffer History

(defun magit-go-backward ()
  "Move backward in current buffer's history."
  (interactive)
  (if help-xref-stack
      (help-xref-go-back (current-buffer))
    (user-error "No previous entry in buffer's history")))

(defun magit-go-forward ()
  "Move forward in current buffer's history."
  (interactive)
  (if help-xref-forward-stack
      (help-xref-go-forward (current-buffer))
    (user-error "No next entry in buffer's history")))

(defun magit-xref-insert-buttons ()
  (when (and (or (eq magit-show-xref-buttons t)
                 (apply 'derived-mode-p magit-show-xref-buttons))
             (or help-xref-stack help-xref-forward-stack))
    (insert "\n")
    (when help-xref-stack
      (magit-xref-insert-button help-back-label
                                'magit-xref-backward))
    (when help-xref-forward-stack
      (when help-xref-stack
        (insert " "))
      (magit-xref-insert-button help-forward-label
                                'magit-xref-forward))))

(defun magit-xref-insert-button (label type)
  (magit-with-section (section button label)
    (insert-text-button label 'type type
                        'help-args (list (current-buffer)))))

(define-button-type 'magit-xref-backward
  :supertype 'help-back
  'mouse-face magit-item-highlight-face
  'help-echo (purecopy "mouse-2, RET: go back to previous history entry"))

(define-button-type 'magit-xref-forward
  :supertype 'help-forward
  'mouse-face magit-item-highlight-face
  'help-echo (purecopy "mouse-2, RET: go back to next history entry"))

(defun magit-setup-xref (item)
  (when help-xref-stack-item
    (push (cons (point) help-xref-stack-item) help-xref-stack)
    (setq help-xref-forward-stack nil))
  (when (called-interactively-p 'interactive)
    (--when-let (nthcdr 10 help-xref-stack)
      (setcdr it nil)))
  (setq help-xref-stack-item item))

;;;;; Refresh Machinery

(defun magit-refresh (&optional buffer)
  "Refresh some buffers belonging to the current repository.

Refresh the current buffer if its major mode derives from
`magit-mode', and refresh the corresponding status buffer.
If the global `magit-auto-revert-mode' is turned on, then
also revert all unmodified buffers that visit files being
tracked in the current repository."
  (interactive (list (current-buffer)))
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (when (derived-mode-p 'magit-mode)
      (magit-mode-refresh-buffer buffer))
    (let (status)
      (when (and (not (eq major-mode 'magit-status-mode))
                 (setq status (magit-mode-get-buffer
                               magit-status-buffer-name
                               'magit-status-mode)))
        (magit-mode-refresh-buffer status))))
  (when magit-auto-revert-mode
    (magit-revert-buffers)))

(defun magit-refresh-all ()
  "Refresh all buffers belonging to the current repository.

Refresh all Magit buffers belonging to the current repository.
If the global `magit-auto-revert-mode' is turned on, then also
revert all unmodified buffers that visit files being tracked in
the current repository."
  (interactive)
  (magit-map-magit-buffers #'magit-mode-refresh-buffer default-directory)
  (magit-revert-buffers))

(defun magit-revert-buffers ()
  (let ((topdir (magit-get-top-dir)))
    (when topdir
      (let ((gitdir  (magit-git-dir))
            (tracked (magit-git-lines "ls-tree" "-r" "--name-only" "HEAD")))
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (let ((file (buffer-file-name)))
              (and file (string-prefix-p topdir file)
                   (not (string-prefix-p gitdir file))
                   (member (file-relative-name file topdir) tracked)
                   (let ((auto-revert-mode t))
                     (auto-revert-handler))))))))))

(defvar magit-save-some-buffers-topdir nil)

(defun magit-save-some-buffers (&optional msg pred topdir)
  "Save some buffers if variable `magit-save-some-buffers' is non-nil.
If variable `magit-save-some-buffers' is set to `dontask' then
don't ask the user before saving the buffers, just go ahead and
do it.

Optional argument MSG is displayed in the minibuffer if variable
`magit-save-some-buffers' is nil.

Optional second argument PRED determines which buffers are considered:
If PRED is nil, all the file-visiting buffers are considered.
If PRED is t, then certain non-file buffers will also be considered.
If PRED is a zero-argument function, it indicates for each buffer whether
to consider it or not when called with that buffer current."
  (interactive)
  (let ((predicate-function (or pred magit-save-some-buffers-predicate))
        (magit-save-some-buffers-topdir (or topdir default-directory)))
    (if magit-save-some-buffers
        (save-some-buffers
         (eq magit-save-some-buffers 'dontask)
         predicate-function)
      (when msg
        (message msg)))))

(defun magit-save-buffers-predicate-all ()
  "Prompt to save all buffers with unsaved changes."
  t)

(defun magit-save-buffers-predicate-tree-only ()
  "Only prompt to save buffers which are within the current git project.
As determined by the directory passed to `magit-status'."
  (and buffer-file-name
       (string= (magit-get-top-dir magit-save-some-buffers-topdir)
                (magit-get-top-dir (file-name-directory buffer-file-name)))))

;;; Plumbing
;;;; Repository Paths

(defun magit-git-dir (&optional path)
  "Return absolute path to the GIT_DIR for the current repository.
If optional PATH is non-nil it has to be a path relative to the
GIT_DIR and its absolute path is returned"
  (--when-let (magit-rev-parse "--git-dir")
    (setq it (file-name-as-directory (magit-expand-git-file-name it)))
    (if path (expand-file-name (convert-standard-filename path) it) it)))

(defun magit-toplevel ()
  "Return the top-level directory for the current repository.

Determine the repository which contains `default-directory' in
its work tree and return the absolute path to its top-level
directory.  Otherwise return nil."
  (--when-let (magit-rev-parse "--show-toplevel")
    (file-name-as-directory (magit-expand-git-file-name it))))

(defun magit-get-top-dir (&optional directory)
  "Return the top-level directory for the current repository.

Determine the repository which contains `default-directory' in
either its work tree or git control directory and return the
absolute path to its top-level directory.  If there is no top
directory, because the repository is bare, return the control
directory instead.

If optional DIRECTORY is non-nil then return the top directory
of the repository that contains that instead.  DIRECTORY has to
be an existing directory."
  (setq directory (if directory
                      (file-name-as-directory
                       (expand-file-name directory))
                    default-directory))
  (unless (file-directory-p directory)
    (error "%s isn't an existing directory" directory))
  (let ((default-directory directory))
    (or (magit-toplevel)
        (-when-let (gitdir (magit-git-dir))
          (if (magit-bare-repo-p)
              gitdir
            (file-name-directory (directory-file-name gitdir)))))))

(defun magit-inside-gitdir-p ()
  "Return t if `default-directory' is below a repository directory."
  (magit-rev-parse-p "--is-inside-git-dir"))

(defun magit-inside-worktree-p ()
  "Return t if `default-directory' is below the work tree of a repository."
  (magit-rev-parse-p "--is-inside-work-tree"))

(defun magit-bare-repo-p ()
  "Return t if the current repository is bare."
  (magit-rev-parse-p "--is-bare-repository"))

(defun magit-file-relative-name (file)
  "Return the path of FILE relative to the repository root.
If FILE isn't inside a Git repository then return nil."
  (setq file (file-truename file))
  (--when-let (magit-get-top-dir (file-name-directory file))
    (substring file (length it))))

(defun magit-expand-git-file-name (filename)
  (when (tramp-tramp-file-p default-directory)
    (setq filename (file-relative-name filename
                                       (with-parsed-tramp-file-name
                                           default-directory nil
                                         localname))))
  (expand-file-name filename))

(defun magit-decode-git-path (path)
  (if (eq (aref path 0) ?\")
      (string-as-multibyte (read path))
    path))

;;;; Predicates

(defun magit-no-commit-p ()
  "Return t if there is no commit in the current git repository."
  (not (magit-git-string "rev-list" "-1" "HEAD")))

(defun magit-anything-staged-p (&rest files)
  "Return t if there are any staged changes.
If optional FILES is non-nil, then only changes to those files
are considered."
  (magit-git-failure "diff" "--quiet" "--cached" "--" files))

(defun magit-anything-unstaged-p (&rest files)
  "Return t if there are any unstaged changes.
If optional FILES is non-nil, then only changes to those files
are considered."
  (magit-git-failure "diff" "--quiet" "--" files))

(defun magit-anything-modified-p (&rest files)
  "Return t if there are any staged or unstaged changes.
If optional FILES is non-nil, then only changes to those files
are considered."
  (or (apply 'magit-anything-staged-p files)
      (apply 'magit-anything-unstaged-p files)))

;;;; Revisions and References

(defun magit-rev-parse (&rest args)
  "Execute `git rev-parse ARGS', returning first line of output.
If there is no output return nil."
  (apply #'magit-git-string "rev-parse" args))

(defun magit-rev-parse-p (&rest args)
  "Execute `git rev-parse ARGS', returning t if it prints \"true\".
Return t if the first (and usually only) output line is the
string \"true\", otherwise return nil."
  (magit-git-true "rev-parse" args))

(defun magit-get-shortname (rev)
  (let ((fn (apply-partially 'magit-git-string "name-rev"
                             "--name-only" "--no-undefined" rev)))
    (setq rev (or (funcall fn "--refs=refs/tags/*")
                  (funcall fn "--refs=refs/heads/*")
                  (funcall fn "--refs=refs/remotes/*" "--always")))
    (if (and (string-match "^\\(?:tags\\|remotes\\)/\\(.+\\)" rev)
             (magit-unambiguous-refname-p (match-string 1 rev)))
        (match-string 1 rev)
      rev)))

(defun magit-ref-exists-p (ref)
  (magit-git-success "show-ref" "--verify" ref))

(defun magit-unambiguous-refname-p (name)
  "Return t if REF is unambiguous, nil otherwise."
  ;; An ambiguous ref does not cause `git rev-parse --abbrev-ref'
  ;; to exits with a non-zero status.  But there is nothing on
  ;; stdout in that case.
  (not (magit-rev-parse "--abbrev-ref" name)))

(defun magit-get-current-branch ()
  "Return the refname of the currently checked out branch.
Return nil if no branch is currently checked out."
  (magit-git-string "symbolic-ref" "--short" "HEAD"))

(defun magit-get-previous-branch ()
  "Return the refname of the previously checked out branch.
Return nil if the previously checked out branch no longer exists."
  (let ((current (magit-get-current-branch))
        (i 1) prev)
    (while (and (setq prev (magit-rev-parse "--verify" (format "@{-%i}" i)))
                (setq prev (magit-get-shortname prev))
                (equal prev current))
      (cl-incf i))
    prev))

(defun magit-get-tracked-branch (&optional branch qualified)
  "Return the name of the tracking branch the local branch name BRANCH.

If optional QUALIFIED is non-nil return the full branch path,
otherwise try to shorten it to a name (which may fail)."
  (unless branch
    (setq branch (magit-get-current-branch)))
  (when branch
    (let ((remote (magit-get "branch" branch "remote"))
          (merge  (magit-get "branch" branch "merge")))
      (when (and (not merge)
                 (not (equal remote ".")))
        (setq merge branch))
      (when (and remote merge)
        (if (string= remote ".")
            (cond (qualified merge)
                  ((string-match "^refs/heads/" merge)
                   (substring merge 11))
                  ((string-match "^refs/" merge)
                   merge))
          (let* ((fetch (mapcar (lambda (f) (split-string f "[+:]" t))
                                (magit-get-all "remote" remote "fetch")))
                 (match (cadr (assoc merge fetch))))
            (unless match
              (let* ((prefix (nreverse (split-string merge "/")))
                     (unique (list (car prefix))))
                (setq prefix (cdr prefix))
                (setq fetch
                      (cl-mapcan
                       (lambda (f)
                         (cl-destructuring-bind (from to) f
                           (setq from (nreverse (split-string from "/")))
                           (when (equal (car from) "*")
                             (list (list (cdr from) to)))))
                       fetch))
                (while (and prefix (not match))
                  (if (setq match (cadr (assoc prefix fetch)))
                      (setq match (concat (substring match 0 -1)
                                          (mapconcat 'identity unique "/")))
                    (push (car prefix) unique)
                    (setq prefix (cdr prefix))))))
            (cond ((not match) nil)
                  (qualified match)
                  ((string-match "^refs/remotes/" match)
                   (substring match 13))
                  (t match))))))))

(defun magit-get-remote (branch)
  "Return the name of the remote for BRANCH.
If branch is nil or it has no remote, but a remote named
\"origin\" exists, return that.  Otherwise, return nil."
  (let ((remote (or (and branch (magit-get "branch" branch "remote"))
                    (and (magit-get "remote" "origin" "url") "origin"))))
    (unless (string= remote "")
      remote)))

(defun magit-get-current-remote ()
  "Return the name of the remote for the current branch.
If there is no current branch, or no remote for that branch,
but a remote named \"origin\" is configured, return that.
Otherwise, return nil."
  (magit-get-remote (magit-get-current-branch)))

(defun magit-get-remote/branch (&optional branch verify)
  "Return the remote-tracking branch of BRANCH used for pulling.
Return a string of the form \"REMOTE/BRANCH\".

If optional BRANCH is nil return the remote-tracking branch of
the current branch.  If optional VERIFY is non-nil verify that
the remote branch exists; else return nil."
  (save-match-data
    (let (remote remote-branch remote/branch)
      (and (or branch (setq branch (magit-get-current-branch)))
           (setq remote (magit-get "branch" branch "remote"))
           (setq remote-branch (magit-get "branch" branch "merge"))
           (string-match "^refs/heads/\\(.+\\)" remote-branch)
           (setq remote/branch
                 (concat remote "/" (match-string 1 remote-branch)))
           (or (not verify)
               (magit-rev-parse "--verify" remote/branch))
           remote/branch))))

(defun magit-get-current-tag (&optional with-distance)
  "Return the closest tag reachable from \"HEAD\".

If optional WITH-DISTANCE is non-nil then return (TAG COMMITS),
if it is `dirty' return (TAG COMMIT DIRTY). COMMITS is the number
of commits in \"HEAD\" but not in TAG and DIRTY is t if there are
uncommitted changes, nil otherwise."
  (--when-let (magit-git-string "describe" "--long" "--tags"
                                (and (eq with-distance 'dirty) "--dirty"))
    (save-match-data
      (string-match
       "\\(.+\\)-\\(?:0[0-9]*\\|\\([0-9]+\\)\\)-g[0-9a-z]+\\(-dirty\\)?$" it)
      (if with-distance
          (list (match-string 1 it)
                (string-to-number (or (match-string 2 it) "0"))
                (and (match-string 3 it) t))
        (match-string 1 it)))))

(defun magit-get-next-tag (&optional with-distance)
  "Return the closest tag from which \"HEAD\" is reachable.

If no such tag can be found or if the distance is 0 (in which
case it is the current tag, not the next) return nil instead.

If optional WITH-DISTANCE is non-nil then return (TAG COMMITS)
where COMMITS is the number of commits in TAG but not in \"HEAD\"."
  (--when-let (magit-git-string "describe" "--contains" "HEAD")
    (save-match-data
      (when (string-match "^[^^~]+" it)
        (setq it (match-string 0 it))
        (unless (equal it (magit-get-current-tag))
          (if with-distance
              (list it (car (magit-rev-diff-count it "HEAD")))
            it))))))

(defun magit-list-refs (&rest args)
  (magit-git-lines
   "for-each-ref" "--format=%(refname)"
   (or args (list "refs/heads" "refs/remotes" "refs/tags"))))

(defun magit-list-branches ()
  (magit-list-refs "refs/heads" "refs/remotes"))

(defun magit-list-local-branches ()
  (magit-list-refs "refs/heads"))

(defun magit-list-remote-branches (&optional remote)
  (magit-list-refs (concat "refs/remotes/" remote)))

(defun magit-list-refnames (&rest args)
  (magit-git-lines
   "for-each-ref" "--format=%(refname:short)"
   (or args (list "refs/heads" "refs/remotes" "refs/tags"))))

(defun magit-list-branch-names ()
  (magit-list-refnames "refs/heads" "refs/remotes"))

(defun magit-list-local-branch-names ()
  (magit-list-refnames "refs/heads"))

(defun magit-list-remote-branch-names (&optional remote relative)
  (if (and remote relative)
      (let ((regexp (format "^refs/remotes/%s/\\(.+\\)" remote)))
        (cl-mapcan (lambda (ref)
                     (and (string-match regexp ref)
                          (list (match-string t ref))))
                   (magit-list-remote-branches remote)))
    (magit-list-refnames (concat "refs/remotes/" remote))))

(defun magit-rev-diff-count (a b)
  "Return the commits in A but not B and vice versa.
Return a list of two integers: (A>B B>A)."
  (mapcar 'string-to-number
          (split-string (magit-git-string "rev-list"
                                          "--count" "--left-right"
                                          (concat a "..." b))
                        "\t")))

(defun magit-abbrev-length ()
  (string-to-number (or (magit-get "core.abbrev") "7")))

(defun magit-abbrev-arg ()
  (format "--abbrev=%d" (magit-abbrev-length)))

(defun magit-commit-parents (commit)
  (cdr (split-string (magit-git-string "rev-list" "-1" "--parents" commit))))

(defun magit-assert-one-parent (commit command)
  (when (> (length (magit-commit-parents commit)) 1)
    (user-error "Cannot %s a merge commit" command)))

(defun magit-reflog-enable (ref)
  (let ((logfile (magit-git-dir (concat "logs/" ref))))
    (unless (file-exists-p logfile)
      (make-directory (file-name-directory logfile) t)
      (with-temp-file logfile))))

(defun magit-rev-format (format &optional rev)
  "Return first line of `git log -1 --format=format:FORMAT [REV]'.
Execute Git, returning the first line of its output.  If there is
no output return nil."
  (magit-git-string "log" "-1" (concat "--format=format:" format) rev))

(defun magit-format-rev-summary (rev)
  (--when-let (magit-rev-format "%h %s" rev)
    (string-match " " it)
    (put-text-property 0 (match-beginning 0) 'face 'magit-log-sha1 it)
    it))

(defun magit-format-ref-label (ref)
  (cl-destructuring-bind (re face fn)
      (--first (string-match (car it) ref) magit-refs-namespaces)
    (if fn
        (funcall fn ref face)
      (propertize (or (match-string 1 ref) ref) 'face face))))

(defun magit-format-ref-labels (string)
  (save-match-data
    (mapconcat 'magit-format-ref-label
               (split-string string "\\(tag: \\|[(), ]\\)" t) " ")))

(defun magit-insert-ref-labels (string)
  (save-match-data
    (dolist (ref (split-string string "\\(tag: \\|[(), ]\\)" t) " ")
      (cl-destructuring-bind (re face fn)
          (cl-find-if (lambda (elt) (string-match (car elt) ref))
                      magit-refs-namespaces)
        (if fn
            (let ((text (funcall fn ref face)))
              (magit-insert text (get-text-property 1 'face text) ?\s))
        (magit-insert (or (match-string 1 ref) ref) face ?\s))))))

(defmacro magit-with-blob (commit file &rest body)
  (declare (indent 2))
  `(with-temp-buffer
     (let ((buffer-file-name ,file))
       (save-excursion
         (magit-git-insert "cat-file" "-p"
                           (concat ,commit ":" buffer-file-name)))
       (decode-coding-inserted-region
        (point-min) (point-max) buffer-file-name t nil nil t)
       ,@body)))

;;;; Variables

(defun magit-get (&rest keys)
  "Return the value of Git config entry specified by KEYS."
  (magit-git-string "config" (mapconcat 'identity keys ".")))

(defun magit-get-all (&rest keys)
  "Return all values of the Git config entry specified by KEYS."
  (magit-git-lines "config" "--get-all" (mapconcat 'identity keys ".")))

(defun magit-get-boolean (&rest keys)
  "Return the boolean value of Git config entry specified by KEYS."
  (magit-git-true "config" "--bool" (mapconcat 'identity keys ".")))

(defun magit-set (val &rest keys)
  "Set Git config settings specified by KEYS to VAL."
  (if val
      (magit-git-string "config" (mapconcat 'identity keys ".") val)
    (magit-git-string "config" "--unset" (mapconcat 'identity keys "."))))

;;;; Completion
;;;;; Completing Read

(defun magit-completing-read
  (prompt collection &optional predicate require-match initial-input hist def)
  "Magit wrapper around `completing-read' or an alternative function.

Option `magit-completing-read-function' can be used to wrap
around another `completing-read'-like function.  Unless it
doesn't have the exact same signature, an additional wrapper is
required.  Even if it has the same signature it might be a good
idea to wrap it, so that `magit-prompt-with-default' can be used.

See `completing-read' for the meanings of the arguments, but note
that this wrapper makes the following changes:

- If REQUIRE-MATCH is nil and the user exits without a choise,
  then return nil instead of an empty string.

- If REQUIRE-MATCH is non-nil and the users exits without a
  choise, then raise a user-error.

- For historic reasons \": \" is appended to PROMPT.  This will
  likely be fixed.

- If a `magit-completing-read-function' is used which in turn
  uses `magit-prompt-with-completion' and DEF is non-nil, then
  PROMPT is modified to end with \" (default DEF): \".

The use of another completing function and/or wrapper obviously
results in additional differences."
  (let ((reply (funcall magit-completing-read-function
                        (concat prompt ": ") collection predicate
                        require-match initial-input hist def)))
    (if (string= reply "")
        (if require-match
            (user-error "Nothing selected")
          nil)
      reply)))

(defun magit-builtin-completing-read
  (prompt choices &optional predicate require-match initial-input hist def)
  "Magit wrapper for standard `completing-read' function."
  (completing-read (magit-prompt-with-default prompt def)
                   choices predicate require-match
                   initial-input hist def))

(defun magit-ido-completing-read
  (prompt choices &optional predicate require-match initial-input hist def)
  "Ido-based completing-read almost-replacement."
  (require 'ido)
  (let ((reply (ido-completing-read
                prompt
                (if (consp (car choices))
                    (mapcar #'car choices)
                  choices)
                predicate require-match initial-input hist def)))
    (or (and (consp (car choices))
             (cdr (assoc reply choices)))
        reply)))

(defun magit-iswitchb-completing-read
  (prompt choices &optional predicate require-match initial-input hist def)
  "Iswitchb-based completing-read almost-replacement."
  (require 'iswitchb)
  (let ((iswitchb-make-buflist-hook
         (lambda ()
           (setq iswitchb-temp-buflist (if (consp (car choices))
                                           (mapcar #'car choices)
                                         choices)))))
    (iswitchb-read-buffer prompt (or initial-input def) require-match)))

(defun magit-prompt-with-default (prompt def)
  (if (and def (> (length prompt) 2)
           (string-equal ": " (substring prompt -2)))
      (format "%s (default %s): " (substring prompt 0 -2) def)
    prompt))

;;;;; Revision Completion

(defvar magit-read-rev-history nil)

(defun magit-read-rev (prompt &optional default exclude noselection)
  (setq default (magit-git-string "rev-parse" "--symbolic" default)
        exclude (magit-git-string "rev-parse" "--symbolic" exclude))
  (magit-completing-read prompt (delete exclude (magit-list-refnames))
                         nil nil nil
                         'magit-read-rev-history default))

(defun magit-read-rev-with-default (prompt)
  (magit-read-rev prompt (--when-let (or (magit-guess-branch) "HEAD")
                           (if (string-match "^refs/\\(.*\\)" it)
                               (match-string 1 it)
                             it))))

(defun magit-popup-read-rev (prompt initial-input)
  (magit-completing-read prompt nil nil nil initial-input
                         'magit-read-rev-history))

(defun magit-read-remote-branch (prompt remote &optional default)
  (magit-completing-read prompt (magit-list-remote-branch-names remote t)
                         nil nil nil nil default))

(defun magit-read-tag (prompt &optional require-match)
  (magit-completing-read prompt (magit-git-lines "tag") nil
                         require-match nil 'magit-read-rev-history))

(defun magit-read-stash (prompt)
  (let ((n (read-number  prompt 0))
        (l (1- (length (magit-git-lines "stash" "list")))))
    (if (> n l)
        (user-error "No stash older than stash@{%i}" l)
      (format "stash@{%i}" n))))

;;;;; Miscellaneous Completion

(defun magit-read-remote (prompt &optional default require-match)
  (magit-completing-read prompt (magit-git-lines "remote")
                         nil require-match nil nil
                         (or default (magit-guess-remote))))

(defvar magit-read-file-hist nil)

(defun magit-read-file-from-rev (revision &optional default)
  (unless revision
    (setq revision "HEAD"))
  (let ((default-directory (magit-get-top-dir)))
    (magit-completing-read
     (format "Retrieve file from %s" revision)
     (magit-git-lines "ls-tree" "-r" "-t" "--name-only" revision)
     nil 'require-match
     nil 'magit-read-file-hist
     (or default (magit-buffer-file-name t)))))

(defun magit-read-file-trace (ignored)
  (let ((file  (magit-read-file-from-rev "HEAD"))
        (trace (read-string "Trace: ")))
    (if (string-match
         "^\\(/.+/\\|:[^:]+\\|[0-9]+,[-+]?[0-9]+\\)\\(:\\)?$" trace)
        (concat trace (or (match-string 2 trace) ":") file)
      (user-error "Trace is invalid, see man git-log"))))

(defvar magit-gpg-secret-key-hist nil)

(defun magit-read-gpg-secret-key (prompt &optional initial-input)
  (let ((keys (mapcar
               (lambda (key)
                 (list (epg-sub-key-id (car (epg-key-sub-key-list key)))
                       (-when-let (id-obj (car (epg-key-user-id-list key)))
                         (let    ((id-str (epg-user-id-string id-obj)))
                           (if (stringp id-str)
                               id-str
                             (epg-decode-dn id-obj))))))
               (epg-list-keys (epg-make-context epa-protocol) nil t))))
  (magit-completing-read prompt keys nil nil initial-input nil
                         (or (car magit-gpg-secret-key-hist) (car keys)))))

(defun magit-popup-read-file-name (prompt initial-input)
  (read-file-name prompt nil nil t initial-input))

;;; Modes (1)
;;;; Commit Mode
;;;;; Commit Core

(define-derived-mode magit-commit-mode magit-mode "Magit"
  "Mode for looking at a git commit.

\\<magit-commit-mode-map>Type `\\[magit-visit-item]` to visit the changed file, \
`\\[magit-toggle-section]` to hide or show a hunk,
`\\[magit-diff-more-context]` and `\\[magit-diff-less-context]` to change the \
size of the hunks.
Type `\\[magit-apply-item]` to apply a change to your worktree and \
`\\[magit-revert-item]` to reverse it.

\\{magit-commit-mode-map}
Unless shadowed by the mode specific bindings above, bindings
from the parent keymap `magit-mode-map' are also available."
  :group 'magit)

(defvar magit-commit-buffer-name "*magit-commit*"
  "Name of buffer used to display a commit.")

;;;###autoload
(defun magit-show-commit (commit &optional noselect module)
  "Show information about COMMIT."
  (interactive (list (magit-read-rev-with-default
                      "Show commit (hash or ref)")))
  (let ((default-directory (if module
                               (file-name-as-directory
                                (expand-file-name module (magit-get-top-dir)))
                             default-directory)))
    (when (magit-git-failure "cat-file" "commit" commit)
      (user-error "%s is not a commit" commit))
    (magit-mode-setup magit-commit-buffer-name
                      (if noselect 'display-buffer 'pop-to-buffer)
                      #'magit-commit-mode
                      #'magit-refresh-commit-buffer
                      commit)))

(defun magit-show-item-or-scroll-up ()
  "Update commit or status buffer for item at point.

Either show the commit or stash at point in another buffer,
or if that buffer is already displayed in the current frame
and contains information about that commit or stash, then
instead scroll the buffer up.  If there is no commit or
stash at point, then prompt for a commit."
  (interactive)
  (magit-show-item-or-scroll 'scroll-up))

(defun magit-show-item-or-scroll-down ()
  "Update commit or status buffer for item at point.

Either show the commit or stash at point in another buffer,
or if that buffer is already displayed in the current frame
and contains information about that commit or stash, then
instead scroll the buffer down.  If there is no commit or
stash at point, then prompt for a commit."
  (interactive)
  (magit-show-item-or-scroll 'scroll-down))

(defun magit-show-item-or-scroll (fn)
  (let (rev cmd buf win)
    (magit-section-case (info)
      (commit (setq rev info
                    cmd 'magit-show-commit
                    buf magit-commit-buffer-name))
      (stash  (setq rev info
                    cmd 'magit-diff-stash
                    buf magit-stash-buffer-name)))
    (if rev
        (if (and (setq buf (get-buffer buf))
                 (setq win (get-buffer-window buf))
                 (with-current-buffer buf
                   (equal rev (car magit-refresh-args))))
            (with-selected-window win
              (condition-case err
                  (funcall fn)
                (error
                 (goto-char (cl-case fn
                              (scroll-up   (point-min))
                              (scroll-down (point-max)))))))
          (funcall cmd rev t))
      (call-interactively 'magit-show-commit))))

(defun magit-refresh-commit-buffer (commit)
  (magit-git-insert-section (commitbuf nil)
      #'magit-wash-commit
    "log" "-1"
    "--decorate=full" "--pretty=medium"
    "--cc" "-p" (and magit-show-diffstat "--stat")
    magit-diff-options commit))

;;;;; Commit Washing

(defun magit-wash-commit ()
  (looking-at "^commit \\([a-z0-9]+\\)\\(?: \\(.+\\)\\)?$")
  (let ((rev  (match-string 1))
        (refs (match-string 2)))
    (delete-region (point) (1+ (line-end-position)))
    (magit-with-section
        (section headers 'headers
         (concat (propertize rev 'face 'magit-log-sha1)
                 (and refs (concat " "(magit-format-ref-labels refs)))
                 "\n"))
      (while (re-search-forward "^\\([a-z]+\\): +\\(.+\\)$" nil t)
        (when (string-match-p (match-string 1) "Merge")
          (let ((revs (match-string 2)))
            (delete-region (match-beginning 2) (match-end 2))
            (dolist (rev (split-string revs))
              (magit-insert-commit-button rev)
              (insert ?\s)))))
      (forward-line)))
  (forward-line)
  (let ((bound (save-excursion
                 (when (re-search-forward "^diff" nil t)
                   (copy-marker (match-beginning 0)))))
        (summary (buffer-substring-no-properties
                  (point) (line-end-position))))
    (delete-region (point) (1+ (line-end-position)))
    (magit-with-section (section message 'message (concat summary "\n"))
      (cond ((re-search-forward "^---" bound t)
             (goto-char (match-beginning 0))
             (delete-region (match-beginning 0) (match-end 0)))
            ((re-search-forward "^.[^ ]" bound t)
             (goto-char (1- (match-beginning 0)))))))
  (forward-line)
  (when magit-show-diffstat
    (magit-wash-diffstats))
  (forward-line)
  (magit-wash-diffs))

(defun magit-insert-commit-button (hash)
  (magit-with-section (section commit hash)
    (insert-text-button hash
                        'help-echo "Visit commit"
                        'action (lambda (button)
                                  (save-excursion
                                    (goto-char button)
                                    (magit-visit-item)))
                        'follow-link t
                        'mouse-face magit-item-highlight-face
                        'face 'magit-log-sha1)))

;;;; Status Mode

(define-derived-mode magit-status-mode magit-mode "Magit"
  "Mode for looking at git status.

\\<magit-status-mode-map>Type `\\[magit-stage-item]` to stage (add) an item, \
`\\[magit-unstage-item]` to unstage it.
Type `\\[magit-commit-popup]` to have a popup to commit, \
type `\\[magit-dispatch-popup]` to see others available popup.
Type `\\[magit-visit-item]` to visit something, and \
`\\[magit-toggle-section]` to show or hide section.

More information can be found in Info node `(magit)Status'

Other key binding:
\\{magit-status-mode-map}"
  :group 'magit)

(defvar magit-status-buffer-name "*magit: %t*"
  "Name of buffer used to display a repository's status.")

;;;###autoload
(defun magit-status (dir &optional switch-function)
  "Open a Magit status buffer for the Git repository containing DIR.
If DIR is not within a Git repository, offer to create a Git
repository in DIR.

Interactively, a prefix argument means to ask the user which Git
repository to use even if `default-directory' is under Git
control.  Two prefix arguments means to ignore `magit-repo-dirs'
when asking for user input.

Depending on option `magit-status-buffer-switch-function' the
status buffer is shown in another window (the default) or the
current window.  Non-interactively optional SWITCH-FUNCTION
can be used to override this."
  (interactive (list (if current-prefix-arg
                         (magit-read-top-dir
                          (> (prefix-numeric-value current-prefix-arg)
                             4))
                       (or (magit-get-top-dir)
                           (magit-read-top-dir nil)))))
  (magit-save-some-buffers)
  (-when-let (default-directory
              (or (magit-get-top-dir dir)
                  (and (yes-or-no-p
                        (format "No repository in %s.  Create one? " dir))
                       (progn (magit-init dir)
                              (magit-get-top-dir dir)))))
    (magit-mode-setup magit-status-buffer-name
                      (or switch-function
                          magit-status-buffer-switch-function)
                      #'magit-status-mode
                      #'magit-refresh-status)))

(defun magit-refresh-status ()
  (magit-git-exit-code "update-index" "--refresh")
  (magit-with-section (section status 'status)
    (run-hooks 'magit-status-sections-hook))
  (run-hooks 'magit-refresh-status-hook))

;;; Sections
;;;; Real Sections

(defun magit-insert-stashes ()
  (--when-let (magit-git-lines "stash" "list")
    (magit-with-section (section stashes 'stashes "Stashes:")
      (dolist (stash it)
        (string-match "^\\(stash@{\\([0-9]+\\)}\\): \\(.+\\)$" stash)
        (let ((stash (match-string 1 stash))
              (number (match-string 2 stash))
              (message (match-string 3 stash)))
          (magit-with-section (section stash stash)
            (insert number ": " message "\n"))))
      (insert "\n"))))

(defun magit-insert-untracked-files ()
  (--when-let (cl-mapcan (lambda (f)
                           (and (eq (aref f 0) ??) (list f)))
                         (magit-git-lines "status" "--porcelain"))
    (magit-with-section (section untracked 'untracked "Untracked files:")
      (dolist (file it)
        (setq file (magit-decode-git-path (substring file 3)))
        (magit-with-section (section file file)
          (insert "\t" file "\n")))
      (insert "\n"))))

(defun magit-insert-unstaged-changes ()
  (magit-git-insert-section (unstaged "Unstaged changes:")
      #'magit-wash-diffs
    "diff" magit-diff-extra-options))

(defun magit-insert-staged-changes ()
  (magit-git-insert-section (staged "Staged changes:")
      #'magit-wash-diffs
    "diff" "--cached" magit-diff-extra-options))

(defun magit-insert-unpulled-or-recent-commits ()
  (let ((tracked (magit-get-tracked-branch nil t)))
    (if (and tracked (not (equal (magit-rev-parse "HEAD")
                                 (magit-rev-parse tracked))))
        (magit-insert-unpulled-commits)
      (magit-git-insert-section (recent "Recent commits:")
          (apply-partially 'magit-wash-log 'unique)
        "log" "--format=format:%h %s" "-n" "10"))))

(defun magit-insert-unpulled-commits ()
  (-when-let (tracked (magit-get-tracked-branch nil t))
    (magit-git-insert-section (unpulled "Unpulled commits:")
        (apply-partially 'magit-wash-log 'unique)
      "log" "--format=format:%h %s" (concat "HEAD.." tracked))))

(defun magit-insert-unpushed-commits ()
  (-when-let (tracked (magit-get-tracked-branch nil t))
    (magit-git-insert-section (unpushed "Unpushed commits:")
        (apply-partially 'magit-wash-log 'unique)
      "log" "--format=format:%h %s" (concat tracked "..HEAD"))))

(defun magit-insert-unpulled-cherries ()
  (-when-let (tracked (magit-get-tracked-branch nil t))
    (magit-git-insert-section (unpulled "Unpulled commits:")
        (apply-partially 'magit-wash-log 'cherry)
      "cherry" "-v" (magit-abbrev-arg) (magit-get-current-branch) tracked)))

(defun magit-insert-unpushed-cherries ()
  (-when-let (tracked (magit-get-tracked-branch nil t))
    (magit-git-insert-section (unpushed "Unpushed commits:")
        (apply-partially 'magit-wash-log 'cherry)
      "cherry" "-v" (magit-abbrev-arg) tracked)))

;;;; Line Sections

(defun magit-insert-empty-line ()
  (insert "\n"))

(defun magit-insert-status-local-line ()
  (let ((branch (or (magit-get-current-branch) "(detached)")))
    (magit-insert-line-section (branch branch)
      (concat "Local: "
              (propertize branch 'face 'magit-branch)
              " " (abbreviate-file-name default-directory)))))

(defun magit-insert-status-remote-line ()
  (let* ((branch  (magit-get-current-branch))
         (tracked (magit-get-tracked-branch branch)))
    (when tracked
      (magit-insert-line-section (branch tracked)
        (concat "Remote: "
                (and (magit-get-boolean "branch" branch "rebase") "onto ")
                (magit-format-tracked-line tracked branch))))))

(defun magit-format-tracked-line (tracked branch)
  (when tracked
    (setq tracked (propertize tracked 'face 'magit-branch))
    (let ((remote (magit-get "branch" branch "remote")))
      (concat (if (string= "." remote)
                  (concat "branch " tracked)
                (when (string-match (concat "^" remote) tracked)
                  (setq tracked (substring tracked (1+ (length remote)))))
                (concat tracked " @ " remote
                        " (" (magit-get "remote" remote "url") ")"))))))

(defun magit-insert-status-head-line ()
  (-if-let (hash (magit-rev-parse "--verify" "HEAD"))
      (magit-insert-line-section (commit hash)
        (concat "Head: " (magit-format-rev-summary "HEAD")))
    (magit-insert-line-section (no-commit)
      "Head: nothing committed yet")))

(defun magit-insert-status-tags-line ()
  (let* ((current-tag (magit-get-current-tag t))
         (next-tag (magit-get-next-tag t))
         (both-tags (and current-tag next-tag t))
         (tag-subject (eq magit-status-tags-line-subject 'tag)))
    (when (or current-tag next-tag)
      (magit-insert-line-section (line)
        (concat
         (if both-tags "Tags: " "Tag: ")
         (and current-tag (apply 'magit-format-status-tag-sentence
                                 tag-subject current-tag))
         (and both-tags ", ")
         (and next-tag (apply 'magit-format-status-tag-sentence
                              (not tag-subject) next-tag)))))))

(defun magit-format-status-tag-sentence (behindp tag cnt &rest ignored)
  (concat (propertize tag 'face 'magit-tag)
          (and (> cnt 0)
               (concat (if (eq magit-status-tags-line-subject 'tag)
                           (concat " (" (propertize (format "%s" cnt)
                                                    'face 'magit-branch))
                         (format " (%i" cnt))
                       " " (if behindp "behind" "ahead") ")"))))

;;;; Progress Sections

(defun magit-insert-status-merge-line ()
  (-when-let (heads (magit-file-lines (magit-git-dir "MERGE_HEAD")))
    (magit-insert-line-section (line)
      (concat "Merging: "
              (mapconcat 'identity (mapcar 'magit-get-shortname heads) ", ")
              (and magit-status-show-sequence-help
                   "; Resolve conflicts, or press \"m A\" to Abort")))))

(defun magit-insert-status-rebase-lines ()
  (-when-let (rebase (magit-rebase-info))
    (cl-destructuring-bind (onto done total hash am) rebase
      (magit-insert-line-section (line)
        (concat (if am "Applying" "Rebasing")
                (format ": onto %s (%s of %s)" onto done total)
                (and magit-status-show-sequence-help
                     "; Press \"r\" to Abort, Skip, or Continue")))
      (when (and (not am) hash)
        (magit-insert-line-section (commit hash)
          (concat "Stopped: " (magit-format-rev-summary hash)))))))

(defun magit-insert-branch-description ()
  (let ((branch (magit-get-current-branch)))
    (--when-let (magit-git-lines
                 "config" (format "branch.%s.description" branch))
      (magit-with-section
          (section branchdesc branch (concat branch ": " (car it)) t)
        (insert (mapconcat 'identity (cdr it) "\n"))
        (insert "\n\n")))))

(defun magit-insert-rebase-sequence ()
  (let ((f (magit-git-dir "rebase-merge/git-rebase-todo")))
    (when (file-exists-p f)
      (magit-with-section (section rebase-todo 'rebase-todo "Rebasing:")
        (cl-loop
         for line in (magit-file-lines f)
         when (string-match
               "^\\(pick\\|reword\\|edit\\|squash\\|fixup\\) \\([^ ]+\\) \\(.*\\)$"
               line)
         do (let ((cmd  (match-string 1 line))
                  (hash (match-string 2 line))
                  (msg  (match-string 3 line)))
              (magit-with-section (section commit hash)
                (insert cmd " ")
                (insert (propertize
                         (magit-rev-parse "--short" hash)
                         'face 'magit-log-sha1))
                (insert " " msg "\n"))))
        (insert "\n")))))

(defun magit-insert-bisect-output ()
  (when (magit-bisecting-p)
    (let ((lines
           (or (magit-file-lines (magit-git-dir "BISECT_CMD_OUTPUT"))
               (list "Bisecting: (no saved bisect output)"
                     "It appears you have invoked `git bisect' from a shell."
                     "There is nothing wrong with that, we just cannot display"
                     "anything useful here.  Consult the shell output instead.")))
          (done-re "^[a-z0-9]\\{40\\} is the first bad commit$"))
      (magit-with-section
          (section bisect-output 'bisect-output
                   (propertize
                    (or (and (string-match done-re (car lines)) (pop lines))
                        (cl-find-if (apply-partially 'string-match done-re)
                                    lines)
                        (pop lines))
                    'face 'magit-section-title)
                   t)
        (dolist (line lines)
          (insert line "\n"))))
    (insert "\n")))

(defun magit-insert-bisect-rest ()
  (when (magit-bisecting-p)
    (magit-git-insert-section (bisect-view "Bisect Rest:")
        (apply-partially 'magit-wash-log 'bisect-vis)
      "bisect" "visualize" "git" "log"
      "--pretty=format:%h%d %s" "--decorate=full")))

(defun magit-insert-bisect-log ()
  (when (magit-bisecting-p)
    (magit-git-insert-section (bisect-log "Bisect Log:")
        #'magit-wash-bisect-log
      "bisect" "log")))

(defun magit-wash-bisect-log ()
  (let (beg)
    (while (progn (setq beg (point-marker))
                  (re-search-forward "^\\(git bisect [^\n]+\n\\)" nil t))
      (let ((heading (match-string-no-properties 1)))
        (delete-region (match-beginning 0) (match-end 0))
        (save-restriction
          (narrow-to-region beg (point))
          (goto-char (point-min))
          (magit-with-section (section bisect-log 'bisect-log heading t)
            (magit-wash-sequence
             (apply-partially 'magit-wash-log-line 'bisect-log
                              (magit-abbrev-length)))))))
    (when (re-search-forward
           "# first bad commit: \\[\\([a-z0-9]\\{40\\}\\)\\] [^\n]+\n" nil t)
      (let ((hash (match-string-no-properties 1)))
        (delete-region (match-beginning 0) (match-end 0))
        (magit-with-section
            (section 'bisect-log 'bisect-log
                     (concat hash " is the first bad commit\n")))))))

(defun magit-bisecting-p ()
  (file-exists-p (magit-git-dir "BISECT_LOG")))

;;; Porcelain
;;;; Apply
;;;;; Apply Commands
;;;;;; Apply

(defun magit-apply-item ()
  "Apply the item at point to the current working tree."
  (interactive)
  (magit-section-action apply (info)
    (([* unstaged] [* staged])
     (user-error "Change is already in your working tree"))
    (hunk   (magit-apply-hunk-item it))
    (diff   (magit-apply-diff-item it))
    (stash  (magit-stash-apply info))
    (commit (magit-apply-commit info))))

;;;;;; Stage

(defun magit-stage-item (&optional file)
  "Add the item at point to the staging area.
With a prefix argument, prompt for a file to be staged instead."
  (interactive
   (when current-prefix-arg
     (list (file-relative-name (read-file-name "File to stage: " nil nil t)
                               (magit-get-top-dir)))))
  (if file
      (magit-run-git "add" file)
    (magit-section-action stage (info)
      ([file untracked]
       (magit-run-git
        (cond
         ((use-region-p)
          (cons "add" (magit-section-region-siblings #'magit-section-info)))
         ((and (string-match-p "/$" info)
               (file-exists-p (expand-file-name ".git" info)))
          (let ((repo (read-string
                       "Add submodule tracking remote repo (empty to abort): "
                       (let ((default-directory
                               (file-name-as-directory
                                (expand-file-name info default-directory))))
                         (magit-get "remote.origin.url")))))
            (if (equal repo "")
                (user-error "Abort")
              (list "submodule" "add" repo (substring info 0 -1)))))
         (t
          (list "add" info)))))
      (untracked
       (magit-run-git "add" "--" (magit-git-lines "ls-files" "--other"
                                                  "--exclude-standard")))
      ([hunk diff unstaged]
       (magit-apply-hunk-item it "--cached"))
      ([diff unstaged]
       (magit-run-git "add" "-u"
                      (if (use-region-p)
                          (magit-section-region-siblings #'magit-section-info)
                        info)))
      (unstaged
       (magit-stage-all))
      ([* staged]
       (user-error "Already staged"))
      (hunk (user-error "Can't stage this hunk"))
      (diff (user-error "Can't stage this diff")))))

;;;###autoload
(defun magit-stage-all (&optional include-untracked)
  "Add all remaining changes in tracked files to staging area.
With a prefix argument, add remaining untracked files as well.
\('git add [--update] .')."
  (interactive "P")
  (when (or (not magit-stage-all-confirm)
            (not (magit-anything-staged-p))
            (yes-or-no-p "Stage all changes? "))
    (magit-run-git "add" (unless include-untracked "--update") ".")))

;;;;;; Unstage

(defun magit-unstage-item ()
  "Remove the item at point from the staging area."
  (interactive)
  (magit-section-action unstage (info)
    ([hunk diff staged]
     (magit-apply-hunk-item it "--reverse" "--cached"))
    ([diff staged]
     (when (eq info 'unmerged)
       (user-error "Can't unstage an unmerged file.  Resolve it first"))
     (let ((files (if (use-region-p)
                      (magit-section-region-siblings #'magit-section-info)
                    (list info))))
       (if (magit-no-commit-p)
           (magit-run-git "rm" "--cached" "--" files)
         (magit-run-git "reset" "-q" "HEAD" "--" files))))
    (staged
     (magit-unstage-all))
    ([* unstaged]
     (user-error "Already unstaged"))
    (hunk (user-error "Can't unstage this hunk"))
    (diff (user-error "Can't unstage this diff"))))

;;;###autoload
(defun magit-unstage-all ()
  "Remove all changes from staging area.
\('git reset --mixed HEAD')."
  (interactive)
  (when (or (not magit-unstage-all-confirm)
            (and (not (magit-anything-unstaged-p))
                 (not (magit-git-lines "ls-files" "--others" "-t"
                                       "--exclude-standard")))
            (yes-or-no-p "Unstage all changes? "))
    (magit-run-git "reset" "HEAD" "--")))

;;;;;; Discard

(defun magit-discard-item ()
  "Remove the change introduced by the item at point."
  (interactive)
  (magit-section-action discard (info parent-info diff-status)
    ([file untracked]
     (when (yes-or-no-p (format "Delete %s? " info))
       (if (and (file-directory-p info)
                (not (file-symlink-p info)))
           (delete-directory info 'recursive)
         (delete-file info))
       (magit-refresh)))
    (untracked
     (when (yes-or-no-p "Delete all untracked files and directories? ")
       (magit-run-git "clean" "-df")))
    ([hunk diff unstaged]
     (when (yes-or-no-p (if (use-region-p)
                            "Discard changes in region? "
                          "Discard hunk? "))
       (magit-apply-hunk-item it "--reverse")))
    ([hunk diff staged]
     (cond ((magit-anything-unstaged-p parent-info)
            (user-error "Cannot discard this hunk, file has unstaged changes"))
           ((yes-or-no-p (if (use-region-p)
                             "Discard changes in region? "
                           "Discard hunk? "))
            (magit-apply-hunk-item it "--reverse" "--index"))))
    ([diff unstaged]
     (if (eq diff-status 'unmerged)
         (magit-checkout-stage info (magit-checkout-read-stage info))
       (magit-discard-diff it nil)))
    ([diff staged]
     (if (magit-anything-unstaged-p (magit-section-info it))
         (user-error "Cannot discard this hunk, file has unstaged changes")
       (magit-discard-diff it t)))
    (hunk   (user-error "Can't discard this hunk"))
    (diff   (user-error "Can't discard this diff"))
    (stash  (when (yes-or-no-p "Discard stash? ")
              (magit-stash-drop info)))
    (branch (when (yes-or-no-p
                   (if current-prefix-arg
                       (concat "Force delete branch [" info "]? ")
                     (concat "Delete branch [" info "]? ")))
              (magit-branch-delete info current-prefix-arg)))
    (remote (when (yes-or-no-p "Remove remote? ")
              (magit-remote-remove info)))))

;;;;;; Revert

(defun magit-revert-item ()
  "Revert the item at point.
The change introduced by the item is reversed in the current
working tree."
  (interactive)
  (magit-section-action revert (info)
    ([* unstaged] (magit-discard-item))
    (commit (when (or (not magit-revert-item-confirm)
                      (yes-or-no-p "Revert this commit? "))
              (magit-revert-commit info)))
    (diff   (when (or (not magit-revert-item-confirm)
                      (yes-or-no-p "Revert this diff? "))
              (magit-apply-diff-item it "--reverse")))
    (hunk   (when (or (not magit-revert-item-confirm)
                      (yes-or-no-p "Revert this hunk? "))
              (magit-apply-hunk-item it "--reverse")))))

(defun magit-revert-commit (commit)
  (magit-assert-one-parent commit "revert")
  (magit-run-git "revert" "--no-commit" commit))

(defconst magit-revert-backup-file "magit/reverted.diff")

(defun magit-revert-undo ()
  "Re-apply the previously reverted hunk.
Also see option `magit-revert-backup'."
  (interactive)
  (let ((file (magit-git-dir magit-revert-backup-file)))
    (if (file-readable-p file)
        (magit-run-git "apply" file)
      (user-error "No backups exist"))
    (magit-refresh)))

;;;;; Apply Core

(defun magit-discard-diff (diff stagedp)
  (let ((file (magit-section-info diff)))
    (cl-case (magit-section-diff-status diff)
      (deleted
       (when (yes-or-no-p (format "Resurrect %s? " file))
         (when stagedp
           (magit-run-git "reset" "-q" "--" file))
         (magit-run-git "checkout" "--" file)))
      (new
       (when (yes-or-no-p (format "Delete %s? " file))
         (magit-run-git "rm" "-f" "--" file)))
      (t
       (when (yes-or-no-p (format "Discard changes to %s? " file))
         (if stagedp
             (magit-run-git "checkout" "HEAD" "--" file)
           (magit-run-git "checkout" "--" file)))))))

(defun magit-apply-commit (commit)
  (magit-assert-one-parent commit "cherry-pick")
  (magit-run-git "cherry-pick" "--no-commit" commit))

(defun magit-apply-diff-item (diff &rest args)
  (when (member "-U0" magit-diff-options)
    (setq args (cons "--unidiff-zero" args)))
  (let ((buf (generate-new-buffer " *magit-input*")))
    (unwind-protect
        (progn (magit-insert-diff-item-patch diff buf)
               (magit-run-git-with-input
                buf "apply" args "--ignore-space-change" "-"))
      (kill-buffer buf))))

(defun magit-apply-hunk-item (hunk &rest args)
  "Apply single hunk or part of a hunk to the index or working file.

This function is the core of magit's stage, unstage, apply, and
revert operations.  HUNK (or the portion of it selected by the
region) will be applied to either the index, if \"--cached\" is a
member of ARGS, or to the working file otherwise."
  (when (string-match "^diff --cc" (magit-section-parent-info hunk))
    (user-error (concat "Cannot un-/stage individual resolution hunks.  "
                        "Please stage the whole file.")))
  (let ((use-region (use-region-p)))
    (when (member "-U0" magit-diff-options)
      (setq args (cons "--unidiff-zero" args))
      (when use-region
        (user-error (concat "Not enough context to partially apply hunk.  "
                            "Use `+' to increase context."))))
    (let ((buf (generate-new-buffer " *magit-input*")))
      (unwind-protect
          (progn (if use-region
                     (magit-insert-hunk-item-region-patch
                      hunk (member "--reverse" args)
                      (region-beginning) (region-end) buf)
                   (magit-insert-hunk-item-patch hunk buf))
                 (magit-revert-backup buf args)
                 (magit-run-git-with-input
                  buf "apply" args "--ignore-space-change" "-"))
        (kill-buffer buf)))))

(defun magit-insert-diff-item-patch (diff buf)
  (magit-insert-region (magit-section-content-beginning diff)
                       (magit-section-end diff)
                       buf))

(defun magit-insert-hunk-item-patch (hunk buf)
  (magit-diff-item-insert-header (magit-section-parent hunk) buf)
  (magit-insert-region (magit-section-beginning hunk)
                       (magit-section-end hunk)
                       buf))

(defun magit-insert-hunk-item-region-patch (hunk reverse beg end buf)
  (magit-diff-item-insert-header (magit-section-parent hunk) buf)
  (save-excursion
    (goto-char (magit-section-beginning hunk))
    (magit-insert-current-line buf)
    (forward-line)
    (let ((copy-op (if reverse "+" "-")))
      (while (< (point) (magit-section-end hunk))
        (cond ((and (<= beg (point)) (< (point) end))
               (magit-insert-current-line buf))
              ((looking-at " ")
               (magit-insert-current-line buf))
              ((looking-at copy-op)
               (let ((text (buffer-substring-no-properties
                            (+ (point) 1) (line-beginning-position 2))))
                 (with-current-buffer buf
                   (insert " " text)))))
        (forward-line))))
  (with-current-buffer buf
    (diff-fixup-modifs (point-min) (point-max))))

(defun magit-diff-item-insert-header (diff buf)
  (let ((src (magit-section-diff-file2 diff))
        (dst (magit-section-info diff)))
    (with-current-buffer buf
      (insert (format "diff --git a/%s b/%s\n--- a/%s\n+++ b/%s\n"
                      src dst src dst)))))

(defun magit-insert-region (beg end buf)
  (let ((text (buffer-substring-no-properties beg end)))
    (with-current-buffer buf
      (insert text))))

(defun magit-insert-current-line (buf)
  (let ((text (buffer-substring-no-properties
               (line-beginning-position) (line-beginning-position 2))))
    (with-current-buffer buf
      (insert text))))

(defun magit-revert-backup (buffer args)
  (when (and magit-revert-backup (member "--reverse" args))
    (with-current-buffer buffer
      (let ((buffer-file-name (magit-git-dir magit-revert-backup-file))
            (make-backup-files t)
            (backup-directory-alist nil)
            (version-control t)
            (kept-old-versions 0)
            (kept-new-versions 10))
        (make-directory (file-name-directory buffer-file-name) t)
        (save-buffer 16)))))

;;;; Visit

(defun magit-visit-item (&optional other-window)
  "Visit current item.
With a prefix argument, visit in other window."
  (interactive "P")
  (magit-section-action visit (info parent-info)
    ((diff diffstat [file untracked])
     (magit-visit-file-item info other-window))
    (hunk     (magit-visit-file-item parent-info other-window
                                     (magit-hunk-item-target-line it)
                                     (current-column)))
    (staged   (magit-diff-staged))
    (unstaged (magit-diff-unstaged))
    (unpushed (magit-diff-unpushed))
    (unpulled (magit-diff-unpulled))
    (stash    (magit-diff-stash info))
    (commit   (magit-show-commit info))
    (mcommit  (magit-show-commit info nil parent-info))
    (branch   (magit-checkout info))))

(defun magit-visit-file-item (file &optional other-window line column)
  (unless file
    (user-error "Can't get pathname for this file"))
  (unless (file-exists-p file)
    (user-error "Can't visit deleted file: %s" file))
  (if (file-directory-p file)
      (progn
        (setq file (file-name-as-directory (expand-file-name file)))
        (if (equal (magit-get-top-dir (file-name-directory file))
                   (magit-get-top-dir))
            (magit-dired-jump other-window)
          (magit-status file (if other-window
                                 'pop-to-buffer
                               'switch-to-buffer))))
    (if other-window
        (find-file-other-window file)
      (find-file file))
    (when line
      (goto-char (point-min))
      (forward-line (1- line))
      (when (> column 0)
        (move-to-column (1- column))))))

(defun magit-hunk-item-target-line (hunk)
  (save-excursion
    (beginning-of-line)
    (let ((line (line-number-at-pos)))
      (goto-char (magit-section-beginning hunk))
      (unless (looking-at "@@+ .* \\+\\([0-9]+\\)\\(,[0-9]+\\)? @@+")
        (user-error "Hunk header not found"))
      (let ((target (string-to-number (match-string 1))))
        (forward-line)
        (while (< (line-number-at-pos) line)
          ;; XXX - deal with combined diffs
          (unless (looking-at "-")
            (setq target (+ target 1)))
          (forward-line))
        target))))

;;;###autoload
(defun magit-dired-jump (&optional other-window)
  "Visit current item in dired.
With a prefix argument, visit in other window."
  (interactive "P")
  (require 'dired-x)
  (dired-jump other-window
              (file-truename
               (magit-section-action dired-jump (info parent-info)
                 ([file untracked] info)
                 ((diff diffstat) info)
                 (hunk parent-info)
                 (t default-directory)))))

(defvar-local magit-file-log-file nil)
(defvar-local magit-show-current-version nil)

;;;###autoload
(defun magit-show (rev file &optional switch-function)
  "Display and select a buffer containing FILE as stored in REV.

Insert the contents of FILE as stored in the revision REV into a
buffer.  Then select the buffer using `pop-to-buffer' or with a
prefix argument using `switch-to-buffer'.  Non-interactivity use
SWITCH-FUNCTION to switch to the buffer, if that is nil simply
return the buffer, without displaying it."
  (interactive
   (let ((rev (magit-get-current-branch)) file section)
     (magit-section-case (info parent)
       (commit (setq file magit-file-log-file rev info))
       (hunk   (setq file (magit-section-info parent)))
       (diff   (setq file (magit-section-info it))))
     (setq rev  (magit-read-rev "Retrieve file from revision" rev)
           file (cl-case rev
                  (working (read-file-name "Find file: "))
                  (index   (magit-read-file-from-rev "HEAD" file))
                  (t       (magit-read-file-from-rev rev file))))
     (list rev file (if current-prefix-arg
                        'switch-to-buffer
                      'pop-to-buffer))))
  (let (buffer)
    (if (eq rev 'working)
        (setq buffer (find-file-noselect file))
      (let ((name (format "%s.%s" file
                          (if (symbolp rev)
                              (format "@{%s}" rev)
                            (replace-regexp-in-string "/" ":" rev)))))
        (setq buffer (get-buffer name))
        (when buffer
          (with-current-buffer buffer
            (if (and (equal file magit-file-name)
                     (equal rev  magit-show-current-version))
                (let ((inhibit-read-only t))
                  (erase-buffer))
              (setq buffer nil))))
        (with-current-buffer
            (or buffer (setq buffer (create-file-buffer name)))
          (setq buffer-read-only t)
          (with-silent-modifications
            (if (eq rev 'index)
                (let ((temp (car (split-string
                                  (magit-git-string "checkout-index"
                                                    "--temp" file)
                                  "\t")))
                      (inhibit-read-only t))
                  (insert-file-contents temp nil nil nil t)
                  (delete-file temp))
              (magit-git-insert "cat-file" "-p" (concat rev ":" file))))
          (let ((buffer-file-name (expand-file-name file (magit-get-top-dir))))
            (normal-mode t))
          (setq magit-file-name file)
          (setq magit-show-current-version rev)
          (goto-char (point-min)))))
    (when switch-function
      (with-current-buffer buffer
        (funcall switch-function (current-buffer))))
    buffer))

;;;; Act
;;;;; Init

;;;###autoload
(defun magit-init (directory)
  "Create or reinitialize a Git repository.
Read directory name and initialize it as new Git repository.

If the directory is below an existing repository, then the user
has to confirm that a new one should be created inside; or when
the directory is the root of the existing repository, whether
it should be reinitialized.

Non-interactively DIRECTORY is always (re-)initialized."
  (interactive
   (let* ((dir (file-name-as-directory
                (expand-file-name
                 (read-directory-name "Create repository in: "))))
          (top (magit-get-top-dir dir)))
     (if (and top
              (not (yes-or-no-p
                    (if (string-equal top dir)
                        (format "Reinitialize existing repository %s? " dir)
                      (format "%s is a repository.  Create another in %s? "
                              top dir)))))
         (user-error "Abort")
       dir)))
  (magit-run-git "init" (expand-file-name directory)))

;;;;; Merging

(magit-define-popup magit-merge-popup
  "Popup console for merge commands."
  'magit-popups 'magit-popup-sequence-mode
  :man-page "git-merge"
  :switches '((?f "Fast-forward only" "--ff-only")
              (?n "No fast-forward"   "--no-ff")
              (?s "Squash"            "--squash"))
  :options  '((?s "Strategy" "--strategy=" read-from-minibuffer))
  :actions  '((?m "Merge"                  magit-merge)
              (?e "Merge and edit message" magit-merge-editmsg)
              (?n "Merge but don't commit" magit-merge-nocommit))
  :sequence-actions   '((?a "Abort merge"  magit-merge-abort)
                        (?c "Commit merge" magit-commit))
  :sequence-predicate 'magit-merge-state
  :default-action 'magit-merge)

;;;###autoload
(defun magit-merge (rev &optional args nocommit)
  "Merge commit REV into the current branch; using default message.

Unless there are conflicts or a prefix argument is used create a
merge commit using a generic commit message and without letting
the user inspect the result.  With a prefix argument pretend the
merge failed to give the user the opportunity to inspect the
merge.

\(git merge --no-edit|--no-commit [ARGS] REV)"
  (interactive (list (magit-merge-read-rev)
                     magit-current-popup-args
                     current-prefix-arg))
  (magit-merge-assert)
  (magit-run-git "merge" (if nocommit "--no-commit" "--no-edit") args rev))

;;;###autoload
(defun magit-merge-editmsg (rev &optional args)
  "Merge commit REV into the current branch; and edit message.
Perform the merge and prepare a commit message but let the user
edit it.
\n(git merge --edit [ARGS] rev)"
  (interactive (list (magit-merge-read-rev) magit-current-popup-args))
  (magit-merge-assert)
  (magit-run-git-with-editor "merge" "--edit" args rev))

;;;###autoload
(defun magit-merge-nocommit (rev &optional args)
  "Merge commit REV into the current branch; pretending it failed.
Pretend the merge failed to give the user the opportunity to
inspect the merge and change the commit message.
\n(git merge --no-commit [ARGS] rev)"
  (interactive (list (magit-merge-read-rev) magit-current-popup-args))
  (magit-merge-assert)
  (magit-run-git "merge" "--no-commit" args rev))

;;;###autoload
(defun magit-merge-abort ()
  "Abort the current merge operation.
\n(git merge --abort)"
  (interactive)
  (if (file-exists-p (magit-git-dir "MERGE_HEAD"))
      (when (yes-or-no-p "Abort merge? ")
        (magit-run-git-async "merge" "--abort"))
    (user-error "No merge in progress")))

(defun magit-checkout-stage (file arg &optional restore-conflict)
  "During a conflict checkout and stage side, or restore conflict."
  (interactive
   (let ((default-directory (magit-get-top-dir)))
     (if t ; FIXME conflicts occur in other situations too
         ;; (file-exists-p (magit-git-dir "MERGE_HEAD"))
         (let ((file (magit-completing-read
                      "Checkout file"
                      (magit-git-lines "ls-files")
                      nil nil nil 'magit-read-file-hist
                      (magit-section-case (info)
                        ((diff diffstat [file untracked]) info)))))
           (cond
            ((member file (magit-git-lines "diff" "--name-only"
                                           "--diff-filter=U"))
             (list file (magit-checkout-read-stage file)))
            ((yes-or-no-p (format "Restore conflicts in %s? " file))
             (list file "--merge" t))
            (t
             (user-error "Quit"))))
       (user-error "No merge in progress"))))
  (if restore-conflict
      (with-temp-buffer
        (insert "0 0000000000000000000000000000000000000000\t" file "\n")
        (--> (magit-git-string "ls-tree" (magit-git-string
                                          "merge-base" "MERGE_HEAD" "HEAD")
                               file)
          (replace-regexp-in-string "\t" " 1\t" it)
          (insert it "\n"))
        (--> (magit-git-string "ls-tree" "HEAD" file)
          (replace-regexp-in-string "\t" " 2\t" it)
          (insert it "\n"))
        (--> (magit-git-string "ls-tree" "MERGE_HEAD" file)
          (replace-regexp-in-string "\t" " 3\t" it)
          (insert it "\n"))
        (magit-run-git-with-input (current-buffer) "checkout" arg file))
    (magit-call-git "checkout" arg file)
    (if (string= arg "--merge")
        (magit-refresh)
      (magit-run-git "add" file))))

(defun magit-merge-state ()
  (file-exists-p (magit-git-dir "MERGE_HEAD")))

(defun magit-merge-assert ()
  (or (not (magit-anything-modified-p))
      (not magit-merge-warn-dirty-worktree)
      (yes-or-no-p (concat "Running merge in a dirty worktree "
                           "could cause data loss.  Continue?"))
      (error "Abort")))

(defun magit-merge-read-rev ()
  (magit-read-rev "Merge"
                  (or (magit-guess-branch)
                      (magit-get-previous-branch))))

(defun magit-checkout-read-stage (file)
  (magit-read-char-case (format "For %s checkout: " file) t
    (?o "[o]ur stage"   "--ours")
    (?t "[t]heir stage" "--theirs")
    (?c "[c]onflict"    "--merge")))

;;;;; Branching

(magit-define-popup magit-branch-popup
  "Popup console for branch commands."
  'magit-popups
  :man-page "git-branch"
  :switches '((?t "Set upstream configuration" "--track")
              (?m "Merged to HEAD"             "--merged")
              (?M "Merged to master"           "--merged=master")
              (?n "Not merged to HEAD"         "--no-merged")
              (?N "Not merged to master"       "--no-merged=master"))
  :options  '((?c "Contains"   "--contains="  magit-popup-read-rev)
              (?m "Merged"     "--merged="    magit-popup-read-rev)
              (?n "Not merged" "--no-merged=" magit-popup-read-rev))
  :actions  '((?v "Branch manager" magit-branch-manager)
              (?b "Checkout"       magit-checkout)
              (?c "Create"         magit-branch-and-checkout)
              (?r "Rename"         magit-branch-rename)
              (?k "Delete"         magit-branch-delete))
  :default-action 'magit-checkout)

;;;###autoload
(defun magit-checkout (revision)
  "Switch 'HEAD' to REVISION and update working tree.
Fails if working tree or staging area contain uncommitted changes.
\n(git checkout REVISION)."
  (interactive
   (list (let ((current (magit-get-current-branch))
               (default (or (magit-guess-branch)
                            (magit-get-previous-branch))))
           (magit-read-rev "Checkout"
                           (unless (equal default current) default)
                           current))))
  (magit-save-some-buffers)
  (magit-run-git "checkout" revision))

;;;###autoload
(defun magit-branch-and-checkout (branch parent)
  "Switch 'HEAD' to new BRANCH at revision PARENT and update working tree.
Fails if working tree or staging area contain uncommitted changes.
\n(git checkout ARGS -b BRANCH PARENT)."
  (interactive
   (list (read-string "Create and checkout branch: ")
         (magit-read-rev "Parent" (or (magit-guess-branch)
                                      (magit-get-current-branch)))))
  (cond ((run-hook-with-args-until-success
          'magit-branch-create-hook branch parent))
        ((and branch (not (string= branch "")))
         (magit-save-some-buffers)
         (magit-run-git "checkout" magit-current-popup-args
                        "-b" branch parent))))

;;;###autoload
(defun magit-branch-delete (branch &optional force)
  "Delete the BRANCH.
If the branch is the current one, offers to switch to `master'
first.  With prefix, forces the removal even if it hasn't been
merged.  Works with local and remote branches.
\n(git branch -d|-D BRANCH || git push REMOTE :refs/heads/BRANCH)."
  (interactive (list (magit-read-rev "Branch to delete"
                                     (or (magit-guess-branch)
                                         (magit-get-previous-branch)))
                     current-prefix-arg))
  (if (string-match "^\\(?:refs/\\)?remotes/\\([^/]+\\)/\\(.+\\)" branch)
      (magit-run-git-async "push"
                           (match-string 1 branch)
                           (concat ":" (match-string 2 branch)))
    (let* ((current (magit-get-current-branch))
           (is-current (string= branch current))
           (is-master (string= branch "master"))
           (args (list "branch" (if force "-D" "-d") branch)))
      (cond
       ((and is-current is-master)
        (message "Cannot delete master branch while it's checked out."))
       (is-current
        (if (y-or-n-p "Cannot delete current branch.  Switch to master first? ")
            (progn
              (magit-checkout "master")
              (magit-run-git args))
          (message "The current branch was not deleted.")))
       (t
        (magit-run-git args))))))

;;;###autoload
(defun magit-branch-edit-description (branch)
  "Edit the description of BRANCH."
  (interactive (list (magit-read-rev "Edit branch description"
                                     (or (magit-guess-branch)
                                         (magit-get-current-branch)))))
  (magit-run-git-with-editor "branch" "--edit-description"))

;;;###autoload
(defun magit-branch-rename (old new &optional force)
  "Rename branch OLD to NEW.
With prefix, forces the rename even if NEW already exists.
\n(git branch -m|-M OLD NEW)."
  (interactive
   (let* ((old (magit-read-rev-with-default "Old name"))
          (new (read-string "New name: " old)))
     (list old new current-prefix-arg)))
  (if (or (null new) (string= new "")
          (string= old new))
      (message "Cannot rename branch \"%s\" to \"%s\"." old new)
    (magit-run-git "branch" (if force "-M" "-m") old new)))

(defun magit-guess-branch ()
  "Return a branch name depending on the context of cursor.
If no branch is found near the cursor return nil."
  (magit-section-case (info parent-info)
    (branch          info)
    ([commit wazzup] parent-info)
    ([commit       ] (magit-get-shortname info))
    ([       wazzup] info)))

;;;;; Remoting

(magit-define-popup magit-remote-popup
  "Popup console for remote commands."
  'magit-popups
  :man-page "git-remote"
  :actions  '((?v "Remote manager" magit-branch-manager)
              (?a "Add"            magit-remote-add)
              (?r "Rename"         magit-remote-rename)
              (?k "Remove"         magit-remote-remove))
  :default-action 'magit-branch-manager)

;;;###autoload
(defun magit-remote-add (remote url)
  "Add the REMOTE and fetch it.
\n(git remote add -f REMOTE URL)."
  (interactive (list (read-string "Remote name: ")
                     (read-string "Remote url: ")))
  (magit-run-git-async "remote" "add" "-f" remote url))

;;;###autoload
(defun magit-remote-remove (remote)
  "Delete the REMOTE.
\n(git remote rm REMOTE)."
  (interactive (list (magit-read-remote "Delete remote")))
  (magit-run-git "remote" "rm" remote))

;;;###autoload
(defun magit-remote-rename (old new)
  "Rename remote OLD to NEW.
\n(git remote rename OLD NEW)."
  (interactive
   (let* ((old (magit-read-remote "Old name"))
          (new (read-string "New name: " old)))
     (list old new)))
  (if (or (null old) (string= old "")
          (null new) (string= new "")
          (string= old new))
      (message "Cannot rename remote \"%s\" to \"%s\"." old new)
    (magit-run-git "remote" "rename" old new)))

(defun magit-guess-remote ()
  (magit-section-case (info parent-info)
    (remote info)
    (branch parent-info)
    (t      (if (string= info ".") info (magit-get-current-remote)))))

;;;;; Rebasing

(magit-define-popup magit-rebase-popup
  "Key menu for rebasing."
  'magit 'magit-popup-sequence-mode
  :man-page "git-rebase"
  :switches '((?k "Keep empty commits" "--keep-empty")
              (?p "Preserve merges" "--preserve-merges")
              (?c "Lie about author date" "--committer-date-is-author-date")
              (?a "Autosquash" "--autosquash")
              (?A "Autostash" "--autostash"))
  :actions  '((?r "Rebase"      magit-rebase)
              (?o "Rebase onto" magit-rebase-onto)
              (?e "Interactive" magit-rebase-interactive)
              (?f "Autosquash"  magit-rebase-autosquash))
  :sequence-actions '((?r "Continue" magit-rebase-continue)
                      (?s "Skip"     magit-rebase-skip)
                      (?e "Edit"     magit-rebase-edit)
                      (?a "Abort"    magit-rebase-abort))
  :sequence-predicate 'magit-rebase-in-progress-p)

;;;###autoload
(defun magit-rebase (upstream &optional args)
  "Start an non-interactive rebase operation.
\n(git rebase UPSTREAM[^] [ARGS])"
  (interactive
   (if (magit-rebase-in-progress-p)
       (list nil)
     (let ((branch (magit-get-current-branch)))
       (list (magit-read-rev
              "Rebase to" (magit-get-tracked-branch branch) branch)
             magit-current-popup-args))))
  (if upstream
      (progn (message "Rebasing...")
             (magit-run-git "rebase" upstream)
             (message "Rebasing...done"))
    (magit-log-select
      `(lambda (commit)
         (magit-rebase (concat commit "^") (list ,@args))))))

;;;###autoload
(defun magit-rebase-onto (newbase upstream &optional args)
  "Start an non-interactive rebase operation, using `--onto'.
\n(git rebase --onto NEWBASE UPSTREAM[^] [ARGS])"
  (interactive (list (magit-read-rev "Rebase onto") nil))
  (if upstream
      (progn (message "Rebasing...")
             (magit-run-git "rebase" "--onto" newbase upstream args)
             (message "Rebasing...done"))
    (magit-log-select
      `(lambda (commit)
         (magit-rebase-onto ,newbase (concat commit "^") ,args)))))

;;;###autoload
(defun magit-rebase-interactive (commit &optional args)
  "Start an interactive rebase operation.
\n(git rebase -i COMMIT[^] [ARGS])"
  (interactive (let ((commit (magit-section-case (info) (commit info))))
                 (list (and commit (concat commit "^"))
                       magit-current-popup-args)))
  (cond
   ((or (not with-editor-emacsclient-executable)
        (tramp-tramp-file-p default-directory))
    (error "Implementation does not handle remote (tramp) repositories"))
   ((magit-rebase-in-progress-p)
    (magit-rebase-popup))
   ((setq commit (magit-rebase-interactive-assert commit))
    (magit-rebase-async "-i" commit args))
   (t
    (magit-log-select
      `(lambda (commit)
         (magit-rebase-interactive (concat commit "^") (list ,@args)))))))

;;;###autoload
(defun magit-rebase-autosquash (commit &optional args)
  "Combine squash and fixup commits with their intended targets.
\n(git rebase -i COMMIT[^] --autosquash --autostash [ARGS])"
  (interactive (list (magit-get-tracked-branch) magit-current-popup-args))
  (if (setq commit (magit-rebase-interactive-assert commit))
      (let ((process-environment process-environment))
        (setenv "GIT_SEQUENCE_EDITOR" magit-success-executable)
        (magit-rebase-async "-i" commit "--autosquash" "--autostash" args))
    (magit-log-select
      `(lambda (commit)
         (magit-rebase-autosquash (concat commit "^") (list ,@args))))))

;;;###autoload
(defun magit-rebase-continue ()
  "Restart the current rebasing operation."
  (interactive)
  (if (magit-rebase-in-progress-p)
      (if (magit-anything-unstaged-p)
          (error "Cannot continue rebase with unstaged changes")
        (magit-rebase-async "--continue"))
    (error "No rebase in progress")))

;;;###autoload
(defun magit-rebase-skip ()
  "Skip the current commit and restart the current rebase operation."
  (interactive)
  (if (magit-rebase-in-progress-p)
      (magit-rebase-async "--skip")
    (error "No rebase in progress")))

;;;###autoload
(defun magit-rebase-edit ()
  "Edit the todo list of the current rebase operation."
  (interactive)
  (if (magit-rebase-in-progress-p)
      (magit-rebase-async "--edit-todo")
    (error "No rebase in progress")))

;;;###autoload
(defun magit-rebase-abort ()
  "Abort the current rebase operation, restoring the original branch."
  (interactive)
  (if (magit-rebase-in-progress-p)
      (magit-run-git "rebase" "--abort")
    (error "No rebase in progress")))

(defun magit-rebase-async (&rest args)
  (magit-server-visit-args 'rebase)
  (apply #'magit-run-git-with-editor "rebase" args))

(defun magit-rebase-interactive-assert (commit)
  (when commit
    (if (magit-git-lines "rev-list" "--merges" (concat commit "..HEAD"))
        (magit-read-char-case "Proceed despite merge in rebase range?  " nil
          (?c "[c]ontinue" commit)
          (?p "[s]elect other" nil)
          (?a "[a]bort" (user-error "Quit")))
      commit)))

(defun magit-rebase-in-progress-p ()
  "Return t if a rebase is in progress."
  (or (file-directory-p (magit-git-dir "rebase-merge"))
      (file-directory-p (magit-git-dir "rebase-apply"))))

(defun magit-rebase-info ()
  "Return a list indicating the state of an in-progress rebase.

The returned list has the form (ONTO DONE TOTAL STOPPED AM).
ONTO is the commit being rebased onto.
DONE and TOTAL are integers with obvious meanings.
STOPPED is the SHA-1 of the commit at which rebase stopped.
AM is non-nil if the current rebase is actually a git-am.

Return nil if there is no rebase in progress."
  (let ((m (magit-git-dir "rebase-merge"))
        (a (magit-git-dir "rebase-apply")))
    (cond
     ((file-directory-p m) ; interactive
      (list
       (magit-get-shortname (magit-file-line  (expand-file-name "onto" m)))
       (length              (magit-file-lines (expand-file-name "done" m)))
       (cl-loop for line in (magit-file-lines
                             (expand-file-name "git-rebase-todo.backup" m))
                count (string-match "^[^#\n]" line))
       (magit-file-line (expand-file-name "stopped-sha" m))
       nil))

     ((file-regular-p (expand-file-name "onto" a)) ; non-interactive
      (list
       (magit-get-shortname  (magit-file-line (expand-file-name "onto" a)))
       (1- (string-to-number (magit-file-line (expand-file-name "next" a))))
       (string-to-number     (magit-file-line (expand-file-name "last" a)))
       (let ((patch-header (magit-file-line
                            (car (directory-files a t "^[0-9]\\{4\\}$")))))
         (when (string-match "^From \\([a-z0-9]\\{40\\}\\) " patch-header)
           (match-string 1 patch-header)))
       nil))

     ((file-regular-p (expand-file-name "applying" a)) ; am
      (list
       (magit-get-shortname  "HEAD")
       (1- (string-to-number (magit-file-line (expand-file-name "next" a))))
       (string-to-number     (magit-file-line (expand-file-name "last" a)))
       (let ((patch-header (magit-file-line
                            (car (directory-files a t "^[0-9]\\{4\\}$")))))
         (when (string-match "^From \\([a-z0-9]\\{40\\}\\) " patch-header)
           (match-string 1 patch-header)))
       t)))))

;;;;; AM

(magit-define-popup magit-am-popup
  "Popup console for mailbox commands."
  'magit-popups
  :man-page "git-am"
  :switches '((?s "add a Signed-off-by line to the commit message" "--signoff")
              (?3 "allow fall back on 3way merging if needed" "--3way")
              (?k "pass -k flag to git-mailinfo" "--keep")
              (?c "strip everything before a scissors line" "--scissors")
              (?p "pass it through git-apply" "-p")
              (?r "override error message when patch failure occurs" "--resolvemsg")
              (?d "lie about committer date" "--committer-date-is-author-date")
              (?D "use current timestamp for author date" "--ignore-date")
              (?b "pass -b flag to git-mailinfo" "--keep-non-patch"))
  :options  '((?p "format the patch(es) are in" "--patch-format"
                  magit-popup-read-file-name))
  :actions  '((?J "Apply Mailbox" magit-apply-mailbox)))

(defun magit-apply-mailbox (&optional file-or-dir)
  "Apply a series of patches from a mailbox."
  (interactive "fmbox or Maildir file or directory: ")
  (magit-run-git-with-editor "am" file-or-dir))

;;;;; Reset

;;;###autoload
(defun magit-reset-head (revision &optional hard)
  "Switch `HEAD' to REVISION, keeping prior working tree and staging area.
Any differences from REVISION become new changes to be committed.
With prefix argument, all uncommitted changes in working tree
and staging area are lost.
\n(git reset --soft|--hard REVISION)"
  (interactive (list (magit-read-rev (format "%s head to"
                                             (if current-prefix-arg
                                                 "Hard reset"
                                               "Reset"))
                                     (or (magit-guess-branch) "HEAD"))
                     current-prefix-arg))
  (magit-run-git "reset" (if hard "--hard" "--soft") revision "--"))

;;;###autoload
(defun magit-reset-head-hard (revision)
  "Switch `HEAD' to REVISION, losing all changes.
Uncomitted changes in both working tree and staging area are lost.
\n(git reset --hard REVISION)"
  (interactive (list (magit-read-rev (format "Hard reset head to")
                                     (or (magit-guess-branch) "HEAD"))))
  (magit-reset-head revision t))

;;;;; Clean

;;;###autoload
(defun magit-clean (&optional arg)
  "Remove untracked files from the working tree.
With a prefix argument also remove ignored files,
with two prefix arguments remove ignored files only.
\n(git clean -f -d [-x|-X])"
  (interactive "p")
  (when (yes-or-no-p (format "Remove %s files? "
                             (cl-case arg
                               (1 "untracked")
                               (4 "untracked and ignored")
                               (t "ignored"))))
    (magit-run-git "clean" "-f" "-d" (cl-case arg (4 "-x") (16 "-X")))))

;;;;; Fetching

(magit-define-popup magit-fetch-popup
  "Popup console for fetch commands."
  'magit-popups
  :man-page "git-fetch"
  :switches '((?p "Prune"   "--prune"))
  :actions  '((?f "Current" magit-fetch-current)
              (?o "Other"   magit-fetch)
              (?a "All"     magit-remote-update))
  :default-action 'magit-fetch-current)

;;;###autoload
(defun magit-fetch (remote &optional args)
  "Fetch from REMOTE."
  (interactive (list (magit-read-remote "Fetch remote")
                     magit-current-popup-args))
  (magit-run-git-async "fetch" remote args))

;;;###autoload
(defun magit-fetch-current (&optional args)
  "Fetch for the default remote.
If there is no default remote, ask for one."
  (interactive (list magit-current-popup-args))
  (magit-fetch (or (magit-get-current-remote)
                   (magit-read-remote "Fetch remote"))
               args))

;;;###autoload
(defun magit-remote-update ()
  "Update all remotes."
  (interactive)
  (or (run-hook-with-args-until-success 'magit-remote-update-hook)
      (magit-run-git-async "remote" "update" magit-current-popup-args)))

;;;;; Pulling

(magit-define-popup magit-pull-popup
  "Popup console for pull commands."
  'magit-popups
  :man-page "git-pull"
  :switches '((?f "Force"  "--force")
              (?r "Rebase" "--rebase"))
  :actions  '((?F "Pull"   magit-pull))
  :default-action 'magit-pull)

;;;###autoload
(defun magit-pull ()
  "Run git pull.

If there is no default remote, the user is prompted for one and
its values is saved with git config.  If there is no default
merge branch, the user is prompted for one and its values is
saved with git config.  With a prefix argument, the default
remote is not used and the user is prompted for a remote.  With
two prefix arguments, the default merge branch is not used and
the user is prompted for a merge branch.  Values entered by the
user because of prefix arguments are not saved with git config."
  (interactive)
  (or (run-hook-with-args-until-success 'magit-pull-hook)
      (let* ((branch (magit-get-current-branch))
             (branch-remote (magit-get-remote branch))
             (branch-merge (magit-get "branch" branch "merge"))
             (branch-merge-name (and branch-merge
                                     (save-match-data
                                       (string-match "^refs/heads/\\(.+\\)" branch-merge)
                                       (match-string 1 branch-merge))))
             (choose-remote (>= (prefix-numeric-value current-prefix-arg) 4))
             (choose-branch (>= (prefix-numeric-value current-prefix-arg) 16))
             (remote-needed (or choose-remote
                                (not branch-remote)))
             (branch-needed (or choose-branch
                                (not branch-merge-name)))
             (chosen-branch-remote
              (if remote-needed
                  (magit-read-remote "Pull from remote" branch-remote)
                branch-remote))
             (chosen-branch-merge-name
              (if branch-needed
                  (magit-read-remote-branch (format "Pull branch from remote %s"
                                                    chosen-branch-remote)
                                            chosen-branch-remote)
                branch-merge-name)))
        (when (and (not branch-remote)
                   (not choose-remote))
          (magit-set chosen-branch-remote "branch" branch "remote"))
        (when (and (not branch-merge-name)
                   (not choose-branch))
          (magit-set (format "%s" chosen-branch-merge-name)
                     "branch" branch "merge"))
        (magit-run-git-async
         "pull" magit-current-popup-args
         (and choose-remote chosen-branch-remote)
         (and (or choose-remote choose-branch)
              (list (format "refs/heads/%s:refs/remotes/%s/%s"
                            chosen-branch-merge-name
                            chosen-branch-remote
                            chosen-branch-merge-name)))))))

;;;;; Pushing

(magit-define-popup magit-push-popup
  "Popup console for push commands."
  'magit-popups
  :man-page "git-push"
  :switches '((?f "Force"         "--force")
              (?h "Disable hooks" "--no-verify")
              (?d "Dry run"       "-n")
              (?u "Set upstream"  "-u"))
  :actions  '((?P "Push"          magit-push)
              (?t "Push tags"     magit-push-tags))
  :default-action 'magit-push)

;;;###autoload
(defun magit-push-tags ()
  "Push tags to a remote repository.

Push tags to the current branch's remote.  If that isn't set push
to \"origin\" or if that remote doesn't exit but only a single
remote is defined use that.  Otherwise or with a prefix argument
ask the user what remote to use."
  (interactive)
  (let* ((branch  (magit-get-current-branch))
         (remotes (magit-git-lines "remote"))
         (remote  (or (and branch (magit-get-remote branch))
                      (car (member  "origin" remotes))
                      (and (= (length remotes) 1)
                           (car remotes)))))
    (when (or current-prefix-arg (not remote))
      (setq remote (magit-read-remote "Push to remote")))
    (magit-run-git-async "push" remote "--tags")))

;;;###autoload
(defun magit-push ()
  "Push the current branch to a remote repository.

This command runs the `magit-push-remote' hook.  By default that
means running `magit-push-dwim'.  So unless you have customized
the hook this command behaves like this:

With a single prefix argument ask the user what branch to push
to.  With two or more prefix arguments also ask the user what
remote to push to.  Otherwise use the remote and branch as
configured using the Git variables `branch.<name>.remote' and
`branch.<name>.merge'.  If the former is undefined ask the user.
If the latter is undefined push without specifing the remote
branch explicitly.

Also see option `magit-set-upstream-on-push'."
  (interactive)
  (run-hook-with-args-until-success 'magit-push-hook current-prefix-arg))

(defun magit-push-dwim (arg)
  "Push the current branch to a remote repository.

With a single prefix argument ask the user what remote to push
to.  With two or more prefix arguments also ask the user the
name of the remote branch to push to.

Otherwise use the remote and branch as configured using the
Git variables `branch.<name>.remote' and `branch.<name>.merge'.
If the former is undefined ask the user.  If the latter is
undefined push without specifing the remote branch explicitly.

Also see option `magit-set-upstream-on-push'."
  (interactive "P")
  (let* ((branch (or (magit-get-current-branch)
                     (user-error "Don't push a detached head.  That's gross")))
         (auto-remote (magit-get-remote branch))
         (used-remote (if (or arg (not auto-remote))
                          (magit-read-remote
                           (format "Push %s to remote" branch) auto-remote)
                        auto-remote))
         (auto-branch (and (equal used-remote auto-remote)
                           (magit-get "branch" branch "merge")))
         (used-branch (if (>= (prefix-numeric-value arg) 16)
                          (magit-read-remote-branch
                           (format "Push %s as branch" branch)
                           used-remote auto-branch)
                        auto-branch)))
    (cond ;; Pushing to what's already configured.
          ((and auto-branch
                (equal auto-branch used-branch)
                (equal auto-remote used-remote)))
          ;; Setting upstream because of magit-current-popup-args.
          ((member "-u" magit-current-popup-args))
          ;; Two prefix arguments; ignore magit-set-upstream-on-push.
          ((>= (prefix-numeric-value arg) 16)
           (and (yes-or-no-p "Set upstream while pushing? ")
                (setq magit-current-popup-args
                      (cons "-u" magit-current-popup-args))))
          ;; Else honor magit-set-upstream-on-push.
          ((eq magit-set-upstream-on-push 'refuse)
           (user-error "Not pushing since no upstream has been set."))
          ((or (eq magit-set-upstream-on-push 'dontask)
               (and (eq magit-set-upstream-on-push t)
                    (yes-or-no-p "Set upstream while pushing? ")))
           (setq magit-current-popup-args (cons "-u" magit-current-popup-args))))
    (magit-run-git-async
     "push" "-v" used-remote
     (if used-branch (format "%s:%s" branch used-branch) branch)
     magit-current-popup-args)))

;;;;; Committing

(with-no-warnings ; quiet 24.3.50 byte-compiler
(magit-define-popup magit-commit-popup
  "Popup console for commit commands."
  'magit-popups
  :man-page "git-commit"
  :switches '((?a "Stage all modified and deleted files"   "--all")
              (?e "Allow empty commit"                     "--allow-empty")
              (?v "Show diff of changes to be committed"   "--verbose")
              (?n "Bypass git hooks"                       "--no-verify")
              (?s "Add Signed-off-by line"                 "--signoff")
              (?R "Claim authorship and reset author date" "--reset-author"))
  :options  '((?A "Override the author"  "--author="        read-from-minibuffer)
              (?S "Sign using gpg"       "--gpg-sign="      magit-read-gpg-secret-key)
              (?C "Reuse commit message" "--reuse-message=" read-from-minibuffer))
  :actions  '((?c "Commit"         magit-commit)
              (?e "Extend"         magit-commit-extend)
              (?f "Fixup"          magit-commit-fixup)
              (?F "Instant Fixup"  magit-commit-instant-fixup)
              (?a "Amend"          magit-commit-amend)
              (?r "Reword"         magit-commit-reword)
              (?s "Squash"         magit-commit-squash)
              (?S "Instant Squash" magit-commit-instant-squash))
  :max-action-columns 4
  :default-action 'magit-commit))

(defadvice magit-commit-popup (around pop-to-ongoing activate)
  (--if-let (magit-commit-log-buffer) (switch-to-buffer it) ad-do-it))

;;;###autoload
(defun magit-commit (&optional args)
  "Create a new commit on HEAD.
With a prefix argument amend to the commit at HEAD instead.
\n(git commit [--amend] ARGS)"
  (interactive (if current-prefix-arg
                   (list (cons "--amend" magit-current-popup-args))
                 (list magit-current-popup-args)))
  (when (setq args (magit-commit-assert args))
    (magit-commit-async 'magit-diff-staged args)))

;;;###autoload
(defun magit-commit-amend (&optional args)
  "Amend the last commit.
\n(git commit --amend ARGS)"
  (interactive (list magit-current-popup-args))
  (magit-commit-async 'magit-diff-while-amending "--amend" args))

;;;###autoload
(defun magit-commit-extend (&optional args override-date)
  "Amend the last commit, without editing the message.
With a prefix argument do change the committer date, otherwise
don't.  The option `magit-commit-extend-override-date' can be
used to inverse the meaning of the prefix argument.
\n(git commit --amend --no-edit)"
  (interactive (list magit-current-popup-args
                     (if current-prefix-arg
                         (not magit-commit-reword-override-date)
                       magit-commit-reword-override-date)))
  (when (setq args (magit-commit-assert args (not override-date)))
    (let ((process-environment process-environment))
      (unless override-date
        (setenv "GIT_COMMITTER_DATE" (magit-rev-format "%cd")))
      (magit-commit-async nil "--amend" "--no-edit" args))))

;;;###autoload
(defun magit-commit-reword (&optional args override-date)
  "Reword the last commit, ignoring staged changes.

With a prefix argument do change the committer date, otherwise
don't.  The option `magit-commit-rewrite-override-date' can be
used to inverse the meaning of the prefix argument.

Non-interactively respect the optional OVERRIDE-DATE argument
and ignore the option.
\n(git commit --amend --only)"
  (interactive (list magit-current-popup-args
                     (if current-prefix-arg
                         (not magit-commit-reword-override-date)
                       magit-commit-reword-override-date)))
  (let ((process-environment process-environment))
    (unless override-date
      (setenv "GIT_COMMITTER_DATE" (magit-rev-format "%cd")))
    (magit-commit-async 'magit-diff-while-amending
                        "--amend" "--only" args)))

;;;###autoload
(defun magit-commit-fixup (&optional commit args confirm)
  "Create a fixup commit.
With a prefix argument the target commit has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.
\n(git commit --no-edit --fixup=COMMIT [ARGS])"
  (interactive (magit-commit-squash-read-args))
  (magit-commit-squash-internal 'magit-commit-fixup "--fixup"
                                commit args confirm))

;;;###autoload
(defun magit-commit-squash (&optional commit args confirm)
  "Create a squash commit.
With a prefix argument the target commit has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.
\n(git commit --no-edit --squash=COMMIT [ARGS])"
  (interactive (magit-commit-squash-read-args))
  (magit-commit-squash-internal 'magit-commit-squash "--squash"
                                commit args confirm))

;;;###autoload
(defun magit-commit-instant-fixup (&optional commit args)
  "Create a fixup commit and instantly rebase.
\n(git commit --no-edit --fixup=COMMIT ARGS;
 git rebase -i COMMIT^ --autosquash --autostash)"
  (interactive (list (magit-current-commit) magit-current-popup-args))
  (magit-commit-squash-internal
   (lambda (c a)
     (when (setq c (magit-commit-fixup c a))
       (magit-rebase-autosquash (concat c "^"))))
   "--fixup" commit args t))

;;;###autoload
(defun magit-commit-instant-squash (&optional commit args)
  "Create a squash commit and instantly rebase.
\n(git commit --no-edit --squash=COMMIT ARGS;
 git rebase -i COMMIT^ --autosquash --autostash)"
  (interactive (list (magit-current-commit) magit-current-popup-args))
  (magit-commit-squash-internal
   (lambda (c a)
     (when (setq c (magit-commit-squash c a))
       (magit-rebase-autosquash (concat c "^"))))
   "--squash" commit args t))

(defun magit-commit-squash-read-args ()
  (list (magit-current-commit) magit-current-popup-args
        (or current-prefix-arg magit-commit-squash-confirm)))

(defun magit-commit-squash-internal (fn option commit args confirm)
  (when (setq args (magit-commit-assert args t))
    (if (and commit (not confirm))
        (let ((magit-diff-auto-show nil))
          (magit-commit-async 'magit-diff-staged "--no-edit"
                              (concat option "=" commit) args)
          commit)
      (magit-log-select
        `(lambda (commit) (,fn commit (list ,@args))))
      (when (magit-diff-auto-show-p 'log-select)
        (let ((magit-diff-switch-buffer-function 'display-buffer))
          (magit-diff-staged))))))

(defun magit-commit-assert (args &optional strict)
  (cond
   ((or (magit-anything-staged-p)
        (and (magit-anything-unstaged-p)
             ;; ^ Everything of nothing is still nothing.
             (member "--all" args))
        (and (not strict)
             ;; ^ For amend variants that don't make sense otherwise.
             (or (member "--amend" args)
                 (member "--allow-empty" args))))
    (or args (list "--")))
   ((and (magit-rebase-in-progress-p)
         (y-or-n-p "Nothing staged.  Continue in-progress rebase? "))
    (magit-commit-async "--continue")
    nil)
   (magit-commit-ask-to-stage
    (when (magit-diff-auto-show-p 'stage-all)
      (magit-diff-unstaged))
    (prog1 (when (y-or-n-p "Nothing staged.  Stage and commit everything? ")
             (magit-run-git "add" "-u" ".")
             (or args (list "--")))
      (when (and (magit-diff-auto-show-p 'stage-all)
                 (derived-mode-p 'magit-diff-mode))
        (magit-mode-quit-window))))
   (t
    (user-error "Nothing staged"))))

(defun magit-commit-async (diff-fn &rest args)
  (magit-server-visit-args 'commit t args)
  (when (and diff-fn (magit-diff-auto-show-p 'commit))
    (let ((magit-inhibit-save-previous-winconf t))
      (funcall diff-fn)))
  (if (and with-editor-emacsclient-executable
           (not (tramp-tramp-file-p default-directory)))
      (apply #'magit-run-git-with-editor "commit" args)
    (magit-commit-fallback "commit" (magit-flatten-onelevel args))))

(defun magit-commit-fallback (subcmd args)
  (let ((topdir (magit-get-top-dir))
        (editmsg (magit-git-dir (if (equal subcmd "tag")
                                    "TAG_EDITMSG"
                                  "COMMIT_EDITMSG"))))
    (when (and (member "--amend" args)
               (not (file-exists-p editmsg)))
      (with-temp-file editmsg
        (magit-rev-format "%B")))
    (with-current-buffer (find-file-noselect editmsg)
      (pop-to-buffer (current-buffer))
      (add-hook 'with-editor-finish-noclient-hook
                (apply-partially
                 (lambda (default-directory editmsg args)
                   (magit-run-git args)
                   (ignore-errors (delete-file editmsg)))
                 topdir editmsg
                 `(,subcmd
                   ,"--cleanup=strip"
                   ,(concat "--file=" (file-relative-name
                                       (buffer-file-name)
                                       topdir))
                   ,@args))
                nil t))))

(defun magit-commit-add-log ()
  "Add a template for the current hunk to the commit message buffer."
  (interactive)
  (let* ((section (magit-current-section))
         (fun (if (eq (magit-section-type section) 'hunk)
                  (save-window-excursion
                    (save-excursion
                      (magit-visit-item)
                      (add-log-current-defun)))
                nil))
         (file (magit-section-info
                (cl-case (magit-section-type section)
                  (hunk (magit-section-parent section))
                  (diff section)
                  (t    (user-error "No change at point")))))
         (buffer (magit-commit-log-buffer)))
    (unless buffer
      (magit-commit)
      (while (not (setq buffer (magit-commit-log-buffer)))
        (sit-for 0.01)))
    (pop-to-buffer buffer)
    (goto-char (point-min))
    (cond ((not (re-search-forward (format "^\\* %s" (regexp-quote file))
                                   nil t))
           ;; No entry for file, create it.
           (goto-char (point-max))
           (forward-comment -1000)
           (unless (or (bobp) (looking-back "\\(\\*[^\n]+\\|\n\\)"))
             (insert "\n"))
           (insert (format "\n* %s" file))
           (when fun
             (insert (format " (%s)" fun)))
           (insert ": "))
          (fun
           ;; found entry for file, look for fun
           (let ((limit (save-excursion
                          (or (and (re-search-forward "^\\* " nil t)
                                   (match-beginning 0))
                              (progn (point-max)
                                     (forward-comment -1000)
                                     (point))))))
             (cond ((re-search-forward
                     (format "(.*\\<%s\\>.*):" (regexp-quote fun))
                     limit t)
                    ;; found it, goto end of current entry
                    (if (re-search-forward "^(" limit t)
                        (backward-char 2)
                      (goto-char limit)))
                   (t
                    ;; not found, insert new entry
                    (goto-char limit)
                    (if (bolp)
                        (open-line 1)
                      (newline))
                    (insert (format "(%s): " fun))))))
          (t
           ;; found entry for file, look for beginning  it
           (when (looking-at ":")
             (forward-char 2))))))

;;;;; Tagging

(magit-define-popup magit-tag-popup
  "Popup console for tag commands."
  'magit-popups
  :man-page "git-tag"
  :switches '((?a "Annotate" "--annotate")
              (?f "Force"    "--force")
              (?s "Sign"     "--sign"))
  :actions  '((?t "Create"   magit-tag)
              (?k "Delete"   magit-tag-delete))
  :default-action 'magit-tag)

;;;###autoload
(defun magit-tag (name rev &optional annotate)
  "Create a new tag with the given NAME at REV.
With a prefix argument annotate the tag.
\n(git tag [--annotate] NAME REV)"
  (interactive (list (magit-read-tag "Tag name")
                     (magit-read-rev "Place tag on"
                                     (or (magit-guess-branch) "HEAD"))
                     current-prefix-arg))
  (let ((args (append magit-current-popup-args (list name rev))))
    (if (or (member "--sign" args)
            (member "--annotate" args)
            (and annotate (setq args (cons "--annotate" args))))
        (magit-commit-fallback "tag" args)
      (magit-run-git "tag" args))))

;;;###autoload
(defun magit-tag-delete (name)
  "Delete the tag with the given NAME.
\n(git tag -d NAME)"
  (interactive (list (magit-read-tag "Delete Tag" t)))
  (magit-run-git "tag" "-d" name))

;;;;; Stashing

(magit-define-popup magit-stash-popup
  "Popup console for stash commands."
  'magit-popups
  :man-page "git-stash"
  :switches '((?k "Don't stash index"       "--keep-index")
              (?i "Reinstate stashed index" "--index")
              (?u "Include untracked files" "--include-untracked")
              (?a "Include all files"       "--all"))
  :actions  '((?z "Save"           magit-stash)
              (?s "Snapshot"       magit-stash-snapshot)
              (?p "Pop"            magit-stash-pop)
              (?k "Drop"           magit-stash-drop)
              (?Z "Save index"     magit-stash-index)
              (?S "Snapshot index" magit-stash-snapshot)
              (?a "Apply"          magit-stash-apply)
              (?b "Branch"         magit-stash-branch)
              (?v "View"           magit-diff-stash))
  :default-arguments '("--index")
  :default-action 'magit-stash
  :max-action-columns 4)

;;;###autoload
(defun magit-stash (description &optional args)
  "Create new stash of working tree and staging area named DESCRIPTION.
Working tree and staging area revert to the current `HEAD'.
With prefix argument, changes in staging area are kept.
\n(git stash save [ARGS] DESCRIPTION)"
  (interactive (list (read-string "Stash description: ")
                     (magit-current-popup-args :not "--index")))
  (magit-run-git "stash" "save" args "--" description))

;;;###autoload
(defun magit-stash-snapshot (&optional args)
  "Create new stash of working tree and staging area; keep changes in place.
\n(git stash save [ARGS] \"Snapshot...\";
 git stash apply stash@{0})"
  (interactive (list (magit-current-popup-args :not "--index")))
  (magit-call-git "stash" "save" args (magit-stash-format-snapshot-message))
  (magit-stash-apply 0 "--index"))

(defun magit-stash-index (message &optional snapshot)
  "Create a new stash of the index only."
  (interactive (list (read-string "Stash message: ")))
  (magit-git-string "stash" "save" "--keep-index" (concat "(" message ")"))
  (let ((both (magit-rev-parse "refs/stash")))
    (message "Saved working directory and index state in %s" both)
    (magit-call-git "stash" "drop")
    (magit-call-git "stash" "save" message)
    (if snapshot
        (magit-run-git "stash" "pop" "--index" both)
      (with-temp-buffer
        (magit-git-insert "diff" (concat "stash@{0}.." both))
        (magit-run-git-with-input (current-buffer) "apply")))))

(defun magit-stash-index-snapshot ()
  "Create a new stash of the index only; keep changes in place."
  (interactive)
  (magit-stash-index (magit-stash-format-snapshot-message) t))

(defun magit-stash-apply (stash &optional args)
  "Apply a stash on top of the current working tree state.
\n(git stash apply [ARGS] stash@{N})"
  (interactive (list (magit-read-stash "Apply stash (number): ")
                     (magit-current-popup-args :only "--index")))
  (magit-run-git "stash" "apply" args (magit-stash-as-refname stash)))

(defun magit-stash-pop (stash &optional args)
  "Apply a stash on top of working tree state and remove from stash list.
\n(git stash pop [ARGS] stash@{N})"
  (interactive (list (magit-read-stash "Pop stash (number): ")
                     (magit-current-popup-args :only "--index")))
  (magit-run-git "stash" "pop" args (magit-stash-as-refname stash)))

(defun magit-stash-drop (stash)
  "Remove a stash from the stash list.
\n(git stash drop stash@{N})"
  (interactive (list (magit-read-stash "Drop stash (number): ")))
  (magit-run-git "stash" "drop" (magit-stash-as-refname stash)))

(defun magit-stash-branch (stash branchname)
  "Create and checkout a branch from STASH.
\n(git stash branch BRANCHNAME stash@{N})"
  (interactive (list (magit-read-stash "Branch stash (number): ")
                     (read-string      "Branch name: ")))
  (magit-run-git "stash" "branch" branchname (magit-stash-as-refname stash)))

(defun magit-stash-as-refname (arg)
  (if (stringp arg) arg (format "stash@{%i}" arg)))

(defun magit-stash-format-snapshot-message ()
  (format-time-string magit-stash-snapshot-message-format (current-time)))

;;;;; Cherry-Pick

(defun magit-cherry-pick-item ()
  "Cherry-pick them item at point."
  (interactive)
  (magit-section-action cherry-pick (info)
    (commit (magit-cherry-pick-commit info))
    (stash  (magit-stash-pop info))))

(defun magit-cherry-pick-commit (commit)
  (magit-assert-one-parent commit "cherry-pick")
  (magit-run-git "cherry-pick" commit))

;;;;; Submoduling

(magit-define-popup magit-submodule-popup
  "Popup console for submodule commands."
  'magit-popups
  :man-page "git-submodule"
  :actions  '((?u "Update" magit-submodule-update)
              (?b "Both update and init" magit-submodule-update-init)
              (?i "Init" magit-submodule-init)
              (?s "Sync" magit-submodule-sync)))

;;;###autoload
(defun magit-submodule-update (&optional init)
  "Update the submodule of the current git repository.
With a prefix arg, do a submodule update --init."
  (interactive "P")
  (let ((default-directory (magit-get-top-dir)))
    (magit-run-git-async "submodule" "update" (and init "--init"))))

;;;###autoload
(defun magit-submodule-update-init ()
  "Update and init the submodule of the current git repository."
  (interactive)
  (magit-submodule-update t))

;;;###autoload
(defun magit-submodule-init ()
  "Initialize the submodules."
  (interactive)
  (let ((default-directory (magit-get-top-dir)))
    (magit-run-git-async "submodule" "init")))

;;;###autoload
(defun magit-submodule-sync ()
  "Synchronizes submodule's remote URL configuration."
  (interactive)
  (let ((default-directory (magit-get-top-dir)))
    (magit-run-git-async "submodule" "sync")))

;;;;; Bisecting

(magit-define-popup magit-bisect-popup
  "Popup console for bisect commands."
  'magit-popups
  :man-page "git-bisect"
  :actions  '((?b "Bad"   magit-bisect-bad)
              (?g "Good"  magit-bisect-good)
              (?k "Skip"  magit-bisect-skip)
              (?r "Reset" magit-bisect-reset)
              (?s "Start" magit-bisect-start)
              (?u "Run"   magit-bisect-run)))

;;;###autoload
(defun magit-bisect-start (bad good)
  "Start a bisect session.

Bisecting a bug means to find the commit that introduced it.
This command starts such a bisect session by asking for a know
good and a bad commit.  To move the session forward use the
other actions from the bisect popup (\
\\<magit-status-mode-map>\\[magit-bisect-popup])."
  (interactive
   (if (magit-bisecting-p)
       (user-error "Already bisecting")
     (list (magit-read-rev "Start bisect with known bad revision" "HEAD")
           (magit-read-rev "Good revision" (magit-guess-branch)))))
  (magit-bisect-async "start" (list bad good) t))

;;;###autoload
(defun magit-bisect-reset ()
  "After bisecting cleanup bisection state and return to original HEAD."
  (interactive)
  (when (yes-or-no-p "Reset bisect?")
    (magit-run-git "bisect" "reset")
    (ignore-errors (delete-file (magit-git-dir "BISECT_CMD_OUTPUT")))))

;;;###autoload
(defun magit-bisect-good ()
  "While bisecting, mark the current commit as good.
Use this after you have asserted that the commit does not contain
the bug in question."
  (interactive)
  (magit-bisect-async "good"))

;;;###autoload
(defun magit-bisect-bad ()
  "While bisecting, mark the current commit as bad.
Use this after you have asserted that the commit does contain the
bug in question."
  (interactive)
  (magit-bisect-async "bad"))

;;;###autoload
(defun magit-bisect-skip ()
  "While bisecting, skip the current commit.
Use this if for some reason the current commit is not a good one
to test.  This command lets Git choose a different one."
  (interactive)
  (magit-bisect-async "skip"))

;;;###autoload
(defun magit-bisect-run (cmdline)
  "Bisect automatically by running commands after each step."
  (interactive (list (read-shell-command "Bisect shell command: ")))
  (magit-bisect-async "run" (list cmdline)))

(defun magit-bisect-async (subcommand &optional args no-assert)
  (unless (or no-assert (magit-bisecting-p))
    (user-error "Not bisecting"))
  (let ((file (magit-git-dir "BISECT_CMD_OUTPUT"))
        (default-directory (magit-get-top-dir)))
    (ignore-errors (delete-file file))
    (magit-run-git-with-logfile file "bisect" subcommand args)
    (magit-process-wait)
    (magit-refresh)))

;;;;; Logging

(magit-define-popup magit-log-popup
  "Popup console for log commands."
  'magit-popups
  :man-page "git-log"
  :switches '((?m "Only merge commits"        "--merges")
              (?d "Date Order"                "--date-order")
              (?f "First parent"              "--first-parent")
              (?i "Case insensitive patterns" "-i")
              (?P "Pickaxe regex"             "--pickaxe-regex")
              (?g "Show Graph"                "--graph")
              (?S "Show Signature"            "--show-signature")
              (?n "Name only"                 "--name-only")
              (?M "All match"                 "--all-match")
              (?A "All"                       "--all"))
  :options  '((?r "Relative"       "--relative="  read-directory-name)
              (?c "Committer"      "--committer=" read-from-minibuffer)
              (?> "Since"          "--since="     read-from-minibuffer)
              (?< "Before"         "--before="    read-from-minibuffer)
              (?a "Author"         "--author="    read-from-minibuffer)
              (?g "Grep messages"  "--grep="      read-from-minibuffer)
              (?G "Grep patches"   "-G"           read-from-minibuffer)
              (?L "Trace evolution of line range"
                  "-L" magit-read-file-trace)
              (?s "Pickaxe search" "-S"           read-from-minibuffer)
              (?b "Branches"       "--branches="  read-from-minibuffer)
              (?R "Remotes"        "--remotes="   read-from-minibuffer))
  :actions  '((?l "Oneline"        magit-log-dwim)
              (?L "Verbose"        magit-log-verbose-dwim)
              (?r "Reflog"         magit-reflog)
              (?f "File log"       magit-file-log)
              (?b "Oneline branch" magit-log)
              (?B "Verbose branch" magit-log-verbose)
              (?R "Reflog HEAD"    magit-reflog-head))
  :default-arguments '("--graph")
  :default-action 'magit-log-dwim
  :max-action-columns 4)

;;;###autoload
(defun magit-log (range &optional args)
  (interactive (magit-log-read-args nil nil))
  (magit-mode-setup magit-log-buffer-name nil
                    #'magit-log-mode
                    #'magit-refresh-log-buffer 'oneline range
                    (cl-delete "^-L" args :test 'string-match-p))
  (magit-log-goto-same-commit))

;;;###autoload
(defun magit-log-dwim (range &optional args)
  (interactive (magit-log-read-args t nil))
  (magit-log range args))

;;;###autoload
(defun magit-log-verbose (range &optional args)
  (interactive (magit-log-read-args nil t))
  (magit-mode-setup magit-log-buffer-name nil
                    #'magit-log-mode
                    #'magit-refresh-log-buffer 'long range args)
  (magit-log-goto-same-commit))

;;;###autoload
(defun magit-log-verbose-dwim (range &optional args)
  (interactive (magit-log-read-args t t))
  (magit-log-verbose range args))

(defun magit-log-read-args (dwim patch)
  (let ((default "HEAD"))
    (list (if (if dwim (not current-prefix-arg) current-prefix-arg)
              default
            (magit-read-rev (format "Show %s log for ref/rev/range"
                                    (if patch "verbose" "oneline"))
                            default))
          magit-current-popup-args)))

;;;###autoload
(defun magit-file-log (file &optional use-graph)
  "Display the log for the currently visited file or another one.
With a prefix argument show the log graph."
  (interactive
   (list (magit-read-file-from-rev (magit-get-current-branch)
                                   (magit-buffer-file-name t))
         current-prefix-arg))
  (magit-mode-setup magit-log-buffer-name nil
                    #'magit-log-mode
                    #'magit-refresh-log-buffer
                    'oneline "HEAD"
                    `(,@(and use-graph (list "--graph"))
                      ,@magit-current-popup-args
                      "--follow")
                    file)
  (magit-log-goto-same-commit))

;;;###autoload
(defun magit-reflog (ref)
  "Display the reflog of the current branch.
With a prefix argument another branch can be chosen."
  (interactive (let ((branch (magit-get-current-branch)))
                 (if (and branch (not current-prefix-arg))
                     (list branch)
                   (list (magit-read-rev "Reflog of" branch)))))
  (magit-mode-setup magit-reflog-buffer-name nil
                    #'magit-reflog-mode
                    #'magit-refresh-reflog-buffer ref))

;;;###autoload
(defun magit-reflog-head ()
  "Display the HEAD reflog."
  (interactive)
  (magit-reflog "HEAD"))

;;; Modes (2)
;;;; Log Mode
;;;;; Log Core

(define-derived-mode magit-log-mode magit-mode "Magit Log"
  "Mode for looking at git log.

\\<magit-log-mode-map>Type `\\[magit-visit-item]` to visit a commit, and \
`\\[magit-show-item-or-scroll-up]` to just show it.
Type `\\[magit-log-show-more-entries]` to show more commits, \
and `\\[magit-refresh]` to refresh the log.
Type `\\[magit-diff-working-tree]` to see the diff between current commit and your working tree,
Type `\\[magit-diff]` to see diff between any two version
Type `\\[magit-apply-item]` to apply the change of the current commit to your wortree,
and `\\[magit-cherry-pick-item]` to apply and commit the result.
Type `\\[magit-revert-item]` to revert a commit, and `\\[magit-reset-head]` reset your current head to a commit,

More information can be found in Info node `(magit)History'

Other key binding:
\\{magit-log-mode-map}"
  :group 'magit)

(defvar magit-log-buffer-name "*magit-log*"
  "Name of buffer used to display log entries.")

(defun magit-refresh-log-buffer (style range args &optional file)
  (magit-set-buffer-margin (car magit-log-margin-spec)
                           (and magit-log-show-margin
                                (eq (car magit-refresh-args) 'oneline)))
  (magit-log-margin-set-timeunit-width)
  (setq magit-file-log-file file)
  (when (consp range)
    (setq range (concat (car range) ".." (cdr range))))
  (magit-git-insert-section
      (logbuf (concat "Commits"
                      (and file  (concat " for file " file))
                      (and range (concat " in " range))))
      (apply-partially 'magit-wash-log style 'color t)
    "log"
    (format "--max-count=%d" magit-log-cutoff-length)
    "--decorate=full" "--abbrev-commit" "--color"
    (cl-case style
      (long    (cons "--stat" args))
      (oneline (cons (concat "--pretty=format:%h%d "
                             (and (member "--show-signature" args) "%G?")
                             "[%an][%at]%s")
                     (delete "--show-signature" args))))
    range "--" file)
  (save-excursion
    (goto-char (point-min))
    (magit-format-log-margin)))

;;;;; Log Washing

(defconst magit-log-oneline-re
  (concat "^"
          "\\(?4:\\(?:[-_/|\\*o.] *\\)+ *\\)?"     ; graph
          "\\(?:"
          "\\(?1:[0-9a-fA-F]+\\) "                 ; sha1
          "\\(?:\\(?3:([^()]+)\\) \\)?"            ; refs
          "\\(?7:[BGUN]\\)?"                       ; gpg
          "\\[\\(?5:[^]]*\\)\\]"                   ; author
          "\\[\\(?6:[^]]*\\)\\]"                   ; date
          "\\(?2:.*\\)"                            ; msg
          "\\)?$"))

(defconst magit-log-long-re
  (concat "^"
          "\\(?4:\\(?:[-_/|\\*o.] *\\)+ *\\)?"     ; graph
          "\\(?:"
          "\\(?:commit \\(?1:[0-9a-fA-F]+\\)"      ; sha1
          "\\(?: \\(?3:([^()]+)\\)\\)?\\)"         ; refs
          "\\|"
          "\\(?2:.+\\)\\)$"))                      ; "msg"

(defconst magit-log-unique-re
  (concat "^"
          "\\(?1:[0-9a-fA-F]+\\) "                 ; sha1
          "\\(?2:.*\\)$"))                         ; msg

(defconst magit-log-cherry-re
  (concat "^"
          "\\(?8:[-+]\\) "                         ; cherry
          "\\(?1:[0-9a-fA-F]+\\) "                 ; sha1
          "\\(?2:.*\\)$"))                         ; msg

(defconst magit-log-module-re
  (concat "^"
          "\\(?:\\(?11:[<>]\\) \\)?"               ; side
          "\\(?1:[0-9a-fA-F]+\\) "                 ; sha1
          "\\(?2:.*\\)$"))                         ; msg

(defconst magit-log-bisect-vis-re
  (concat "^"
          "\\(?1:[0-9a-fA-F]+\\) "                 ; sha1
          "\\(?:\\(?3:([^()]+)\\) \\)?"            ; refs
          "\\(?2:.+\\)$"))                         ; msg

(defconst magit-log-bisect-log-re
  (concat "^# "
	  "\\(?3:bad:\\|skip:\\|good:\\) "         ; "refs"
	  "\\[\\(?1:[^]]+\\)\\] "                  ; sha1
	  "\\(?2:.+\\)$"))                         ; msg

(defconst magit-log-reflog-re
  (concat "^"
          "\\(?1:[^ ]+\\) "                        ; sha1
          "\\[\\(?5:[^]]*\\)\\] "                  ; author
          "\\(?6:[^ ]*\\) "                        ; date
          "[^@]+@{\\(?9:[^}]+\\)} "                ; refsel
          "\\(?10:merge\\|[^:]+\\)?:? ?"           ; refsub
          "\\(?2:.+\\)?$"))                        ; msg

(defconst magit-reflog-subject-re
  (concat "\\([^ ]+\\) ?"                          ; command (1)
          "\\(\\(?: ?-[^ ]+\\)+\\)?"               ; option  (2)
          "\\(?: ?(\\([^)]+\\))\\)?"))             ; type    (3)

(defvar magit-log-count nil)

(defun magit-wash-log (style &optional color longer)
  (when color
    (let ((ansi-color-apply-face-function
           (lambda (beg end face)
             (when face
               (put-text-property beg end 'font-lock-face face)))))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (when (eq style 'cherry)
    (reverse-region (point-min) (point-max)))
  (let ((magit-log-count 0))
    (magit-wash-sequence (apply-partially 'magit-wash-log-line style
                                          (magit-abbrev-length)))
    (when (and longer
               (= magit-log-count magit-log-cutoff-length))
      (magit-with-section (section longer 'longer)
        (insert-text-button (substitute-command-keys
                             (format "Type \\<%s>\\[%s] to show more history"
                                     'magit-log-mode-map
                                     'magit-log-show-more-entries))
                            'action (lambda (button)
                                      (magit-log-show-more-entries))
                            'follow-link t
                            'mouse-face magit-item-highlight-face)))))

(defun magit-wash-log-line (style abbrev)
  (looking-at (cl-ecase style
                (oneline magit-log-oneline-re)
                (long    magit-log-long-re)
                (unique  magit-log-unique-re)
                (cherry  magit-log-cherry-re)
                (module  magit-log-module-re)
                (reflog  magit-log-reflog-re)
                (bisect-vis magit-log-bisect-vis-re)
                (bisect-log magit-log-bisect-log-re)))
  (magit-bind-match-strings
      (hash msg refs graph author date gpg cherry refsel refsub side) nil
    (delete-region (point) (point-at-eol))
    (when cherry
      (magit-insert cherry (if (string= cherry "-")
                               'magit-cherry-equivalent
                             'magit-cherry-unmatched) ?\s))
    (when side
      (magit-insert side (if (string= side "<")
                             'magit-diff-del
                           'magit-diff-add) ?\s))
    (unless (eq style 'long)
      (when (eq style 'bisect-log)
	(setq hash (magit-git-string "rev-parse" "--short" hash)))
      (if hash
          (insert (propertize hash 'face 'magit-log-sha1) ?\s)
        (insert (make-string (1+ abbrev) ? ))))
    (when graph
      (if magit-log-format-graph-function
          (insert (funcall magit-log-format-graph-function graph))
        (insert graph)))
    (when (and hash (eq style 'long))
      (magit-insert (if refs hash (magit-rev-parse hash)) 'magit-log-sha1 ?\s))
    (when refs
      (magit-insert-ref-labels refs))
    (when refsub
      (insert (format "%-2s " refsel))
      (insert (magit-log-format-reflog refsub)))
    (when msg
      (magit-insert msg (cl-case (and gpg (aref gpg 0))
                          (?G 'magit-signature-good)
                          (?B 'magit-signature-bad)
                          (?U 'magit-signature-untrusted)
                          (?N 'magit-signature-none)
                          (t  'magit-log-message))))
    (goto-char (line-beginning-position))
    (magit-format-log-margin author date)
    (if hash
        (magit-with-section (section commit hash)
          (when (eq style 'module)
            (setf (magit-section-type section) 'mcommit))
          (when (derived-mode-p 'magit-log-mode)
            (cl-incf magit-log-count))
          (forward-line)
          (when (eq style 'long)
            (magit-wash-sequence
             (lambda ()
               (looking-at magit-log-long-re)
               (when (match-string 2)
                 (magit-wash-log-line 'long abbrev))))))
      (forward-line)))
  t)

(defun magit-format-log-margin (&optional author date)
  (when magit-log-show-margin
    (cl-destructuring-bind (width characterp duration-spec)
        magit-log-margin-spec
      (if author
          (magit-make-margin-overlay
           (propertize (truncate-string-to-width
                        author (- width 1 3 (if characterp 0 1)
                                  magit-log-margin-timeunit-width 1)
                        nil ?\s (make-string 1 magit-ellipsis))
                       'face 'magit-log-author)
           " "
           (propertize (magit-format-duration
                        (abs (truncate (- (float-time)
                                          (string-to-number date))))
                        (symbol-value duration-spec)
                        magit-log-margin-timeunit-width)
                       'face 'magit-log-date)
           (propertize " " 'face 'fringe))
        (magit-make-margin-overlay
         (propertize (make-string (1- width) ?\s) 'face 'default)
         (propertize " " 'face 'fringe))))))

;;;;; Log Commands

(defun magit-log-toggle-margin ()
  "Show or hide the log margin.
This command can only be used inside log buffers (usually
*magit-log*) and only if that displays a `oneline' log.
Also see option `magit-log-show-margin'."
  (interactive)
  (unless (derived-mode-p 'magit-log-mode)
    (user-error "The log margin cannot be used outside of log buffers"))
  (when (eq (car magit-refresh-args) 'long)
    (user-error "The log margin cannot be used with verbose logs"))
  (if magit-log-show-margin
      (magit-set-buffer-margin (car magit-log-margin-spec)
                               (not (cdr (window-margins))))
    (setq-local magit-log-show-margin t)
    (magit-refresh)))

(defun magit-log-show-more-entries (&optional arg)
  "Grow the number of log entries shown.

With no prefix optional ARG, show twice as many log entries.
With a numerical prefix ARG, add this number to the number of shown log entries.
With a non numeric prefix ARG, show all entries"
  (interactive "P")
  (setq-local magit-log-cutoff-length
              (cond ((numberp arg) (+ magit-log-cutoff-length arg))
                    (arg magit-log-infinite-length)
                    (t (* magit-log-cutoff-length 2))))
  (let ((old-point (point)))
    (magit-refresh)
    (goto-char old-point)))

(defun magit-log-maybe-show-more-entries (section)
  (when (and (eq (magit-section-type section) 'longer)
             magit-log-auto-more)
    (magit-log-show-more-entries)
    (forward-line -1)
    (magit-goto-next-section)))

(defun magit-log-maybe-show-commit (section)
  (when (and (eq (magit-section-type section) 'commit)
             (or (and (magit-diff-auto-show-p 'log-follow)
                      (get-buffer-window magit-commit-buffer-name))
                 (and (magit-diff-auto-show-p 'log-oneline)
                      (derived-mode-p 'magit-log-mode)
                      (eq (car magit-refresh-args) 'oneline))))
    (magit-show-commit (magit-section-info section) t)))

(defun magit-log-goto-same-commit ()
  (--when-let (and magit-previous-section
                   (derived-mode-p 'magit-log-mode)
                   (cl-find (magit-section-info magit-previous-section)
                            (magit-section-children magit-root-section)
                            :test 'equal :key 'magit-section-info))
    (goto-char (magit-section-beginning it))))

;;;; Log Select Mode

(define-derived-mode magit-log-select-mode magit-log-mode "Magit Select"
  "Mode for selecting a commit from history."
  :group 'magit)

(defvar-local magit-log-select-pick-function nil)
(defvar-local magit-log-select-quit-function nil)

(defun magit-log-select (pick &optional quit desc branch args)
  (declare (indent defun))
  (magit-mode-setup magit-log-buffer-name nil
                    #'magit-log-select-mode
                    #'magit-refresh-log-buffer 'oneline
                    (or branch (magit-get-current-branch) "HEAD")
                    args)
  (magit-log-goto-same-commit)
  (setq magit-log-select-pick-function pick)
  (setq magit-log-select-quit-function quit)
  (message
   (substitute-command-keys
    (format "Type \\[%s] to select commit at point%s, or \\[%s] to abort"
            'magit-log-select-pick (if desc (concat " " desc) "")
            'magit-log-select-quit))))

(defun magit-log-select-pick ()
  (interactive)
  (let ((fun magit-log-select-pick-function)
        (rev (magit-section-case (info) (commit info))))
    (kill-buffer (current-buffer))
    (funcall fun rev)))

(defun magit-log-select-quit ()
  (interactive)
  (kill-buffer (current-buffer))
  (when magit-log-select-quit-function
    (funcall magit-log-select-quit-function)))

;;;; Cherry Mode

(define-derived-mode magit-cherry-mode magit-mode "Magit Cherry"
  "Mode for looking at commits not merged upstream.

\\<magit-cherry-mode-map>Type `\\[magit-toggle-section]` to show or hide \
section, `\\[magit-visit-item]` to visit an item and \
`\\[magit-show-item-or-scroll-up]` to show it.
Type `\\[magit-diff-working-tree]` to display change with your working tree, \
when `\\[magit-diff]` to display change
between any two commit.
Type `\\[magit-cherry-pick-item]` to cherry-pick a commit, and \
`\\[magit-apply-item]` to apply its change to your
working tree, without committing, and `\\[magit-merge-popup]` to merge.
`\\[magit-refresh]` will refresh current buffer.


Other key binding:
\\{magit-cherry-mode-map}")

(defvar magit-cherry-buffer-name "*magit-cherry*"
  "Name of buffer used to display commits not merged upstream.")

;;;###autoload
(defun magit-cherry (head upstream)
  "Show commits in a branch that are not merged in the upstream branch."
  (interactive
   (let  ((head (magit-read-rev "Cherry head" (magit-get-current-branch))))
     (list head (magit-read-rev "Cherry upstream"
                                (magit-get-tracked-branch head)))))
  (magit-mode-setup magit-cherry-buffer-name nil
                    #'magit-cherry-mode
                    #'magit-refresh-cherry-buffer upstream head))

(defun magit-refresh-cherry-buffer (upstream head)
  (magit-with-section (section cherry 'cherry)
    (run-hooks 'magit-cherry-sections-hook)))

(defun magit-insert-cherry-head-line ()
  (magit-insert-line-section (line)
    (concat "Head: "
            (propertize (cadr magit-refresh-args) 'face 'magit-branch) " "
            (abbreviate-file-name default-directory))))

(defun magit-insert-cherry-upstream-line ()
  (magit-insert-line-section (line)
    (concat "Upstream: "
            (propertize (car magit-refresh-args) 'face 'magit-branch))))

(defun magit-insert-cherry-help-lines ()
  (when (derived-mode-p 'magit-cherry-mode)
    (insert "\n")
    (magit-insert-line-section (line)
      (concat (propertize "-" 'face 'magit-cherry-equivalent)
              " equivalent exists in both refs"))
    (magit-insert-line-section (line)
      (concat (propertize "+" 'face 'magit-cherry-unmatched)
              " unmatched commit tree"))))

(defun magit-insert-cherry-commits ()
  (magit-git-insert-section (cherries "Cherry commits:")
      (apply-partially 'magit-wash-log 'cherry)
    "cherry" "-v" (magit-abbrev-arg) magit-refresh-args))

;;;; Reflog Mode

(defvar magit-reflog-buffer-name "*magit-reflog*"
  "Name of buffer used to display reflog entries.")

(define-derived-mode magit-reflog-mode magit-log-mode "Magit Reflog"
  "Mode for looking at git reflog.

\\<magit-reflog-mode-map>Type `\\[magit-visit-item]` to visit a commit, and \
`\\[magit-show-item-or-scroll-up]` to just show it.
Type `\\[magit-diff-working-tree]` to see the diff between current commit and \
your working tree,
Type `\\[magit-diff]` to see the between any two version.
Type `\\[magit-reset-head]` to reset your head to the current commit, and \
`\\[magit-apply-item]` to apply its change
to your working tree and `\\[magit-cherry-pick-item]` to cherry pick it.

More information can be found in Info node `(magit)Reflogs'

Other key binding:
\\{magit-reflog-mode-map}"
  :group 'magit)

(defun magit-refresh-reflog-buffer (ref)
  (magit-log-margin-set-timeunit-width)
  (magit-git-insert-section
      (reflogbuf (format "Local history of branch %s" ref))
      (apply-partially 'magit-wash-log 'reflog t)
    "reflog" "show" "--format=format:%h [%an] %ct %gd %gs"
    (format "--max-count=%d" magit-log-cutoff-length) ref))

(defvar magit-reflog-labels
  '(("commit"      . magit-log-reflog-label-commit)
    ("amend"       . magit-log-reflog-label-amend)
    ("merge"       . magit-log-reflog-label-merge)
    ("checkout"    . magit-log-reflog-label-checkout)
    ("branch"      . magit-log-reflog-label-checkout)
    ("reset"       . magit-log-reflog-label-reset)
    ("rebase"      . magit-log-reflog-label-rebase)
    ("cherry-pick" . magit-log-reflog-label-cherry-pick)
    ("initial"     . magit-log-reflog-label-commit)
    ("pull"        . magit-log-reflog-label-remote)
    ("clone"       . magit-log-reflog-label-remote)))

(defun magit-log-format-reflog (subject)
  (let* ((match (string-match magit-reflog-subject-re subject))
         (command (and match (match-string 1 subject)))
         (option  (and match (match-string 2 subject)))
         (type    (and match (match-string 3 subject)))
         (label (if (string= command "commit")
                    (or type command)
                  command))
         (text (if (string= command "commit")
                   label
                 (mapconcat #'identity
                            (delq nil (list command option type))
                            " "))))
    (format "%-16s "
            (propertize text 'face
                        (or (cdr (assoc label magit-reflog-labels))
                            'magit-log-reflog-label-other)))))

;;;; Ediff Support

(defvar magit-ediff-windows nil
  "The window configuration that will be restored when Ediff is finished.")

;;;###autoload
(defun magit-interactive-resolve (file)
  "Resolve a merge conflict using Ediff."
  (interactive (list (magit-section-case (info) (diff (cadr info)))))
  (require 'ediff)
  (let ((merge-status (magit-git-lines "ls-files" "-u" "--" file))
        (base-buffer)
        (our-buffer (generate-new-buffer (concat file ".current")))
        (their-buffer (generate-new-buffer (concat file ".merged")))
        (merger-buffer)
        (windows (current-window-configuration)))
    (unless merge-status
      (user-error "Cannot resolve %s" file))
    (when (string-match "^[0-9]+ [0-9a-f]+ 1" (car merge-status))
      (pop merge-status)
      (setq base-buffer (generate-new-buffer (concat file ".base")))
      (with-current-buffer base-buffer
        (magit-git-insert "cat-file" "blob" (concat ":1:" file))))
    ;; If the second or third version do not exit, we use an empty buffer for the deleted file
    (with-current-buffer our-buffer
      (when (string-match "^[0-9]+ [0-9a-f]+ 2" (car merge-status))
        (pop merge-status)
        (magit-git-insert "cat-file" "blob" (concat ":2:" file)))
      (let ((buffer-file-name file))
        (normal-mode t)))
    (with-current-buffer their-buffer
      (when (string-match "^[0-9]+ [0-9a-f]+ 3" (car merge-status))
        (magit-git-insert "cat-file" "blob" (concat ":3:" file)))
      (let ((buffer-file-name file))
        (normal-mode t)))
    ;; We have now created the 3 buffer with ours, theirs and the ancestor files
    (if base-buffer
        (setq merger-buffer (ediff-merge-buffers-with-ancestor
                             our-buffer their-buffer base-buffer nil nil file))
      (setq merger-buffer (ediff-merge-buffers our-buffer their-buffer nil nil file)))
    (with-current-buffer merger-buffer
      (setq ediff-show-clashes-only t)
      (setq-local magit-ediff-windows windows)
      (make-local-variable 'ediff-quit-hook)
      (add-hook 'ediff-quit-hook
                (lambda ()
                  (let ((buffer-A ediff-buffer-A)
                        (buffer-B ediff-buffer-B)
                        (buffer-C ediff-buffer-C)
                        (buffer-Ancestor ediff-ancestor-buffer)
                        (windows magit-ediff-windows))
                    (ediff-cleanup-mess)
                    (kill-buffer buffer-A)
                    (kill-buffer buffer-B)
                    (when (bufferp buffer-Ancestor)
                      (kill-buffer buffer-Ancestor))
                    (set-window-configuration windows)))))))

;;;; Diff Mode
;;;;; Diff Core

(define-derived-mode magit-diff-mode magit-mode "Magit Diff"
  "Mode for looking at a git diff.

\\<magit-diff-mode-map>Type `\\[magit-visit-item]` to visit the changed file, \
`\\[magit-toggle-section]` to hide or show a hunk,
`\\[magit-diff-larger-hunks]` and `\\[magit-diff-smaller-hunks]` to change \
the size of the hunks.
Type `\\[magit-apply-item]` to apply a change to your worktree and \
`\\[magit-revert-item]` to reverse it.

More information can be found in Info node `(magit)Diffing'

\\{magit-diff-mode-map}"
  :group 'magit)

(defvar magit-diff-buffer-name "*magit-diff*"
  "Name of buffer used to display a diff.")

(defvar magit-stash-buffer-name "*magit-stash*"
  "Name of buffer used to display a stash.")

;;;;; Diff Entry Commands

(magit-define-popup magit-diff-popup
  "Key menu for diffing."
  'magit 'magit-popup-mode 'magit-diff-options
  :man-page "git-diff"
  :switches '((?W "Show surrounding functions"   "--function-context")
              (?b "Ignore whitespace changes"    "--ignore-space-change")
              (?w "Ignore all whitespace"        "--ignore-all-space"))
  :options  '((?h "Context lines" "-U" read-from-minibuffer)
              (?a "Diff algorithm"
                  "--diff-algorithm=" magit-select-diff-algorithm))
  :actions  '((?d "Diff unstaged"     magit-diff-unstaged)
              (?c "Show commit"       magit-show-commit)
              (?R "Refresh diff"      magit-popup-set-local-variable)
              (?i "Diff staged"       magit-diff-staged)
              (?r "Diff commits"      magit-diff)
              (?S "Refresh and set"   magit-popup-set-variable)
              (?w "Diff working tree" magit-diff-working-tree)
              (?p "Diff paths"        magit-diff-paths)
              (?W "Refresh and save"  magit-popup-save-variable))
  :default-action 'magit-diff-working-tree
  :max-action-columns 3)

(defvar magit-diff-switch-buffer-function 'pop-to-buffer)

;;;###autoload
(defun magit-diff (range &optional working args)
  "Show changes between two commits."
  (interactive (list (magit-read-rev "Diff for ref/rev/range")))
  (magit-mode-setup magit-diff-buffer-name
                    magit-diff-switch-buffer-function
                    #'magit-diff-mode
                    #'magit-refresh-diff-buffer range working args))

;;;###autoload
(defun magit-diff-working-tree (&optional rev)
  "Show changes between the current working tree and the `HEAD' commit.
With a prefix argument show changes between the working tree and
a commit read from the minibuffer."
  (interactive (and current-prefix-arg
                    (list (magit-read-rev-with-default
                           "Diff working tree and commit"))))
  (magit-diff (or rev "HEAD") t))

;;;###autoload
(defun magit-diff-staged (&optional commit)
  "Show changes between the index and the `HEAD' commit.
With a prefix argument show changes between the index and
a commit read from the minibuffer."
  (interactive (and current-prefix-arg
                    (list (magit-read-rev-with-default
                           "Diff index and commit"))))
  (magit-diff nil nil (cons "--cached" (and commit (list commit)))))

;;;###autoload
(defun magit-diff-unstaged ()
  "Show changes between the working tree and the index."
  (interactive)
  (magit-diff nil))

;;;###autoload
(defun magit-diff-unpushed ()
  "Show unpushed changes."
  (interactive)
  (-if-let (tracked (magit-get-tracked-branch nil t))
      (magit-diff (concat tracked "..."))
    (error "Detached HEAD or upstream unset")))

;;;###autoload
(defun magit-diff-unpulled ()
  "Show unpulled changes."
  (interactive)
  (-if-let (tracked (magit-get-tracked-branch nil t))
      (magit-diff (concat "..." tracked))
    (error "Detached HEAD or upstream unset")))

;;;###autoload
(defun magit-diff-while-committing ()
  (interactive)
  (let* ((toplevel (magit-get-top-dir))
         (diff-buf (magit-mode-get-buffer magit-diff-buffer-name
                                          'magit-diff-mode toplevel))
         (commit-buf (magit-commit-log-buffer)))
    (if commit-buf
        (if (and (or ;; certainly an explicit amend
                     (with-current-buffer commit-buf
                       (member "--amend" magit-refresh-args))
                     ;; most likely an explicit amend
                     (not (magit-anything-staged-p))
                     ;; explicitly toggled from within diff
                     (and (eq (current-buffer) diff-buf)))
                 (or (not diff-buf)
                     (with-current-buffer diff-buf
                       (or ;; default to include last commit
                           (not (equal (magit-get-top-dir) toplevel))
                           ;; toggle to include last commit
                           (not (car magit-refresh-args))))))
            (magit-diff-while-amending)
          (magit-diff-staged))
      (user-error "No commit in progress"))))

(defun magit-diff-while-amending ()
  (magit-diff "HEAD^" nil (list "--cached")))

;;;###autoload
(defun magit-diff-paths (a b)
  "Show changes between any two files on disk."
  (interactive (list (read-file-name "First file: " nil nil t)
                     (read-file-name "Second file: " nil nil t)))
  (magit-diff nil nil (list "--no-index" "--" a b)))

;;;###autoload
(defun magit-diff-stash (stash &optional noselect)
  "Show changes in a stash.
A stash consist of more than just one commit.  This command uses
a special diff range so that the stashed changes appear as if the
actually were a single commit."
  (interactive (list (magit-read-stash "Show stash (number): ")))
  (magit-mode-setup magit-commit-buffer-name
                    (if noselect 'display-buffer 'pop-to-buffer)
                    #'magit-diff-mode
                    #'magit-refresh-diff-buffer
                    (concat stash "^2^.." stash)))

(defun magit-diff-less-context (&optional count)
  "Decrease the context for diff hunks by COUNT."
  (interactive "p")
  (setq magit-diff-options
        (cons (format "-U%i" (max 0 (- (magit-diff-previous-context-lines)
                                       count)))
              magit-diff-options))
  (magit-refresh))

(defun magit-diff-more-context (&optional count)
  "Increase the context for diff hunks by COUNT."
  (interactive "p")
  (setq magit-diff-options
        (cons (format "-U%i" (+ (magit-diff-previous-context-lines) count))
              magit-diff-options))
  (magit-refresh))

(defun magit-diff-default-context ()
  "Reset context for diff hunks to the default size."
  (interactive)
  (magit-diff-previous-context-lines)
  (magit-refresh))

(defun magit-diff-previous-context-lines ()
  (--if-let (cl-find "^-U\\([0-9]+\\)$" magit-diff-options
                     :test 'string-match)
      (progn (setq magit-diff-options (delete it magit-diff-options))
             (string-to-number (match-string 1 it)))
    3))

(defun magit-refresh-diff-buffer (range &optional working args)
  (magit-git-insert-section
      (diffbuf (cond (working
                      (format "Changes from %s to working tree" range))
                     ((not range)
                      (if (member "--cached" args)
                          "Staged changes"
                        "Unstaged changes"))
                     (t
                      (format "Changes in %s" range))))
      #'magit-wash-diffs
    "diff" "-p" (and magit-show-diffstat "--stat")
    magit-diff-extra-options
    range args magit-diff-options "--"))

(defun magit-diff-auto-show-p (op)
  (if (eq (car magit-diff-auto-show) 'not)
      (not (memq op (cdr magit-diff-auto-show)))
    (memq op magit-diff-auto-show)))

(defun magit-select-diff-algorithm (&optional noop1 noop2)
  (magit-read-char-case nil t
    (?d "[d]efault/myers" "default")
    (?m "[m]inimal"       "minimal")
    (?p "[p]atience"      "patience")
    (?h "[h]istogram"     "histogram")))

;;;;; Diff Washing

(defconst magit-diff-headline-re
  "^\\(@@@?\\|diff\\|\\* Unmerged path\\|Submodule\\)")

(defconst magit-diff-statline-re
  (concat "^ ?"
          "\\(.*\\)"     ; file
          "\\( +| +\\)"  ; separator
          "\\([0-9]+\\|Bin\\(?: +[0-9]+ -> [0-9]+ bytes\\)?$\\) ?"
          "\\(\\+*\\)"   ; add
          "\\(-*\\)$"))  ; del

(defconst magit-diff-submodule-re "^Submodule \
\\([^\s\n]+\\) \\(?:\\([^:\n]+\\):\\|contains modified content\\)$")

(defun magit-wash-diffs ()
  (let ((diffstats (magit-wash-diffstats)))
    (when (re-search-forward magit-diff-headline-re nil t)
      (goto-char (line-beginning-position))
      (magit-wash-sequence
       (lambda ()
         (magit-wash-diff (pop diffstats))))))
  (goto-char (point-max))
  (magit-xref-insert-buttons))

(defun magit-wash-diffstats ()
  (let (heading diffstats (beg (point)))
    (when (re-search-forward "^ ?\\([0-9]+ +files? change[^\n]*\n\\)" nil t)
      (setq heading (match-string-no-properties 1))
      (delete-region (match-beginning 0) (match-end 0))
      (goto-char beg)
      (magit-with-section (section diffstats 'diffstats heading)
        (magit-wash-sequence
         (lambda ()
           (when (looking-at magit-diff-statline-re)
             (magit-bind-match-strings (file sep cnt add del) nil
               (delete-region (point) (1+ (line-end-position)))
               (magit-with-section (s diffstat 'diffstat)
                 (insert " " file sep cnt " ")
                 (when add (insert (propertize add 'face 'magit-diff-add)))
                 (when del (insert (propertize del 'face 'magit-diff-del)))
                 (insert "\n"))))))
        (setq diffstats (magit-section-children section))))
    diffstats))

(defun magit-wash-diff (diffstat)
  (cond
   ((looking-at magit-diff-submodule-re)
    (let* ((module (match-string 1))
           (range  (match-string 2))
           (dirty  (not range)))
      (delete-region (point) (1+ (line-end-position)))
      (when (and dirty
                 (looking-at magit-diff-submodule-re)
                 (string= (match-string 1) module))
        (setq range (match-string 2))
        (delete-region (point) (1+ (line-end-position))))
      (while (looking-at "^  \\([<>]\\) \\(.+\\)$")
        (delete-region (point) (1+ (line-end-position))))
      (if range
          (let ((default-directory
                  (file-name-as-directory
                   (expand-file-name module (magit-get-top-dir)))))
            (setf (magit-section-info
                   (magit-git-insert-section
                       (diff (propertize (format "modified%s  %s\n"
                                                 (if dirty "%" "/") module)
                                         'face 'magit-diff-file-header))
                       (apply-partially 'magit-wash-log 'module)
                     "log" "--oneline" "--left-right" range))
                  module))
        (magit-with-section
            (section dirty module
                     (propertize (format "dirty      %s\n" module)
                                 'face 'magit-diff-file-header))))))
   ((looking-at "^\\* Unmerged path \\(.*\\)")
    (let ((dst (magit-decode-git-path (match-string 1))))
      (delete-region (point) (1+ (line-end-position)))
      (unless (derived-mode-p 'magit-status-mode)
        (magit-with-section
            (section diff dst (propertize (format "unmerged   %s\n" dst)
                                          'face 'magit-diff-file-header))))))
   ((looking-at "^diff \\(?:--git \\(.*\\) \\(.*\\)\\|--cc \\(.*\\)\\)$")
    (let (src dst status modes)
      (if (match-end 1)
          (setq dst (substring (magit-decode-git-path (match-string 2)) 2)
                src (substring (magit-decode-git-path (match-string 1)) 2)
                status "modified")
        (setq dst (magit-decode-git-path (match-string 3))
              src dst status "unmerged"))
      (when diffstat
        (setf (magit-section-info diffstat) dst))
      (delete-region (point) (1+ (line-end-position)))
      (while (not (or (eobp) (looking-at magit-diff-headline-re)))
        (if (looking-at "^old mode \\([^\n]+\\)\nnew mode \\([^\n]+\\)\n")
            (progn (setq modes (match-string-no-properties 0))
                   (delete-region (point) (match-end 0)))
          (when (looking-at "^\\(new file\\|rename\\|deleted\\)")
            (setq status (match-string 1)))
          (delete-region (point) (1+ (line-end-position)))))
      (magit-with-section
          (section diff dst
                   (propertize (if (equal status "rename")
                                   (format "renamed    %s => %s\n" src dst)
                                 (format "%-10s %s\n" status dst))
                               'face 'magit-diff-file-header)
                   (or (equal status "deleted")
                       (derived-mode-p 'magit-status-mode)))
        (setf (magit-section-diff-status section) status)
        (setf (magit-section-diff-file2  section) src)
        (when modes
          (magit-with-section (s hunk) (insert modes)))
        (magit-wash-sequence #'magit-wash-hunk))))))

(defun magit-wash-hunk ()
  (when (looking-at "^@@\\(@\\)?.+")
    (let ((section nil)
          (merging (match-end 1))
          (heading (match-string 0)))
      (delete-region (point) (1+ (line-end-position)))
      (magit-with-section (s hunk heading (concat heading "\n"))
        (while (not (or (eobp) (looking-at magit-diff-headline-re)))
          (magit-put-face-property
           (point) (line-end-position)
           (cond
            ((looking-at "^\\+\\+<<<<<<<") 'magit-diff-merge-current)
            ((looking-at "^\\+\\+=======") 'magit-diff-merge-separator)
            ((looking-at "^\\+\\+|||||||") 'magit-diff-merge-diff3-separator)
            ((looking-at "^\\+\\+>>>>>>>") 'magit-diff-merge-proposed)
            ((looking-at (if merging  "^\\(\\+\\| \\+\\)" "^\\+"))
             (magit-diff-highlight-whitespace merging)
             'magit-diff-add)
            ((looking-at (if merging  "^\\(-\\| -\\)" "^-"))
             'magit-diff-del)
            (t
             'magit-diff-none)))
            (forward-line))
        (setq section s))
      (when (eq magit-diff-refine-hunk 'all)
        (magit-diff-refine-hunk section))
      (magit-put-face-property (magit-section-beginning section)
                               (magit-section-content-beginning section)
                               'magit-diff-hunk-header))
    t))

(defun magit-diff-highlight-whitespace (merging)
  (when (and magit-highlight-whitespace
             (or (derived-mode-p 'magit-status-mode)
                 (not (eq magit-highlight-whitespace 'status))))
    (let ((prefix (if merging "^[-\\+\s]\\{2\\}" "^[-\\+]"))
          (indent
           (if (local-variable-p 'magit-highlight-indentation)
               magit-highlight-indentation
             (setq-local
              magit-highlight-indentation
              (cdr (cl-find-if (lambda (pair)
                                 (string-match-p (car pair) default-directory))
                               (default-value magit-highlight-indentation)
                               :from-end t))))))
      (when (and magit-highlight-trailing-whitespace
                 (looking-at (concat prefix ".*?\\([ \t]+\\)$")))
        (magit-put-face-property (match-beginning 1) (match-end 1)
                                 'magit-whitespace-warning-face))
      (when (or (and (eq indent 'tabs)
                     (looking-at (concat prefix "\\( *\t[ \t]*\\)")))
                (and (integerp indent)
                     (looking-at (format "%s\\([ \t]* \\{%s,\\}[ \t]*\\)"
                                         prefix indent))))
        (magit-put-face-property (match-beginning 1) (match-end 1)
                                 'magit-whitespace-warning-face)))))

;;;;; Diff Mode Commands

(defun magit-diff-toggle-refine-hunk (&optional other)
  "Turn diff-hunk refining on or off.

If hunk refining is currently on, then hunk refining is turned off.
If hunk refining is off, then hunk refining is turned on, in
`selected' mode (only the currently selected hunk is refined).

With a prefix argument, the \"third choice\" is used instead:
If hunk refining is currently on, then refining is kept on, but
the refining mode (`selected' or `all') is switched.
If hunk refining is off, then hunk refining is turned on, in
`all' mode (all hunks refined).

Customize variable `magit-diff-refine-hunk' to change the default mode."
  (interactive "P")
  (let ((hunk (and magit-highlighted-section
                   (eq (magit-section-type magit-highlighted-section) 'hunk)
                   magit-highlighted-section))
        (old magit-diff-refine-hunk))
    (setq-local magit-diff-refine-hunk
                (if other
                    (if (eq old 'all) t 'all)
                  (not old)))
    (cond ((or (eq old 'all)
               (eq magit-diff-refine-hunk 'all))
           (magit-refresh))
          ((not hunk))
          (magit-diff-refine-hunk
           (magit-diff-refine-hunk hunk))
          (t
           (magit-diff-unrefine-hunk hunk)))
    (message "magit-diff-refine-hunk: %s" magit-diff-refine-hunk)))

(defun magit-diff-refine-hunk (hunk)
  (save-excursion
    (goto-char (magit-section-beginning hunk))
    ;; `diff-refine-hunk' does not handle combined diffs.
    (unless (looking-at "@@@")
      (diff-refine-hunk))))

(defun magit-diff-unrefine-hunk (hunk)
  (remove-overlays (magit-section-beginning hunk)
                   (magit-section-end hunk)
                   'diff-mode 'fine))

;;;; Wazzup Mode

(define-derived-mode magit-wazzup-mode magit-mode "Magit Wazzup"
  "Mode for looking at git commits not merged into current HEAD.

\\<magit-wazzup-mode-map>Type `\\[magit-toggle-section]` to show or hide \
section, `\\[magit-visit-item]` to visit an item \
`\\[magit-show-item-or-scroll-up]` to show it.
Type `\\[magit-diff-working-tree]` to display change with your working tree, \
and `\\[magit-diff]` to display change
between any two commit.
Type `\\[magit-cherry-pick-item]` to cherry-pick a commit, and \
`\\[magit-apply-item]` to apply its change to your
working tree, without committing, and `\\[magit-merge-popup]` \
to merge those change.
Type `\\[magit-refresh]` to refresh current buffer.

More information can be found in Info node `(magit)Wazzup'

\\{magit-wazzup-mode-map}"
  :group 'magit)

(defvar magit-wazzup-buffer-name "*magit-wazzup*"
  "Name of buffer used to display commits not merged into current HEAD.")

;;;###autoload
(defun magit-wazzup (branch)
  "Show a list of branches in a dedicated buffer.
Unlike in the buffer created by `magit-branch-manager' each
branch can be expanded to show a list of commits not merged
into the selected branch."
  (interactive
   (let ((branch (magit-get-current-branch)))
     (list (if current-prefix-arg
               (magit-read-rev "Wazzup branch" branch)
             branch))))
  (magit-mode-setup magit-wazzup-buffer-name nil
                    #'magit-wazzup-mode
                    #'magit-refresh-wazzup-buffer branch))

(defun magit-refresh-wazzup-buffer (head)
  (magit-with-section (section wazzupbuf 'wazzupbuf)
    (run-hooks 'magit-wazzup-sections-hook)))

(defun magit-insert-wazzup-branches ()
  (dolist (upstream (magit-list-branches))
    (magit-insert-wazzup-commits upstream (car magit-refresh-args))))

(defun magit-insert-wazzup-commits (upstream head)
  (let ((count (string-to-number
                (magit-git-string "rev-list" "--count" "--right-only"
                                  (concat head "..." upstream))))
        (label (magit-format-ref-label upstream))
        (focus (string-match-p (format "^refs/heads/%s$" head) upstream))
        s)
    (when (or (> count 0) focus)
      (magit-with-section
          (section wazzup upstream
                   (format "%3s %s\n"
                           (if focus
                               (propertize " * " 'face 'magit-branch)
                             count)
                           (magit-format-ref-label upstream))
		   t)
        (if (magit-section-hidden section)
            (setf (magit-section-washer section)
                  (apply-partially #'magit-insert-wazzup-cherries
                                   head upstream))
          (magit-insert-wazzup-cherries head upstream))
        (setq s section))
      (magit-put-face-property (+ (magit-section-beginning s) 4)
                               (- (magit-section-content-beginning s) 1)
                               (get-text-property 0 'face label)))))

(defun magit-insert-wazzup-cherries (head upstream)
  (let ((beg (point)))
    (magit-git-insert "cherry" "-v" "--abbrev" head upstream)
    (save-restriction
      (narrow-to-region beg (point))
      (goto-char (point-min))
      (magit-wash-log 'cherry))))

;;;; Branch Manager Mode

(define-derived-mode magit-branch-manager-mode magit-mode "Magit Branch"
  "Mode for looking at git branches.

\\<magit-branch-manager-mode-map>\
Type `\\[magit-visit-item]` to checkout a branch, \
`\\[magit-reset-head]' to reset current branch,
you can also merge the branch with `\\[magit-merge-popup]`

Type `\\[magit-discard-item]' to delete a branch, \
or `\\[universal-argument] \\[magit-discard-item]' to force the deletion.
Type `\\[magit-rename-item]' to Rename a branch.

More information can be found in Info node `(magit)The branch list'

\\{magit-branch-manager-mode-map}
Unless shadowed by the mode specific bindings above, bindings
from the parent keymap `magit-mode-map' are also available.")

(defvar magit-branches-buffer-name "*magit-branches*"
  "Name of buffer used to display and manage branches.")

;;;###autoload
(defun magit-branch-manager ()
  "Show a list of branches in a dedicated buffer."
  (interactive)
  (magit-mode-setup magit-branches-buffer-name nil
                    #'magit-branch-manager-mode
                    #'magit-refresh-branch-manager))

(defun magit-refresh-branch-manager ()
  (magit-with-section (section branchbuf 'branchbuf nil t)
    (run-hooks 'magit-branch-manager-sections-hook)))

(defun magit-rename-item ()
  "Rename the item at point."
  (interactive)
  (magit-section-action rename ()
    (branch (call-interactively 'magit-branch-rename))
    (remote (call-interactively 'magit-remote-rename))))

(defconst magit-wash-branch-line-re
  (concat "^"
          "\\(?1:[ \\*]\\) "                ; marker
          "\\(?2:[^ ]+?\\)"                 ; branch
          "\\(?3: +\\)"                     ; fill
          "\\(?4:[0-9a-fA-F]+\\) "          ; sha1
          "\\(?:\\["
          "\\(?5:[^:\n]+?\\)\\(?:: \\)?"    ; tracked
          "\\(?:ahead \\(?6:[0-9]+\\)\\)?"  ; ahead
          "\\(?:, \\)?"
          "\\(?:behind \\(?7:[0-9]+\\)\\)?" ; behind
          "\\] \\)?"
          "\\(?8:.*\\)"))                   ; message

(defvar magit-local-branch-format "%c %n%f %3a %3b %t\n")
(defvar magit-remote-branch-format "  %n\n")
(defvar magit-tags-format "  %n\n")

(defun magit-insert-local-branches ()
  (let ((hash-length (magit-abbrev-length))
        (branches (magit-list-local-branch-names)))
    (magit-with-section (section local "." "Branches:")
      (dolist (line (magit-git-lines "branch" "-vv"))
        (string-match magit-wash-branch-line-re line)
        (magit-bind-match-strings
            (marker branch fill hash tracked ahead behind message) line
          (magit-with-section (section branch branch)
            (magit-insert
             (format-spec
              magit-local-branch-format
              `((?a . ,(or ahead ""))
                (?b . ,(or behind ""))
                (?c . ,(propertize marker 'face 'magit-branch))
                (?f . ,fill)
                (?m . ,message)
                (?n . ,(propertize branch 'face 'magit-log-head-label-local))
                (?s . ,(if hash
                           (propertize hash 'face 'magit-log-sha1)
                         (make-string hash-length ?\s)))
                (?t . ,(if tracked
                           (propertize tracked 'face
                                       (if (member tracked branches)
                                           'magit-log-head-label-local
                                         'magit-log-head-label-remote))
                         ""))))))))
      (insert ?\n))))

(defun magit-insert-remote-branches ()
  (dolist (remote (magit-git-lines "remote"))
    (let ((url     (magit-get "remote" remote "url"))
          (pushurl (magit-get "remote" remote "pushurl")))
      (magit-with-section
          (section remote remote
                   (format "%s (%s):" (capitalize remote)
                           (concat url (and url pushurl ", ") pushurl)))
        (dolist (branch (magit-list-remote-branch-names remote))
          (magit-with-section (s branch branch)
            (magit-insert
             (format-spec
              magit-remote-branch-format
              `((?n . ,(propertize
                        (if (string-match (format "^%s/\\(.+\\)" remote) branch)
                            (match-string 1 branch)
                          branch)
                        'face 'magit-log-head-label-remote)))))))
        (insert ?\n)))))

(defun magit-insert-tags ()
  (magit-with-section (section tags 'tags "Tags:")
    (dolist (tag (magit-git-lines "tag"))
      (magit-with-section (section tag 'tag)
        (magit-insert
         (format-spec magit-tags-format
                      `((?n . ,(propertize tag 'face 'magit-tag)))))))
    (insert ?\n)))

;;; Miscellaneous
;;;; Miscellaneous Commands

(defun magit-copy-item-as-kill ()
  "Copy sha1 of commit at point into kill ring."
  (interactive)
  (magit-section-action copy (info)
    ((branch commit mcommit file diff)
     (kill-new info)
     (message "%s" info))))

(defun magit-ignore-item (edit &optional local)
  "Ignore the item at point.
With a prefix argument edit the ignore string."
  (interactive "P")
  (magit-section-action ignore (info)
    ([file untracked]
     (magit-ignore-file (concat "/" info) edit local)
     (magit-refresh))
    (diff
     (when (yes-or-no-p (format "%s is tracked.  Untrack and ignore? " info))
       (magit-ignore-file (concat "/" info) edit local)
       (magit-run-git "rm" "--cached" info)))))

(defun magit-ignore-item-locally (edit)
  "Ignore the item at point locally only.
With a prefix argument edit the ignore string."
  (interactive "P")
  (magit-ignore-item edit t))

(defun magit-ignore-file (file &optional edit local)
  "Add FILE to the list of files to ignore.
If EDIT is non-nil, prompt the user for the string to be ignored
instead of using FILE.  The changes are written to .gitignore
except if LOCAL is non-nil in which case they are written to
.git/info/exclude."
  (let* ((local-ignore-dir (magit-git-dir "info/"))
         (ignore-file (if local
                          (concat local-ignore-dir "exclude")
                        ".gitignore")))
    (when edit
      (setq file (magit-ignore-edit-string file)))
    (when (and local (not (file-exists-p local-ignore-dir)))
      (make-directory local-ignore-dir t))
    (with-temp-buffer
      (when (file-exists-p ignore-file)
        (insert-file-contents ignore-file))
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert file "\n")
      (write-region nil nil ignore-file))))

(defun magit-ignore-edit-string (file)
  "Prompt the user for the string to be ignored.
A list of predefined values with wildcards is derived from the
filename FILE."
  (let* ((extension (concat "*." (file-name-extension file)))
         (extension-in-dir (concat (file-name-directory file) extension))
         (filename (file-name-nondirectory file))
         (completions (list extension extension-in-dir filename file)))
    (magit-completing-read "File/pattern to ignore"
                           completions nil nil nil nil file)))

;;;; ChangeLog

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
      (magit-visit-item)
      (setq buf (current-buffer)
            pos (point)))
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

;;;; Read Repository

(defun magit-read-top-dir (dir)
  "Ask the user for a Git repository.
The choices offered by auto-completion will be the repositories
under `magit-repo-dirs'.  If `magit-repo-dirs' is nil or DIR is
non-nil, then autocompletion will offer directory names."
  (if (and (not dir) magit-repo-dirs)
      (let* ((repos (magit-list-repos-uniquify
                     (--map (cons (file-name-nondirectory it) it)
                            (magit-list-repos))))
             (reply (magit-completing-read "Git repository" repos)))
        (file-name-as-directory
         (or (cdr (assoc reply repos))
             (if (file-directory-p reply)
                 (expand-file-name reply)
               (user-error "Not a repository or a directory: %s" reply)))))
    (file-name-as-directory
     (read-directory-name "Git repository: "
                          (or (magit-get-top-dir) default-directory)))))

(defun magit-list-repos ()
  (--mapcat (magit-list-repos* it magit-repo-dirs-depth) magit-repo-dirs))

(defun magit-list-repos* (directory depth)
  (cond ((file-exists-p (expand-file-name ".git" directory))
         (list directory))
        ((> depth 0)
         (cl-loop for file in (directory-files directory t "^[^.]" t)
                  when (file-directory-p file)
                  append (magit-list-repos* file (1- depth))))))

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
                        (mapcar
                         (lambda (elt)
                           (cons (concat key "\\"
                                         (file-name-nondirectory
                                          (directory-file-name
                                           (substring elt 0 (- (length key))))))
                                 elt))
                         value))))))
     dict)
    result))

;;;; Wip Minor Mode

;;;###autoload
(define-minor-mode magit-wip-save-mode
  "On each save, also commit to a work-in-progress ref.

After saving the buffer this mode also commits the changes to
the work-in-progress ref for the current branch.  Use option
`magit-wip-ref-format' to configure what refname is used.

While this mode can be activated manually it is better to do
so using either

  git config --add magit.extension wip-save

to activate it in individual repositories or

  git config --global --add magit.extension wip-save

to activate it in all repositories.  These settings only take
effect after _also_ turning on `global-magit-wip-save-mode'."
  :lighter magit-wip-save-mode-lighter
  (if magit-wip-save-mode
      (if (and (buffer-file-name)
               (magit-inside-worktree-p))
          (add-hook 'after-save-hook 'magit-wip-save t t)
        (setq magit-wip-save-mode nil)
        (user-error "Need a repository and a file"))
    (remove-hook 'after-save-hook 'magit-wip-save t)))

;;;###autoload
(define-globalized-minor-mode global-magit-wip-save-mode
  magit-wip-save-mode turn-on-magit-wip-save
  :group 'magit-wip)

(defun turn-on-magit-wip-save ()
  "Conditionally turn on Magit-Wip-Save mode.

If the current buffer visits a file tracked in a Git repository,
then turn on `magit-wip-save-mode' provided the `wip-save' Magit
extension has been enabled in that repository."
  (when (and (buffer-file-name)
             (magit-inside-worktree-p)
             (magit-git-success "ls-files" "--error-unmatch"
                                (buffer-file-name))
             (member "wip-save" (magit-get-all "magit.extension")))
    (magit-wip-save-mode 1)))

(defun magit-wip-save (&optional filename wipref)
  "Commit changes to FILENAME in work-in-progress ref WIPREF.
If optional FILENAME is nil or undefined use `buffer-file-name'.
If optional WIPREF is nil or undefined use a ref in accordance
to the current branch and `magit-wip-ref-format'."
  (let* ((filename (or filename (buffer-file-name)))
         (toplevel (magit-toplevel))
         (blobname (file-relative-name filename toplevel))
         (spec   `((?r . ,blobname)
                   (?a . ,filename)
                   (?t . ,toplevel)))
         (ref    (magit-git-string "symbolic-ref" "HEAD"))
         (wipref (or wipref
                     (format-spec
                      magit-wip-ref-format
                      `((?r . ,(or ref "HEAD"))
                        (?b . ,(if ref (substring ref 11) "HEAD"))))))
         (parent (if (and (magit-rev-parse "--verify" wipref)
                          (equal (magit-git-string "merge-base" wipref ref)
                                 (magit-rev-parse "--verify" ref)))
                     wipref
                   (or ref "HEAD")))
         (tree   (let ((process-environment process-environment)
                       (index-file (make-temp-name "index")))
                   (setenv "GIT_INDEX_FILE" index-file)
                   (magit-call-git "read-tree" parent)
                   (magit-call-git "add" filename)
                   (prog1 (magit-git-string "write-tree")
                     (delete-file index-file)))))
    (when (magit-git-failure "diff-tree" "--exit-code" tree parent)
      (magit-reflog-enable wipref)
      (magit-run-git "update-ref" wipref
                     "-m" (concat "magit-wip-save: " blobname)
                     (magit-git-string
                      "commit-tree" tree "-p" parent
                      "-m" (format-spec magit-wip-commit-message spec)))
      (when magit-wip-save-message
        (message (format-spec magit-wip-save-message spec))))))

;;;; Maintenance Tools

(defun magit-describe-item ()
  "Show information about the section at point.
This command is intended for debugging purposes."
  (interactive)
  (let* ((s (magit-current-section))
         (b (magit-section-beginning s))
         (c (magit-section-content-beginning s))
         (e (magit-section-end s)))
    (message "Section: %S %S [%s (%s) - %s (%s) - %s (%s)]"
             (magit-section-context-type s)
             (magit-section-info s)
             (marker-position b) (marker-insertion-type b)
             (marker-position c) (marker-insertion-type c)
             (marker-position e) (marker-insertion-type e))))

;;; magit.el ends soon

(defconst magit-font-lock-keywords
  (eval-when-compile
    `((,(concat "(\\(" (regexp-opt
                        '("magit-define-section-jumper"
                          "magit-define-popup"))
                "\\)\\_>[ \t'\(]*\\(\\sw+\\|\\s_\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      (,(concat "(" (regexp-opt
                     '("magit-with-section"
                       "magit-cmd-insert-section"
                       "magit-git-insert-section"
                       "magit-insert-line-section"
                       "magit-section-action"
                       "magit-section-case"
                       "magit-bind-match-strings"
                       "magit-with-blob"
                       "magit-tests--with-temp-dir"
                       "magit-tests--with-temp-repo"
                       "magit-tests--with-temp-clone") t)
                "\\_>")
       . 1))))

(font-lock-add-keywords 'emacs-lisp-mode magit-font-lock-keywords)

(defvar magit-version 'undefined
  "The version of Magit that you're using.
Use the function by the same name instead of this variable.")

(defun magit-version (&optional noerror)
  "The version of Magit that you're using.\n\n\(fn)"
  (interactive)
  (let ((toplib (or load-file-name buffer-file-name)))
    (unless (and toplib
                 (equal (file-name-nondirectory toplib) "magit.el"))
      (setq toplib (locate-library "magit.el")))
    (when toplib
      (let* ((dir (file-name-directory toplib))
             (static (expand-file-name "magit-version.el" dir))
             (gitdir (expand-file-name ".git" dir)))
        (cond ((file-exists-p gitdir)
               (setq magit-version
                     (let ((default-directory dir))
                       (magit-git-string "describe" "--tags" "--dirty")))
               (ignore-errors (delete-file static)))
              ((file-exists-p static)
               (load-file static))
              ((featurep 'package)
               (setq magit-version
                     (or (ignore-errors ; < 24.3.50
                           (package-version-join
                            (package-desc-vers
                             (cdr (assq 'magit package-alist)))))
                         (ignore-errors ; >= 24.3.50
                           (package-version-join
                            (package-desc-version
                             (cadr (assq 'magit package-alist)))))))))))
    (if (stringp magit-version)
        (when (called-interactively-p 'any)
          (message "magit-%s" magit-version))
      (if noerror
          (progn (setq magit-version 'error)
                 (message "Cannot determine Magit's version"))
        (error "Cannot determine Magit's version")))
    magit-version))

(cl-eval-when (load eval) (magit-version t))

(provide 'magit)

(require 'magit-extras)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit.el ends here
