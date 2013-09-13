;;; magit.el --- control Git from Emacs

;; Copyright (C) 2008-2013  The Magit Project Developers.
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

;; Author: Marius Vollmer <marius.vollmer@gmail.com>
;; Maintainers:
;;	Jonas Bernoulli   <jonas@bernoul.li>
;;	Nicolas Dudebout  <nicolas.dudebout@gatech.edu>
;;	Rémi Vanicat      <vanicat@debian.org>
;;	Yann Hodique      <yann.hodique@gmail.com>
;; Former-Maintainers:
;;	Peter J. Weisberg <pj@irregularexpressions.net>
;;	Phil Jackson      <phil@shellarchive.co.uk>

;; Keywords: vc tools
;; Package: magit
;; Package-Requires: ((cl-lib "0.3") (git-commit-mode "0.14.0") (git-rebase-mode "0.14.0"))

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

(defvar magit-version 'undefined
  "The version of Magit that you're using.
Use the function by the same name instead of this variable.")
;; The value is set at the end of this file, using the
;; function `magit-version' which is also defined there.

(when (version< emacs-version "23.2")
  (error "Magit requires at least GNU Emacs 23.2"))

(require 'magit-compat)

(require 'git-commit-mode)
(require 'git-rebase-mode)

(require 'ansi-color)
(require 'cl-lib)
(require 'diff-mode)
(require 'easymenu)
(require 'epa)
(require 'grep)
(require 'ring)
(require 'server)

(eval-when-compile
  (require 'dired)
  (require 'dired-x)
  (require 'ediff)
  (require 'eshell)
  (require 'ido)
  (require 'iswitchb)
  (require 'package nil t)
  (require 'view))

(declare-function dired-jump 'dired-x)
(declare-function dired-uncache 'dired)
(declare-function ediff-cleanup-mess 'ediff)
(declare-function eshell-parse-arguments 'eshell)
(declare-function ido-completing-read 'ido)
(declare-function iswitchb-read-buffer 'iswitchb)
(declare-function magit--bisect-info-for-status 'magit-bisect)
(declare-function magit--bisecting-p 'magit-bisect)
(declare-function package-desc-vers 'package)
(declare-function package-desc-version 'package)
(declare-function package-version-join 'package)
(declare-function view-mode 'view)

(defvar magit-custom-options)
(defvar package-alist)


;;; Options
;;;; Setters

(defun magit-set-variable-and-refresh (symbol value)
  "Set SYMBOL to VALUE and call `magit-refresh-all'."
  (set-default symbol value)
  ;; If magit isn't fully loaded yet no buffer that might
  ;; need refreshing can exist and we can take a shortcut.
  (when (featurep 'magit)
    (magit-refresh-all)))

(defun magit-set-default-diff-options (symbol value)
  "Set the default for `magit-diff-options' based on popup value.
Also set the local value in all Magit buffers and refresh them.
\n(fn)" ; The arguments are an internal implementation detail.
  (interactive (list 'magit-diff-options magit-custom-options))
  (set-default symbol value)
  (when (featurep 'magit)
    (dolist (buffer (buffer-list))
      (when (derived-mode-p 'magit-mode)
        (with-current-buffer buffer
          (with-no-warnings
            (setq-local magit-diff-options value))
          (magit-refresh-buffer))))))

;;;; Variables

(defgroup magit nil
  "Controlling Git from Emacs."
  :prefix "magit-"
  :group 'tools)

(custom-add-to-group 'magit 'git-commit 'custom-group)
(custom-add-to-group 'magit 'git-rebase 'custom-group)
(custom-add-to-group 'magit 'vc-follow-symlinks 'custom-variable)

(defcustom magit-git-executable "git"
  "The name of the Git executable."
  :group 'magit
  :type 'string)

(defcustom magit-gitk-executable (executable-find "gitk")
  "The Gitk executable."
  :group 'magit
  :type 'string)

(defcustom magit-emacsclient-executable
  (let ((version (format "%s.%s"
                         emacs-major-version
                         emacs-minor-version)))
    (or (let ((exec-path (list invocation-directory)))
          (or (executable-find (format "emacsclient-%s" version))
              (executable-find (format "emacsclient-%s.exe" version))
              (executable-find "emacsclient")
              (executable-find "emacsclient.exe")))
        (executable-find (format "emacsclient-%s" version))
        (executable-find (format "emacsclient-%s.exe" version))
        (executable-find "emacsclient")
        (executable-find "emacsclient.exe")))
  "The Emacsclient executable.

The default value is the full path to the emacsclient executable
located in the same directory as the executable of the current
Emacs instance.  If the emacsclient cannot be located in that
directory then the first executable found anywhere on the
`exec-path' is used instead.

If no executable can be located then nil becomes the default
value, and some important Magit commands will fallback to an
alternative code path.  However `magit-interactive-rebase'
will stop working at all."
  :group 'magit
  :type '(choice (string :tag "Executable")
                 (const :tag "Don't use Emacsclient" nil)))

(defcustom magit-quote-curly-braces
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
  :group 'magit
  :set-after '(magit-git-executable)
  :type 'boolean)

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

(defcustom magit-set-upstream-on-push nil
  "Whether `magit-push' may use --set-upstream when pushing a branch.
This only applies if the branch does not have an upstream set yet.

nil        don't use --set-upstream.
t          ask if --set-upstream should be used.
`dontask'  always use --set-upstream.
`refuse'   refuse to push unless a remote branch has already been set.

--set-upstream is supported with git > 1.7.0"
  :group 'magit
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Ask" t)
                 (const :tag "Ask if not set" askifnotset)
                 (const :tag "Refuse" refuse)
                 (const :tag "Always" dontask)))

(defcustom magit-refresh-file-buffer-hook
  '(magit-revert-buffer)
  "List of functions to be called to refresh a file visiting buffer.

After many Magit commands, this hook is run for each file
visiting buffer inside the current git repository.

The functions are called without any arguments and with the the
file buffer current.  They have to ensure the same buffer is
still current when they return which can be easily done using:

  (with-current-buffer (current-buffer) DO-STUFF)"
  :group 'magit
  :type 'hook
  :options '(magit-revert-buffer magit-update-vc-modeline))

(defcustom magit-save-some-buffers t
  "Whether \\[magit-status] saves modified buffers before running.

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

(defcustom magit-default-tracking-name-function
  'magit-default-tracking-name-remote-plus-branch
  "Function used to generate default tracking branch names
when doing a \\[magit-checkout].

The default is `magit-default-tracking-name-remote-plus-branch',
which generates a tracking name of the form \"REMOTE-BRANCHNAME\"."
  :group 'magit
  :type '(radio (function-item magit-default-tracking-name-remote-plus-branch)
                (function-item magit-default-tracking-name-branch-only)
                (function :tag "Other")))

(defcustom magit-commit-mode-show-buttons t
  "Whether to show navigation buttons in the *magit-commit* buffer."
  :group 'magit
  :type 'boolean)

(defcustom magit-merge-warn-dirty-worktree t
  "Whether to issue a warning when attempting to start a merge in a dirty worktree."
  :group 'magit
  :type 'boolean
  :package-version '(magit . "1.3.0"))

(defcustom magit-sha1-abbrev-length 7
  "The number of digits to show when a sha1 is displayed in abbreviated form."
  :group 'magit
  :type 'integer)

(defcustom magit-log-cutoff-length 100
  "The maximum number of commits to show in the log and whazzup buffers."
  :group 'magit
  :type 'integer)

(defcustom magit-log-infinite-length 99999
  "Number of log used to show as maximum for `magit-log-cutoff-length'."
  :group 'magit
  :type 'integer)

(defcustom magit-log-auto-more nil
  "Insert more log entries automatically when moving past the last entry.

Only considered when moving past the last entry with
`magit-goto-*-section' commands."
  :group 'magit
  :type 'boolean)

(defcustom magit-log-show-author-date t
  "Show author and date for each commit in short log mode."
  :group 'magit
  :type 'boolean)

(defcustom magit-log-author-date-max-length 25
  "The maximum width of the author-date margin in log buffers."
  :type 'integer
  :group 'magit)

(defcustom magit-log-show-gpg-status nil
  "Display signature verification information as part of the log."
  :group 'magit
  :type 'boolean)

(defcustom magit-status-insert-sections-hook
  '(magit-insert-status-local-line
    magit-insert-status-remote-line
    magit-insert-status-head-line
    magit-insert-status-tags-line
    magit-insert-status-merge-line
    magit-insert-status-rebase-lines
    magit-insert-empty-line
    magit-insert-stashes
    magit-insert-untracked-files
    magit-insert-pending-changes
    magit-insert-pending-commits
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
  :group 'magit
  :type 'hook)

(defcustom magit-status-insert-tags-line nil
  "Whether to display related tags in the status buffer.

Also see option `magit-status-tags-line-subject' which controls how
this information is displayed."
  :group 'magit
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
they are not (due to semantic considerations).

Option `magit-status-insert-tags-line' has to be non-nil for this
information to be displayed at all."
  :group 'magit
  :type '(choice (const :tag "tags are the subjects" tag)
                 (const :tag "head is the subject" head)))

(defcustom magit-status-verbose-untracked t
  "Whether to show the contents of or just the untracked directory."
  :group 'magit
  :type '(choice (const :tag "show only directory" nil)
                 (const :tag "show directory contents" t)))

(defcustom magit-process-popup-time -1
  "Popup the process buffer if a command takes longer than this many seconds."
  :group 'magit
  :type '(choice (const :tag "Never" -1)
                 (const :tag "Immediately" 0)
                 (integer :tag "After this many seconds")))

(defcustom magit-revert-item-confirm t
  "Require acknowledgment before reverting an item."
  :group 'magit
  :type 'boolean)

(defcustom magit-remote-ref-format 'remote-slash-branch
  "What format to use for autocompleting refs, in pariticular for remotes.

Autocompletion is used by functions like `magit-checkout',
`magit-interactive-rebase' and others which offer branch name
completion.

The value 'name-then-remote means remotes will be of the form
\"branch (remote)\", while the value 'remote-slash-name means that
they'll be of the form \"remote/branch\".  I.e. something that's
listed as \"remotes/upstream/next\" by \"git branch -l -a\" will
be \"upstream/next\"."
  :group 'magit
  :version "1.3.0"
  :type '(choice (const :tag "branch (remote)" branch-then-remote)
                 (const :tag "remote/branch" remote-slash-branch)))

(defcustom magit-process-connection-type (not (eq system-type 'cygwin))
  "Connection type used for the git process.

If nil, use pipes: this is usually more efficient, and works on Cygwin.
If t, use ptys: this enables magit to prompt for passphrases when needed."
  :group 'magit
  :type '(choice (const :tag "pipe" nil)
                 (const :tag "pty" t)))

(defcustom magit-process-yes-or-no-prompt-regexp
   " [\[(]\\([Yy]\\(?:es\\)?\\)[/|]\\([Nn]o?\\)[\])] ?[?:] ?$"
  "Regexp matching Yes-or-No prompts of git and its subprocesses."
  :group 'magit
  :type 'regexp)

(defcustom magit-process-password-prompt-regexps
  '("^\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$"
    "^\\(Enter \\)?[Pp]assword\\( for '.*'\\)?: ?$"
    "^.*'s password: ?$"
    "^Yubikey for .*: ?$")
  "List of regexps matching password prompts of git and its subprocesses."
  :group 'magit
  :type '(repeat (regexp)))

(defcustom magit-process-username-prompt-regexps
  '("^Username for '.*': ?$")
  "List of regexps matching username prompts of git and its subprocesses."
  :group 'magit
  :type '(repeat (regexp)))

(defconst magit-server-window-type
  '(choice
    (const :tag "Use selected window"
           :match (lambda (widget value)
                    (not (functionp value)))
           nil)
    (function-item :tag "Display in new frame" switch-to-buffer-other-frame)
    (function-item :tag "Use pop-to-buffer" pop-to-buffer)
    (function :tag "Other function")))

(defcustom magit-server-window-for-commit 'pop-to-buffer
  "Function used to select a window for displaying commit message buffers.
It should take one argument (a buffer) and display and select it.
A common value is `pop-to-buffer'.  It can also be nil in which
case the selected window is used."
  :group 'magit
  :type magit-server-window-type)

(defcustom magit-server-window-for-rebase server-window
  "Function used to select a window for displaying interactive rebase buffers.
It should take one argument (a buffer) and display and select it.
A common value is `pop-to-buffer'.  It can also be nil in which
case the selected window is used."
  :group 'magit
  :type magit-server-window-type
  :set-after '(server-window))

(defcustom magit-completing-read-function 'magit-builtin-completing-read
  "Function to be called when requesting input from the user."
  :group 'magit
  :type '(radio (function-item magit-iswitchb-completing-read)
                (function-item magit-ido-completing-read)
                (function-item magit-builtin-completing-read)
                (function :tag "Other")))

(defcustom magit-status-buffer-switch-function 'pop-to-buffer
  "Function for `magit-status' to use for switching to the status buffer.

The function is given one argument, the status buffer."
  :group 'magit
  :type '(radio (function-item switch-to-buffer)
                (function-item pop-to-buffer)
                (function :tag "Other")))

(defcustom magit-restore-window-configuration nil
  "Whether quitting a Magit buffer restores previous window configuration.

Function `magit-display-mode-buffer' is used to display and
select Magit buffers.  Unless the buffer was already displayed in
a window of the selected frame it also stores the previous window
configuration.  If this option is non-nil that configuration will
later be restored by `magit-quit-window', provided the buffer has
not since been displayed in another frame.

This works best when only two windows are usually displayed in a
frame.  If this isn't the case setting this to t might often lead
to undesirable behaviour.  Also quitting a Magit buffer while
another Magit buffer that was created earlier is still displayed
will cause that buffer to be hidden, which might or might not be
what you want."
  :group 'magit
  :type 'boolean)

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

(defcustom magit-highlight-whitespace t
  "Specify where to highlight whitespace errors.
See `magit-highlight-trailing-whitespace',
`magit-highlight-indentation'.  The symbol t means in all diffs,
`status' means only in the status buffer, and nil means nowhere."
  :group 'magit
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (const :tag "In status buffer" status))
  :set 'magit-set-variable-and-refresh)

(defcustom magit-highlight-trailing-whitespace t
  "Whether to highlight whitespace at the end of a line in diffs.
Used only when `magit-highlight-whitespace' is non-nil."
  :group 'magit
  :type 'boolean
  :set 'magit-set-variable-and-refresh)

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
  :type `(repeat (cons (string :tag "Directory regexp")
                       (choice (const :tag "Tabs" tabs)
                               (integer :tag "Spaces" :value ,tab-width)
                               (const :tag "Neither" nil))))
  :set 'magit-set-variable-and-refresh)

(defcustom magit-diff-options nil
  "Git options used to display diffs.
For more information about the options see man:git-diff.
This variable can be conveniently set in Magit buffers
using `magit-key-mode-popup-diff-options' (bound to \
\\<magit-mode-map>\\[magit-key-mode-popup-diff-options])."
  :group 'magit
  :type '(set :greedy t
              (const :tag
                     "--minimal              Show smallest possible diff"
                     "--minimal")
              (const :tag
                     "--patience             Use patience diff algorithm"
                     "--patience")
              (const :tag
                     "--histogram            Use histogram diff algorithm"
                     "--histogram")
              (const :tag
                     "--ignore-space-change  Ignore whitespace changes"
                     "--ignore-space-change")
              (const :tag
                     "--ignore-all-space     Ignore all whitespace"
                     "--ignore-all-space")
              (const :tag
                     "--function-context     Show surrounding functions"
                     "--function-context"))
  :set 'magit-set-default-diff-options)

(put 'magit-diff-options 'permanent-local t)

(defcustom magit-diff-refine-hunk nil
  "Show fine (word-granularity) differences within diff hunks.

There are three possible settings:

nil    never show fine differences
t      show fine differences for the selected diff hunk only
`all'  show fine differences for all displayed diff hunks"
  :group 'magit
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Selected only" t)
                 (const :tag "All" all))
  :set 'magit-set-variable-and-refresh)

(defcustom magit-diff-use-overlays t
  "Whether to use overlays to highlight various diffs components.

Visualizing added and removed lines in diffs using the background
color conflicts with also using the background color to highlight
the current item.  This can be overcome by using overlays in both
cases - but this comes with a performance penalty, which might or
might not be noticable.

When the background color is used in both cases then using text
properties for changes lines is not an option because overlays
always override text properties and that would cause the changed
lines inside the selected hunk to lose their distinct look.  This
also effects diff hunk and diff file headers, but that isn't as
severe.

Multiple solutions to this problem exist:

1. Use overlays with a higher priority than the highlighting to
   set the background color of added/remove lines.  When this
   option is non-nil (the default) this is exactly what is done.

2. Highlight the current item with something other than the
   background color.  One possibility is to make the selected
   item bold, which is quiet ugly but always works.

   This used to be the default and we relied on theme authors or
   the user herself to override this using the third option below.

   Setting this option to nil causes `magit-item-hightlight's
   default to use bold for highlighting.  (It's default is set
   after the possibly customized value of this option is set.)

3. Use the foreground color to give added/removed lines a distinct
   look and continue to use the background color for highlighting
   the selected item.

Both the first and third option are visually appealing.  While
the first is the default, the third is actually the preferred
option because it not only looks okay but also doesn't come with
a performance penaltiy.

Because using overlays is less efficient than using properties
the first (and default) option is not a good fit when you often
deal with large diffs.  In that case you either have to live with
the the performance issue, use the second option, or take the
time to implement the third for the theme you are using yourself.

The faces involved in this are `magit-item-highlight',
`magit-diff-add', `magit-diff-del', `magit-diff-none',
`magit-diff-hunk-header' and `magit-diff-file-header'."
  :group 'magit
  :type 'boolean)

(defcustom magit-expand-staged-on-commit nil
  "Whether to expand staged changes when creating a commit.
When this is non-nil and the current buffer is the status buffer
expand the section containing staged changes.  If this is `full'
always expand all subsections; if it is t subsections that were
previously hidden remain hidden.

In the event that expanding very large patches takes a long time
\\<global-map>\\[keyboard-quit] can be used to abort that step.
This is especially useful when you would normally not look at the
changes, e.g. because you are committing some binary files."
  :group 'magit
  :type '(choice (const :tag "Expand all subsections" full)
                 (const :tag "Expand top section" t)
                 (const :tag "Don't expand" nil)))

;; Not an option to avoid advertising it.
(defvar magit-rigid-key-bindings nil
  "Use rigid key bindings instead of thematic key popups.
If you enable this a lot of functionality is lost.  You most
likely don't want that.  This variable only has an effect if
set before loading libary `magit'.")

;;;; Faces

(defgroup magit-faces nil
  "Customize the appearance of Magit."
  :prefix "magit-"
  :group 'faces
  :group 'magit)

;; Add to faces group because it affects `magit-item-highlight's
;; default, and because the doc-strings of many faces refer to it.
(custom-add-to-group 'magit-faces 'magit-diff-use-overlays 'custom-group)

(custom-add-to-group 'magit-faces 'git-commit-faces 'custom-group)
(custom-add-to-group 'magit-faces 'git-rebase-faces 'custom-group)

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
  '((t :inherit diff-file-header))
  "Face for diff file header lines."
  :group 'magit-faces)

(defface magit-diff-hunk-header
  '((t :inherit diff-hunk-header))
  "Face for diff hunk header lines."
  :group 'magit-faces)

(defface magit-diff-add
  '((t :inherit diff-added))
  "Face for lines in a diff that have been added.
Also see option `magit-diff-use-overlays'."
  :group 'magit-faces)

(defface magit-diff-del
  '((t :inherit diff-removed))
  "Face for lines in a diff that have been deleted.
Also see option `magit-diff-use-overlays'."
  :group 'magit-faces)

(defface magit-diff-none
  '((t :inherit diff-context))
  "Face for lines in a diff that are unchanged.
Also see option `magit-diff-use-overlays'."
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

(defface magit-log-author-date-cutoff
  '((t :inherit magit-log-author
       :bold t))
  "Face for the author element's cutoff mark."
  :group 'magit-faces)

(defface magit-log-date
  '((t))
  "Face for the date element of the log output."
  :group 'magit-faces)

(defface magit-log-message
  '((t))
  "Face for the message element of the log output."
  :group 'magit-faces)

(defface magit-item-highlight
  (if magit-diff-use-overlays
      '((((class color) (background light))
         :background "darkseagreen2")
        (((class color) (background dark))
         :background "darkolivegreen")
        (((class color) (min-colors 8))
         :background "blue"
         :foreground "white")
        (t :inverse-video t))
    '((t :bold t)))
  "Face for highlighting the current item.
Also see option `magit-diff-use-overlays'."
  :group 'magit-faces
  :set-after '(magit-diff-use-overlays))

(defface magit-item-mark
  '((t :inherit secondary-selection))
  "Face for highlighting marked item."
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

(defface magit-valid-signature
  (if (require 'epa nil t)
      '((t :inherit epa-validity-high))
    '((t :weight bold :foreground "PaleTurquoise")))
  "Face for valid gpg signatures."
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


;;; Keymaps

(defvar magit-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "n") 'magit-goto-next-section)
    (define-key map (kbd "p") 'magit-goto-previous-section)
    (define-key map (kbd "^") 'magit-goto-parent-section)
    (define-key map (kbd "M-n") 'magit-goto-next-sibling-section)
    (define-key map (kbd "M-p") 'magit-goto-previous-sibling-section)
    (define-key map (kbd "TAB") 'magit-toggle-section)
    (define-key map (kbd "<backtab>") 'magit-expand-collapse-section)
    (define-key map (kbd "1") 'magit-show-level-1)
    (define-key map (kbd "2") 'magit-show-level-2)
    (define-key map (kbd "3") 'magit-show-level-3)
    (define-key map (kbd "4") 'magit-show-level-4)
    (define-key map (kbd "M-1") 'magit-show-level-1-all)
    (define-key map (kbd "M-2") 'magit-show-level-2-all)
    (define-key map (kbd "M-3") 'magit-show-level-3-all)
    (define-key map (kbd "M-4") 'magit-show-level-4-all)
    (define-key map (kbd "M-h") 'magit-show-only-files)
    (define-key map (kbd "M-H") 'magit-show-only-files-all)
    (define-key map (kbd "M-s") 'magit-show-level-4)
    (define-key map (kbd "M-S") 'magit-show-level-4-all)
    (define-key map (kbd "g") 'magit-refresh)
    (define-key map (kbd "G") 'magit-refresh-all)
    (define-key map (kbd "?") 'magit-describe-item)
    (define-key map (kbd ":") 'magit-git-command)
    (define-key map (kbd "C-x 4 a") 'magit-add-change-log-entry-other-window)
    (define-key map (kbd "L") 'magit-add-change-log-entry-no-option)
    (define-key map (kbd "RET") 'magit-visit-item)
    (define-key map (kbd "SPC") 'magit-show-item-or-scroll-up)
    (define-key map (kbd "DEL") 'magit-show-item-or-scroll-down)
    (define-key map (kbd "C-w") 'magit-copy-item-as-kill)
    (define-key map (kbd "R") 'magit-rebase-step)
    (cond (magit-rigid-key-bindings
           (define-key map (kbd "c") 'magit-commit)
           (define-key map (kbd "m") 'magit-merge)
           (define-key map (kbd "b") 'magit-checkout)
           (define-key map (kbd "M") 'magit-branch-manager)
           (define-key map (kbd "r") 'undefined)
           (define-key map (kbd "f") 'magit-fetch-current)
           (define-key map (kbd "F") 'magit-pull)
           (define-key map (kbd "!") 'magit-shell-command)
           (define-key map (kbd "P") 'magit-push)
           (define-key map (kbd "t") 'magit-tag)
           (define-key map (kbd "l") 'magit-log)
           (define-key map (kbd "o") 'magit-submodule-update)
           (define-key map (kbd "B") 'undefined))
          (t
           (define-key map (kbd "c") 'magit-key-mode-popup-committing)
           (define-key map (kbd "m") 'magit-key-mode-popup-merging)
           (define-key map (kbd "b") 'magit-key-mode-popup-branching)
           (define-key map (kbd "M") 'magit-key-mode-popup-remoting)
           (define-key map (kbd "r") 'magit-key-mode-popup-rewriting)
           (define-key map (kbd "f") 'magit-key-mode-popup-fetching)
           (define-key map (kbd "F") 'magit-key-mode-popup-pulling)
           (define-key map (kbd "!") 'magit-key-mode-popup-running)
           (define-key map (kbd "P") 'magit-key-mode-popup-pushing)
           (define-key map (kbd "t") 'magit-key-mode-popup-tagging)
           (define-key map (kbd "l") 'magit-key-mode-popup-logging)
           (define-key map (kbd "o") 'magit-key-mode-popup-submodule)
           (define-key map (kbd "B") 'magit-key-mode-popup-bisecting)))
    (define-key map (kbd "$") 'magit-display-process)
    (define-key map (kbd "E") 'magit-interactive-rebase)
    (define-key map (kbd "e") 'magit-ediff)
    (define-key map (kbd "w") 'magit-wazzup)
    (define-key map (kbd "y") 'magit-cherry)
    (define-key map (kbd "q") 'magit-quit-window)
    (define-key map (kbd "x") 'magit-reset-head)
    (define-key map (kbd "v") 'magit-revert-item)
    (define-key map (kbd "a") 'magit-apply-item)
    (define-key map (kbd "A") 'magit-cherry-pick-item)
    (define-key map (kbd "d") 'magit-diff-working-tree)
    (define-key map (kbd "D") 'magit-diff)
    (define-key map (kbd "-") 'magit-diff-smaller-hunks)
    (define-key map (kbd "+") 'magit-diff-larger-hunks)
    (define-key map (kbd "0") 'magit-diff-default-hunks)
    (define-key map (kbd "h") 'magit-key-mode-popup-diff-options)
    (define-key map (kbd "H") 'magit-toggle-diff-refine-hunk)
    (define-key map (kbd "M-g") 'magit-goto-diffstats)
    map))

(defvar magit-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-b") 'magit-show-commit-backward)
    (define-key map (kbd "C-c C-f") 'magit-show-commit-forward)
    map))

(defvar magit-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") 'magit-stage-item)
    (define-key map (kbd "S") 'magit-stage-all)
    (define-key map (kbd "u") 'magit-unstage-item)
    (define-key map (kbd "U") 'magit-unstage-all)
    (define-key map (kbd "i") 'magit-ignore-item)
    (define-key map (kbd "I") 'magit-ignore-item-locally)
    (define-key map (kbd ".") 'magit-mark-item)
    (define-key map (kbd "=") 'magit-diff-with-mark)
    (define-key map (kbd "k") 'magit-discard-item)
    (define-key map (kbd "C") 'magit-commit-add-log)
    (define-key map (kbd "X") 'magit-reset-working-tree)
    (if magit-rigid-key-bindings
        (define-key map (kbd "z") 'magit-stash)
      (define-key map (kbd "z") 'magit-key-mode-popup-stashing))
    map))

(eval-after-load 'dired-x
  '(define-key magit-status-mode-map [remap dired-jump] 'magit-dired-jump))

(defvar magit-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ".") 'magit-mark-item)
    (define-key map (kbd "=") 'magit-diff-with-mark)
    (define-key map (kbd "e") 'magit-log-show-more-entries)
    map))

(defvar magit-wazzup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ".") 'magit-mark-item)
    (define-key map (kbd "=") 'magit-diff-with-mark)
    (define-key map (kbd "i") 'magit-ignore-item)
    map))

(defvar magit-branch-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'magit-create-branch)
    (define-key map (kbd "a") 'magit-add-remote)
    (define-key map (kbd "r") 'magit-rename-item)
    (define-key map (kbd "k") 'magit-discard-item)
    (define-key map (kbd "T") 'magit-change-what-branch-tracks)
    map))

(defvar magit-diffstat-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") 'magit-diffstat-ediff)
    map))

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
    ["Commit" magit-key-mode-popup-committing t]
    ["Add log entry" magit-commit-add-log t]
    ["Tag" magit-tag t]
    "---"
    ["Diff working tree" magit-diff-working-tree t]
    ["Diff" magit-diff t]
    ("Log"
     ["Short Log" magit-log t]
     ["Long Log" magit-log-long t]
     ["Reflog" magit-reflog t]
     ["Extended..." magit-key-mode-popup-logging t])
    "---"
    ["Cherry pick" magit-cherry-pick-item t]
    ["Apply" magit-apply-item t]
    ["Revert" magit-revert-item t]
    "---"
    ["Ignore" magit-ignore-item t]
    ["Ignore locally" magit-ignore-item-locally t]
    ["Discard" magit-discard-item t]
    ["Reset head" magit-reset-head t]
    ["Reset working tree" magit-reset-working-tree t]
    ["Stash" magit-stash t]
    ["Snapshot" magit-stash-snapshot t]
    "---"
    ["Branch..." magit-checkout t]
    ["Merge" magit-merge t]
    ["Interactive resolve" magit-interactive-resolve-item t]
    ["Rebase" magit-rebase-step t]
    ("Rewrite"
     ["Start" magit-rewrite-start t]
     ["Stop" magit-rewrite-stop t]
     ["Finish" magit-rewrite-finish t]
     ["Abort" magit-rewrite-abort t]
     ["Set used" magit-rewrite-set-used t]
     ["Set unused" magit-rewrite-set-unused t])
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
    ["Display Git output" magit-display-process t]
    ["Quit Magit" magit-quit-window t]))

;;; Various Utilities
;;;; Minibuffer Input

(defun magit-iswitchb-completing-read
  (prompt choices &optional predicate require-match initial-input hist def)
  "iswitchb-based completing-read almost-replacement."
  (require 'iswitchb)
  (let ((iswitchb-make-buflist-hook
         (lambda ()
           (setq iswitchb-temp-buflist (if (consp (car choices))
                                           (mapcar #'car choices)
                                         choices)))))
    (iswitchb-read-buffer prompt (or initial-input def) require-match)))

(defun magit-ido-completing-read
  (prompt choices &optional predicate require-match initial-input hist def)
  "ido-based completing-read almost-replacement."
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

(defun magit-builtin-completing-read
  (prompt choices &optional predicate require-match initial-input hist def)
  "Magit wrapper for standard `completing-read' function."
  (let ((reply (completing-read
                (if (and def (> (length prompt) 2)
                         (string-equal ": " (substring prompt -2)))
                    (format "%s (default %s): " (substring prompt 0 -2) def)
                  prompt)
                choices predicate require-match initial-input hist def)))
    (if (string= reply "")
        (if require-match
            (error "Nothing selected")
          nil)
      reply)))

(defun magit-completing-read
  (prompt collection &optional predicate require-match initial-input hist def)
  "Call function in `magit-completing-read-function' to read user input.

Read `completing-read' documentation for the meaning of the argument."
  (funcall magit-completing-read-function
           prompt collection predicate
           require-match initial-input hist def))

;;;; String and File Utilities

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
  (let* ((topdir (magit-get-top-dir))
         (filename (or buffer-file-name
                       (when (buffer-base-buffer)
                         (with-current-buffer (buffer-base-buffer)
                           buffer-file-name))
                       (when magit-file-name
                         (expand-file-name magit-file-name topdir)))))
    (when filename
      (if relative
          (file-relative-name filename topdir)
        filename))))

;;;; Emacsclient Support

(defmacro magit-with-emacsclient (server-window &rest body)
  "Arrange for Git to use Emacsclient as \"the git editor\".

Git processes that use \"the editor\" have to be asynchronous.
The use of this macro ensures that such processes inside BODY use
Emacsclient as \"the editor\" by setting the environment variable
$GIT_EDITOR accordingly around calls to Git and starting the
server if necessary."
  (declare (indent 1))
  `(let* ((process-environment process-environment)
          (magit-process-popup-time -1))
     ;; Make sure the client is usable.
     (magit-assert-emacsclient "use `magit-with-emacsclient'")
     ;; Make sure the server is running.
     (unless server-process
       (if (server-running-p "server")
           (unless (eq system-type 'windows-nt)
             (setq server-name (format "server%s" (emacs-pid)))
             (server-start))
         (server-start)))
     ;; Tell Git to use the client.
     (setenv "GIT_EDITOR"
             (concat magit-emacsclient-executable
                     ;; Tell Emacsclient to use this server,
                     ;; if necessary and possible.
                     (unless (or (equal server-name "server")
                                 (eq system-type 'windows-nt)
                                 server-use-tcp)
                       (concat " --socket-name="
                               (expand-file-name server-name
                                                 server-socket-dir)))))
     (when server-use-tcp
       (setenv "EMACS_SERVER_FILE"
               (expand-file-name server-name server-auth-dir)))
     ;; As last resort fallback to a new Emacs instance.
     (setenv "ALTERNATE_EDITOR"
             (expand-file-name invocation-name invocation-directory))
     ;; Git has to be called asynchronously in BODY or we create a
     ;; dead lock.  By the time Emacsclient is called the dynamic
     ;; binding is no longer in effect and our primitives don't
     ;; support callbacks.  Temporarily set the default value and
     ;; restore the old value using a timer.
     (let ((window ,server-window))
       (unless (equal window server-window)
         (run-at-time "0.2 sec" nil
                      (apply-partially (lambda (value)
                                         (setq server-window value))
                                       server-window))
         (setq-default server-window window)))
     ,@body))

(defun magit-use-emacsclient-p ()
  (and magit-emacsclient-executable
       (cl-find-if #'display-graphic-p (terminal-list))
       (not (and (fboundp 'tramp-tramp-file-p)
                 (tramp-tramp-file-p default-directory)))))

(defun magit-assert-emacsclient (action)
  (unless magit-emacsclient-executable
    (error "Cannot %s when `magit-emacsclient-executable' is nil" action))
  (unless (cl-find-if #'display-graphic-p (terminal-list))
    (error "Cannot %s when no window system is available" action))
  (when (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p default-directory))
    (error "Cannot %s when accessing repository using tramp" action)))

;;; Git Utilities
;;;; Git Output

(defvar magit-git-standard-options '("--no-pager")
  "Standard options when running Git.")

(defun magit-git-string (&rest args)
  "Execute Git with ARGS, returning the first line of its output.
If there is no output return nil.  If the output begins with a
newline return an empty string."
  (with-temp-buffer
    (apply 'process-file magit-git-executable nil (list t nil) nil
           (append magit-git-standard-options args))
    (goto-char (point-min))
    (unless (= (point-min) (point-max))
      (buffer-substring-no-properties
       (line-beginning-position)
       (line-end-position)))))

(defun magit-git-lines (&rest args)
  "Execute Git with ARGS, returning its output as a list of lines.
Empty lines anywhere in the output are omitted."
  (with-temp-buffer
    (apply 'process-file magit-git-executable nil (list t nil) nil
           (append magit-git-standard-options args))
    (goto-char (point-min))
    (let (lines)
      (while (not (eobp))
        (let ((beg (line-beginning-position))
              (end (line-end-position)))
          (unless (= beg end)
            (push (buffer-substring-no-properties beg end) lines)))
        (forward-line 1))
      (nreverse lines))))

(defun magit-git-insert (&rest args)
  "Execute Git with ARGS, inserting its output at point."
  (apply 'magit-cmd-insert magit-git-executable
         (append magit-git-standard-options args)))

(defun magit-cmd-insert (&rest args)
  (insert (with-output-to-string
            (with-current-buffer standard-output
              (apply #'process-file
                     (car args) nil
                     (list t nil) nil
                     (cdr args))))))

(defun magit-git-exit-code (&rest args)
  "Execute Git with ARGS, returning its exit code."
  (apply #'process-file magit-git-executable nil nil nil
         (append magit-git-standard-options args)))

(defun magit-git-success (&rest args)
  "Execute Git with ARGS, returning t if its exit code is 0."
  (= (apply 'magit-git-exit-code args) 0))

(defun magit-decode-git-path (path)
  (if (eq (aref path 0) ?\")
      (string-as-multibyte (read path))
    path))

;;;; Git Config

(defun magit-get (&rest keys)
  "Return the value of Git config entry specified by KEYS."
  (magit-git-string "config" (mapconcat 'identity keys ".")))

(defun magit-get-all (&rest keys)
  "Return all values of the Git config entry specified by KEYS."
  (magit-git-lines "config" "--get-all" (mapconcat 'identity keys ".")))

(defun magit-get-boolean (&rest keys)
  "Return the boolean value of Git config entry specified by KEYS."
  (equal (magit-git-string "config" "--bool" (mapconcat 'identity keys "."))
         "true"))

(defun magit-set (val &rest keys)
  "Set Git config settings specified by KEYS to VAL."
  (if val
      (magit-git-string "config" (mapconcat 'identity keys ".") val)
    (magit-git-string "config" "--unset" (mapconcat 'identity keys "."))))

;;;; Git Low-Level

(defun magit-git-repo-p (dir)
  (file-exists-p (expand-file-name ".git" dir)))

(defun magit-git-dir (&optional path)
  "Return absolute path to the GIT_DIR for the current repository.
If optional PATH is non-nil it has to be a path relative to the
GIT_DIR and its absolute path is returned"
  (let ((gitdir (file-name-as-directory
                 (expand-file-name
                  (magit-git-string "rev-parse" "--git-dir")))))
    (if path
        (expand-file-name (convert-standard-filename path) gitdir)
      gitdir)))

(defun magit-no-commit-p ()
  "Return non-nil if there is no commit in the current git repository."
  (not (magit-git-string "rev-list" "-1" "HEAD")))

(defun magit-get-top-dir (&optional cwd)
  (setq cwd (expand-file-name (file-truename (or cwd default-directory))))
  (when (file-directory-p cwd)
    (let* ((default-directory (file-name-as-directory cwd))
           (cdup (magit-git-string "rev-parse" "--show-cdup")))
      (when cdup
        (file-name-as-directory (expand-file-name cdup cwd))))))

(defun magit-file-relative-name (filename)
  "Return the path of FILENAME relative to its git repository.

If FILENAME is absolute, return a path relative to the git
repository containing it. Otherwise, return a path relative to
the current git repository."
  (let ((topdir (expand-file-name
                 (magit-get-top-dir (file-name-directory filename))))
        (file (file-truename filename)))
    (when (and (not (string= topdir ""))
               ;; FILE must start with the git repository path
               (zerop (string-match-p (concat "\\`" topdir) file)))
      (substring file (length topdir)))))

(defun magit-get-ref (ref)
  (magit-git-string "symbolic-ref" "-q" ref))

(defun magit-get-current-branch ()
  (let ((head (magit-get-ref "HEAD")))
    (when (and head (string-match "^refs/heads/" head))
      (substring head 11))))

(defun magit-get-tracked-branch (&optional branch qualified pretty)
  "Return the name of the tracking branch the local branch name BRANCH.

If optional QUALIFIED is non-nil return the full branch path,
otherwise try to shorten it to a name (which may fail).  If
optional PRETTY is non-nil additionally format the branch name
according to option `magit-remote-ref-format'."
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
                                          (mapconcat 'identity
                                                     (nreverse unique)
                                                     "/")))
                    (setq unique (cons (car prefix) unique)
                          prefix (cdr prefix))))))
            (cond ((not match) nil)
                  (qualified match)
                  ((string-match "^refs/remotes/" match)
                   (if pretty
                       (substring match (+ 14 (length match)))
                     (substring match 13)))
                  (t match))))))))

(defun magit-get-previous-branch ()
  "Return the refname of the previously checked out branch.
Return nil if the previously checked out branch no longer exists."
  (magit-name-rev (magit-git-string "rev-parse" "--verify" "@{-1}")))

(defun magit-get-current-tag (&optional with-distance-p)
  "Return the closest tag reachable from \"HEAD\".

If optional WITH-DISTANCE-P is non-nil then return (TAG COMMITS
DIRTY) where COMMITS is the number of commits in \"HEAD\" but not
in TAG and DIRTY is t if there are uncommitted changes, nil
otherwise."
  (let ((tag (magit-git-string "describe" "--long" "--tags" "--dirty")))
    (save-match-data
      (when tag
        (string-match
         "\\(.+\\)-\\(?:0[0-9]*\\|\\([0-9]+\\)\\)-g[0-9a-z]+\\(-dirty\\)?$" tag)
        (if with-distance-p
            (list (match-string 1 tag)
                  (string-to-number (or (match-string 2 tag) "0"))
                  (and (match-string 3 tag) t))
          (match-string 1 tag))))))

(defun magit-get-next-tag (&optional with-distance-p)
  "Return the closest tag from which \"HEAD\" is reachable.

If no such tag can be found or if the distance is 0 (in which
case it is the current tag, not the next) return nil instead.

If optional WITH-DISTANCE-P is non-nil then return (TAG COMMITS)
where COMMITS is the number of commits in TAG but not in \"HEAD\"."
  (let ((rev (magit-git-string "describe" "--contains" "HEAD")))
    (save-match-data
      (when (and rev (string-match "^[^^~]+" rev))
        (let ((tag (match-string 0 rev)))
          (unless (equal tag (magit-get-current-tag))
            (if with-distance-p
                (list tag (car (magit-rev-diff-count tag "HEAD")))
              tag)))))))

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

(defun magit-ref-exists-p (ref)
  (magit-git-success "show-ref" "--verify" ref) 0)

(defun magit-rev-parse (ref)
  "Return the SHA hash for REF."
  (magit-git-string "rev-parse" ref))

(defun magit-ref-ambiguous-p (ref)
  "Return whether or not REF is ambiguous."
  ;; If REF is ambiguous, rev-parse just prints errors,
  ;; so magit-git-string returns nil.
  (not (magit-git-string "rev-parse" "--abbrev-ref" ref)))

(defun magit-rev-diff-count (a b)
  "Return the commits in A but not B and vice versa.
Return a list of two integers: (A>B B>A)."
  (mapcar 'string-to-number
          (split-string (magit-git-string "rev-list"
                                          "--count" "--left-right"
                                          (concat a "..." b))
                        "\t")))

(defun magit-name-rev (rev &optional no-trim)
  "Return a human-readable name for REV.
Unlike `git name-rev', this will remove \"tags/\" and \"remotes/\"
prefixes if that can be done unambiguously (unless optional arg
NO-TRIM is non-nil).  In addition, it will filter out revs
involving HEAD."
  (when rev
    (let ((name (magit-git-string "name-rev" "--no-undefined" "--name-only" rev)))
      ;; There doesn't seem to be a way of filtering HEAD out from name-rev,
      ;; so we have to do it manually.
      ;; HEAD-based names are too transient to allow.
      (when (and (stringp name)
                 (string-match "^\\(.*\\<HEAD\\)\\([~^].*\\|$\\)" name))
        (let ((head-ref (match-string 1 name))
              (modifier (match-string 2 name)))
          ;; Sometimes when name-rev gives a HEAD-based name,
          ;; rev-parse will give an actual branch or remote name.
          (setq name (concat (magit-git-string "rev-parse" "--abbrev-ref" head-ref)
                             modifier))
          ;; If rev-parse doesn't give us what we want, just use the SHA.
          (when (or (null name) (string-match-p "\\<HEAD\\>" name))
            (setq name (magit-rev-parse rev)))))
      (setq rev (or name rev))
      (when (string-match "^\\(?:tags\\|remotes\\)/\\(.*\\)" rev)
        (let ((plain-name (match-string 1 rev)))
          (unless (or no-trim (magit-ref-ambiguous-p plain-name))
            (setq rev plain-name))))
      rev)))

(defun magit-file-uptodate-p (file)
  (magit-git-success "diff" "--quiet" "--" file))

(defun magit-anything-staged-p ()
  (not (magit-git-success "diff" "--quiet" "--cached")))

(defun magit-everything-clean-p ()
  (and (not (magit-anything-staged-p))
       (magit-git-success "diff" "--quiet")))

(defun magit-commit-parents (commit)
  (cdr (split-string (magit-git-string "rev-list" "-1" "--parents" commit))))

(defun magit-assert-one-parent (commit command)
  (when (> (length (magit-commit-parents commit)) 1)
    (error (format "Cannot %s a merge commit" command))))

(defun magit-format-commit (commit format)
  (magit-git-string "log" "-1" commit
                    (magit-diff-abbrev-arg)
                    (concat "--pretty=format:" format)))

;;;; Git Macros

(defmacro magit-with-refresh (&rest body)
  (declare (indent 0))
  `(magit-refresh-wrapper (lambda () ,@body)))

;;; Revisions and Ranges
;;__ FIXME The parens indicate preliminary subsections.
;;;; (insane "rev" reading)

(defvar-local magit-current-range nil
  "The range described by the current buffer.
This is only non-nil in diff and log buffers.

This has three possible (non-nil) forms.  If it's a string REF or
a singleton list (REF), then the range is from REF to the current
working directory state (or HEAD in a log buffer).  If it's a
pair (START . END), then the range is START..END.")

(defun magit-list-interesting-refs (&optional uninteresting)
  "Return interesting references as given by `git show-ref'.
Removes references matching UNINTERESTING from the results.
UNINTERESTING can be either a function taking a single
argument or a list of strings used as regexps."
  (cl-loop for ref-line in (magit-git-lines "show-ref")
           for ref-name =  (cadr (split-string ref-line " "))
           with ref-fmt
           unless (or (if (functionp uninteresting)
                          (funcall uninteresting ref-name)
                        (cl-loop for i in uninteresting
                                 thereis (string-match i ref-name)))
                      (not (setq ref-fmt (magit-format-ref ref-name))))
           collect (cons ref-fmt
                         (replace-regexp-in-string
                          "^refs/heads/" "" ref-name))))

(defun magit-format-ref (ref)
  "Convert fully-specified ref REF into its displayable form
according to `magit-remote-ref-format'"
  (cond ((null ref)
         nil)
        ((string-match "refs/heads/\\(.*\\)" ref)
         (match-string 1 ref))
        ((string-match "refs/tags/\\(.*\\)" ref)
         (format (if (eq magit-remote-ref-format 'branch-then-remote)
                     "%s (tag)"
                   "%s")
                 (match-string 1 ref)))
        ((string-match "refs/remotes/\\([^/]+\\)/\\(.+\\)" ref)
         (if (eq magit-remote-ref-format 'branch-then-remote)
             (format "%s (%s)"
                     (match-string 2 ref)
                     (match-string 1 ref))
           (substring ref 13)))))

(defvar magit-uninteresting-refs
  '("refs/remotes/\\([^/]+\\)/HEAD$" "refs/stash"))

(defvar magit-read-file-hist nil)

(defun magit-read-file-from-rev (revision)
  (magit-completing-read (format "Retrieve file from %s: " revision)
                         (magit-git-lines "ls-tree" "-r" "--name-only" revision)
                         nil 'require-match
                         nil 'magit-read-file-hist
                         (magit-buffer-file-name t)))

(defvar magit-read-rev-history nil
  "The history of inputs to `magit-read-rev' and `magit-read-tag'.")

(defun magit-read-rev (prompt &optional default uninteresting noselection)
  (let* ((interesting-refs (magit-list-interesting-refs
                            (or uninteresting magit-uninteresting-refs)))
         (reply (magit-completing-read (concat prompt ": ")
                                       interesting-refs nil nil nil
                                       'magit-read-rev-history default))
         (rev (or (cdr (assoc reply interesting-refs)) reply)))
    (unless (or rev noselection)
      (error "No rev selected"))
    rev))

(defun magit-read-rev-with-default (prompt &optional no-trim uninteresting)
  "Ask user for revision like `magit-read-rev' but default is set
appropriately depending on context.  If NO-TRIM is non-nil, strip
off prefixes such as \"refs/remotes/\" (see `magit-name-rev').
PROMPT and UNINTERESTING are passed to `magit-read-rev'."
  (magit-read-rev prompt (magit-default-rev no-trim) uninteresting))

(defun magit-read-rev-range (op &optional def-beg def-end)
  (let ((beg (magit-read-rev (format "%s range or start" op) def-beg)))
    (save-match-data
      (if (string-match "^\\(.+\\)\\.\\.\\(.+\\)$" beg)
          (cons (match-string 1 beg) (match-string 2 beg))
        (cons beg (magit-read-rev (format "%s end" op) def-end nil t))))))

;;;; (worrisome to-git converters)

(defvar magit-marked-commit) ; tempory kludge

(defun magit-rev-to-git (rev)
  (cond ((not rev)
         (error "No revision specified"))
        ((string= rev ".")
         magit-marked-commit)
        (t
         rev)))

(defun magit-rev-range-to-git (range)
  (cond ((not range)
         (error "No revision range specified"))
        ((stringp range)
         range)
        ((cdr range)
         (format "%s..%s"
                 (magit-rev-to-git (car range))
                 (magit-rev-to-git (cdr range))))
        (t
         (magit-rev-to-git (car range)))))

(defun magit-rev-describe (rev)
  (cond ((not rev)
         (error "No revision specified"))
        ((string= rev ".")
         "mark")
        (t
         (magit-name-rev rev))))

(defun magit-rev-range-describe (range things)
  (cond ((not range)
         (error "No revision range specified"))
        ((stringp range)
         (format "%s in %s" things range))
        ((cdr range)
         (format "%s from %s to %s" things
                 (magit-rev-describe (car range))
                 (if (eq (cdr range) 'working)
                     "working directory"
                   (magit-rev-describe (cdr range)))))
        (t
         (format "%s at %s" things (magit-rev-describe (car range))))))

;;;; (default and at-point stuff)

(defun magit-default-rev (&optional no-trim)
  (or (magit-name-rev (magit-commit-at-point) no-trim)
      (let ((branch (magit-guess-branch)))
        (when branch
          (if (string-match "^refs/\\(.*\\)" branch)
              (match-string 1 branch)
            branch)))))

(defun magit-commit-at-point ()
  (let ((section (magit-current-section)))
    (if (and section (eq (magit-section-type section) 'commit))
        (magit-section-info section)
      (get-text-property (point) 'revision))))

;;;; (more reading)

(defun magit-read-remote (prompt &optional def require-match)
  "Read the name of a remote.
PROMPT is used as the prompt, and defaults to \"Remote\".
DEF is the default value.  If optional REQUIRE-MATCH is non-nil then
the user is not allowed to exit unless the input is or completes to
an existing remote."
  (magit-completing-read (concat prompt ": ")
                         (magit-git-lines "remote")
                         nil require-match nil nil
                         (or def (magit-guess-remote))))

(defun magit-read-remote-branch (remote &optional prompt default)
  (let* ((prompt (or prompt (format "Remote branch (in %s)" remote)))
         (branches (cl-mapcan
                    (lambda (b)
                      (and (not (string-match " -> " b))
                           (string-match (format "^ *%s/\\(.*\\)$"
                                                 (regexp-quote remote)) b)
                           (list (match-string 1 b))))
                    (magit-git-lines "branch" "-r")))
         (reply (magit-completing-read (concat prompt ": ") branches
                                       nil nil nil nil default)))
    (unless (string= reply "")
      reply)))

;;; Sections

;; A buffer in magit-mode is organized into hierarchical sections.
;; These sections are used for navigation and for hiding parts of the
;; buffer.
;;
;; Most sections also represent the objects that Magit works with,
;; such as files, diffs, hunks, commits, etc.  The 'type' of a section
;; identifies what kind of object it represents (if any), and the
;; parent and grand-parent, etc provide the context.

(cl-defstruct magit-section
  parent title beginning end children hidden type info
  needs-refresh-on-show)

;;;; Section Variables

(defvar-local magit-top-section nil
  "The top section of the current buffer.")
(put 'magit-top-section 'permanent-local t)

(defvar magit-old-top-section nil)

(defvar magit-section-hidden-default nil)

(defvar magit-show-diffstat t
  "If non-nil, diff and commit log will display diffstat.")

(defvar-local magit-diffstat-cached-sections nil)
(put 'magit-diffstat-cached-sections 'permanent-local t)

;;;; Section Creation

(defmacro magit-define-inserter (sym arglist &rest body)
  (declare (indent defun))
  (let ((fun (intern (format "magit-insert-%s" sym)))
        (before (intern (format "magit-before-insert-%s-hook" sym)))
        (after (intern (format "magit-after-insert-%s-hook" sym)))
        (doc (format "Insert items for `%s'." sym)))
    `(progn
       (defvar ,before nil)
       (defvar ,after nil)
       (defun ,fun ,arglist
         ,doc
         (run-hooks ',before)
         ,@body
         (run-hooks ',after))
       (put ',before 'definition-name ',sym)
       (put ',after 'definition-name ',sym)
       (put ',fun 'definition-name ',sym))))

(defun magit-new-section (title type)
  "Create a new section with title TITLE and type TYPE in current buffer.

If `magit-top-section' buffer local value is nil, the new section
will be the new top-section; otherwise the new-section will be a
child of the current top-section.

If TYPE is nil, the section won't be highlighted."
  (let* ((s (make-magit-section :parent magit-top-section
                                :title title
                                :type type
                                :hidden magit-section-hidden-default))
         (old (and magit-old-top-section
                   (magit-find-section (magit-section-path s)
                                       magit-old-top-section))))
    (if magit-top-section
        (push s (magit-section-children magit-top-section))
      (setq magit-top-section s))
    (when old
      (setf (magit-section-hidden s) (magit-section-hidden old)))
    s))

(defun magit-cancel-section (section)
  "Delete the section SECTION."
  (delete-region (magit-section-beginning section)
                 (magit-section-end section))
  (let ((parent (magit-section-parent section)))
    (if parent
        (setf (magit-section-children parent)
              (delq section (magit-section-children parent)))
      (setq magit-top-section nil))))

(defmacro magit-with-section (title type &rest body)
  "Create a new section of title TITLE and type TYPE and evaluate BODY there.

Sections created inside BODY will become children of the new
section. BODY must leave point at the end of the created section.

If TYPE is nil, the section won't be highlighted."
  (declare (indent 2) (debug (form form body)))
  (let ((s (make-symbol "*section*")))
    `(let* ((,s (magit-new-section ,title ,type))
            (magit-top-section ,s))
       (setf (magit-section-beginning ,s) (point))
       ,@body
       (setf (magit-section-end ,s) (point))
       (setf (magit-section-children ,s)
             (nreverse (magit-section-children ,s)))
       ,s)))

(defun magit-insert-section
  (section-title-and-type buffer-title washer cmd &rest args)
  "Run CMD and put its result in a new section.

SECTION-TITLE-AND-TYPE is either a string that is the title of
the section or (TITLE . TYPE) where TITLE is the title of the
section and TYPE is its type.

If there is no type, or if type is nil, the section won't be
highlighted.

BUFFER-TITLE is the inserted title of the section

WASHER is a function that will be run after CMD.  The buffer will
be narrowed to the inserted text.  It should add sectioning as
needed for Magit interaction.

CMD is an external command that will be run with ARGS as arguments."
  (let* ((body-beg nil)
         (section-title (if (consp section-title-and-type)
                            (car section-title-and-type)
                          section-title-and-type))
         (section-type (if (consp section-title-and-type)
                           (cdr section-title-and-type)
                         nil))
         (section
          (magit-with-section section-title section-type
            (when buffer-title
              (insert (propertize buffer-title 'face 'magit-section-title)
                      "\n"))
            (setq body-beg (point))
            (apply 'magit-cmd-insert cmd args)
            (unless (eq (char-before) ?\n)
              (insert "\n"))
            (when washer
              (save-restriction
                (narrow-to-region body-beg (point))
                (goto-char (point-min))
                (funcall washer)
                (goto-char (point-max)))))))
    (if (= body-beg (point))
        (magit-cancel-section section)
      (insert "\n"))
    section))

(defun magit-git-section (section-title-and-type
                          buffer-title washer &rest args)
  "Run Git and put its result in a new section.
See `magit-insert-section' for meaning of the arguments"
  (apply #'magit-insert-section
         section-title-and-type
         buffer-title
         washer
         magit-git-executable
         (append magit-git-standard-options args)))

(defun magit-set-section (title type start end)
  "Create a new section of title TITLE and type TYPE.
Use the specified START and END positions."
  (let ((section (magit-new-section title type)))
    (setf (magit-section-beginning section) start)
    (setf (magit-section-end section) end)
    section))

(defun magit-set-section-info (info &optional section)
  "Set the info of SECTION.
If SECTION is nil, default to setting `magit-top-section'"
  (setf (magit-section-info (or section magit-top-section)) info))

(defun magit-set-section-needs-refresh-on-show (flag &optional section)
  (setf (magit-section-needs-refresh-on-show
         (or section magit-top-section))
        flag))

(defmacro magit-create-buffer-sections (&rest body)
  "Empty current buffer of text and Magit's sections, and then eval BODY."
  (declare (indent 0) (debug t))
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
  "Add text-property needed for SECTION."
  (put-text-property (magit-section-beginning section)
                     (magit-section-end section)
                     'magit-section section)
  (mapc 'magit-propertize-section
        (magit-section-children section)))

;;;; Section Searching

(defun magit-find-section (path top)
  "Find the section at the path PATH in subsection of section TOP."
  (if (null path)
      top
    (let ((secs (magit-section-children top)))
      (while (and secs (not (equal (car path)
                                   (magit-section-title (car secs)))))
        (setq secs (cdr secs)))
      (when (car secs)
        (magit-find-section (cdr path) (car secs))))))

(defun magit-section-path (section)
  "Return the path of SECTION."
  (let ((parent (magit-section-parent section)))
    (when parent
      (append (magit-section-path parent)
              (list (magit-section-title section))))))

(defun magit-find-section-after (pos)
  "Find the first section that begins after POS."
  (magit-find-section-after* pos (list magit-top-section)))

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
      magit-top-section))

;;;; Section Jumping

(defun magit-goto-next-section ()
  "Go to the next section."
  (interactive)
  (let ((next (magit-find-section-after (point))))
    (if next
        (magit-goto-section next)
      (message "No next section"))))

(defun magit-goto-previous-section ()
  "Go to the previous section."
  (interactive)
  (if (eq (point) 1)
      (message "No previous section")
    (magit-goto-section (magit-find-section-before (point)))))

(defun magit-goto-parent-section ()
  "Go to the parent section."
  (interactive)
  (let ((parent (magit-section-parent (magit-current-section))))
    (when parent
      (goto-char (magit-section-beginning parent)))))

(defun magit-goto-next-sibling-section ()
  "Go to the next sibling section."
  (interactive)
  (let* ((initial (point))
         (section (magit-current-section))
         (end (- (magit-section-end section) 1))
         (parent (magit-section-parent section))
         (siblings (and parent (magit-section-children parent)))
         (next-sibling (magit-find-section-after* end siblings)))
    (if next-sibling
        (magit-goto-section next-sibling)
      (magit-goto-next-section))))

(defun magit-goto-previous-sibling-section ()
  "Go to the previous sibling section."
  (interactive)
  (let* ((section (magit-current-section))
         (beginning (magit-section-beginning section))
         (parent (magit-section-parent section))
         (siblings (and parent (magit-section-children parent)))
         (previous-sibling (magit-find-section-before* beginning siblings)))
    (if previous-sibling
        (magit-goto-section previous-sibling)
      (magit-goto-parent-section))))

(defun magit-goto-section (section)
  (goto-char (magit-section-beginning section))
  (cond
   ((and magit-log-auto-more
         (eq (magit-section-type section) 'longer))
    (magit-log-show-more-entries)
    (forward-line -1)
    (magit-goto-next-section))
   ((and (eq (magit-section-type section) 'commit)
         (derived-mode-p 'magit-log-mode))
    (magit-show-commit section))))

(defun magit-goto-section-at-path (path)
  "Go to the section described by PATH."
  (let ((sec (magit-find-section path magit-top-section)))
    (if sec
        (goto-char (magit-section-beginning sec))
      (message "No such section"))))


(defun magit-goto-diff-section-at-file (file)
  "Go to the section containing by the pathname, FILE"
  (let ((pos (catch 'diff-section-found
               (dolist (sec (magit-section-children magit-top-section))
                 (when (and (eq (magit-section-type sec) 'diff)
                            (string-equal (magit-diff-item-file sec) file))
                   (throw 'diff-section-found
                          (magit-section-beginning sec)))))))
    (when pos
      (goto-char pos))))

(defun magit-goto-diffstats ()
  "Go to the diffstats section if exists"
  (interactive)
  (let ((pos (catch 'section-found
               (dolist (sec (magit-section-children magit-top-section))
                 (when (eq (magit-section-type sec) 'diffstats)
                   (throw 'section-found
                          (magit-section-beginning sec)))))))
    (if pos
        (goto-char pos)
      (when (called-interactively-p 'interactive)
        (message "No diffstats section found")))))

(defmacro magit-define-section-jumper (sym title)
  "Define an interactive function to go to section SYM.
TITLE is the displayed title of the section."
  (let ((fun (intern (format "magit-jump-to-%s" sym)))
        (doc (format "Jump to section `%s'." title)))
    `(progn
       (defun ,fun ()
         ,doc
         (interactive)
         (magit-goto-section-at-path '(,sym)))
       (put ',fun 'definition-name ',sym))))

(magit-define-section-jumper untracked "Untracked files")
(magit-define-section-jumper unstaged  "Unstaged changes")
(magit-define-section-jumper staged    "Staged changes")
(magit-define-section-jumper unpushed  "Unpushed commits")

;;;; Section Utilities

(defun magit-map-sections (function section)
  "Apply FUNCTION to SECTION and recursively its subsections."
  (funcall function section)
  (mapc (apply-partially 'magit-map-sections function)
        (magit-section-children section)))

(defun magit-wash-sequence (function)
  "Repeatedly call FUNCTION until it returns nil or eob is reached.
FUNCTION has to move point forward or return nil."
  (while (and (not (eobp)) (funcall function))))

;;;; Section Visibility

(defun magit-section-set-hidden (section hidden)
  "Hide SECTION if HIDDEN is not nil, show it otherwise."
  (setf (magit-section-hidden section) hidden)
  (if (and (not hidden)
           (magit-section-needs-refresh-on-show section))
      (magit-refresh)
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
        (magit-section-set-hidden c (magit-section-hidden c))))))

(defun magit-section-any-hidden (section)
  "Return true if SECTION or any of its children is hidden."
  (or (magit-section-hidden section)
      (let ((kids (magit-section-children section)))
        (while (and kids (not (magit-section-any-hidden (car kids))))
          (setq kids (cdr kids)))
        kids)))

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
  (when (eq 'hunk (car (magit-section-context-type (magit-current-section))))
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

(defun magit-section-lineage (section)
  "Return list of parent, grand-parents... for SECTION."
  (when section
    (cons section (magit-section-lineage (magit-section-parent section)))))

(defun magit-section-show-level (section level threshold path)
  (magit-section-set-hidden section (>= level threshold))
  (when (and (< level threshold)
             (not (magit-no-commit-p)))
    (if path
        (magit-section-show-level (car path) (1+ level) threshold (cdr path))
      (dolist (c (magit-section-children section))
        (magit-section-show-level c (1+ level) threshold nil)))))

(defun magit-show-level (level all)
  "Show section whose level is less than LEVEL, hide the others.
If ALL is non nil, do this in all sections, otherwise do it only
on ancestors and descendants of current section."
  (magit-with-refresh
    (if all
        (magit-section-show-level magit-top-section 0 level nil)
      (let ((path (reverse (magit-section-lineage (magit-current-section)))))
        (magit-section-show-level (car path) 0 level (cdr path))))))

(defun magit-show-only-files ()
  "Show section that are files, but not there subsection.

Do this in on ancestors and descendants of current section."
  (interactive)
  (if (derived-mode-p 'magit-status-mode)
      (call-interactively 'magit-show-level-2)
    (call-interactively 'magit-show-level-1)))

(defun magit-show-only-files-all ()
  "Show section that are files, but not there subsection.
Do this for all sections"
  (interactive)
  (if (derived-mode-p 'magit-status-mode)
      (call-interactively 'magit-show-level-2-all)
    (call-interactively 'magit-show-level-1-all)))

(defmacro magit-define-level-shower-1 (level all)
  "Define an interactive function to show function of level LEVEL.

If ALL is non nil, this function will affect all section,
otherwise it will affect only ancestors and descendants of
current section."
  (let ((fun (intern (format "magit-show-level-%s%s"
                             level (if all "-all" ""))))
        (doc (format "Show sections on level %s." level)))
    `(defun ,fun ()
       ,doc
       (interactive)
       (magit-show-level ,level ,all))))

(defmacro magit-define-level-shower (level)
  "Define two interactive function to show function of level LEVEL.
One for all, one for current lineage."
  `(progn
     (magit-define-level-shower-1 ,level nil)
     (magit-define-level-shower-1 ,level t)))

(magit-define-level-shower 1)
(magit-define-level-shower 2)
(magit-define-level-shower 3)
(magit-define-level-shower 4)

;;;; Section Highlighting

(defvar-local magit-highlighted-section nil)
(defvar-local magit-highlight-overlay nil)

(defun magit-highlight-section ()
  "Highlight current section if it has a type."
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
                     'face 'magit-item-highlight))
      (cond ((and section (magit-section-type section))
             (when (funcall refinep)
               (magit-diff-refine-hunk section))
             (move-overlay magit-highlight-overlay
                           (magit-section-beginning section)
                           (magit-section-end section)
                           (current-buffer)))
            (t
             (delete-overlay magit-highlight-overlay))))))

;;;; Section Actions

(defun magit-section-context-type (section)
  (when section
    (let ((c (or (magit-section-type section)
                 (and (symbolp (magit-section-title section))
                      (magit-section-title section)))))
      (when c
        (cons c (magit-section-context-type
                 (magit-section-parent section)))))))

(defun magit-prefix-p (l1 l2)
  "Return non-nil if list L1 is a prefix of list L2.
L1 is a prefix of L2 if each of it's element is `equal' to the
element at the same position in L2.  As a special case `*' in
L1 matches zero or more arbitrary elements in L2."
  (or (null l1)
      (if (eq (car l1) '*)
          (or (magit-prefix-p (cdr l1) l2)
              (and l2
                   (magit-prefix-p l1 (cdr l2))))
        (and l2
             (equal (car l1) (car l2))
             (magit-prefix-p (cdr l1) (cdr l2))))))

(defun magit-section-match (condition &optional section)
  "Return t if the context type of SECTION matches CONDITION.

CONDITION is a list beginning with the type of the least narrow
section and recursively the more narrow sections.  It may also
contain wildcards (see `magit-prefix-p').

Optional SECTION is a section, if it is nil use the current
section."
  (magit-prefix-p (reverse condition)
                  (magit-section-context-type
                   (or section (magit-current-section)))))

(defmacro magit-section-case (head &rest clauses)
  "Choose among clauses depending on the current section.

Each clause looks like (SECTION-TYPE BODY...).  The current
section is compared against SECTION-TYPE; the corresponding
BODY is evaluated and it's value returned.  If no clause
succeeds return nil.

SECTION-TYPE is a list of symbols identifying a section and it's
section context; beginning with the most narrow section.  Whether
a clause succeeds is determined using `magit-section-match'.
A SECTION-TYPE of t is allowed only in the final clause, and
matches if no other SECTION-TYPE matches.

While evaluating the selected BODY SECTION is dynamically bound
to the current section and INFO to information about this
section (see `magit-section-info').

\(fn (SECTION INFO) (SECTION-TYPE BODY...)...)"
  (declare (indent 1))
  (let ((section (car head))
        (info (cadr head)))
    `(let* ((,section (magit-current-section))
            (,info (and ,section (magit-section-info ,section))))
       (cond ,@(mapcar (lambda (clause)
                         (let ((condition (car clause)))
                           `(,(if (eq condition t)
                                  t
                                `(magit-section-match ',condition ,section))
                             ,@(cdr clause))))
                       clauses)))))

(defconst magit-section-action-success
  (make-symbol "magit-section-action-success"))

(defmacro magit-section-action (head &rest clauses)
  "Choose among action clauses depending on the current section.

Like `magit-section-case' (which see) but if no CLAUSE succeeds
try additional CLAUSES added with `magit-add-action-clauses'.
Return the value of BODY of the clause that succeeded.

Each use of `magit-section-action' should use an unique OPNAME.

\(fn (SECTION INFO OPNAME) (SECTION-TYPE BODY...)...)"
  (declare (indent 1) (debug (sexp &rest (sexp body))))
  (let ((value (make-symbol "*value*"))
        (opname (car (cddr head)))
        (disallowed (car (or (assq t clauses)
                             (assq 'otherwise clauses)))))
    (when disallowed
      (error "%s is an invalid section type" disallowed))
    `(magit-with-refresh
       (let ((,value
              (magit-section-case ,(butlast head)
                ,@clauses
                (t
                 (or (run-hook-with-args-until-success
                      ',(intern (format "magit-%s-action-hook" opname)))
                     (let* ((section (magit-current-section))
                            (type (and section (magit-section-type section))))
                       (if type
                           (error ,(format "Can't %s a %%s" opname)
                                  (or (get type 'magit-description) type))
                         (error ,(format "Nothing to %s here" opname)))))))))
         (unless (eq ,value magit-section-action-success)
           ,value)))))

(defmacro magit-add-action-clauses (head &rest clauses)
  "Add additional clauses to the OPCODE section action.

Add to the section action with the same OPNAME additional
CLAUSES.  If none of the default clauses defined using
`magit-section-action' succeed try the clauses added with this
function (which can be used multiple times with the same OPNAME).

See `magit-section-case' for more information on SECTION, INFO
and CLAUSES.

\(fn (SECTION INFO OPNAME) (SECTION-TYPE BODY...)...)"
  (declare (indent 1))
  `(add-hook ',(intern (format "magit-%s-action-hook" (car (cddr head))))
             (lambda ()
               ,(macroexpand
                 `(magit-section-case ,(butlast head)
                    ,@(mapcar (lambda (clause)
                                `(,(car clause)
                                  (or (progn ,@(cdr clause))
                                      magit-section-action-success)))
                              clauses))))))

(define-obsolete-function-alias 'magit-add-action
  'magit-add-action-clauses "1.3.0")

;;; Command Macro

(defmacro magit-define-command (sym arglist &rest body)
  "Macro to define a magit command.
It will define the magit-SYM function having ARGLIST as argument.
It will also define the magit-SYM-command-hook variable.

The defined function will call the function in the hook in order
until one return non nil. If they all return nil then body will
be called.

It is used to define hookable magit command: command defined by
this function can be enriched by magit extension like
magit-topgit and magit-svn"
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&optional ("interactive" interactive)]
                           def-body)))
  (let ((fun (intern (format "magit-%s" sym)))
        (hook (intern (format "magit-%s-command-hook" sym)))
        (doc (format "Command for `%s'." sym))
        (inter nil)
        (instr body))
    (when (stringp (car body))
      (setq doc (car body)
            instr (cdr body)))
    (let ((form (car instr)))
      (when (eq (car form) 'interactive)
        (setq inter form
              instr (cdr instr))))
    `(progn
       (defvar ,hook nil)
       (defun ,fun ,arglist
         ,doc
         ,inter
         (unless (run-hook-with-args-until-success
                  ',hook ,@(remq '&optional (remq '&rest arglist)))
           ,@instr))
       (put ',fun 'definition-name ',sym)
       (put ',hook 'definition-name ',sym))))

;;; Git Processes

(defun magit-run-git (&rest args)
  (magit-with-refresh
    (magit-run-git* args)))

(defun magit-run-git-with-input (input &rest args)
  (magit-with-refresh
    (magit-run-git* args nil nil nil nil input)))

(defun magit-run-git-async (&rest args)
  (message "Running %s %s" magit-git-executable (mapconcat 'identity args " "))
  (magit-run-git* args nil nil nil t))

(defun magit-run-git* (subcmd-and-args
                       &optional logline noerase noerror nowait input)
  (magit-run* (append (cons magit-git-executable
                            magit-git-standard-options)
                      subcmd-and-args)
              logline noerase noerror nowait input))

(defvar magit-process nil)
(defvar magit-process-buffer-name "*magit-process*"
  "Buffer name for running git commands.")

(defun magit-run* (cmd-and-args
                   &optional logline noerase noerror nowait input)
  (when magit-process
    (cl-case (process-status magit-process)
      (run  (error "Git is already running"))
      (stop (error "Git is stopped") )
      ((exit signal)
       (message "Git is not running anymore, but magit thinks it is")
       (setq magit-process nil))))
  (let ((cmd (car cmd-and-args))
        (args (cdr cmd-and-args))
        (default-dir default-directory)
        (process-buf (get-buffer-create magit-process-buffer-name))
        (command-buf (current-buffer))
        (successp nil))
    (when magit-quote-curly-braces
      (setq args (mapcar (apply-partially 'replace-regexp-in-string
                                          "{\\([0-9]+\\)}" "\\\\{\\1\\\\}")
                         args)))
    (magit-set-mode-line-process
     (magit-process-indicator-from-command cmd-and-args))
    (with-current-buffer process-buf
      (setq default-directory default-dir)
      (view-mode 1)
      (setq-local view-no-disable-on-exit t)
      (setq view-exit-action
            (lambda (buffer)
              (with-current-buffer buffer
                (bury-buffer))))
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (if noerase
            (goto-char (point-max))
          (erase-buffer))
        (insert "$ " (or logline
                         (mapconcat 'identity cmd-and-args " "))
                "\n")
        (cond (nowait
               (setq magit-process
                     (let ((process-connection-type
                            magit-process-connection-type))
                       (apply 'start-file-process
                              (file-name-nondirectory cmd)
                              process-buf cmd args)))
               (set-process-sentinel
                magit-process
                (apply-partially #'magit-process-sentinel command-buf))
               (set-process-filter magit-process 'magit-process-filter)
               (when input
                 (with-current-buffer input
                   (process-send-region magit-process
                                        (point-min) (point-max)))
                 (process-send-eof magit-process)
                 (sit-for 0.1 t))
               (magit-display-process magit-process)
               (setq successp t))
              (input
               (with-current-buffer input
                 (setq default-directory default-dir)
                 (setq magit-process
                       ;; Don't use a pty, because it would set icrnl
                       ;; which would modify the input (issue #20).
                       (let ((process-connection-type nil))
                         (apply 'start-file-process
                                (file-name-nondirectory cmd)
                                process-buf cmd args)))
                 (set-process-sentinel
                  magit-process
                  (lambda (process event)
                    (when (memq (process-status process)
                                '(exit signal))
                      (setq successp
                            (equal (process-exit-status magit-process) 0))
                      (setq magit-process nil))))
                 (set-process-filter magit-process 'magit-process-filter)
                 (process-send-region magit-process
                                      (point-min) (point-max))
                 (process-send-eof magit-process)
                 (while magit-process
                   (sit-for 0.1 t)))
               (magit-set-mode-line-process)
               (magit-need-refresh command-buf))
              (t
               (setq successp
                     (equal (apply 'process-file cmd nil process-buf nil args) 0))
               (magit-set-mode-line-process)
               (magit-need-refresh command-buf))))
      (or successp
          noerror
          (error
           "%s ... [%s buffer %s for details]"
           (or (with-current-buffer process-buf
                 (when (re-search-backward
                        (concat "^error: \\(.*\\)" paragraph-separate) nil t)
                   (match-string 1)))
               "Git failed")
           (with-current-buffer command-buf
             (let ((key (key-description (car (where-is-internal
                                               'magit-display-process)))))
               (if key (format "Hit %s to see" key) "See")))
           (buffer-name process-buf)))
      successp)))

(defun magit-process-sentinel (command-buf process event)
  (when (memq (process-status process) '(exit signal))
    (setq magit-process nil)
    (let ((msg (format "%s %s." (process-name process) (substring event 0 -1)))
          (successp (string-match "^finished" event))
          (key (if (buffer-live-p command-buf)
                   (with-current-buffer command-buf
                     (key-description (car (where-is-internal
                                            'magit-display-process))))
                 "M-x magit-display-process")))
      (when (buffer-live-p (process-buffer process))
        (with-current-buffer (process-buffer process)
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert msg "\n")
            (message (if successp
                         msg
                       (format "%s Hit %s or see buffer %s for details."
                               msg key (current-buffer)))))
          (when (featurep 'dired)
            (dired-uncache default-directory))))
      (magit-set-mode-line-process)
      (when (and (buffer-live-p command-buf)
                 (with-current-buffer command-buf
                   (derived-mode-p 'magit-mode)))
        (magit-refresh-buffer command-buf)))))

(defun magit-process-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (magit-process-yes-or-no-prompt proc string)
      (magit-process-username-prompt  proc string)
      (magit-process-password-prompt  proc string)
      (goto-char (process-mark proc))
      ;; Find last ^M in string.  If one was found, ignore everything
      ;; before it and delete the current line.
      (let ((ret-pos (length string)))
        (while (and (>= (setq ret-pos (1- ret-pos)) 0)
                    (/= ?\r (aref string ret-pos))))
        (cond ((>= ret-pos 0)
               (goto-char (line-beginning-position))
               (delete-region (point) (line-end-position))
               (insert (substring string (+ ret-pos 1))))
              (t
               (insert string))))
      (set-marker (process-mark proc) (point)))))

(defun magit-process-yes-or-no-prompt (proc string)
  (let ((beg (string-match magit-process-yes-or-no-prompt-regexp string))
        (max-mini-window-height 30))
    (when beg
      (process-send-string
       proc
       (downcase
        (concat (match-string (if (yes-or-no-p (substring string 0 beg)) 1 2)
                              string)
                "\n"))))))

(defun magit-process-password-prompt (proc string)
  "Forward password prompts to the user."
  (let ((prompt (magit-process-match-prompt
                 magit-process-password-prompt-regexps string)))
    (when prompt
      (process-send-string proc (concat (read-passwd prompt) "\n")))))

(defun magit-process-username-prompt (proc string)
  "Forward username prompts to the user."
  (let ((prompt (magit-process-match-prompt
                 magit-process-username-prompt-regexps string)))
    (when prompt
      (process-send-string proc
                           (concat (read-string prompt nil nil
                                                (user-login-name))
                                   "\n")))))

(defun magit-process-match-prompt (prompts string)
  (when (cl-find-if (lambda (regex)
                      (string-match regex string))
                    prompts)
    (let ((prompt (match-string 0 string)))
      (cond ((string-match ": $" prompt) prompt)
            ((string-match ":$"  prompt) (concat prompt " "))
            (t                           (concat prompt ": "))))))

(defun magit-set-mode-line-process (&optional string)
  (magit-map-magit-buffers
   (apply-partially (lambda (s) (setq mode-line-process s)) string)))

(defun magit-process-indicator-from-command (comps)
  (when (magit-prefix-p (cons magit-git-executable
                              magit-git-standard-options)
                        comps)
    (setq comps (nthcdr (+ (length magit-git-standard-options) 1) comps)))
  (cond ((or (null (cdr comps))
             (not (member (car comps) '("remote"))))
         (concat " " (car comps)))
        (t
         (concat " " (car comps) " " (cadr comps)))))

(defun magit-display-process (&optional process buffer)
  "Display output from most recent Git process.

Non-interactively the behaviour depends on the optional PROCESS
and BUFFER arguments.  If non-nil display BUFFER (provided it is
still alive).  Otherwise if PROCESS is non-nil display its buffer
but only if it is still alive after `magit-process-popup-time'
seconds.  Finally if both PROCESS and BUFFER are nil display the
buffer of the most recent process, like in the interactive case."
  (interactive)
  (cond ((not process)
         (unless buffer
           (setq buffer (get-buffer magit-process-buffer-name))
           (unless buffer
             (error "No Git commands have run"))
           (when (buffer-live-p buffer)
             (display-buffer buffer)
             (with-current-buffer buffer
               (goto-char (point-max))))))
        ((= magit-process-popup-time 0)
         (magit-display-process nil (process-buffer process)))
        ((> magit-process-popup-time 0)
         (run-with-timer magit-process-popup-time
                         nil #'magit-display-process
                         nil (process-buffer process)))))

;;; Magit Mode
;;;; Hooks

(defvar magit-mode-hook nil "Hook run by `magit-mode'.")

(put 'magit-mode 'mode-class 'special)

(defvar-local magit-refresh-function nil)
(put 'magit-refresh-function 'permanent-local t)

(defvar-local magit-refresh-args nil)
(put 'magit-refresh-args 'permanent-local t)

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

;;;; Initialize Mode

(defun magit-mode ()
  "Review the status of a git repository and act on it.

Please see the manual for a complete description of Magit.

\\{magit-mode-map}"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq buffer-read-only t
        truncate-lines t
        major-mode 'magit-mode
        mode-name "Magit"
        mode-line-process "")
  (add-hook 'pre-command-hook #'magit-remember-point nil t)
  (add-hook 'post-command-hook #'magit-correct-point-after-command t t)
  (add-hook 'post-command-hook #'magit-highlight-section t t)
  (use-local-map magit-mode-map)
  ;; Emacs' normal method of showing trailing whitespace gives weird
  ;; results when `magit-whitespace-warning-face' is different from
  ;; `trailing-whitespace'.
  (when (and magit-highlight-whitespace magit-highlight-trailing-whitespace)
    (setq show-trailing-whitespace nil))
  (run-mode-hooks 'magit-mode-hook))

(defmacro magit-mode-setup (buffer mode refresh-func &rest refresh-args)
  "Display and select BUFFER, turn on MODE, and refresh a first time.
Display BUFFER using `magit-display-mode-buffer', then turn on
MODE in BUFFER, set the local value of `magit-refresh-function'
to REFRESH-FUNC and that of `magit-refresh-args' to REFRESH-ARGS
and finally \"refresh\" a first time.  All arguments are
evaluated before switching to BUFFER."
  (let ((init-args (cl-gensym "init-args")))
    `(let ((,init-args (list (magit-get-top-dir default-directory)
                             ,mode ,refresh-func
                             ,@refresh-args)))
       (magit-display-mode-buffer ,buffer)
       (apply #'magit-mode-init ,init-args))))

(defun magit-mode-init (dir mode refresh-func &rest refresh-args)
  "Turn on MODE and refresh in the current buffer.
Turn on MODE, set the local value of `magit-refresh-function' to
REFRESH-FUNC and that of `magit-refresh-args' to REFRESH-ARGS and
finally \"refresh\" a first time.

Also see `magit-mode-setup', a more convenient variant."
  (setq default-directory dir
        magit-refresh-function refresh-func
        magit-refresh-args refresh-args)
  (funcall mode)
  (magit-refresh-buffer))

;;;; Find Buffer

(defun magit-find-buffer (submode &optional dir)
  (let ((topdir (magit-get-top-dir dir)))
    (cl-find-if (lambda (buf)
                  (with-current-buffer buf
                    (and (eq major-mode submode)
                         default-directory
                         (equal (expand-file-name default-directory)
                                topdir))))
                (buffer-list))))

(defun magit-find-status-buffer (&optional dir)
  (magit-find-buffer 'magit-status-mode dir))

;;;; Refresh Buffer

(defun magit-refresh-buffer (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let* ((old-line (line-number-at-pos))
           (old-point (point))
           (old-section (magit-current-section))
           (old-path (and old-section
                          (magit-section-path (magit-current-section)))))
      (beginning-of-line)
      (let ((section-line (and old-section
                               (count-lines
                                (magit-section-beginning old-section)
                                (point))))
            (line-char (- old-point (point))))
        (when magit-refresh-function
          (apply magit-refresh-function
                 magit-refresh-args))
        (magit-refresh-marked-commits-in-buffer)
        (let ((s (and old-path (magit-find-section old-path magit-top-section))))
          (cond (s
                 (goto-char (magit-section-beginning s))
                 (forward-line section-line)
                 (forward-char line-char))
                (t
                 (save-restriction
                   (widen)
                   (goto-char (point-min))
                   (forward-line (1- old-line)))))
          (dolist (w (get-buffer-window-list (current-buffer)))
            (set-window-point w (point)))
          (magit-highlight-section))))))

(defun magit-revert-buffer ()
  "Replace current buffer text with the text of the visited file on disk.

This is intended for use in `magit-refresh-file-buffer-hook'.
It calls function `revert-buffer' (which see) but only after a
few sanity checks."
  (with-current-buffer (current-buffer)
    (when (and (not (buffer-modified-p))
               ;; Don't revert indirect buffers, as the parent would be
               ;; reverted.
               (not (buffer-base-buffer))
               (not (verify-visited-file-modtime (current-buffer)))
               (file-readable-p (buffer-file-name)))
      (revert-buffer t t nil))))

(defun magit-update-vc-modeline ()
  "Update the Vc status information in the modeline.

By default the built-in Version Control package shows the status
of file visiting buffers in the modeline.  Calling this function
forces the status to be updated in the current buffer.

This is intended for use in `magit-refresh-file-buffer-hook'.
Because this can be a costly operation it is not part of the
hook's default value.

Unless you add this function to the hook you might also want to
consider completely disabling Vc for git repositories.  To do so
remove the symbol `Git' from `vc-handled-backends'."
  ;; Don't use this directly so we can provide the above
  ;; instructions.  Don't use an alias to avoid confusion.
  (with-current-buffer (current-buffer)
    (vc-find-file-hook)))

;;;; Refresh Machinery

(defvar magit-refresh-needing-buffers nil)
(defvar magit-refresh-pending nil)

(defun magit-refresh-wrapper (func)
  (if magit-refresh-pending
      (funcall func)
    (let ((magit-refresh-pending t)
          (magit-refresh-needing-buffers nil)
          (status-buffer (magit-find-status-buffer default-directory)))
      (unwind-protect
          (funcall func)
        ;; Refresh magit buffers.
        (let (magit-custom-options)
          (when status-buffer
            (cl-pushnew status-buffer magit-refresh-needing-buffers))
          (when magit-refresh-needing-buffers
            (mapc 'magit-refresh-buffer magit-refresh-needing-buffers)))
        ;; Refresh file visiting buffers.
        (dolist (buffer (buffer-list))
          (when (and (buffer-file-name buffer)
                     (string-prefix-p default-directory
                                      (buffer-file-name buffer)))
            (with-current-buffer buffer
              (run-hooks 'magit-refresh-file-buffer-hook))))))))

(defun magit-need-refresh (&optional buffer)
  "Mark BUFFER as needing to be refreshed.
If optional BUFFER is nil, use the current buffer.  If the
buffer's mode doesn't derive from `magit-mode' do nothing."
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'magit-mode)
      (cl-pushnew (current-buffer)
                  magit-refresh-needing-buffers :test 'eq))))

(defun magit-refresh ()
  "Refresh current buffer to match repository state.
Also revert every unmodified buffer visiting files
in the corresponding directory."
  (interactive)
  (magit-with-refresh
    (magit-need-refresh)))

(defun magit-refresh-all ()
  "Refresh all magit buffers to match respective repository states.
Also revert every unmodified buffer visiting files
in the corresponding directories."
  (interactive)
  (magit-map-magit-buffers #'magit-refresh-buffer default-directory))

(defun magit-map-magit-buffers (func &optional dir)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (derived-mode-p 'magit-mode)
                 (or (null dir)
                     (equal default-directory dir)))
        (funcall func)))))

;;;; Display Buffer

(defvar-local magit-previous-window-configuration nil)
(put 'magit-previous-window-configuration 'permanent-local t)

(defun magit-display-mode-buffer (buffer &optional switch-function)
  "Display BUFFER in some window and select it.
This is intended for buffers whose major mode derive from Magit
mode.

Unless BUFFER is already displayed in the selected frame store the
previous window configuration as a buffer local value, so that it
can later be restored by `magit-quit-window'.

Then display and select BUFFER using SWITCH-FUNCTION.  If that is
nil either use `pop-to-buffer' if the current buffer's major mode
derives from Magit mode; or else use `switch-to-buffer'."
  (unless (get-buffer-window buffer (selected-frame))
    (with-current-buffer (get-buffer-create buffer)
      (setq magit-previous-window-configuration
            (current-window-configuration))))
  (funcall (or switch-function
               (if (derived-mode-p 'magit-mode)
                   'switch-to-buffer
                 'pop-to-buffer))
           buffer))

(defun magit-quit-window (&optional kill-buffer)
  "Bury the current buffer and delete its window.
With a prefix argument, kill the buffer instead.

If `magit-restore-window-configuration' is non-nil and the last
configuration stored by `magit-display-mode-buffer' originates
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
            (setq magit-previous-window-configuration nil)))))))

;;; Untracked Files

(defun magit-wash-untracked-file ()
  (if (looking-at "^? \\(.*\\)$")
      (let ((file (magit-decode-git-path (match-string-no-properties 1))))
        (delete-region (point) (+ (line-end-position) 1))
        (magit-with-section file 'file
          (magit-set-section-info file)
          (insert "\t" file "\n"))
        t)
    nil))

(defun magit-wash-untracked-files ()
  ;; Setting magit-old-top-section to nil speeds up washing: no time
  ;; is wasted looking up the old visibility, which doesn't matter for
  ;; untracked files.
  ;;
  ;; XXX - speed this up in a more general way.
  ;;
  (let ((magit-old-top-section nil))
    (magit-wash-sequence #'magit-wash-untracked-file)))

(magit-define-inserter untracked-files ()
  (unless (string= (magit-get "status" "showUntrackedFiles") "no")
    (apply 'magit-git-section
           `(untracked
             "Untracked files:"
             magit-wash-untracked-files
             "ls-files" "--others" "-t" "--exclude-standard"
             ,@(unless magit-status-verbose-untracked
                 '("--directory"))))))

;;; Diffs and Hunks
;;__ FIXME The parens indicate preliminary subsections.
;;__ See https://gist.github.com/tarsius/6539717 for
;;__ an attempt to make sense of this disorder.
;;;; (section info for diff)

(defun magit-diff-item-kind (diff)
  (car (magit-section-info diff)))

(defun magit-diff-item-file (diff)
  (cadr (magit-section-info diff)))

(defun magit-diff-item-file2 (diff)
  (car (cddr (magit-section-info diff))))

(defun magit-diff-item-range (diff)
  (nth 3 (magit-section-info diff)))

;;;; (section info for diffstat)

(defun magit-diffstat-item-kind (diffstat)
  (car (magit-section-info diffstat)))

(defun magit-diffstat-item-file (diffstat)
  (let ((file (cadr (magit-section-info diffstat))))
    ;; Git diffstat may shorten long pathname with the prefix "..."
    ;; (e.g. ".../long/sub/dir/file" or "...longfilename")
    (save-match-data
      (unless (string-match "\\`\\.\\.\\." file)
        file))))

(defun magit-diffstat-item-status (diffstat)
  "Return 'completed or 'incomplete depending on the processed status"
  (car (cddr (magit-section-info diffstat))))

;;;; (diff context)

(defvar magit-diff-context-lines 3)

(defun magit-diff-U-arg ()
  (format "-U%d" magit-diff-context-lines))

(defun magit-diff-abbrev-arg ()
  (format "--abbrev=%d" magit-sha1-abbrev-length))

(defun magit-diff-smaller-hunks (&optional count)
  "Decrease the context for diff hunks by COUNT."
  (interactive "p")
  (setq magit-diff-context-lines (max 0 (- magit-diff-context-lines count)))
  (magit-refresh))

(defun magit-diff-larger-hunks (&optional count)
  "Increase the context for diff hunks by COUNT."
  (interactive "p")
  (setq magit-diff-context-lines (+ magit-diff-context-lines count))
  (magit-refresh))

(defun magit-diff-default-hunks ()
  "Reset context for diff hunks to the default size."
  (interactive)
  (setq magit-diff-context-lines 3)
  (magit-refresh))

;;;; (diff options)

(defun magit-set-diff-options ()
  "Set local `magit-diff-options' based on popup state.
And refresh the current Magit buffer."
  (interactive)
  (setq-local magit-diff-options magit-custom-options)
  (magit-refresh))

;; `magit-set-default-diff-options' is defined in "Options"/"Setters".

(defun magit-save-default-diff-options ()
  "Set and save the default for `magit-diff-options' based on popup value.
Also set the local value in all Magit buffers and refresh them."
  (interactive)
  (customize-save-variable 'magit-diff-options magit-custom-options))

(defun magit-reset-diff-options ()
  "Reset local `magit-diff-options' to default value.
And refresh the current Magit buffer."
  (interactive)
  (setq-local magit-diff-options (default-value 'magit-diff-options))
  (magit-refresh))

;;;; (hunk refinement)

(defun magit-toggle-diff-refine-hunk (&optional other)
  "Turn diff-hunk refining on or off.

If hunk refining is currently on, then hunk refining is turned off.
If hunk refining is off, then hunk refining is turned on, in
`selected' mode (only the currently selected hunk is refined).

With a prefix argument, the \"third choice\" is used instead:
If hunk refining is currently on, then refining is kept on, but
the refining mode (`selected' or `all') is switched.
If hunk refining is off, then hunk refining is turned on, in
`all' mode (all hunks refined).

Customize `magit-diff-refine-hunk' to change the default mode."
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

;;;; (wash diffs)

(defun magit-wash-diffs ()
  (magit-wash-diffstats)
  (magit-wash-sequence #'magit-wash-diff-or-other-file))

(defun magit-wash-diff-or-other-file ()
  (or (magit-wash-diff)
      (magit-wash-other-file)))

(defun magit-wash-other-file ()
  (when (looking-at "^? \\(.*\\)$")
    (let ((file (magit-decode-git-path (match-string-no-properties 1))))
      (magit-wash-diffstats-postwork file)
      (delete-region (point) (+ (line-end-position) 1))
      (magit-with-section file 'file
        (magit-set-section-info file)
        (insert "\tNew      " file "\n"))
      t)))

;;;; (wash diffstats)

(defun magit-wash-diffstat (&optional guess)
  (when (looking-at "^ ?\\(.*?\\)\\( +| +.*\\)$")
    ;; Don't decode pseudo filename.
    (let ((file (match-string-no-properties 1))
          (remaining (match-string-no-properties 2)))
      (delete-region (point) (+ (line-end-position) 1))
      (magit-with-section "diffstat" 'diffstat
        ;;(magit-set-section-info 'incomplete)

        ;; diffstat entries will look like
        ;;
        ;; ' PSEUDO-FILE-NAME | +++---'
        ;;
        ;; Since PSEUDO-FILE-NAME is not always real pathname, we
        ;; don't know the pathname yet.  Thus, section info will be
        ;; (diffstat FILE incomplete MARKER-BEGIN MARKER-END) for
        ;; now, where MARKER-BEGIN points the beginning of the
        ;; PSEUDO-FILE-NAME, MARKER-END points the end of the
        ;; PSEUDO-FILE-NAME, and FILE will be abbreviated filename.
        ;; Later in `magit-wash-diff-or-other-file`, the section
        ;; info will be updated.

        ;; Note that FILE is the 2nd element of the section-info;
        ;; this is intentional, so that `magit-diff-item-file` can
        ;; return the FILE part.

        ;; (list 'diffstat
        ;;       (or (and (> (length file) 3)
        ;;                (string-equal (substring file 0 3) "...")
        ;;                (message "got the invalid file here")
        ;;                'truncated)
        ;;           file)))

        (insert " ")
        (let ((f-begin (point-marker)) f-end)
          (insert file)
          (setq f-end (point-marker))

          (magit-set-section-info (list 'diffstat
                                        file 'incomplete f-begin f-end))
          (insert remaining)
          (put-text-property (line-beginning-position)
                             (line-beginning-position 2)
                             'keymap magit-diffstat-keymap)
          (insert "\n")
          (add-to-list 'magit-diffstat-cached-sections
                       magit-top-section))
        ;; (insert (propertize (concat " "
        ;;                             (propertize file
        ;;                                         'face
        ;;                                         'magit-diff-file-header)
        ;;                             remaining)
        ;;                     'keymap
        ;;                     magit-diffstat-keymap)
        ;;         "\n")
        ))))

(defun magit-wash-diffstats ()
  (let ((entry-regexp "^ ?\\(.*?\\)\\( +| +.*\\)$")
        (title-regexp "^ ?\\([0-9]+ +files? change.*\\)\n+")
        (pos (point)))
    (save-restriction
      (save-match-data
        (when (and (looking-at entry-regexp)
                   (re-search-forward title-regexp nil t))
          (let ((title-line (match-string-no-properties 1))
                (stat-end (point-marker)))
            (delete-region (match-beginning 0) (match-end 0))
            (narrow-to-region pos stat-end)
            (goto-char (point-min))
            (magit-with-section "diffstats" 'diffstats
              (insert title-line)
              (insert "\n")
              (setq-local magit-diffstat-cached-sections nil)
              (magit-wash-sequence #'magit-wash-diffstat))
            (setq magit-diffstat-cached-sections
                  (nreverse magit-diffstat-cached-sections))
            (insert "\n")))))))

(defun magit-wash-diffstats-postwork (file)
  (when magit-diffstat-cached-sections
    (magit-set-section-info (list 'diffstat file 'completed)
                            (pop magit-diffstat-cached-sections))))

;;;; (wash top-section, mostly and disorganized)
;;__ TODO sort internally

(defvar magit-hide-diffs nil)

(defvar magit-indentation-level 1)

(defun magit-insert-diff-title (status file file2)
  (let ((status-text (cl-case status
                       ((unmerged)
                        (format "Unmerged   %s" file))
                       ((new)
                        (format "New        %s" file))
                       ((deleted)
                        (format "Deleted    %s" file))
                       ((renamed)
                        (format "Renamed    %s   (from %s)"
                                file file2))
                       ((modified)
                        (format "Modified   %s" file))
                       ((typechange)
                        (format "Typechange %s" file))
                       (t
                        (format "?          %s" file)))))
    (insert (make-string magit-indentation-level ?\t) status-text "\n")))

(defvar magit-current-diff-range nil
  "Used internally when setting up magit diff sections.")

(defun magit-wash-typechange-section (file)
  (magit-set-section-info (list 'typechange file))
  (let ((first-start (point-marker))
        (second-start (progn (forward-line 1)
                             (search-forward-regexp "^diff")
                             (beginning-of-line)
                             (point-marker)))
        (magit-indentation-level (+ magit-indentation-level 1)))
    (save-restriction
      (narrow-to-region first-start second-start)
      (goto-char (point-min))
      (magit-with-section file 'diff
        (magit-wash-diff-section)))
    (save-restriction
      (narrow-to-region second-start (point-max))
      (goto-char (point-min))
      (magit-with-section file 'diff
        (magit-wash-diff-section)))))

(defun magit-wash-diff-section ()
  (cond ((looking-at "^\\* Unmerged path \\(.*\\)")
         (let ((file (magit-decode-git-path (match-string-no-properties 1))))
           (delete-region (point) (line-end-position))
           (insert "\tUnmerged " file "\n")
           (magit-set-section-info (list 'unmerged file nil))
           t))
        ((looking-at "^diff")
         (let ((file (magit-diff-line-file))
               (end (save-excursion
                      (forward-line) ;; skip over "diff" line
                      (if (search-forward-regexp "^diff\\|^@@" nil t)
                          (goto-char (match-beginning 0))
                        (goto-char (point-max)))
                      (point-marker))))
           (magit-wash-diffstats-postwork file)

           (let* ((status (cond
                           ((looking-at "^diff --cc")
                            'unmerged)
                           ((save-excursion
                              (search-forward-regexp "^new file" end t))
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
                           (match-string-no-properties 1)))))
             (magit-set-section-info (list status
                                           file
                                           (or file2 file)
                                           magit-current-diff-range))
             (magit-insert-diff-title status file file2)
             (when (search-forward-regexp
                    "\\(--- \\(.*\\)\n\\+\\+\\+ \\(.*\\)\n\\)" nil t)
               (let ((set-face
                      (lambda (subexp face)
                        (if magit-diff-use-overlays
                            (overlay-put (make-overlay (match-beginning subexp)
                                                       (match-end subexp))
                                         'face face)
                          (put-text-property (match-beginning subexp)
                                             (match-end subexp)
                                             'face face)))))
                 (funcall set-face 1 'magit-diff-hunk-header)
                 (funcall set-face 2 'magit-diff-file-header)
                 (funcall set-face 3 'magit-diff-file-header)))
             (goto-char end)
             (let ((magit-section-hidden-default nil))
               (magit-wash-sequence #'magit-wash-hunk))))
         t)))

(defun magit-diff-line-file ()
  (cond ((looking-at "^diff --git \\(\".*\"\\) \\(\".*\"\\)$")
         (substring (magit-decode-git-path (match-string-no-properties 2)) 2))
        ((looking-at "^diff --git ./\\(.*\\) ./\\(.*\\)$")
         (match-string-no-properties 2))
        ((looking-at "^diff --cc +\\(.*\\)$")
         (match-string-no-properties 1))
        (t
         nil)))

(defun magit-wash-diff ()
  (let ((magit-section-hidden-default magit-hide-diffs))
    (magit-with-section
        (buffer-substring-no-properties (line-beginning-position)
                                        (line-end-position))
        'diff
      (magit-wash-diff-section))))

;;;; (wash hunks)

(defun magit-wash-hunk ()
  (when (looking-at "\\(^@+\\)[^@]*@+.*")
    (let ((n-columns (1- (length (match-string 1))))
          (head (match-string 0))
          (hunk-start-pos (point))
          (set-line-face
           (lambda (face)
             (if magit-diff-use-overlays
                 (overlay-put (make-overlay (line-beginning-position)
                                            (line-beginning-position 2))
                              'face face)
               (put-text-property (line-beginning-position)
                                  (line-beginning-position 2)
                                  'face face)))))
      (magit-with-section head 'hunk
        (funcall set-line-face 'magit-diff-hunk-header)
        (forward-line)
        (while (not (or (eobp)
                        (looking-at "^diff\\|^@@")))
          (magit-highlight-line-whitespace)
          (let ((prefix (buffer-substring-no-properties
                         (point) (min (+ (point) n-columns) (point-max))))
                (line (buffer-substring-no-properties (point) (line-end-position))))
            (cond ((string-match "^[\\+]+<<<<<<< " line)
                   (funcall set-line-face 'magit-diff-merge-current))
                  ((string-match "^[\\+]+=======" line)
                   (funcall set-line-face 'magit-diff-merge-separator))
                  ((string-match "^[\\+]+|||||||" line)
                   (funcall set-line-face 'magit-diff-merge-diff3-separator))
                  ((string-match "^[\\+]+>>>>>>> " line)
                   (funcall set-line-face 'magit-diff-merge-proposed))
                  ((string-match "\\+" prefix)
                   (funcall set-line-face 'magit-diff-add))
                  ((string-match "-" prefix)
                   (funcall set-line-face 'magit-diff-del))
                  (t
                   (funcall set-line-face 'magit-diff-none))))
          (forward-line))
        (when (eq magit-diff-refine-hunk 'all)
          (magit-diff-refine-hunk (magit-current-section)))))
    t))

(defun magit-highlight-line-whitespace ()
  (when (and magit-highlight-whitespace
             (or (derived-mode-p 'magit-status-mode)
                 (not (eq magit-highlight-whitespace 'status))))
    (let ((indent
           (if (local-variable-p 'magit-highlight-indentation)
               magit-highlight-indentation
             (setq-local
              magit-highlight-indentation
              (cdr (cl-find-if (lambda (pair)
                                 (string-match-p (car pair) default-directory))
                               (default-value magit-highlight-indentation)
                               :from-end t))))))
      (when (and magit-highlight-trailing-whitespace
                 (looking-at "^[-+].*?\\([ \t]+\\)$"))
        (overlay-put (make-overlay (match-beginning 1) (match-end 1))
                     'face 'magit-whitespace-warning-face))
      (when (or (and (eq indent 'tabs)
                     (looking-at "^[-+]\\( *\t[ \t]*\\)"))
                (and (integerp indent)
                     (looking-at (format "^[-+]\\([ \t]* \\{%s,\\}[ \t]*\\)"
                                         indent))))
        (overlay-put (make-overlay (match-beginning 1) (match-end 1))
                     'face 'magit-whitespace-warning-face)))))

;;;; (wash raw diffs)

(defun magit-insert-diff (file status)
  (let ((beg (point)))
    (apply 'magit-git-insert "-c" "diff.submodule=short" "diff"
           `(,(magit-diff-U-arg) ,@magit-diff-options "--" ,file))
    (unless (eq (char-before) ?\n)
      (insert "\n"))
    (save-restriction
      (narrow-to-region beg (point))
      (goto-char beg)
      (cond ((eq status 'typechange)
             (magit-insert-diff-title status file file)
             (magit-wash-typechange-section file))
            (t
             (magit-wash-diff-section)))
      (goto-char (point-max)))))

(defvar magit-last-raw-diff nil)
(defvar magit-ignore-unmerged-raw-diffs nil)

(defun magit-wash-raw-diffs ()
  (let ((magit-last-raw-diff nil))
    (magit-wash-sequence #'magit-wash-raw-diff)))

(defun magit-wash-raw-diff ()
  (when (looking-at
         ":\\([0-7]+\\) \\([0-7]+\\) [0-9a-f]+ [0-9a-f]+ \\(.\\)[0-9]*\t\\([^\t\n]+\\)$")
    (let ((old-perm (match-string-no-properties 1))
          (new-perm (match-string-no-properties 2))
          (status (cl-case (string-to-char (match-string-no-properties 3))
                    (?A 'new)
                    (?D 'deleted)
                    (?M 'modified)
                    (?U 'unmerged)
                    (?T 'typechange)
                    (t     nil)))
          (file (magit-decode-git-path (match-string-no-properties 4))))
      ;; If this is for the same file as the last diff, ignore it.
      ;; Unmerged files seem to get two entries.
      ;; We also ignore unmerged files when told so.
      (if (or (equal file magit-last-raw-diff)
              (and magit-ignore-unmerged-raw-diffs (eq status 'unmerged)))
          (delete-region (point) (+ (line-end-position) 1))
        (setq magit-last-raw-diff file)
        ;; The 'diff' section that is created here will not work with
        ;; magit-insert-diff-item-patch etc when we leave it empty.
        ;; Luckily, raw diffs are only produced for staged and
        ;; unstaged changes, and we never call
        ;; magit-insert-diff-item-patch on them.  This is a bit
        ;; brittle, of course.
        (let ((magit-section-hidden-default magit-hide-diffs))
          (magit-with-section file 'diff
            (delete-region (point) (+ (line-end-position) 1))
            (if (not (magit-section-hidden magit-top-section))
                (magit-insert-diff file status)
              (magit-set-section-info (list status file nil))
              (magit-set-section-needs-refresh-on-show t)
              (magit-insert-diff-title status file nil)))))
      t)))

(defun magit-hunk-item-diff (hunk)
  (let ((diff (magit-section-parent hunk)))
    (or (eq (magit-section-type diff) 'diff)
        (error "Huh?  Parent of hunk not a diff"))
    diff))

(defun magit-diff-item-insert-header (diff buf)
  (magit-insert-region (save-excursion
                         (goto-char (magit-section-beginning diff))
                         (forward-line)
                         (point))
                       (if (magit-section-children diff)
                           (magit-section-beginning
                            (car (magit-section-children diff)))
                         (magit-section-end diff))
                       buf))

(defun magit-insert-diff-item-patch (diff buf)
  (magit-insert-region (save-excursion
                         (goto-char (magit-section-beginning diff))
                         (forward-line)
                         (point))
                       (magit-section-end diff)
                       buf))

(defun magit-insert-hunk-item-patch (hunk buf)
  (magit-diff-item-insert-header (magit-hunk-item-diff hunk) buf)
  (magit-insert-region (magit-section-beginning hunk)
                       (magit-section-end hunk)
                       buf))

(defun magit-insert-region (beg end buf)
  (let ((text (buffer-substring-no-properties beg end)))
    (with-current-buffer buf
      (insert text))))

(defun magit-insert-hunk-item-region-patch (hunk reverse beg end buf)
  (magit-diff-item-insert-header (magit-hunk-item-diff hunk) buf)
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

(defun magit-insert-current-line (buf)
  (let ((text (buffer-substring-no-properties
               (line-beginning-position) (line-beginning-position 2))))
    (with-current-buffer buf
      (insert text))))

;;;; (utility used elsewhere)

(defun magit-hunk-item-target-line (hunk)
  (save-excursion
    (beginning-of-line)
    (let ((line (line-number-at-pos)))
      (goto-char (magit-section-beginning hunk))
      (unless (looking-at "@@+ .* \\+\\([0-9]+\\)\\(,[0-9]+\\)? @@+")
        (error "Hunk header not found"))
      (let ((target (string-to-number (match-string 1))))
        (forward-line)
        (while (< (line-number-at-pos) line)
          ;; XXX - deal with combined diffs
          (unless (looking-at "-")
            (setq target (+ target 1)))
          (forward-line))
        target))))

;;;; (apply)

(defun magit-apply-diff-item (diff &rest args)
  (when (zerop magit-diff-context-lines)
    (setq args (cons "--unidiff-zero" args)))
  (let ((buf (generate-new-buffer " *magit-input*")))
    (unwind-protect
        (progn (magit-insert-diff-item-patch diff buf)
               (apply #'magit-run-git-with-input buf
                      "apply" (append args (list "-"))))
      (kill-buffer buf))))

(defun magit-apply-hunk-item* (hunk reverse &rest args)
  "Apply single hunk or part of a hunk to the index or working file.

This function is the core of magit's stage, unstage, apply, and
revert operations.  HUNK (or the portion of it selected by the
region) will be applied to either the index, if \"--cached\" is a
member of ARGS, or to the working file otherwise."
  (let ((zero-context (zerop magit-diff-context-lines))
        (use-region (use-region-p)))
    (when zero-context
      (setq args (cons "--unidiff-zero" args)))
    (when reverse
      (setq args (cons "--reverse" args)))
    (when (and use-region zero-context)
      (error (concat "Not enough context to partially apply hunk.  "
                     "Use `+' to increase context.")))
    (let ((buf (generate-new-buffer " *magit-input*")))
      (unwind-protect
          (progn (if use-region
                     (magit-insert-hunk-item-region-patch
                      hunk reverse (region-beginning) (region-end) buf)
                   (magit-insert-hunk-item-patch hunk buf))
                 (apply #'magit-run-git-with-input buf
                        "apply" (append args (list "-"))))
        (kill-buffer buf)))))

(defun magit-apply-hunk-item (hunk &rest args)
  (apply #'magit-apply-hunk-item* hunk nil args))

(defun magit-apply-hunk-item-reverse (hunk &rest args)
  (apply #'magit-apply-hunk-item* hunk t args))

;;;; (section inserters)

(magit-define-inserter unstaged-changes ()
  (let ((magit-hide-diffs t)
        (magit-current-diff-range (cons 'index 'working))
        (magit-diff-options (append '() magit-diff-options)))
    (magit-git-section 'unstaged
                       "Unstaged changes:" 'magit-wash-raw-diffs
                       "diff-files")))

(magit-define-inserter staged-changes ()
  (let ((no-commit (not (magit-git-success "log" "-1" "HEAD"))))
    (when (or no-commit (magit-anything-staged-p))
      (let ((magit-current-diff-range (cons "HEAD" 'index))
            (magit-hide-diffs t)
            (base (if no-commit
                      (magit-git-string "mktree")
                    "HEAD"))
            (magit-diff-options (append '("--cached") magit-diff-options))
            (magit-ignore-unmerged-raw-diffs t))
        (magit-git-section 'staged "Staged changes:" 'magit-wash-raw-diffs
                           "diff-index" "--cached"
                           base)))))

;;; Logs
;;__ FIXME The parens indicate preliminary subsections.
;;;; (utilities)

(cl-defstruct magit-log-line
  chart sha1 author date msg refs gpg refsub)

(defvar magit-log-count ()
  "Internal var used to count the number of logs actually added in a buffer.")

(defmacro magit-create-log-buffer-sections (&rest body)
  "Empty current buffer of text and magit's section, and then evaluate BODY.

if the number of logs inserted in the buffer is `magit-log-cutoff-length'
insert a line to tell how to insert more of them"
  (declare (indent 0) (debug t))
  `(let ((magit-log-count 0) (inhibit-read-only t))
     (magit-create-buffer-sections
       (magit-with-section 'log nil
         ,@body
         (when (= magit-log-count magit-log-cutoff-length)
           (magit-with-section "longer"  'longer
             (insert "type \"e\" to show more logs\n")))))))

;;;; (variables for washing, mostly)

(defun magit-log-cutoff-length-arg ()
  (format "--max-count=%d" magit-log-cutoff-length))

(defvar magit-reflog-format "--format=format:* \C-?%h\C-?%gs")

(defvar magit-log-format "--format=format:* %h %s")

(defconst magit-unpushed-or-unpulled-commit-re
  (concat "^\\* "
          "\\([0-9a-fA-F]+\\) " ; sha
          "\\(.*\\)$")          ; message
  "Regexp for parsing `magit-log-format'.")

;; Regexps for parsing ref names
;;
;; see the `git-check-ref-format' manpage for details

(defconst magit-ref-nonchars "\000-\037\177 ~^:?*[\\"
  "Characters specifically disallowed from appearing in Git symbolic refs.

Evaluate (man \"git-check-ref-format\") for details")

(defconst magit-ref-nonslash-re
  (concat "\\(?:"
          ;; "no slash-separated component can begin with a dot ." (rule 1)
          "[^" magit-ref-nonchars "./]"
          ;; "cannot have two consecutive dots ..  anywhere." (rule 3)
          "\\.?"
          "\\)*")
  "Regexp that matches the non-slash parts of a ref name.

Evaluate (man \"git-check-ref-format\") for details")

(defconst magit-refname-re
  (concat
   "\\(?:HEAD\\|"

   "\\(?:tag: \\)?"

   ;; optional non-slash sequence at the beginning
   magit-ref-nonslash-re

   ;; any number of slash-prefixed sequences
   "\\(?:"
   "/"
   magit-ref-nonslash-re
   "\\)*"

   "/" ;; "must contain at least one /." (rule 2)
   magit-ref-nonslash-re

   ;; "cannot end with a slash / nor a dot .." (rule 5)
   "[^" magit-ref-nonchars "./]"

   "\\)"
   )
  "Regexp that matches a git symbolic reference name.

Evaluate (man \"git-check-ref-format\") for details")

(defconst magit-log-oneline-re
  (concat
   "^\\(\\(?:[---_\\*|/.] ?\\)+ *\\)?"             ; graph   (1)
   "\\(?:"
   "\\([0-9a-fA-F]+\\)"                            ; sha1    (2)
   "\\(?:"                                         ; refs    (3)
   " "
   "\\("
   "("
   magit-refname-re "\\(?:, " magit-refname-re "\\)*"
   ")"
   "\\)"
   "\\)?"
   "\\)?"
   " ?"
   "\\(?:"
   "\\([BGUN]\\)?"                                  ; gpg     (4)
   "\\[\\([^]]*\\)\\]"                              ; author  (5)
   "\\[\\([^]]*\\)\\]"                              ; date    (6)
   "\\)?"
   "\\(.+\\)?$"                                     ; msg     (7)
   ))

(defconst magit-log-longline-re
  (concat
   "\\(\\(?: ?[---_\\*|/.]+ \\)* *\\)"             ; graph   (1)
   "\\(?:"
   "commit "
   "\\([0-9a-fA-F]+\\)"                            ; sha1    (2)
   "\\(?:"
   " "
   "\\("                                           ; refs    (3)
   "("
   magit-refname-re "\\(?:, " magit-refname-re "\\)*"
   ")"
   "\\)"
   "\\)?$"
   "\\)?"
   "\\(.+\\)?$"                                    ; msg     (4)
   ))

(defconst magit-log-reflog-re
  (concat "^\\([^\C-?]+\\)\C-??"                   ; graph   (1)
          "\\([^\C-?]+\\)\C-?"                     ; sha1    (2)
          "\\([^:]+\\)?"                           ; refsub  (3)
          "\\(?:: \\)?"
          "\\(.+\\)?$"))                           ; msg     (4)

(defvar magit-reflog-subject-re
  (concat "\\([^ ]+\\) ?"                          ; command (1)
          "\\(\\(?: ?[^---(][^ ]+\\)+\\)? ?"       ; status  (2)
          "\\(\\(?: ?-[^ ]+\\)+\\)?"               ; option  (3)
          "\\(?: ?(\\([^)]+\\))\\)?"))             ; type    (4)

(defvar magit-present-log-line-function 'magit-present-log-line
  "The function to use when generating a log line.
It takes four args: CHART, SHA1, REFS and MESSAGE.  The function
must return a string which will represent the log line.")

(defun magit-log-get-bisect-state-color (suffix)
  (list suffix
        (if (string= suffix "bad")
            'magit-log-head-label-bisect-bad
          'magit-log-head-label-bisect-good)))

(defun magit-log-get-patches-color (suffix)
  (list (and (string-match ".+/\\(.+\\)" suffix)
             (match-string 1 suffix))
        'magit-log-head-label-patches))

(defvar magit-log-remotes-color-hook nil)

(defun magit-log-get-remotes-color (suffix)
  (or (run-hook-with-args-until-success
       'magit-log-remotes-color-hook suffix)
      (list suffix 'magit-log-head-label-remote)))

(defvar magit-refs-namespaces
  '(("HEAD" . magit-log-head-label-head)
    ("tags" . magit-log-head-label-tags)
    ("remotes" magit-log-get-remotes-color)
    ("heads" . magit-log-head-label-local)
    ("patches" magit-log-get-patches-color)
    ("bisect" magit-log-get-bisect-state-color)))

(defun magit-ref-get-label-color (r)
  (if (cl-loop for re in magit-uninteresting-refs
               thereis (string-match re r))
      (list nil nil)
    (let* ((ref-re "\\(?:refs/\\([^/]+\\)/\\)?\\(.+\\)")
           (match-style (when (string-match ref-re r)
                          (string= r (match-string 2 r))))
           (label (match-string 2 r))
           (colorizer (cdr (assoc (if match-style
                                      label
                                    (match-string 1 r))
                                  magit-refs-namespaces))))
      (cond ((null colorizer)
             (list label 'magit-log-head-label-default))
            ((symbolp colorizer)
             (list label colorizer))
            ((listp colorizer)
             (funcall (car colorizer) label))))))

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
         (status  (and match (match-string 2 subject)))
         (option  (and match (match-string 3 subject)))
         (type    (and match (match-string 4 subject)))
         (label (if (string= command "commit")
                    (or type command)
                  command))
         (text (if (string= command "commit")
                   label
                 (mapconcat #'identity
                            (delq nil (list command option type status))
                            " "))))
    (format "%-11s "
            (propertize text 'face
                        (or (cdr (assoc label magit-reflog-labels))
                            'magit-log-reflog-label-other)))))

;;;; (line generator)

(defun magit-present-log-line (line)
  "The default log line generator."
  (let ((graph (magit-log-line-chart line))
        (sha1 (magit-log-line-sha1 line))
        (refs (magit-log-line-refs line))
        (author (magit-log-line-author line))
        (date (magit-log-line-date line))
        (refsub (magit-log-line-refsub line))
        (message (magit-log-line-msg line))
        (gpg-status (magit-log-line-gpg line)))
    (let* ((string-refs
            (when refs
              (let ((colored-labels
                     (cl-mapcan
                      (lambda (r)
                        (cl-destructuring-bind (label face)
                            (magit-ref-get-label-color r)
                          (when label
                            (list (propertize label 'face face)))))
                      refs)))
                (concat
                 (mapconcat 'identity colored-labels " ")
                 " "))))
           (lhs (concat
                 (if sha1
                     (propertize sha1 'face 'magit-log-sha1)
                   (make-string magit-sha1-abbrev-length ? ))
                 " "
                 graph
                 string-refs
                 (when refsub
                   (magit-log-format-reflog refsub))
                 (when message
                   (font-lock-append-text-property
                    0 (length message)
                    'face (if gpg-status
                              (if (string= gpg-status "B")
                                  'error
                                'magit-valid-signature)
                            'magit-log-message)
                    message)
                   message))))
      (when (and magit-log-show-author-date author date)
        (magit-log-make-author-date-overlay author date))
      lhs)))

;;;; (author overlays)

(defvar-local magit-log-author-date-string-length nil)
(defvar-local magit-log-author-string-length nil)
(defvar-local magit-log-date-string-length nil)
(defvar-local magit-log-author-date-overlay nil)

(defun magit-log-make-author-date-overlay (author date)
  (let ((overlay (make-overlay (point) (1+ (point)))))
    (setq author (propertize author 'face 'magit-log-author)
          date (delete "ago" (split-string date "[ ,]+"))
          date (propertize (concat (format "%2s %5s"
                                           (nth 0 date)
                                           (nth 1 date))
                                   (when (nth 2 date)
                                     (format " %2s %1.1s "
                                             (nth 2 date)
                                             (nth 3 date))))
                           'face 'magit-log-date))
    (overlay-put overlay 'magit-log-overlay (cons author date))
    (overlay-put overlay 'evaporate t)
    (setq magit-log-author-date-overlay
          (cons overlay magit-log-author-date-overlay))
    (when (> (length author) magit-log-author-string-length)
      (setq magit-log-author-string-length (length author)))
    (when (> (length date) magit-log-date-string-length)
      (setq magit-log-date-string-length (length date)))))

(defun magit-log-create-author-date-overlays ()
  (when magit-log-author-date-overlay
    (let* ((author-length magit-log-author-string-length)
           (date-length magit-log-date-string-length)
           (max-length (if (< (+ author-length date-length 1)
                              magit-log-author-date-max-length)
                           (+ author-length date-length 1)
                         magit-log-author-date-max-length))
           (author-length (- max-length date-length 1))
           (author-length-string (number-to-string author-length))
           (date-length-string (number-to-string date-length))
           (format-string (concat "%-" author-length-string "s "
                                  "%-" date-length-string "s")))
      (mapc (lambda (overlay)
              (let* ((data (overlay-get overlay 'magit-log-overlay))
                     (author (car data))
                     (date (cdr data))
                     (author-date
                      (format format-string
                              (if (< author-length (length author))
                                  (concat
                                   (substring author
                                              0 (1- author-length))
                                   (propertize
                                    "-" 'face
                                    'magit-log-author-date-cutoff))
                                author)
                              date)))
                (overlay-put overlay 'before-string
                             (propertize " " 'display
                                         (list '(margin right-margin)
                                               author-date)))))
            magit-log-author-date-overlay)
      (setq magit-log-author-date-string-length max-length))
    (magit-log-refresh-author-date)
    (when magit-log-author-date-overlay
      (add-hook 'window-configuration-change-hook
                'magit-log-refresh-author-date
                nil t))))

(defvar magit-log-buffer-name "*magit-log*"
  "Buffer name for display of log entries.")

(defun magit-log-refresh-author-date ()
  (let ((window (get-buffer-window)))
    (when window
      (with-selected-window window
        (set-window-margins nil (car (window-margins))
                            magit-log-author-date-string-length)))))

(defun magit-log-setup-author-date ()
  (setq magit-log-author-date-string-length 0
        magit-log-author-string-length 0
        magit-log-date-string-length 0
        magit-log-author-date-overlay nil)
  (remove-hook 'window-configuration-change-hook
               'magit-log-refresh-author-date t))

;;;; (washing)

(defun magit-parse-log-line (line style)
  ;; TODO make sure STYLE is never nil; then remove this
  (unless style
    (setq style 'oneline))
  (when (string-match (cl-ecase style
                        (oneline magit-log-oneline-re)
                        (long    magit-log-longline-re)
                        (reflog  magit-log-reflog-re))
                      line)
    (let ((match-style-string
           (lambda (oneline long reflog)
             (when (symbol-value style)
               (match-string (symbol-value style) line)))))
      (make-magit-log-line
       :chart  (funcall match-style-string 1   1   1)
       :sha1   (funcall match-style-string 2   2   2)
       :author (funcall match-style-string 5   nil nil)
       :date   (funcall match-style-string 6   nil nil)
       :gpg    (funcall match-style-string 4   nil nil)
       :msg    (funcall match-style-string 7   4   4)
       :refsub (funcall match-style-string nil nil 3)
       :refs   (when (funcall match-style-string 3 3 nil)
                 (cl-mapcan
                  (lambda (s)
                    (unless (string= s "tag:")
                      (list s)))
                  (split-string (funcall match-style-string 3 3 nil)
                                "[(), ]" t)))))))

(defun magit-wash-log-line (style)
  (beginning-of-line)
  (let* ((bol (point-at-bol))
         (eol (point-at-eol))
         (line (magit-parse-log-line
                (buffer-substring bol eol)
                style)))
    (if line
        (progn
          (delete-region bol eol)
          (insert (funcall magit-present-log-line-function line))
          (goto-char bol)
          (let ((sha1 (magit-log-line-sha1 line)))
            (if sha1
                (magit-with-section sha1 'commit
                  (when magit-log-count
                    (setq magit-log-count (1+ magit-log-count)))
                  (magit-set-section-info sha1)
                  (forward-line))
              (forward-line))))
      (forward-line))
    t))

(defun magit-wash-log (&optional style)
  (let ((magit-old-top-section nil))
    (when (derived-mode-p 'magit-log-mode)
      (magit-log-setup-author-date))
    (magit-wash-sequence (apply-partially 'magit-wash-log-line style))
    (when (derived-mode-p 'magit-log-mode)
      (magit-log-create-author-date-overlays))))

(defun magit-wash-color-log (&optional style)
  (let ((ansi-color-apply-face-function
         (lambda (beg end face)
           (when face
             (put-text-property beg end 'font-lock-face face)))))
    (ansi-color-apply-on-region (point-min) (point-max)))
  (magit-wash-log style))

;;; Commit Mode

(defvar magit-currently-shown-commit nil)

(defvar-local magit-back-navigation-history nil
  "History items that will be visited by successively going \"back\".")
(put 'magit-back-navigation-history 'permanent-local t)

(defvar-local magit-forward-navigation-history nil
  "History items that will be visited by successively going \"forward\".")
(put 'magit-forward-navigation-history 'permanent-local t)

(defun magit-wash-commit ()
  (let ((magit-current-diff-range)
        (merge-commit))
    (when (looking-at "^commit \\([0-9a-fA-F]\\{40\\}\\)")
      (setq magit-current-diff-range (match-string 1))
      (add-text-properties (match-beginning 1) (match-end 1)
                           '(face magit-log-sha1)))
    (cond
     ((search-forward-regexp
       "^Merge: \\([0-9a-fA-F]+\\) \\([0-9a-fA-F]+\\)$" nil t)
      (setq magit-current-diff-range (cons (cons (match-string 1)
                                                 (match-string 2))
                                           magit-current-diff-range)
            merge-commit t)
      (let ((first  (magit-set-section nil 'commit
                                       (match-beginning 1) (match-end 1)))
            (second (magit-set-section nil 'commit
                                       (match-beginning 2) (match-end 2))))
        (magit-set-section-info (match-string 1) first)
        (magit-set-section-info (match-string 2) second))
      (magit-make-commit-button (match-beginning 1) (match-end 1))
      (magit-make-commit-button (match-beginning 2) (match-end 2)))
     (t
      (setq magit-current-diff-range
            (cons (concat magit-current-diff-range "^")
                  magit-current-diff-range))
      (setq merge-commit nil)))

    (search-forward-regexp "^$")        ; point at the beginning of log msgs

    (when magit-show-diffstat
      (let ((pos (point)))
        (save-excursion
          (forward-char)
          (when (search-forward-regexp (if merge-commit "^$" "^---$")
                                       nil t)
            (delete-region (match-beginning 0)
                           (+ (match-end 0) 1))
            (insert "\n")

            (magit-wash-diffstats)))))

    (while (and
            (search-forward-regexp
             "\\(\\b[0-9a-fA-F]\\{4,40\\}\\b\\)\\|\\(^diff\\)" nil 'noerror)
            (not (match-string 2)))
      (let ((sha1 (match-string 1))
            (start (match-beginning 1))
            (end (match-end 1)))
        (when (string-equal "commit" (magit-git-string "cat-file" "-t" sha1))
          (magit-make-commit-button start end)
          (let ((section (magit-set-section sha1 'commit start end)))
            (magit-set-section-info sha1 section)))))
    (beginning-of-line)
    (when (looking-at "^diff")
      (magit-wash-diffs))
    (goto-char (point-max))
    (when magit-commit-mode-show-buttons
      (insert "\n")
      (when magit-back-navigation-history
        (magit-with-section "[back]" 'button
          (insert-text-button "[back]"
                              'help-echo "Previous commit"
                              'action 'magit-show-commit-backward
                              'follow-link t
                              'mouse-face 'magit-item-highlight)))
      (insert " ")
      (when magit-forward-navigation-history
        (magit-with-section "[forward]" 'button
          (insert-text-button "[forward]"
                              'help-echo "Next commit"
                              'action 'magit-show-commit-forward
                              'follow-link t
                              'mouse-face 'magit-item-highlight))))))

(defun magit-make-commit-button (start end)
  (make-text-button start end
                    'help-echo "Visit commit"
                    'action (lambda (button)
                              (save-excursion
                                (goto-char button)
                                (magit-visit-item)))
                    'follow-link t
                    'mouse-face 'magit-item-highlight
                    'face 'magit-log-sha1))

(defun magit-refresh-commit-buffer (commit)
  (magit-create-buffer-sections
    (apply #'magit-git-section nil nil
           'magit-wash-commit
           "log" "-1" "--decorate=full"
           "--pretty=medium" "--no-abbrev-commit"
           "--cc" "-p" commit
           `(,@(and magit-show-diffstat (list "--stat"))
             ,@magit-diff-options))))

(define-derived-mode magit-commit-mode magit-mode "Magit"
  "Mode to view a git commit.

\\{magit-commit-mode-map}"
  :group 'magit)

(defvar magit-commit-buffer-name "*magit-commit*"
  "Buffer name for displaying commit log messages.")

(defun magit-show-commit (commit &optional scroll inhibit-history select)
  "Show information about a commit.
Show it in the buffer named by `magit-commit-buffer-name'.
COMMIT can be any valid name for a commit in the current Git
repository.

When called interactively or when SELECT is non-nil, switch to
the commit buffer using `pop-to-buffer'.

Unless INHIBIT-HISTORY is non-nil, the commit currently shown
will be pushed onto `magit-back-navigation-history' and
`magit-forward-navigation-history' will be cleared.

Noninteractively, if the commit is already displayed and SCROLL
is provided, call SCROLL's function definition in the commit
window.  (`scroll-up' and `scroll-down' are typically passed in
for this argument.)"
  (interactive (list (magit-read-rev "Show commit (hash or ref)")
                     nil nil t))
  (when (magit-section-p commit)
    (setq commit (magit-section-info commit)))
  (unless (magit-git-success "cat-file" "commit" commit)
    (error "%s is not a commit" commit))
  (let ((dir (magit-get-top-dir))
        (buf (get-buffer-create magit-commit-buffer-name)))
    (cond
     ((and (equal magit-currently-shown-commit commit)
           ;; if it's empty then the buffer was killed
           (with-current-buffer buf
             (> (length (buffer-string)) 1)))
      (let ((win (get-buffer-window buf)))
        (cond ((not win)
               (display-buffer buf))
              (scroll
               (with-selected-window win
                 (funcall scroll))))))
     (commit
      (display-buffer buf)
      (with-current-buffer buf
        (unless inhibit-history
          (push (cons default-directory magit-currently-shown-commit)
                magit-back-navigation-history)
          (setq magit-forward-navigation-history nil))
        (setq magit-currently-shown-commit commit)
        (goto-char (point-min))
        (magit-mode-init dir 'magit-commit-mode
                         #'magit-refresh-commit-buffer commit))))
    (when select
      (pop-to-buffer buf))))

(defun magit-show-commit-backward (&optional ignored)
  ;; Ignore argument passed by push-button
  "Show the commit at the head of `magit-back-navigation-history'
in `magit-commit-buffer-name'."
  (interactive)
  (with-current-buffer magit-commit-buffer-name
    (unless magit-back-navigation-history
      (error "No previous commit."))
    (let ((histitem (pop magit-back-navigation-history)))
      (push (cons default-directory magit-currently-shown-commit)
            magit-forward-navigation-history)
      (setq default-directory (car histitem))
      (magit-show-commit (cdr histitem) nil 'inhibit-history))))

(defun magit-show-commit-forward (&optional ignored)
  ;; Ignore argument passed by push-button
  "Show the commit at the head of `magit-forward-navigation-history'
in `magit-commit-buffer-name'."
  (interactive)
  (with-current-buffer magit-commit-buffer-name
    (unless magit-forward-navigation-history
      (error "No next commit."))
    (let ((histitem (pop magit-forward-navigation-history)))
      (push (cons default-directory magit-currently-shown-commit)
            magit-back-navigation-history)
      (setq default-directory (car histitem))
      (magit-show-commit (cdr histitem) nil 'inhibit-history))))

;;; Commit Mark

(defvar magit-marked-commit nil)

(defvar-local magit-mark-overlay nil)
(put 'magit-mark-overlay 'permanent-local t)

(defun magit-refresh-marked-commits ()
  (magit-map-magit-buffers #'magit-refresh-marked-commits-in-buffer))

(defun magit-refresh-marked-commits-in-buffer ()
  (unless magit-mark-overlay
    (setq magit-mark-overlay (make-overlay 1 1))
    (overlay-put magit-mark-overlay 'face 'magit-item-mark))
  (delete-overlay magit-mark-overlay)
  (magit-map-sections
   (lambda (section)
     (when (and (eq (magit-section-type section) 'commit)
                (equal (magit-section-info section)
                       magit-marked-commit))
       (move-overlay magit-mark-overlay
                     (magit-section-beginning section)
                     (magit-section-end section)
                     (current-buffer))))
   magit-top-section))

;;; Status Mode
;;;; Status Sections

(defun magit-wash-unpulled-or-unpushed ()
  (save-match-data
    (let ((magit-old-top-section nil))
      (magit-wash-sequence
       (lambda ()
         (beginning-of-line)
         (if (looking-at magit-unpushed-or-unpulled-commit-re)
             (let* ((sha1 (match-string 1))
                    (bol (point-at-bol))
                    (line (make-magit-log-line :sha1 sha1
                                               :msg (match-string 2))))
               (delete-region bol (point-at-eol))
               (insert (funcall magit-present-log-line-function line))
               (goto-char bol)
               (if sha1
                   (magit-with-section sha1 'commit
                     (magit-set-section-info sha1)
                     (forward-line))
                 (forward-line)))
           (forward-line))
         t)))))

(magit-define-inserter unpulled-commits ()
  (let ((tracked (magit-get-tracked-branch nil t)))
    (when tracked
      (magit-git-section 'unpulled "Unpulled commits:"
                         #'magit-wash-unpulled-or-unpushed
                         "log" magit-log-format
                         (magit-diff-abbrev-arg)
                         (concat "HEAD.." tracked)))))

(magit-define-inserter unpushed-commits ()
  (let ((tracked (magit-get-tracked-branch nil t)))
    (when tracked
      (magit-git-section 'unpushed "Unpushed commits:"
                         #'magit-wash-unpulled-or-unpushed
                         "log" magit-log-format
                         (magit-diff-abbrev-arg)
                         (concat tracked "..HEAD")))))

(defvar magit-status-line-align-to 9)

(defun magit-insert-status-line (heading info-string)
  (declare (indent 1))
  (insert heading ":"
          (make-string (max 1 (- magit-status-line-align-to
                                 (length heading))) ?\ )
          info-string "\n"))

(defun magit-insert-status-local-line ()
  (magit-insert-status-line "Local"
    (concat (propertize (if (magit--bisecting-p)
                            (magit--bisect-info-for-status)
                          (or (magit-get-current-branch)
                              "(detached)"))
                        'face 'magit-branch)
            " " (abbreviate-file-name default-directory))))

(defun magit-insert-status-remote-line ()
  (let* ((branch  (magit-get-current-branch))
         (remote  (magit-get "branch" branch "remote"))
         (tracked (magit-get-tracked-branch branch)))
    (when tracked
      (setq tracked (propertize tracked 'face 'magit-branch))
      (magit-insert-status-line "Remote"
        (concat
         (and (magit-get-boolean "branch" branch "rebase") "onto ")
         (if (string= "." remote)
             (concat "branch " tracked)
           (when (string-match (concat "^" remote) tracked)
             (setq tracked (substring tracked (1+ (length remote)))))
           (concat tracked " @ " remote
                   " (" (magit-get "remote" remote "url") ")")))))))

(defun magit-insert-status-head-line ()
  (magit-insert-status-line "Head"
    (or (magit-format-commit "HEAD" "%h %s")
        "nothing committed (yet)")))

(defun magit-insert-status-tags-line ()
  (when magit-status-insert-tags-line
    (let* ((current-tag (magit-get-current-tag t))
           (next-tag (magit-get-next-tag t))
           (both-tags (and current-tag next-tag t))
           (tag-subject (eq magit-status-tags-line-subject 'tag)))
      (when (or current-tag next-tag)
        (magit-insert-status-line (if both-tags "Tags" "Tag")
          (concat
           (and current-tag (apply 'magit-format-status-tag-sentence
                                   tag-subject current-tag))
           (and both-tags ", ")
           (and next-tag (apply 'magit-format-status-tag-sentence
                                (not tag-subject) next-tag))))))))

(defun magit-format-status-tag-sentence (behindp tag cnt &rest ignored)
  (concat (propertize tag 'face 'magit-tag)
          (and (> cnt 0)
               (concat (if (eq magit-status-tags-line-subject 'tag)
                           (concat " (" (propertize (format "%s" cnt)
                                                    'face 'magit-branch))
                         (format " (%i" cnt))
                       " " (if behindp "behind" "ahead") ")"))))

(defun magit-insert-status-merge-line ()
  (let ((heads (magit-file-lines (magit-git-dir "MERGE_HEAD"))))
    (when heads
      (magit-insert-status-line "Merging"
        (concat
         (mapconcat 'identity (mapcar 'magit-name-rev heads) ", ")
         "; Resolve conflicts, or press \"m A\" to Abort")))))

(defun magit-insert-status-rebase-lines ()
  (let ((rebase (magit-rebase-info)))
    (when rebase
      (magit-insert-status-line "Rebasing"
        (apply 'format
               "onto %s (%s of %s); Press \"R\" to Abort, Skip, or Continue"
               rebase))
      (when (nth 3 rebase)
        (magit-insert-status-line "Stopped"
          (magit-format-commit (nth 3 rebase) "%h %s"))))))

(defun magit-insert-empty-line ()
  (insert "\n"))

;;;; Status Refresh

(defun magit-refresh-status ()
  (magit-git-exit-code "update-index" "--refresh")
  (magit-create-buffer-sections
    (magit-with-section 'status nil
      (run-hooks 'magit-status-insert-sections-hook)))
  (run-hooks 'magit-refresh-status-hook))

(define-derived-mode magit-status-mode magit-mode "Magit"
  "Mode for looking at git status.

\\{magit-status-mode-map}"
  :group 'magit)

;;;; Save Buffers

(defvar magit-default-directory nil)

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
        (magit-default-directory (or topdir default-directory)))
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
       (string= (magit-get-top-dir magit-default-directory)
                (magit-get-top-dir (file-name-directory buffer-file-name)))))

;;;; Status Commands

;;;###autoload
(defun magit-status (dir)
  "Open a Magit status buffer for the Git repository containing DIR.
If DIR is not within a Git repository, offer to create a Git
repository in DIR.

Interactively, a prefix argument means to ask the user which Git
repository to use even if `default-directory' is under Git
control.  Two prefix arguments means to ignore `magit-repo-dirs'
when asking for user input."
  (interactive (list (if current-prefix-arg
                         (magit-read-top-dir
                          (> (prefix-numeric-value current-prefix-arg)
                             4))
                       (or (magit-get-top-dir)
                           (magit-read-top-dir nil)))))
  (let ((topdir (magit-get-top-dir dir)))
    (unless topdir
      (when (y-or-n-p
             (format "There is no Git repository in %S.  Create one? " dir))
        (magit-init dir)
        (setq topdir (magit-get-top-dir dir))))
    (when topdir
      (magit-save-some-buffers topdir)
      (let ((buf (or (magit-find-status-buffer topdir)
                     (generate-new-buffer
                      (concat "*magit: "
                              (file-name-nondirectory
                               (directory-file-name topdir)) "*")))))
        (magit-display-mode-buffer buf magit-status-buffer-switch-function)
        (magit-mode-init topdir 'magit-status-mode #'magit-refresh-status)))))

(defun magit-init (dir)
  "Initialize git repository in the DIR directory."
  (interactive (list (read-directory-name "Directory for Git repository: ")))
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (topdir (magit-get-top-dir dir)))
    (when (or (not topdir)
              (yes-or-no-p
               (format
                (if (string-equal topdir dir)
                    "There is already a Git repository in %s. Reinitialize? "
                  "There is a Git repository in %s. Create another in %s? ")
                topdir dir)))
      (unless (file-directory-p dir)
        (and (y-or-n-p (format "Directory %s does not exists.  Create it? " dir))
             (make-directory dir)))
      (let ((default-directory dir))
        (magit-run-git* (list "init"))))))

;;;; Read Repository

(defun magit-read-top-dir (dir)
  "Ask the user for a Git repository.
The choices offered by auto-completion will be the repositories
under `magit-repo-dirs'.  If `magit-repo-dirs' is nil or DIR is
non-nil, then autocompletion will offer directory names."
  (if (and (not dir) magit-repo-dirs)
      (let* ((repos (magit-list-repos magit-repo-dirs))
             (reply (magit-completing-read "Git repository: " repos)))
        (file-name-as-directory
         (or (cdr (assoc reply repos))
             (if (file-directory-p reply)
                 (expand-file-name reply)
               (error "Not a repository or a directory: %s" reply)))))
    (file-name-as-directory
     (read-directory-name "Git repository: "
                          (or (magit-get-top-dir) default-directory)))))

(defun magit-list-repos (dirs)
  (magit-remove-conflicts
   (cl-loop for dir in dirs
            append (cl-loop for repo in
                            (magit-list-repos* dir magit-repo-dirs-depth)
                    collect (cons (file-name-nondirectory repo) repo)))))

(defun magit-list-repos* (dir depth)
  "Return a list of repos found in DIR, recursing up to DEPTH levels deep."
  (if (magit-git-repo-p dir)
      (list (expand-file-name dir))
    (and (> depth 0)
         (file-directory-p dir)
         (not (member (file-name-nondirectory dir)
                      '(".." ".")))
         (cl-loop for entry in (directory-files dir t nil t)
                  append (magit-list-repos* entry (1- depth))))))

(defun magit-remove-conflicts (alist)
  (let ((dict (make-hash-table :test 'equal))
        (alist (delete-dups alist))
        (result nil))
    (dolist (a alist)
      (puthash (car a) (cons (cdr a) (gethash (car a) dict))
               dict))
    (maphash
     (lambda (key value)
       (if (= (length value) 1)
           (push (cons key (car value)) result)
         (let ((sub (magit-remove-conflicts
                     (mapcar
                      (lambda (entry)
                        (let ((dir (directory-file-name
                                    (substring entry 0 (- (length key))))))
                          (cons (concat (file-name-nondirectory dir) "/" key)
                                entry)))
                      value))))
           (setq result (append result sub)))))
     dict)
    result))

;;; Acting (1)
;;;; Merging

(defun magit-merge-check-clean ()
  (or (magit-everything-clean-p)
      (not magit-merge-warn-dirty-worktree)
      (yes-or-no-p "Running merge in a dirty worktree could cause data loss.  Continue?")))

(defun magit-merge (revision &optional do-commit)
  "Merge REVISION into the current 'HEAD'; leave changes uncommitted.
\('git merge --no-commit REVISION')."
  (interactive (list (when (magit-merge-check-clean)
                       (magit-read-rev "Merge" (magit-guess-branch)))
                     current-prefix-arg))
  (when revision
    (apply 'magit-run-git
           "merge"
           (magit-rev-to-git revision)
           (if do-commit
               magit-custom-options
             (cons "--no-commit" magit-custom-options)))
    (when (file-exists-p ".git/MERGE_MSG")
      (let ((magit-custom-options nil))
        (magit-commit)))))

(defun magit-merge-abort ()
  "Abort the current merge operation."
  (interactive)
  (if (file-exists-p (magit-git-dir "MERGE_HEAD"))
      (when (yes-or-no-p "Abort merge?")
        (magit-run-git-async "merge" "--abort"))
    (error "No merge in progress")))

;;;; Stage and Unstage

(defun magit-stage-item (&optional file)
  "Add the item at point to the staging area.
With a prefix argument, prompt for a file to be staged instead."
  (interactive
   (when current-prefix-arg
     (list (file-relative-name (read-file-name "File to stage: " nil nil t)
                               (magit-get-top-dir)))))
  (if file
      (magit-run-git "add" file)
    (magit-section-action (item info "stage")
      ((untracked file)
       (magit-run-git "add" info))
      ((untracked)
       (apply #'magit-run-git "add" "--"
              (magit-git-lines "ls-files" "--other" "--exclude-standard")))
      ((unstaged diff hunk)
       (if (string-match "^diff --cc"
                         ;; XXX Using the title is a bit too clever.
                         (magit-section-title (magit-hunk-item-diff item)))
           (error (concat "Can't stage individual resolution hunks.  "
                          "Please stage the whole file."))
         (magit-apply-hunk-item item "--cached")))
      ((unstaged diff)
       (magit-run-git "add" "-u" (magit-diff-item-file item)))
      ((unstaged)
       (magit-stage-all))
      ((staged *)
       (error "Already staged"))
      ((diff diff)
       (save-excursion
         (magit-goto-parent-section)
         (magit-stage-item)))
      ((diff diff hunk)
       (save-excursion
         (magit-goto-parent-section)
         (magit-goto-parent-section)
         (magit-stage-item)))
      ((hunk)
       (error "Can't stage this hunk"))
      ((diff)
       (error "Can't stage this diff")))))

(defun magit-unstage-item ()
  "Remove the item at point from the staging area."
  (interactive)
  (magit-section-action (item info "unstage")
    ((staged diff hunk)
     (magit-apply-hunk-item-reverse item "--cached"))
    ((staged diff)
     (when (eq (car info) 'unmerged)
       (error "Can't unstage an unmerged file.  Resolve it first"))
     (if (magit-no-commit-p)
         (magit-run-git "rm" "--cached" "--" (magit-diff-item-file item))
       (magit-run-git "reset" "-q" "HEAD" "--" (magit-diff-item-file item))))
    ((staged)
     (magit-unstage-all))
    ((unstaged *)
     (error "Already unstaged"))
    ((diff diff)
     (save-excursion
       (magit-goto-parent-section)
       (magit-unstage-item)))
    ((diff diff hunk)
     (save-excursion
       (magit-goto-parent-section)
       (magit-goto-parent-section)
       (magit-unstage-item)))
    ((hunk)
     (error "Can't unstage this hunk"))
    ((diff)
     (error "Can't unstage this diff"))))

(defun magit-stage-all (&optional including-untracked)
  "Add all remaining changes in tracked files to staging area.
With a prefix argument, add remaining untracked files as well.
\('git add [-u] .')."
  (interactive "P")
  (if including-untracked
      (magit-run-git "add" ".")
    (magit-run-git "add" "-u" ".")))

(defun magit-unstage-all ()
  "Remove all changes from staging area.
\('git reset --mixed HEAD')."
  (interactive)
  (magit-run-git "reset" "HEAD" "--"))

;;;; Branching

(defun magit-escape-branch-name (branch)
  "Escape branch name BRANCH to remove problematic characters."
  (replace-regexp-in-string "[/]" "-" branch))

(defun magit-default-tracking-name-remote-plus-branch (remote branch)
  "Use the remote name plus a hyphen plus the escaped branch name for tracking branches."
  (concat remote "-" (magit-escape-branch-name branch)))

(defun magit-default-tracking-name-branch-only (remote branch)
  "Use just the escaped branch name for tracking branches."
  (magit-escape-branch-name branch))

(defun magit-get-tracking-name (remote branch)
  "Given a REMOTE and a BRANCH name, ask the user for a local
tracking brach name suggesting a sensible default."
  (when (yes-or-no-p
         (format "Create local tracking branch for %s? " branch))
    (let* ((default-name
            (funcall magit-default-tracking-name-function remote branch))
           (chosen-name
            (read-string (format "Call local branch (%s): " default-name)
                         nil nil default-name)))
      (when (magit-ref-exists-p (concat "refs/heads/" chosen-name))
        (error "'%s' already exists." chosen-name))
      chosen-name)))

(defun magit-maybe-create-local-tracking-branch (rev)
  "Depending on the users wishes, create a tracking branch for
REV... maybe."
  (if (string-match "^\\(?:refs/\\)?remotes/\\([^/]+\\)/\\(.+\\)" rev)
      (let* ((remote (match-string 1 rev))
             (branch (match-string 2 rev))
             (tracker-name (magit-get-tracking-name remote branch)))
        (when tracker-name
          (magit-run-git "checkout" "-b" tracker-name rev)
          t))
    nil))

(magit-define-command checkout (revision)
  "Switch 'HEAD' to REVISION and update working tree.
Fails if working tree or staging area contain uncommitted changes.
If REVISION is a remote branch, offer to create a local tracking branch.
\('git checkout [-b] REVISION')."
  (interactive
   (list (let ((current-branch (magit-get-current-branch))
               (default (magit-default-rev)))
           (magit-read-rev (format "Switch from '%s' to" current-branch)
                           (unless (string= current-branch default)
                             default)
                           (if current-branch
                               (cons (concat "refs/heads/" current-branch "$")
                                     magit-uninteresting-refs)
                             magit-uninteresting-refs)))))
  (when (and revision
             (not (magit-maybe-create-local-tracking-branch revision)))
    (magit-save-some-buffers)
    (magit-run-git "checkout" (magit-rev-to-git revision))))

(magit-define-command create-branch (branch parent)
  "Switch 'HEAD' to new BRANCH at revision PARENT and update working tree.
Fails if working tree or staging area contain uncommitted changes.
\('git checkout -b BRANCH REVISION')."
  (interactive
   (list (read-string "Create branch: ")
         (magit-read-rev "Parent"
                         (or (magit-name-rev (magit-commit-at-point))
                             (magit-get-current-branch)))))
  (when (and branch (not (string= branch ""))
             parent)
    (magit-save-some-buffers)
    (apply #'magit-run-git
           "checkout" "-b"
           branch
           (append magit-custom-options (list (magit-rev-to-git parent))))))

(defun magit-delete-branch (branch &optional force)
  "Delete the BRANCH.
If the branch is the current one, offers to switch to `master' first.
With prefix, forces the removal even if it hasn't been merged.
Works with local or remote branches.
\('git branch [-d|-D] BRANCH' or 'git push <remote-part-of-BRANCH> :refs/heads/BRANCH')."
  (interactive (list (magit-read-rev-with-default "Branch to delete" 'notrim)
                     current-prefix-arg))
  (let* ((remote (magit-remote-part-of-branch branch))
         (current (magit-get-current-branch))
         (is-current (string= branch current))
         (is-master (string= branch "master"))
         (args (list "branch"
                     (if force "-D" "-d")
                     branch)))
    (cond
     (remote
      (magit-run-git-async "push" remote
                           (concat ":refs/heads/"
                                   (magit-branch-no-remote branch))))
     ((and is-current is-master)
      (message "Cannot delete master branch while it's checked out."))
     (is-current
      (if (y-or-n-p "Cannot delete current branch. Switch to master first? ")
          (progn
            (magit-checkout "master")
            (apply 'magit-run-git args))
        (message "The current branch was not deleted.")))
     (t
      (apply 'magit-run-git args)))))

(defun magit-rename-branch (old new &optional force)
  "Rename branch OLD to NEW.
With prefix, forces the rename even if NEW already exists.
\('git branch [-m|-M] OLD NEW')."
  (interactive (list (magit-read-rev-with-default "Old name")
                     (read-string "New name: ")
                     current-prefix-arg))
  (magit-run-git "branch"
                 (if force "-M" "-m")
                 (magit-rev-to-git old) new))

(defun magit-guess-branch ()
  "Return a branch name depending on the context of cursor.
If no branch is found near the cursor return nil."
  (magit-section-case (item info)
    ((branch)        (magit-section-info (magit-current-section)))
    ((wazzup commit) (magit-section-info (magit-section-parent item)))
    ((commit)        (magit-name-rev info))
    ((wazzup)        info)
    (t               (magit-get-previous-branch))))

;;;; Remoting

(defun magit-add-remote (remote url)
  "Add the REMOTE and fetch it.
\('git remote add REMOTE URL')."
  (interactive (list (read-string "Add remote: ")
                     (read-string "URL: ")))
  (magit-run-git-async "remote" "add" "-f" remote url))

(defun magit-remove-remote (remote)
  "Delete the REMOTE.
\('git remote rm REMOTE')."
  (interactive (list (magit-read-remote "Remote to delete")))
  (magit-run-git "remote" "rm" remote))

(defun magit-rename-remote (old new)
  "Rename remote OLD to NEW.
\('git remote rename OLD NEW')."
  (interactive (list (magit-read-remote "Old name")
                     (read-string "New name: ")))
  (magit-run-git "remote" "rename" old new))

(defun magit-guess-remote ()
  (magit-section-case (item info)
    ((branch)
     (magit-section-info (magit-section-parent item)))
    ((remote)
     info)
    (t
     (if (string= info ".")
         info
       (magit-get-current-remote)))))

;;;; Rebase

(defun magit-rebase-info ()
  "Return a list indicating the state of an in-progress rebase.

The returned list has the form (ONTO DONE TOTAL STOPPED).
ONTO is the commit being rebased onto.
DONE and TOTAL are integers with obvious meanings.
STOPPED is the SHA-1 of the commit at which rebase stopped.

Return nil if there is no rebase in progress."
  (let ((m (magit-git-dir "rebase-merge"))
        (a (magit-git-dir "rebase-apply")))
    (cond
     ((file-directory-p m) ; interactive
      (list
       (magit-name-rev (magit-file-line  (expand-file-name "onto" m)))
       (length         (magit-file-lines (expand-file-name "done" m)))
       (cl-loop for line in (magit-file-lines
                             (expand-file-name "git-rebase-todo.backup" m))
                count (string-match "^[^#\n]" line))
       (magit-file-line (expand-file-name "stopped-sha" m))))

     ((file-regular-p (expand-file-name "onto" a)) ; non-interactive
      (list
       (magit-name-rev       (magit-file-line (expand-file-name "onto" a)))
       (1- (string-to-number (magit-file-line (expand-file-name "next" a))))
       (string-to-number     (magit-file-line (expand-file-name "last" a)))
       (let ((patch-header (magit-file-line
                            (car (directory-files a t "^[0-9]\\{4\\}$")))))
         (when (string-match "^From \\([a-z0-9]\\{40\\}\\) " patch-header)
           (match-string 1 patch-header))))))))

(defun magit-rebase-step ()
  (interactive)
  (if (magit-rebase-info)
      (let ((cursor-in-echo-area t)
            (message-log-max nil))
        (message "Rebase in progress. [A]bort, [S]kip, or [C]ontinue? ")
        (cl-case (read-event)
          ((?A ?a) (magit-run-git-async "rebase" "--abort"))
          ((?S ?s) (magit-run-git-async "rebase" "--skip"))
          ((?C ?c) (magit-run-git-async "rebase" "--continue"))))
    (let* ((branch (magit-get-current-branch))
           (rev (magit-read-rev
                 "Rebase to"
                 (magit-get-tracked-branch branch nil t)
                 (if branch
                     (cons (concat "refs/heads/" branch)
                           magit-uninteresting-refs)
                   magit-uninteresting-refs))))
      (magit-run-git "rebase" (magit-rev-to-git rev)))))

(defun magit-interactive-rebase (commit)
  "Start a git rebase -i session, old school-style."
  (interactive
   (let* ((section (get-text-property (point) 'magit-section))
          (commit (and (member 'commit (magit-section-context-type section))
                       (magit-section-info section))))
     (list (if commit
               (concat commit "^")
             (magit-read-rev "Interactively rebase to" (magit-guess-branch))))))
  (magit-assert-emacsclient "rebase interactively")
  (magit-with-emacsclient magit-server-window-for-rebase
    (magit-run-git-async "rebase" "-i" commit)))

;;;; Reset

(magit-define-command reset-head (revision &optional hard)
  "Switch 'HEAD' to REVISION, keeping prior working tree and staging area.
Any differences from REVISION become new changes to be committed.
With prefix argument, all uncommitted changes in working tree
and staging area are lost.
\('git reset [--soft|--hard] REVISION')."
  (interactive (list (magit-read-rev (format "%s head to"
                                             (if current-prefix-arg
                                                 "Hard reset"
                                               "Reset"))
                                     (or (magit-default-rev) "HEAD^"))
                     current-prefix-arg))
  (magit-run-git "reset" (if hard "--hard" "--soft")
                 (magit-rev-to-git revision) "--"))

(magit-define-command reset-head-hard (revision)
  "Switch 'HEAD' to REVISION, losing all changes.
Uncomitted changes in both working tree and staging area are lost.
\('git reset --hard REVISION')."
  (interactive (list (magit-read-rev (format "Hard reset head to")
                                     (or (magit-default-rev) "HEAD"))))
  (magit-reset-head revision t))

(magit-define-command reset-working-tree (&optional arg)
  "Revert working tree and clear changes from staging area.
\('git reset --hard HEAD').

With a prefix arg, also remove untracked files.
With two prefix args, remove ignored files as well."
  (interactive "p")
  (let ((include-untracked (>= arg 4))
        (include-ignored (>= arg 16)))
    (when (yes-or-no-p (format "Discard all uncommitted changes%s%s? "
                               (if include-untracked
                                   ", untracked files"
                                 "")
                               (if include-ignored
                                   ", ignored files"
                                 "")))
      (magit-reset-head-hard "HEAD")
      (when include-untracked
        (magit-run-git "clean" "-fd" (if include-ignored "-x" ""))))))

;;;; Rewriting

(defun magit-read-rewrite-info ()
  (when (file-exists-p (magit-git-dir "magit-rewrite-info"))
    (with-temp-buffer
      (insert-file-contents (magit-git-dir "magit-rewrite-info"))
      (goto-char (point-min))
      (read (current-buffer)))))

(defun magit-write-rewrite-info (info)
  (with-temp-file (magit-git-dir "magit-rewrite-info")
    (prin1 info (current-buffer))
    (princ "\n" (current-buffer))))

(magit-define-inserter pending-commits ()
  (let* ((info (magit-read-rewrite-info))
         (pending (cdr (assq 'pending info))))
    (when pending
      (magit-with-section 'pending nil
        (insert (propertize "Pending commits:\n"
                            'face 'magit-section-title))
        (dolist (p pending)
          (let* ((commit (car p))
                 (properties (cdr p))
                 (used (plist-get properties 'used)))
            (magit-with-section commit 'commit
              (magit-set-section-info commit)
              (insert (magit-git-string
                       "log" "-1"
                       (if used
                           "--pretty=format:. %s"
                         "--pretty=format:* %s")
                       commit "--")
                      "\n")))))
      (insert "\n"))))

(defun magit-rewrite-set-commit-property (commit prop value)
  (let* ((info (magit-read-rewrite-info))
         (pending (cdr (assq 'pending info)))
         (p (assoc commit pending)))
    (when p
      (setf (cdr p) (plist-put (cdr p) prop value))
      (magit-write-rewrite-info info)
      (magit-need-refresh))))

(defun magit-rewrite-set-used ()
  (interactive)
  (magit-section-case (item info)
    ((pending commit)
     (magit-rewrite-set-commit-property info 'used t))))

(defun magit-rewrite-set-unused ()
  (interactive)
  (magit-section-case (item info)
    ((pending commit)
     (magit-rewrite-set-commit-property info 'used nil))))

(magit-define-inserter pending-changes ()
  (let* ((info (magit-read-rewrite-info))
         (orig (cadr (assq 'orig info))))
    (when orig
      (let ((magit-hide-diffs t))
        (magit-git-section 'pending-changes
                           "Pending changes"
                           'magit-wash-diffs
                           "diff" (magit-diff-U-arg) "-R" orig)))))

(defun magit-rewrite-start (from &optional onto)
  (interactive (list (magit-read-rev-with-default "Rewrite from")))
  (or (magit-everything-clean-p)
      (error "You have uncommitted changes"))
  (or (not (magit-read-rewrite-info))
      (error "Rewrite in progress"))
  (let* ((orig (magit-rev-parse "HEAD"))
         (base (if (or (eq magit-rewrite-inclusive t)
                       (and (eq magit-rewrite-inclusive 'ask)
                            (y-or-n-p "Include selected revision in rewrite? ")))
                   (or (car (magit-commit-parents from))
                       (error "Can't rewrite a parentless commit."))
                 from))
         (pending (magit-git-lines "rev-list" (concat base ".."))))
    (magit-write-rewrite-info `((orig ,orig)
                                (pending ,@(mapcar #'list pending))))
    (magit-run-git "reset" "--hard" base "--")))

(defun magit-rewrite-stop (&optional noconfirm)
  (interactive)
  (let* ((info (magit-read-rewrite-info)))
    (or info
        (error "No rewrite in progress"))
    (when (or noconfirm
              (yes-or-no-p "Stop rewrite? "))
      (magit-write-rewrite-info nil)
      (magit-refresh))))

(defun magit-rewrite-abort ()
  (interactive)
  (let* ((info (magit-read-rewrite-info))
         (orig (cadr (assq 'orig info))))
    (or info
        (error "No rewrite in progress"))
    (or (magit-everything-clean-p)
        (error "You have uncommitted changes"))
    (when (yes-or-no-p "Abort rewrite? ")
      (magit-write-rewrite-info nil)
      (magit-run-git "reset" "--hard" orig "--"))))

(defun magit-rewrite-finish ()
  (interactive)
  (magit-with-refresh
    (magit-rewrite-finish-step)))

(defun magit-rewrite-finish-step ()
  (let ((info (magit-read-rewrite-info)))
    (or info
        (error "No rewrite in progress"))
    (let* ((pending (cdr (assq 'pending info)))
           (first-unused
            (let ((rpend (reverse pending)))
              (while (and rpend (plist-get (cdr (car rpend)) 'used))
                (setq rpend (cdr rpend)))
              (car rpend)))
           (commit (car first-unused)))
      (cond ((not first-unused)
             (magit-rewrite-stop t))
            ((magit-cherry-pick-commit commit)
             (magit-rewrite-set-commit-property commit 'used t)
             (magit-rewrite-finish-step))))))

;;;; Fetching

(magit-define-command fetch (remote)
  "Fetch from REMOTE."
  (interactive (list (magit-read-remote "Fetch remote")))
  (apply 'magit-run-git-async "fetch" remote magit-custom-options))

(magit-define-command fetch-current ()
  "Run fetch for default remote.

If there is no default remote, ask for one."
  (interactive)
  (magit-fetch (or (magit-get-current-remote)
                   (magit-read-remote "Fetch remote"))))

(magit-define-command remote-update ()
  "Update all remotes."
  (interactive)
  (apply 'magit-run-git-async "remote" "update" magit-custom-options))

;;;; Pulling

(magit-define-command pull ()
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
              (magit-read-remote-branch chosen-branch-remote
                                        (format "Pull branch from remote %s"
                                                chosen-branch-remote))
            branch-merge-name)))
    (when (and (not branch-remote)
               (not choose-remote))
      (magit-set chosen-branch-remote "branch" branch "remote"))
    (when (and (not branch-merge-name)
               (not choose-branch))
      (magit-set (format "%s" chosen-branch-merge-name)
                 "branch" branch "merge"))
    (apply 'magit-run-git-async "pull" "-v"
           (append
            magit-custom-options
            (when choose-remote
              (list chosen-branch-remote))
            (when choose-branch
               (list (format "refs/heads/%s:refs/remotes/%s/%s"
                             chosen-branch-merge-name
                             chosen-branch-remote
                             chosen-branch-merge-name)))))))

;;;; Running

(defun magit-parse-arguments (command)
  (require 'eshell)
  (with-temp-buffer
    (insert command)
    (mapcar 'eval (eshell-parse-arguments (point-min) (point-max)))))

(defun magit-shell-command (command)
  "Perform arbitrary shell COMMAND."
  (interactive "sCommand: ")
  (let ((args (magit-parse-arguments command))
        (magit-process-popup-time 0))
    (magit-run* args nil nil nil t)))

(defvar magit-git-command-history nil)

(defun magit-git-command (command)
  "Perform arbitrary Git COMMAND.

Similar to `magit-shell-command', but involves slightly less
typing and automatically refreshes the status buffer."
  (interactive
   (list (read-string "Run git like this: " nil 'magit-git-command-history)))
  (require 'pcomplete)
  (let ((args (magit-parse-arguments command))
        (magit-process-popup-time 0))
    (magit-with-refresh
      (magit-run-git* args nil nil nil t))))

;;;; Pushing

(magit-define-command push-tags ()
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

(magit-define-command push ()
  "Push the current branch to a remote repository.

By default push to the remote specified by the git-config(1) option
branch.<name>.remote or else origin.  Otherwise or with a prefix
argument instead ask the user what remote to push to.

When pushing to branch.<name>.remote push to the branch specified by
branch.<name>.merge.  When pushing to another remote or if the latter
option is not set push to the remote branch with the same name as the
local branch being pushed.  With two or more prefix arguments instead
ask the user what branch to push to.  In this last case actually push
even if `magit-set-upstream-on-push's value is `refuse'."
  (interactive)
  (let* ((branch (or (magit-get-current-branch)
                     (error "Don't push a detached head.  That's gross")))
         (branch-remote (and branch (magit-get "branch" branch "remote")))
         (origin-remote (and (magit-get "remote" "origin" "url") "origin"))
         (push-remote (if (or current-prefix-arg
                              (and (not branch-remote)
                                   (not origin-remote)))
                          (magit-read-remote
                           (format "Push %s to remote" branch)
                           (or branch-remote origin-remote))
                        (or branch-remote origin-remote)))
         ref-name ref-branch)
    (cond ((>= (prefix-numeric-value current-prefix-arg) 16)
           (setq ref-name (magit-read-remote-branch
                           push-remote
                           (format "Push %s as branch" branch)))
           (setq ref-branch (if (string-prefix-p "refs/" ref-name)
                                ref-name
                              (concat "refs/heads/" ref-name))))
          ((equal branch-remote push-remote)
           (setq ref-branch (magit-get "branch" branch "merge"))))
    (if (and (not ref-branch)
             (eq magit-set-upstream-on-push 'refuse))
        (error "Not pushing since no upstream has been set")
      (let ((set-upstream-on-push
             (and (not ref-branch)
                  (or (eq magit-set-upstream-on-push 'dontask)
                      (and (or (eq magit-set-upstream-on-push t)
                               (and (not branch-remote)
                                    (eq magit-set-upstream-on-push 'askifnotset)))
                           (yes-or-no-p "Set upstream while pushing? "))))))
        (apply 'magit-run-git-async "push" "-v" push-remote
               (if ref-branch
                   (format "%s:%s" branch ref-branch)
                 branch)
               (if set-upstream-on-push
                   (cons "--set-upstream" magit-custom-options)
                 magit-custom-options))
        ;; Although git will automatically set up the remote,
        ;; it doesn't set up the branch to merge (at least as of Git 1.6.6.1),
        ;; so we have to do that manually.
        (when (and ref-branch
                   (or set-upstream-on-push
                       (member "-u" magit-custom-options)))
          (magit-set ref-branch "branch" branch "merge"))))))

;;;; Committing

(defun magit-commit (&optional amendp)
  "Create a new commit on HEAD.
With a prefix argument amend to the commit at HEAD instead.
\('git commit [--amend]')."
  (interactive "P")
  (if (not (or (magit-anything-staged-p)
               (member "--allow-empty" magit-custom-options)
               (member "--all"         magit-custom-options)
               (member "--amend"       magit-custom-options)
               (and amendp (setq magit-custom-options
                                 (cons "--amend" magit-custom-options)))))
      (if (and (magit-rebase-info)
               (y-or-n-p "Nothing staged.  Continue in-progress rebase? "))
          (magit-run-git-async "rebase" "--continue")
        (error
         "Nothing staged.  Set --allow-empty, --all, or --amend in popup."))
    (when (and magit-expand-staged-on-commit
               (derived-mode-p 'magit-status-mode))
      (magit-jump-to-staged)
      (with-local-quit
        (if (eq magit-expand-staged-on-commit 'full)
            (magit-show-level 4 nil)
          (magit-expand-section)))
      (recenter 0))
    (magit-commit-internal "commit" magit-custom-options)))

(defun magit-commit-internal (subcmd args)
  (if (magit-use-emacsclient-p)
      (magit-with-emacsclient magit-server-window-for-commit
        (apply 'magit-run-git-async subcmd args))
    (let ((topdir (magit-get-top-dir)))
      (with-current-buffer
          (find-file-noselect
           (magit-git-dir (if (equal subcmd "tag")
                              "TAG_EDITMSG"
                            "COMMIT_EDITMSG")))
        (funcall (if (functionp magit-server-window-for-commit)
                     magit-server-window-for-commit
                   'switch-to-buffer)
                 (current-buffer))
        (add-hook 'git-commit-commit-hook
                  (apply-partially
                   (lambda (default-directory args)
                     (magit-run-git* args))
                   topdir `(,subcmd
                            ,"--cleanup=strip"
                            ,(concat "--file=" (file-relative-name
                                                (buffer-file-name)
                                                topdir))
                            ,@args))
                  nil t)))))

(defun magit-commit-add-log ()
  "Add a template for the current hunk to the commit message buffer."
  (interactive)
  (let ((section (magit-current-section)))
    (let ((fun (if (eq (magit-section-type section) 'hunk)
                   (save-window-excursion
                     (save-excursion
                       (magit-visit-item)
                       (add-log-current-defun)))
                 nil))
          (file (magit-diff-item-file
                 (cl-case (magit-section-type section)
                   (hunk (magit-hunk-item-diff section))
                   (diff section)
                   (t    (error "No change at point")))))
          (buffer (cl-find-if (lambda (buf)
                                (with-current-buffer buf
                                  (derived-mode-p 'git-commit-mode)))
                              (append (buffer-list (selected-frame))
                                      (buffer-list)))))
      (unless buffer
        (error "No commit message buffer found"))
      (pop-to-buffer buffer)
      (goto-char (point-min))
      (cond ((not (search-forward-regexp
                   (format "^\\* %s" (regexp-quote file)) nil t))
             ;; No entry for file, create it.
             (goto-char (point-max))
             (insert (format "\n* %s" file))
             (when fun
               (insert (format " (%s)" fun)))
             (insert ": "))
            (fun
             ;; found entry for file, look for fun
             (let ((limit (or (save-excursion
                                (and (search-forward-regexp "^\\* "
                                                            nil t)
                                     (match-beginning 0)))
                              (point-max))))
               (cond ((search-forward-regexp (format "(.*\\<%s\\>.*):"
                                                     (regexp-quote fun))
                                             limit t)
                      ;; found it, goto end of current entry
                      (if (search-forward-regexp "^(" limit t)
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
               (forward-char 2)))))))

;;;; Tagging

(magit-define-command tag (name rev &optional annotate)
  "Create a new tag with the given NAME at REV.
With a prefix argument annotate the tag.
\('git tag [--annotate] NAME REV')."
  (interactive (list (magit-read-tag "Tag name: ")
                     (magit-read-rev "Place tag on: "
                                     (or (magit-default-rev) "HEAD"))
                     current-prefix-arg))
  (let ((args (append magit-custom-options (list name rev))))
    (if (or (member "--annotate" args)
            (and annotate (setq args (cons "--annotate" args))))
        (magit-commit-internal "tag" args)
      (apply #'magit-run-git "tag" args))))

(magit-define-command delete-tag (name)
  "Delete the tag with the given NAME.
\('git tag -d NAME')."
  (interactive (list (magit-read-tag "Delete Tag: " t)))
  (apply #'magit-run-git "tag" "-d"
         (append magit-custom-options (list name))))

(defun magit-read-tag (prompt &optional require-match)
  (magit-completing-read prompt (magit-git-lines "tag") nil
                         require-match nil 'magit-read-rev-history))

;;;; Stashing

(defun magit-wash-stash ()
  (if (search-forward-regexp "stash@{\\(.*?\\)}" (line-end-position) t)
      (let ((stash (match-string-no-properties 0))
            (name (match-string-no-properties 1)))
        (delete-region (match-beginning 0) (match-end 0))
        (goto-char (match-beginning 0))
        (fixup-whitespace)
        (goto-char (line-beginning-position))
        (insert name)
        (goto-char (line-beginning-position))
        (magit-with-section stash 'stash
          (magit-set-section-info stash)
          (forward-line)))
    (forward-line))
  t)

(defun magit-wash-stashes ()
  (let ((magit-old-top-section nil))
    (magit-wash-sequence #'magit-wash-stash)))

(magit-define-inserter stashes ()
  (magit-git-section 'stashes
                     "Stashes:" 'magit-wash-stashes
                     "stash" "list"))

(defvar magit-read-stash-history nil
  "The history of inputs to `magit-stash'.")

(magit-define-command stash (description)
  "Create new stash of working tree and staging area named DESCRIPTION.
Working tree and staging area revert to the current 'HEAD'.
With prefix argument, changes in staging area are kept.
\('git stash save [--keep-index] DESCRIPTION')"
  (interactive (list (read-string "Stash description: " nil
                                  'magit-read-stash-history)))
  (apply 'magit-run-git "stash" "save"
         `(,@magit-custom-options "--" ,description)))

(magit-define-command stash-snapshot ()
  "Create new stash of working tree and staging area; keep changes in place.
\('git stash save \"Snapshot...\"; git stash apply stash@{0}')"
  (interactive)
  (magit-with-refresh
    (apply 'magit-run-git "stash" "save"
           `(,@magit-custom-options
             ,(format-time-string "Snapshot taken at %Y-%m-%d %H:%M:%S"
                                  (current-time))))
    (magit-run-git "stash" "apply" "stash@{0}")))

(defvar magit-currently-shown-stash nil)

(define-derived-mode magit-stash-mode magit-mode "Magit Stash"
  "Mode for looking at a git stash.

\\{magit-stash-mode-map}"
  :group 'magit)

(defvar magit-stash-buffer-name "*magit-stash*"
  "Buffer name for displaying a stash.")

(defun magit-show-stash (stash &optional scroll)
  (when (magit-section-p stash)
    (setq stash (magit-section-info stash)))
  (let ((dir default-directory)
        (buf (get-buffer-create magit-stash-buffer-name))
        (stash-id (magit-git-string "rev-list" "-1" stash)))
    (cond ((and (equal magit-currently-shown-stash stash-id)
                (with-current-buffer buf
                  (> (length (buffer-string)) 1)))
           (let ((win (get-buffer-window buf)))
             (cond ((not win)
                    (display-buffer buf))
                   (scroll
                    (with-selected-window win
                      (funcall scroll))))))
          (t
           (setq magit-currently-shown-stash stash-id)
           (display-buffer buf)
           (with-current-buffer buf
             (goto-char (point-min))
             (let* ((range (cons (concat stash "^2^") stash))
                    (magit-current-diff-range range)
                    (args (magit-rev-range-to-git range)))
               (magit-mode-init dir 'magit-diff-mode #'magit-refresh-diff-buffer
                                range args)))))))
;;;; Apply

(defun magit-apply-item ()
  (interactive)
  (magit-section-action (item info "apply")
    ((pending commit)
     (magit-apply-commit info)
     (magit-rewrite-set-commit-property info 'used t))
    ((commit)
     (magit-apply-commit info))
    ((unstaged *)
     (error "Change is already in your working tree"))
    ((staged *)
     (error "Change is already in your working tree"))
    ((hunk)
     (magit-apply-hunk-item item))
    ((diff)
     (magit-apply-diff-item item))
    ((stash)
     (magit-run-git "stash" "apply" info))))

(defun magit-apply-commit (commit)
  (magit-assert-one-parent commit "cherry-pick")
  (magit-run-git "cherry-pick" "--no-commit" commit))

;;;; Cherry-Pick

(defun magit-cherry-pick-item ()
  (interactive)
  (magit-section-action (item info "cherry-pick")
    ((pending commit)
     (magit-cherry-pick-commit info)
     (magit-rewrite-set-commit-property info 'used t))
    ((commit)
     (magit-cherry-pick-commit info))
    ((stash)
     (magit-run-git "stash" "pop" info))))

(defun magit-cherry-pick-commit (commit)
  (magit-assert-one-parent commit "cherry-pick")
  (magit-run-git "cherry-pick" commit))

;;;; Revert

(defun magit-revert-item ()
  (interactive)
  (cl-flet ((confirm ()
              (or (yes-or-no-p "Really revert this item? ")
                  (error "Abort"))))
    (magit-section-action (item info "revert")
      ((pending commit)
       (confirm)
       (magit-revert-commit info)
       (magit-rewrite-set-commit-property info 'used nil))
      ((commit)
       (confirm)
       (magit-revert-commit info))
      ((unstaged *)
       ;; This already asks for confirmation.
       (magit-discard-item))
      ((hunk)
       (confirm)
       (magit-apply-hunk-item-reverse item))
      ((diff)
       (confirm)
       (magit-apply-diff-item item "--reverse")))))

(defun magit-revert-commit (commit)
  (magit-assert-one-parent commit "revert")
  (magit-run-git "revert" "--no-commit" commit))

;;;; Submoduling

(defun magit-submodule-update (&optional init)
  "Update the submodule of the current git repository.
With a prefix arg, do a submodule update --init."
  (interactive "P")
  (let ((default-directory (magit-get-top-dir)))
    (apply #'magit-run-git-async "submodule" "update"
           (and init '("--init")))))

(defun magit-submodule-update-init ()
  "Update and init the submodule of the current git repository."
  (interactive)
  (magit-submodule-update t))

(defun magit-submodule-init ()
  "Initialize the submodules."
  (interactive)
  (let ((default-directory (magit-get-top-dir)))
    (magit-run-git-async "submodule" "init")))

(defun magit-submodule-sync ()
  "Synchronizes submodule's remote URL configuration."
  (interactive)
  (let ((default-directory (magit-get-top-dir)))
    (magit-run-git-async "submodule" "sync")))

;;;; Logging

(magit-define-command log (&optional range)
  (interactive)
  (unless range (setq range "HEAD"))
  (magit-mode-setup magit-log-buffer-name
                    #'magit-log-mode
                    #'magit-refresh-log-buffer
                    range 'oneline
                    (cons (magit-rev-range-to-git range)
                          magit-custom-options)))

(magit-define-command log-ranged (range)
  (interactive (list (magit-read-rev-range "Log" "HEAD")))
  (magit-log range))

(magit-define-command log-long (&optional range)
  (interactive)
  (unless range (setq range "HEAD"))
  (magit-mode-setup magit-log-buffer-name
                    #'magit-log-mode
                    #'magit-refresh-log-buffer
                    range 'long
                    (cons (magit-rev-range-to-git range)
                          magit-custom-options)))

(magit-define-command log-long-ranged (range)
  (interactive (list (magit-read-rev-range "Long Log" "HEAD")))
  (magit-log-long range))

(magit-define-command reflog (ref)
  (interactive (list (magit-read-rev "Reflog of"
                                     (or (magit-guess-branch) "HEAD"))))
  (magit-mode-setup "*magit-reflog*"
                    #'magit-reflog-mode
                    #'magit-refresh-reflog-buffer
                    ref))

(magit-define-command reflog-head ()
  (interactive)
  (magit-reflog "HEAD"))

;;; Log Mode

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

(defun magit-refresh-log-buffer (range style args)
  (setq magit-current-range range)
  (magit-create-log-buffer-sections
    (apply #'magit-git-section nil
           (if (or (member "--all" args) (member "--all-match" args))
               "Commits"
             (magit-rev-range-describe range "Commits"))
           (apply-partially 'magit-wash-color-log style)
           "log" (magit-log-cutoff-length-arg)
           "--decorate=full" "--abbrev-commit" "--color"
           (magit-diff-abbrev-arg)
           `(,@(cl-case style
                 (long
                  (if magit-log-show-gpg-status
                      (list "--stat" "--show-signature")
                    (list "--stat")))
                 (oneline
                  (list (concat "--pretty=format:%h%d "
                                (and magit-log-show-gpg-status "%G?")
                                "[%an][%ar]%s"))))
             ,@args "--"))))

(define-derived-mode magit-log-mode magit-mode "Magit Log"
  "Mode for looking at git log.

\\{magit-log-mode-map}"
  :group 'magit)

;;; Reflog Mode

(defvar-local magit-reflog-head nil
  "The HEAD of the reflog in the current buffer.
This is only non-nil in reflog buffers.")

(defun magit-refresh-reflog-buffer (ref)
  (setq magit-reflog-head ref)
  (magit-create-log-buffer-sections
    (magit-git-section 'reflog (format "Local history of branch %s" ref)
                       (apply-partially 'magit-wash-log 'reflog)
                       "log" magit-reflog-format
                       (magit-diff-abbrev-arg)
                       "--walk-reflogs" (magit-log-cutoff-length-arg)
                       (magit-rev-to-git ref))))

(define-derived-mode magit-reflog-mode magit-log-mode "Magit Reflog"
  "Mode for looking at git reflog.

\\{magit-reflog-mode-map}"
  :group 'magit)

;;; Ediff Support

(defvar magit-ediff-buffers nil
  "List of buffers that may be killed by `magit-ediff-restore'.")

(defvar magit-ediff-windows nil
  "The window configuration that will be restored when Ediff is finished.")

(defvar-local magit-show-current-version ()
  "Which version of MAGIT-FILE-NAME is shown in this buffer.")

(defun magit-ediff ()
  "View the current DIFF section in ediff."
  (interactive)
  (let ((diff (magit-current-section)))
    (when (magit-section-hidden diff)
      ;; Range is not set until the first time the diff is visible.
      ;; This somewhat hackish code makes sure it's been visible at
      ;; least once.
      (magit-toggle-section)
      (magit-toggle-section)
      (setq diff (magit-current-section)))
    (when (eq 'hunk (magit-section-type diff))
      (setq diff (magit-section-parent diff)))
    (unless (eq 'diff (magit-section-type diff))
      (error "No diff at this location"))
    (let* ((type (magit-diff-item-kind diff))
           (file1 (magit-diff-item-file diff))
           (file2 (magit-diff-item-file2 diff))
           (range (magit-diff-item-range diff)))
      (cond
       ((memq type '(new deleted typechange))
        (message "Why ediff a %s file?" type))
       ((and (eq type 'unmerged)
             (eq (cdr range) 'working))
        (magit-interactive-resolve file1))
       ((consp (car range))
        (magit-ediff* (magit-show (caar range) file2)
                      (magit-show (cdar range) file2)
                      (magit-show (cdr range) file1)))
       (t
        (magit-ediff* (magit-show (car range) file2)
                      (magit-show (cdr range) file1)))))))

(defun magit-diffstat-ediff ()
  (interactive)
  (magit-goto-diff-section-at-file
   (magit-diff-item-file (magit-current-section)))
  (call-interactively 'magit-ediff))

(defun magit-ediff-add-cleanup ()
  (make-local-variable 'magit-ediff-buffers)
  (setq-default magit-ediff-buffers ())

  (make-local-variable 'magit-ediff-windows)
  (setq-default magit-ediff-windows ())

  (add-hook 'ediff-cleanup-hook 'magit-ediff-restore 'append 'local))

(defun magit-ediff* (a b &optional c)
  (setq magit-ediff-buffers (list a b c))
  (setq magit-ediff-windows (current-window-configuration))
  (if c
      (ediff-buffers3 a b c '(magit-ediff-add-cleanup))
    (ediff-buffers a b '(magit-ediff-add-cleanup))))

(defun magit-ediff-restore ()
  "Kill any buffers in `magit-ediff-buffers' that are not visiting files and
restore the window state that was saved before ediff was called."
  (dolist (buffer magit-ediff-buffers)
    (when (and (null (buffer-file-name buffer))
               (buffer-live-p buffer))
      (with-current-buffer buffer
        (when (and (eq magit-show-current-version 'index)
                   (buffer-modified-p))
          (magit-save-index)))
      (kill-buffer buffer)))
  (let ((buf (current-buffer)))
    (set-window-configuration magit-ediff-windows)
    (set-buffer buf)))

(defun magit-save-index ()
  "Add the content of current file as if it was the index."
  (interactive)
  (unless (eq magit-show-current-version 'index)
    (error "Current buffer doesn't visit the index version of a file"))
  (when (y-or-n-p (format "Stage current version of %s" magit-file-name))
    (let ((buf (current-buffer))
          (name (magit-git-dir "magit-add-index")))
      (with-temp-file name
        (insert-buffer-substring buf))
      (let ((hash (magit-git-string "hash-object" "-t" "blob" "-w"
                                    (concat "--path=" magit-file-name)
                                    "--" name))
            (perm (substring (magit-git-string "ls-files" "-s"
                                               magit-file-name)
                             0 6)))
        (magit-run-git "update-index" "--cacheinfo"
                       perm hash magit-file-name)))))

;;; Diff Mode

(defun magit-refresh-diff-buffer (range args)
  (let ((magit-current-diff-range (cond
                                   ((stringp range)
                                    (cons range 'working))
                                   ((null (cdr range))
                                    (cons (car range) 'working))
                                   (t
                                    range))))
    (setq magit-current-range range)
    (magit-create-buffer-sections
      (apply #'magit-git-section
             'diffbuf
             (magit-rev-range-describe magit-current-diff-range "Changes")
             'magit-wash-diffs
             "diff" (magit-diff-U-arg)
             `(,@(and magit-show-diffstat (list "--patch-with-stat"))
               ,args "--")))))

(define-derived-mode magit-diff-mode magit-mode "Magit Diff"
  "Mode for looking at a git diff.

\\{magit-diff-mode-map}"
  :group 'magit)

(magit-define-command diff (range)
  (interactive (list (magit-read-rev-range "Diff")))
  (let ((buf (get-buffer-create "*magit-diff*")))
    (display-buffer buf)
    (with-current-buffer buf
      (magit-mode-init default-directory
                       'magit-diff-mode
                       #'magit-refresh-diff-buffer
                       range
                       (magit-rev-range-to-git range)))))

(magit-define-command diff-working-tree (rev)
  (interactive (list (magit-read-rev-with-default "Diff working tree with")))
  (magit-diff (or rev "HEAD")))

(defun magit-diff-with-mark (commit)
  (interactive (list (or (magit-commit-at-point)
                         (magit-read-rev "Diff"))))
  (unless magit-marked-commit
    (error "No commit marked"))
  (magit-diff (cons magit-marked-commit commit)))

;;; Wazzup Mode

(defun magit-wazzup-toggle-ignore (branch edit)
  (let ((ignore-file (magit-git-dir "info/wazzup-exclude")))
    (when edit
      (setq branch (read-string "Branch to ignore for wazzup: " branch)))
    (let ((ignored (magit-file-lines ignore-file)))
      (cond ((member branch ignored)
             (when (or (not edit)
                       (y-or-n-p "Branch %s is already ignored.  Unignore? "))
               (setq ignored (delete branch ignored))))
            (t
             (setq ignored (append ignored (list branch)))))
      (with-temp-file ignore-file
        (insert (mapconcat 'identity ignored "\n")))
      (magit-need-refresh))))

(defun magit-refresh-wazzup-buffer (head all)
  (let ((branch-desc (or head "(detached) HEAD")))
    (unless head (setq head "HEAD"))
    (magit-create-buffer-sections
      (magit-with-section 'wazzupbuf nil
        (insert (format "Wazzup, %s\n\n" branch-desc))
        (let* ((excluded (magit-file-lines
                          (magit-git-dir "info/wazzup-exclude")))
               (all-branches (magit-list-interesting-refs))
               (branches (if all
                             all-branches
                           (delq nil (mapcar
                                      (lambda (b)
                                        (and (not
                                              (member (cdr b) excluded))
                                             b))
                                      all-branches))))
               (reported (make-hash-table :test #'equal)))
          (dolist (branch branches)
            (let* ((name (car branch))
                   (ref (cdr branch))
                   (hash (magit-rev-parse ref))
                   (reported-branch (gethash hash reported)))
              (unless (or (and reported-branch
                               (string= (file-name-nondirectory ref)
                                        reported-branch))
                          (not (magit-git-string "merge-base" head ref)))
                (puthash hash (file-name-nondirectory ref) reported)
                (let* ((n (length (magit-git-lines "log" "--pretty=oneline"
                                                   (concat head ".." ref))))
                       (section
                        (let ((magit-section-hidden-default t))
                          (magit-git-section
                           (cons ref 'wazzup)
                           (format "%s unmerged commits in %s%s"
                                   n name
                                   (if (member ref excluded)
                                       " (normally ignored)"
                                     ""))
                           'magit-wash-log
                           "log" (magit-log-cutoff-length-arg)
                           "--abbrev-commit" "--graph" "--pretty=oneline"
                           (magit-diff-abbrev-arg)
                           (format "%s..%s" head ref)
                           "--"))))
                  (magit-set-section-info ref section))))))))))

(define-derived-mode magit-wazzup-mode magit-mode "Magit Wazzup"
  "Mode for looking at commits that could be merged from other branches.

\\{magit-wazzup-mode-map}"
  :group 'magit)

(defun magit-wazzup (&optional all)
  (interactive "P")
  (magit-mode-setup "*magit-wazzup*"
                    #'magit-wazzup-mode
                    #'magit-refresh-wazzup-buffer
                    (magit-get-current-branch)
                    all))

;;; Acting (2)
;;;; File Log

;; This variable is used to keep track of the current file in the
;; *magit-log* buffer when this one is dedicated to showing the log of
;; just 1 file.
(defvar-local magit-file-log-file nil)

(defun magit-refresh-file-log-buffer (file range style)
  "Refresh the current file-log buffer by calling git.

FILE is the path of the file whose log must be displayed.

`magit-current-range' will be set to the value of RANGE.

STYLE controls the display.  It is either `long', `oneline',
or something else."
  (setq magit-current-range range)
  (setq magit-file-log-file file)
  (magit-create-log-buffer-sections
    (apply #'magit-git-section nil
           (magit-rev-range-describe range (format "Commits for file %s" file))
           (apply-partially 'magit-wash-log style)
           "log" (magit-log-cutoff-length-arg)
           "--decorate=full" "--abbrev-commit" "--graph"
           (magit-diff-abbrev-arg)
           `(,@(cl-case style
                 (long    (list "--stat" "-z"))
                 (oneline (list "--pretty=oneline")))
             "--" ,file))))

(defun magit-file-log (&optional all)
  "Display the log for the currently visited file or another one.

With a prefix argument or if no file is currently visited, ask
for the file whose log must be displayed."
  (interactive "P")
  (magit-mode-setup magit-log-buffer-name
                    #'magit-log-mode
                    #'magit-refresh-file-log-buffer
                    (magit-file-relative-name
                     (if (or current-prefix-arg (not buffer-file-name))
                         (magit-read-file-from-rev (magit-get-current-branch))
                       buffer-file-name))
                    "HEAD" 'oneline))

(magit-define-command single-file-log (&optional ranged)
  (interactive)
  (magit-file-log))

(defun magit-show-file-revision ()
  "Open a new buffer showing the current file in the revision at point."
  (interactive)
  (let ((show-file-from-diff
         (lambda (item)
           (switch-to-buffer-other-window
            (magit-show (cdr (magit-diff-item-range item))
                        (magit-diff-item-file item))))))
    (magit-section-action (item info "show")
      ((commit)
       (let ((current-file (or magit-file-log-file
                               (magit-read-file-from-rev info))))
         (switch-to-buffer-other-window
          (magit-show info current-file))))
      ((hunk) (funcall show-file-from-diff (magit-hunk-item-diff item)))
      ((diff) (funcall show-file-from-diff item)))))

;;;; Ignore

(defun magit-edit-ignore-string (file)
  "Prompt the user for the string to be ignored.
A list of predefined values with wildcards is derived from the
filename FILE."
  (let* ((extension (concat "*." (file-name-extension file)))
         (extension-in-dir (concat (file-name-directory file) extension))
         (filename (file-name-nondirectory file))
         (completions (list extension extension-in-dir filename file)))
    (magit-completing-read "File/pattern to ignore: "
                           completions nil nil nil nil file)))

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
      (setq file (magit-edit-ignore-string file)))
    (when (and local (not (file-exists-p local-ignore-dir)))
      (make-directory local-ignore-dir t))
    (with-temp-buffer
      (when (file-exists-p ignore-file)
        (insert-file-contents ignore-file))
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert file "\n")
      (write-region nil nil ignore-file))
    (magit-need-refresh)))

(defun magit-ignore-item (edit local)
  "Ignore the item at point.
With a prefix argument edit the ignore string."
  (interactive "P")
  (magit-section-action (item info "ignore")
    ((untracked file)
     (magit-ignore-file (concat "/" info) edit local))
    ((wazzup)
     (magit-wazzup-toggle-ignore info edit))))

(defun magit-ignore-item-locally (edit)
  "Ignore the item at point locally only.
With a prefix argument edit the ignore string."
  (interactive "P")
  (magit-ignore-item edit t))

;;;; Discard

(defun magit-discard-diff (diff stagedp)
  (let ((file (magit-diff-item-file diff)))
    (cl-case (magit-diff-item-kind diff)
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

(defun magit-discard-item ()
  (interactive)
  (magit-section-action (item info "discard")
    ((untracked file)
     (when (yes-or-no-p (format "Delete %s? " info))
       (if (and (file-directory-p info)
                (not (file-symlink-p info)))
           (delete-directory info 'recursive)
         (delete-file info))
       (magit-refresh-buffer)))
    ((untracked)
     (when (yes-or-no-p "Delete all untracked files and directories? ")
       (magit-run-git "clean" "-df")))
    ((unstaged diff hunk)
     (when (yes-or-no-p (if (use-region-p)
                            "Discard changes in region? "
                          "Discard hunk? "))
       (magit-apply-hunk-item-reverse item)))
    ((staged diff hunk)
     (if (magit-file-uptodate-p (magit-diff-item-file
                                 (magit-hunk-item-diff item)))
         (when (yes-or-no-p (if (use-region-p)
                                "Discard changes in region? "
                              "Discard hunk? "))
           (magit-apply-hunk-item-reverse item "--index"))
       (error "Can't discard this hunk.  Please unstage it first")))
    ((unstaged diff)
     (magit-discard-diff item nil))
    ((staged diff)
     (if (magit-file-uptodate-p (magit-diff-item-file item))
         (magit-discard-diff item t)
       (error (concat "Can't discard staged changes to this file. "
                      "Please unstage it first"))))
    ((diff diff)
     (save-excursion
       (magit-goto-parent-section)
       (magit-discard-item)))
    ((diff diff hunk)
     (save-excursion
       (magit-goto-parent-section)
       (magit-goto-parent-section)
       (magit-discard-item)))
    ((hunk)
     (error "Can't discard this hunk"))
    ((diff)
     (error "Can't discard this diff"))
    ((stash)
     (when (yes-or-no-p "Discard stash? ")
       (magit-run-git "stash" "drop" info)))
    ((branch)
     (when (yes-or-no-p (if current-prefix-arg
                            (concat "Force delete branch [" info "]? ")
                          (concat "Delete branch [" info "]? ")))
       (magit-delete-branch info current-prefix-arg)))
    ((remote)
     (when (yes-or-no-p "Remove remote? ")
       (magit-remove-remote info)))))

;;;; Rename

(defun magit-rename-item ()
  (interactive)
  (magit-section-action (item info "rename")
    ((branch)
     (call-interactively 'magit-rename-branch))
    ((remote)
     (call-interactively 'magit-rename-remote))))

;;;; ChangeLog

(defmacro magit-visiting-file-item (&rest body)
  (declare (debug t))
  `(let ((marker (save-window-excursion
                   (magit-visit-file-item)
                   (set-marker (make-marker) (point)))))
     (save-excursion
       (with-current-buffer (marker-buffer marker)
         (goto-char marker)
         ,@body))))

(defun magit-add-change-log-entry-no-option (&optional other-window)
  "Add a change log entry for current change.
With a prefix argument, edit in other window.
The name of the change log file is set by
variable change-log-default-name."
  (interactive "P")
  (magit-visiting-file-item
   (if other-window
       (add-change-log-entry-other-window)
     (add-change-log-entry))))

(defun magit-add-change-log-entry-other-window ()
  (interactive)
  (magit-visiting-file-item
   (call-interactively 'add-change-log-entry-other-window)))

;;;; Dired

(eval-after-load 'dired-x
  '(defun magit-dired-jump (&optional other-window)
    "Visit current item.
With a prefix argument, visit in other window."
    (interactive "P")
    (require 'dired-x)
    (magit-section-action (item info "dired-jump")
      ((untracked file)
       (dired-jump other-window (file-truename info)))
      ((diff)
       (dired-jump other-window (file-truename (magit-diff-item-file item))))
      ((diffstat)
       (let ((file (magit-diffstat-item-file item)))
         (if file
             (dired-jump other-window (file-truename file))
           (error "Can't get the pathname for this file"))))
      ((hunk)
       (dired-jump other-window
                   (file-truename (magit-diff-item-file
                                   (magit-hunk-item-diff item)))))
      (nil (dired-jump other-window)))))

;;;; Visit

(defun magit-visit-file-item (&optional other-window)
  "Visit current file associated with item.
With a prefix argument, visit in other window."
  (interactive "P")
  (let* (line
         column
         (file
          (magit-section-action (item info "visit-file")
            ((untracked file) info)
            ((diff)           (magit-diff-item-file item))
            ((diffstat)       (magit-diffstat-item-file item))
            ((hunk)
             (setq line (magit-hunk-item-target-line item)
                   column (current-column))
             (magit-diff-item-file (magit-hunk-item-diff item))))))
    (unless file
      (error "Can't get pathname for this file"))
    (unless (file-exists-p file)
      (error "Can't visit deleted file: %s" file))
    (cond ((file-directory-p file) (magit-status file))
          (other-window            (find-file-other-window file))
          (t                       (find-file file)))
    (when line
      (goto-char (point-min))
      (forward-line (1- line))
      (when (> column 0)
        (move-to-column (1- column))))))

(defun magit-visit-item (&optional other-window)
  "Visit current item.
With a prefix argument, visit in other window."
  (interactive "P")
  (magit-section-action (item info "visit")
    ((untracked file)
     (call-interactively 'magit-visit-file-item))
    ((diff)
     (call-interactively 'magit-visit-file-item))
    ((diffstat)
     (call-interactively 'magit-visit-file-item))
    ((hunk)
     (call-interactively 'magit-visit-file-item))
    ((commit)
     (magit-show-commit info nil nil 'select))
    ((stash)
     (magit-show-stash info)
     (pop-to-buffer magit-stash-buffer-name))
    ((branch)
     (magit-checkout info))
    ((longer)
     (magit-log-show-more-entries ()))))

;;;; Show

(defun magit-show-item-or-scroll-up ()
  (interactive)
  (magit-section-case (item info)
    ((commit)
     (magit-show-commit info #'scroll-up))
    ((stash)
     (magit-show-stash info #'scroll-up))
    (t
     (scroll-up))))

(defun magit-show-item-or-scroll-down ()
  (interactive)
  (magit-section-case (item info)
    ((commit)
     (magit-show-commit info #'scroll-down))
    ((stash)
     (magit-show-stash info #'scroll-down))
    (t
     (scroll-down))))

;;;; Mark

(defun magit-mark-item (&optional unmark)
  (interactive "P")
  (if unmark
      (setq magit-marked-commit nil)
    (magit-section-action (item info "mark")
      ((commit)
       (setq magit-marked-commit
             (if (eq magit-marked-commit info) nil info)))))
  (magit-refresh-marked-commits))

;;;; Describe

(defun magit-describe-item ()
  (interactive)
  (let ((section (magit-current-section)))
    (message "Section: %s %s-%s %S %S %S"
             (magit-section-type section)
             (magit-section-beginning section)
             (magit-section-end section)
             (magit-section-title section)
             (magit-section-info section)
             (magit-section-context-type section))))

(defun magit-copy-item-as-kill ()
  "Copy sha1 of commit at point into kill ring."
  (interactive)
  (magit-section-action (item info "copy")
    ((commit)
     (kill-new info)
     (message "%s" info))))

;;;; Grep

(magit-define-command grep (pattern)
  (interactive
   (list (read-string "git grep: "
                      (shell-quote-argument (grep-tag-default)))))
  (with-current-buffer (generate-new-buffer "*Magit Grep*")
    (setq default-directory (magit-get-top-dir))
    (insert magit-git-executable " "
            (mapconcat 'identity magit-git-standard-options " ")
            " grep -n "
            (shell-quote-argument pattern) "\n\n")
    (magit-git-insert "grep" "--line-number" pattern)
    (grep-mode)
    (pop-to-buffer (current-buffer))))

;;;; Resolve

(defun magit-interactive-resolve (file)
  (require 'ediff)
  (let ((merge-status (magit-git-string "ls-files" "-u" "--" file))
        (base-buffer (generate-new-buffer (concat file ".base")))
        (our-buffer (generate-new-buffer (concat file ".current")))
        (their-buffer (generate-new-buffer (concat file ".merged")))
        (windows (current-window-configuration)))
    (unless merge-status
      (error "Cannot resolve %s" file))
    (with-current-buffer base-buffer
      (when (string-match "^[0-9]+ [0-9a-f]+ 1" merge-status)
        (magit-git-insert "cat-file" "blob" (concat ":1:" file))))
    (with-current-buffer our-buffer
      (when (string-match "^[0-9]+ [0-9a-f]+ 2" merge-status)
        (magit-git-insert "cat-file" "blob" (concat ":2:" file)))
      (let ((buffer-file-name file))
        (normal-mode)))
    (with-current-buffer their-buffer
      (when (string-match "^[0-9]+ [0-9a-f]+ 3" merge-status)
        (magit-git-insert "cat-file" "blob" (concat ":3:" file)))
      (let ((buffer-file-name file))
        (normal-mode)))
    ;; We have now created the 3 buffer with ours, theirs and the ancestor files
    (with-current-buffer (ediff-merge-buffers-with-ancestor
                          our-buffer their-buffer base-buffer nil nil file)
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

(defun magit-interactive-resolve-item ()
  (interactive)
  (magit-section-action (item info "resolv")
    ((diff)
     (magit-interactive-resolve (cadr info)))))

;;; Branch Manager Mode
;;__ FIXME The parens indicate preliminary subsections.
;;;; (the mode)

(define-derived-mode magit-branch-manager-mode magit-mode "Magit Branch"
  "Magit Branches")

;;;; (wacky utilities)

(defun magit--branch-name-at-point ()
  "Get the branch name in the line at point."
  (or (magit-section-info (magit-current-section))
      (error "No branch at point")))

(defun magit--branches-for-remote-repo (remote)
  "Return a list of remote branch names for REMOTE.
These are the branch names with the remote name stripped."
  (cl-loop for branch in (magit-git-lines "branch" "-r" "--list"
                                          (format "%s/*" remote))
           collect (substring branch (+ 3 (length remote)))))

(defvar magit-branches-buffer-name "*magit-branches*")

(defun magit--is-branch-at-point-remote ()
  "Return non-nil if the branch at point is a remote tracking branch."
  (magit-remote-part-of-branch (magit--branch-name-at-point)))

(defun magit-remote-part-of-branch (branch)
  (when (string-match-p "^\\(?:refs/\\)?remotes\\/" branch)
    (cl-loop for remote in (magit-git-lines "remote")
             when (string-match-p (format "^\\(?:refs/\\)?remotes\\/%s\\/"
                                          (regexp-quote remote))
                                  branch)
             return remote)))

(defun magit-branch-no-remote (branch)
  (let ((remote (magit-remote-part-of-branch branch)))
    (if remote
        (progn
          ;; This has to match if remote is non-nil
          (cl-assert (string-match
                      (format "^\\(?:refs/\\)?remotes\\/%s\\/\\(.*\\)"
                              (regexp-quote remote))
                      branch)
                     'show-args "Unexpected string-match failure: %s %s")
          (match-string 1 branch))
      branch)))

;;;; (washing)

(defun magit-wash-branch-line (&optional remote-name)
  (looking-at (concat
               "^\\([ *] \\)"                 ; 1: current branch marker
               "\\(.+?\\) +"                  ; 2: branch name

               "\\(?:"

               "\\([0-9a-fA-F]+\\)"           ; 3: sha1
               " "
               "\\(?:\\["
               "\\([^:\n]+?\\)"               ; 4: tracking
               "\\(?:: \\)?"
               "\\(?:ahead \\([0-9]+\\)\\)?"  ; 5: ahead
               "\\(?:, \\)?"
               "\\(?:behind \\([0-9]+\\)\\)?" ; 6: behind
               "\\] \\)?"
               "\\(?:.*\\)"                   ; message

               "\\|"                          ; or

               "-> "                          ; the pointer to
               "\\(.+\\)"                     ; 7: a ref

               "\\)\n"))

  (let* ((current-string (match-string 1))
         (branch         (match-string 2))
         (sha1           (match-string 3))
         (tracking       (match-string 4))
         (ahead          (match-string 5))
         (behind         (match-string 6))
         (other-ref      (match-string 7))
         (current (string-match-p "^\\*" current-string)))

    ;; the current line is deleted before being reconstructed
    (delete-region (point)
                   (line-beginning-position 2))

    (magit-with-section branch 'branch
      (magit-set-section-info branch)
      (insert-before-markers
       ;; sha1
       (propertize (or sha1
                       (make-string magit-sha1-abbrev-length ? ))
                   'face 'magit-log-sha1)
       " "
       ;; current marker
       (if current
           "# "
         "  ")
       ;; branch name
       (apply 'propertize (magit-branch-no-remote branch)
              (if current
                  '(face magit-branch)))
       ;; other ref that this branch is pointing to
       (if other-ref
           (concat " -> " (substring other-ref (+ 1 (length remote-name))))
         "")
       ;; tracking information
       (if (and tracking
                (equal (magit-get-tracked-branch branch t)
                       (concat "refs/remotes/" tracking)))
           (concat " ["
                   ;; getting rid of the tracking branch name if it is
                   ;; the same as the branch name
                   (let* ((tracking-remote (magit-get "branch" branch "remote"))
                          (tracking-branch (substring tracking
                                                      (+ 1 (length tracking-remote)))))
                     (propertize (if (string= branch tracking-branch)
                                     (concat "@ " tracking-remote)
                                   (concat tracking-branch " @ " tracking-remote))
                                 'face 'magit-log-head-label-remote))
                   ;; ahead/behind information
                   (if (or ahead
                           behind)
                       ": "
                     "")
                   (if ahead
                       (concat "ahead "
                               (propertize ahead
                                           'face (if current
                                                     'magit-branch))
                               (if behind
                                   ", "
                                 ""))
                     "")
                   (if behind
                       (concat "behind "
                               (propertize behind
                                           'face 'magit-log-head-label-remote))
                     "")
                   "]")
         "")
       "\n"))))

(defun magit-wash-remote-branches-group (group)
  (let* ((remote-name (car group))
         (url (magit-get "remote" remote-name "url"))
         (push-url (magit-get "remote" remote-name "pushurl"))
         (urls (concat url (if push-url
                               (concat ", "push-url)
                             "")))
         (marker (cadr group)))

    (magit-with-section (concat "remote:" remote-name) 'remote
      (magit-set-section-info remote-name)
      (insert-before-markers (propertize (format "%s (%s):" remote-name urls)
                                         'face 'magit-section-title) "\n")
      (magit-wash-branches-between-point-and-marker marker remote-name))
    (insert-before-markers "\n")))

(defun magit-wash-branches-between-point-and-marker (marker &optional remote-name)
  (save-restriction
    (narrow-to-region (point) marker)
    (magit-wash-sequence
     (if remote-name
         (apply-partially 'magit-wash-branch-line remote-name)
       #'magit-wash-branch-line))))

(defun magit-wash-branches ()
  ;; get the names of the remotes
  (let* ((remotes (magit-git-lines "remote"))
         ;; get the location of remotes in the buffer
         (markers
          (append (mapcar (lambda (remote)
                            (save-excursion
                              (when (search-forward-regexp
                                     (concat "^  remotes\\/" remote) nil t)
                                (beginning-of-line)
                                (point-marker))))
                          remotes)
                  (list (save-excursion
                          (goto-char (point-max))
                          (point-marker)))))
         ;; list of remote elements to display in the buffer
         (remote-groups
          (cl-loop for remote in remotes
                   for end-markers on (cdr markers)
                   for marker = (cl-loop for x in end-markers thereis x)
                   collect (list remote marker))))

    ;; actual displaying of information
    (magit-with-section "local" nil
      (insert-before-markers (propertize "Local:" 'face 'magit-section-title)
                             "\n")
      (magit-set-section-info ".")
      (magit-wash-branches-between-point-and-marker
       (cl-loop for x in markers thereis x)))

    (insert-before-markers "\n")

    (mapc 'magit-wash-remote-branches-group remote-groups)

    ;; make sure markers point to nil so that they can be garbage collected
    (mapc (lambda (marker)
            (when marker
             (set-marker marker nil)))
          markers)))

;;;; (refresh and invocation)

(defun magit-refresh-branch-manager ()
  (magit-create-buffer-sections
    (apply #'magit-git-section
           "branches" nil 'magit-wash-branches
           "branch" "-vva" (magit-diff-abbrev-arg)
           magit-custom-options)))

(magit-define-command branch-manager ()
  (interactive)
  (magit-mode-setup magit-branches-buffer-name
                    #'magit-branch-manager-mode
                    #'magit-refresh-branch-manager))

;;;; (wacky non-generic set-tracked)

(defun magit-change-what-branch-tracks ()
  "Change which remote branch the current branch tracks."
  (interactive)
  (when (magit--is-branch-at-point-remote)
    (error "Cannot modify a remote branch"))
  (let* ((local-branch (magit--branch-name-at-point))
         (new-tracked (magit-read-rev  "Change tracked branch to"
                                       nil
                                       (lambda (ref)
                                         (not (string-match-p "refs/remotes/"
                                                              ref)))))
         new-remote new-branch)
    (unless (string= (or new-tracked "") "")
      (cond (;; Match refs that are unknown in the local repository if
             ;; `magit-remote-ref-format' is set to
             ;; `name-then-remote'. Can be useful if you want to
             ;; create a new branch in a remote repository.
             (string-match "^\\([^ ]+\\) +(\\(.+\\))$" ; 1: branch name; 2: remote name
                           new-tracked)
             (setq new-remote (match-string 2 new-tracked)
                   new-branch (concat "refs/heads/" (match-string 1 new-tracked))))
            ((string-match "^\\(?:refs/remotes/\\)?\\([^/]+\\)/\\(.+\\)" ; 1: remote name; 2: branch name
                           new-tracked)
             (setq new-remote (match-string 1 new-tracked)
                   new-branch (concat "refs/heads/" (match-string 2 new-tracked))))
            (t (error "Cannot parse the remote and branch name"))))
    (magit-set new-remote "branch" local-branch "remote")
    (magit-set new-branch "branch" local-branch "merge")
    (magit-branch-manager)
    (when (string= (magit-get-current-branch) local-branch)
      (magit-refresh-buffer (magit-find-status-buffer default-directory)))))

;;; Miscellaneous
;;;; Miscellaneous Commands

(defun magit-show (commit filename &optional select prefix)
  "Return a buffer containing the file FILENAME, as stored in COMMIT.

COMMIT may be one of the following:
- A string with the name of a commit, such as \"HEAD\" or
  \"dae86e\".  See 'git help revisions' for syntax.
- The symbol 'index, indicating that you want the version in
  Git's index or staging area.
- The symbol 'working, indicating that you want the version in
  the working directory.  In this case you'll get a buffer
  visiting the file.  If there's already a buffer visiting that
  file, you'll get that one.

When called interactively or when SELECT is non-nil, make the
buffer active, either in another window or (with a prefix
argument) in the current window."
  (interactive
   (let* ((revision (magit-read-rev "Retrieve file from revision"))
          (filename (magit-read-file-from-rev revision)))
     (list revision filename t current-prefix-arg)))
  (if (eq commit 'working)
      (find-file-noselect filename)
    (let ((buffer (create-file-buffer
                   (format "%s.%s" filename
                           (replace-regexp-in-string
                            ".*/" "" (prin1-to-string commit t))))))
      (cond
       ((eq commit 'index)
        (let ((checkout-string (magit-git-string "checkout-index"
                                                 "--temp"
                                                 filename)))
          (string-match "^\\(.*\\)\t" checkout-string)
          (with-current-buffer buffer
            (let ((tmpname (match-string 1 checkout-string)))
              (with-silent-modifications
               (insert-file-contents tmpname nil nil nil t))
              (delete-file tmpname)))))
       (t
        (with-current-buffer buffer
          (with-silent-modifications
           (magit-git-insert "cat-file" "-p"
                             (concat commit ":" filename))))))
      (with-current-buffer buffer
        (let ((buffer-file-name
               (expand-file-name filename (magit-get-top-dir))))
          (normal-mode))
        (setq magit-file-name filename)
        (setq magit-show-current-version commit)
        (goto-char (point-min)))
      (if select
          (if prefix
              (switch-to-buffer buffer)
            (switch-to-buffer-other-window buffer))
        buffer))))

;;;; External Tools

(defun magit-run-git-gui ()
  "Run `git gui' for the current git repository."
  (interactive)
  (let* ((default-directory (magit-get-top-dir)))
    (start-file-process "Git Gui" nil magit-git-executable "gui")))

(defun magit-run-git-gui-blame (commit filename &optional linenum)
  "Run `git gui blame' on the given FILENAME and COMMIT.
When the current buffer is visiting FILENAME instruct
blame to center around the line point is on."
  (interactive
   (let* ((revision (magit-read-rev "Retrieve file from revision" "HEAD"))
          (filename (magit-read-file-from-rev revision)))
     (list revision filename
           (and (equal filename
                       (magit-file-relative-name (buffer-file-name)))
                (line-number-at-pos)))))
  (let ((default-directory (magit-get-top-dir default-directory)))
    (apply 'start-file-process "Git Gui Blame" nil
           magit-git-executable "gui" "blame"
           (list (and linenum (format "--line=%d" linenum))
                 commit
                 filename))))

(defun magit-run-gitk ()
  "Run `gitk --all' for the current git repository."
  (interactive)
  (let ((default-directory (magit-get-top-dir)))
    (cond
     ((eq system-type 'windows-nt)
      ;; Gitk is a shell script, and Windows doesn't know how to
      ;; "execute" it.  The Windows version of Git comes with an
      ;; implementation of "sh" and everything else it needs, but
      ;; Windows users might not have added the directory where it's
      ;; installed to their path
      (let* ((git-bin-dir
             ;; According to #824, when using stand-alone installation
             ;; Gitk maybe installed in ...cmd or ...bin; while Sh
             ;; is installed in ...bin.
             (expand-file-name "bin"
                               (file-name-directory
                                (directory-file-name
                                 (file-name-directory
                                  magit-gitk-executable)))))
            ;; Adding it onto the end so that anything the user
            ;; specified will get tried first.  Emacs looks in
            ;; exec-path; PATH is the environment variable inherited by
            ;; the process.  I need to change both.
            (exec-path (append exec-path (list git-bin-dir)))
            (process-environment process-environment))
        (setenv "PATH"
                (format "%s;%s"
                        (getenv "PATH")
                        (replace-regexp-in-string "/" "\\\\" git-bin-dir)))
        (start-file-process "Gitk" nil "sh" magit-gitk-executable "--all")))
     (t
      (start-file-process "Gitk" nil magit-gitk-executable "--all")))))

;;;; Magit Extensions

(defun magit-load-config-extensions ()
  "Try to load magit extensions that are defined at git config layer.
This can be added to `magit-mode-hook' for example"
  (dolist (ext (magit-get-all "magit.extension"))
    (let ((sym (intern (format "magit-%s-mode" ext))))
      (when (and (fboundp sym)
                 (not (eq sym 'magit-wip-save-mode)))
        (funcall sym 1)))))

;;;; Magit Font-Lock

(defconst magit-font-lock-keywords
  (eval-when-compile
    `((,(concat "(\\(" (regexp-opt
                     '("magit-define-level-shower"
                       "magit-define-section-jumper"
                       "magit-define-inserter"
                       "magit-define-command"))
                "\\)\\>[ \t'\(]*\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      (,(concat "(" (regexp-opt
                     '("magit-with-refresh"
                       "magit-with-section"
                       "magit-create-buffer-sections"
                       "magit-section-action"
                       "magit-section-case"
                       "magit-add-action-clauses"
                       "magit-create-log-buffer-sections"
                       "magit-with-revert-confirmation"
                       "magit-visiting-file-item"
                       "magit-tests--with-temp-dir"
                       "magit-tests--with-temp-repo"
                       "magit-tests--with-temp-clone") t)
                "\\>")
       . 1)))
  "Magit expressions to highlight in Emacs-Lisp mode.
To highlight Magit expressions add something like this to your
init file:

  (require 'magit)
  (font-lock-add-keywords 'emacs-lisp-mode
                          magit-font-lock-keywords)")

;;;; Magit Version

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
              ((featurep 'package) ; shouldn't that make it easier?
               (setq magit-version
                     (or (ignore-errors ; < 24.4
                           (package-version-join
                            (package-desc-vers
                             (cdr (assq 'magit package-alist)))))
                         (ignore-errors ; = 24.4
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

;; rest of magit core
(require 'magit-key-mode)
(require 'magit-bisect)
(require 'magit-cherry)

;;; magit.el ends here
