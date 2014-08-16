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
;; Package-Requires: ((cl-lib "0.5") (dash "2.8.0") (git-commit-mode "0.15.0") (git-rebase-mode "0.15.0") (with-editor "0.15.0"))

;; Magit requires at least GNU Emacs 24.1 and Git 1.7.2.5.

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

;; Invoking the `magit-status' command will show a buffer with the
;; status of the current Git repository and its working tree.  That
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

(when (version< emacs-version "24.1")
  (error "Magit requires at least GNU Emacs 24.1"))

(require 'cl-lib)
(require 'dash)

(require 'git-commit-mode)
(require 'git-rebase-mode)
(require 'with-editor)
(require 'magit-core)
(require 'magit-diff)
(require 'magit-apply)
(require 'magit-log)

(declare-function magit-blame-chunk-get 'magit-blame)
(declare-function magit-blame-mode 'magit-blame)
(defvar magit-blame-mode)

(defvar magit-this-process)

(require 'ansi-color)
(require 'autorevert)
(require 'diff-mode)
(require 'format-spec)
(require 'package nil t) ; used in `magit-version'
(require 'server) ; for `server-visit-hook'

(eval-when-compile (require 'dired))
(declare-function dired-uncache 'dired)
(eval-when-compile (require 'dired-x))
(declare-function dired-jump 'dired-x)
(eval-when-compile (require 'epa)) ; for `epa-protocol'
(eval-when-compile (require 'epg))
(declare-function epg-sub-key-id 'epg)
(declare-function epg-key-sub-key-list 'epg)
(declare-function epg-key-user-id-list 'epg)
(declare-function epg-user-id-string 'epg)
(declare-function epg-decode-dn 'epg)
(declare-function epg-list-keys 'epg)
(eval-when-compile (require 'eshell))
(declare-function eshell-parse-arguments 'eshell)
(eval-when-compile (require 'smerge-mode))


;;; Settings
;;;; Custom Groups

(defgroup magit nil
  "Controlling Git from Emacs."
  :group 'tools)

(defgroup magit-popups nil
  "Command console popups provided by Magit."
  :group 'magit)

(defgroup magit-commands nil
  "Options controlling behavior of certain commands."
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
  "Automatically commit work-in-progress to a dedicated ref."
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

(defcustom magit-success-executable "true"
  "The executable which always succeeds.
An executable, such as \"true\", which does
nothing but return with a zero exit status."
  :package-version '(magit . "2.1.0")
  :group 'magit-process
  :type 'string)

(defcustom magit-process-connection-type (not (eq system-type 'cygwin))
  "Connection type used for the Git process.

If nil, use pipes: this is usually more efficient, and works on Cygwin.
If t, use ptys: this enables Magit to prompt for passphrases when needed."
  :group 'magit-process
  :type '(choice (const :tag "pipe" nil)
                 (const :tag "pty" t)))

(defcustom magit-process-buffer-name-format "*magit-process: %a*"
  "Name format for buffers where output of processes is put.

The following `format'-like specs are supported:
%a the absolute filename of the repository toplevel.
%b the basename of the repository toplevel."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type 'string)

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

(defcustom magit-process-yes-or-no-prompt-regexp
  " [\[(]\\([Yy]\\(?:es\\)?\\)[/|]\\([Nn]o?\\)[\])] ?[?:] ?$"
  "Regexp matching Yes-or-No prompts of Git and its subprocesses."
  :package-version '(magit . "2.1.0")
  :group 'magit-process
  :type 'regexp)

(defcustom magit-process-password-prompt-regexps
  '("^\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$"
    "^\\(Enter \\)?[Pp]assword\\( for '.*'\\)?: ?$"
    "^.*'s password: ?$"
    "^Yubikey for .*: ?$")
  "List of regexps matching password prompts of Git and its subprocesses."
  :package-version '(magit . "2.1.0")
  :group 'magit-process
  :type '(repeat (regexp)))

(defcustom magit-process-username-prompt-regexps
  '("^Username for '.*': ?$")
  "List of regexps matching username prompts of Git and its subprocesses."
  :package-version '(magit . "2.1.0")
  :group 'magit-process
  :type '(repeat (regexp)))

;;;;; Common

(defcustom magit-delete-by-moving-to-trash t
  "Whether Magit uses the system's trash can."
  :package-version '(magit . "2.1.0")
  :group 'magit
  :type 'boolean)

(defcustom magit-apply-backup nil
  "Whether to create a backup before using `git apply'.

WARNING: This only creates a backup when Magit used `git apply'.
That is not sufficient as not every action that might destroy
uncommitted changes eventually uses that Git command.  Don't rely
on this.  Reminder: you are using Magit's `next' branch and are
therefor using a development version.

The diff is stored in the file \".git/magit/apply.diff\" and can
be applied in reversed using `magit-apply-undo'.  Older diffs are
available in the same directory as numbered backup files and have
to be applied manually."
  :package-version '(magit . "2.1.0")
  :group 'magit
  :type 'boolean)

(define-obsolete-variable-alias 'magit-revert-backup 'magit-apply-backup)

;;;;; Highlighting

(defcustom magit-highlight-whitespace t
  "Specify where to highlight whitespace errors.
See `magit-highlight-trailing-whitespace',
`magit-highlight-indentation'.  The symbol t means in all diffs,
`status' means only in the status buffer, and nil means nowhere."
  :group 'magit-diff
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (const :tag "In status buffer" status)))

(defcustom magit-highlight-trailing-whitespace t
  "Whether to highlight whitespace at the end of a line in diffs.
Used only when `magit-highlight-whitespace' is non-nil."
  :group 'magit-diff
  :type 'boolean)

(defcustom magit-highlight-indentation nil
  "Highlight the \"wrong\" indentation style.
Used only when `magit-highlight-whitespace' is non-nil.

The value is a list of cons cells.  The car is a regular
expression, and the cdr is the value that applies to repositories
whose directory matches the regular expression.  If more than one
element matches, then the *last* element in the list applies.
The default value should therefor come first in the list.

If the value is `tabs', highlight indentation with tabs.  If the
value is an integer, highlight indentation with at least that
many spaces.  Otherwise, highlight neither."
  :group 'magit-diff
  :type `(repeat (cons (string :tag "Directory regexp")
                       (choice (const :tag "Tabs" tabs)
                               (integer :tag "Spaces" :value ,tab-width)
                               (const :tag "Neither" nil))))) ;^FIXME

;;;;; Completion

(defcustom magit-repo-dirs nil
  "Directories containing Git repositories.
Magit will look into these directories for Git repositories and
offer them as choices for `magit-status'."
  :group 'magit
  :type '(repeat string))

(defcustom magit-repo-dirs-depth 3
  "The maximum depth to look for Git repositories.
When looking for a Git repository below the directories in
`magit-repo-dirs', Magit will only descend this many levels
deep."
  :group 'magit
  :type 'integer)

;;;;; Modes
;;;;;; Status

(defcustom magit-status-mode-hook nil
  "Hook run when the `magit-status' buffer is created."
  :group 'magit-status
  :type 'hook)

(defcustom magit-status-headers-hook
  '(magit-insert-status-tags-line)
  "Hook run to insert headers into the status buffer.

This hook is run by `magit-insert-status-headers', which always
inserts the \"Head\" and \"Upstream\" headers before the headers
listed here.  `magit-insert-status-headers' has to be a member
of `magit-insert-status-sections', or no headers are inserted."
  :package-version '(magit . "2.1.0")
  :group 'magit-status
  :type 'hook)

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
  "Function used by `magit-status' to switch to the status buffer.

The function is given one argument, the status buffer."
  :group 'magit-status
  :type '(radio (function-item switch-to-buffer)
                (function-item pop-to-buffer)
                (function :tag "Other")))

(defcustom magit-status-buffer-name-format "*magit: %a*"
  "Name format for buffers used to display a repository's status.
The following `format'-like specs are supported:
%a the absolute filename of the repository toplevel.
%b the basename of the repository toplevel."
  :package-version '(magit . "2.1.0")
  :group 'magit-status
  :type 'string)

;;;;;; Diff

(defcustom magit-diff-buffer-name-format "*magit-diff: %a*"
  "Name format for buffers used to display a diff.

The following `format'-like specs are supported:
%a the absolute filename of the repository toplevel.
%b the basename of the repository toplevel."
  :package-version '(magit . "2.1.0")
  :group 'magit-diff
  :type 'string)

(defcustom magit-diff-show-diffstat t
  "Whether to show diffstat in diff buffers."
  :package-version '(magit . "2.1.0")
  :group 'magit-diff
  :type 'boolean)

(defcustom magit-diff-show-xref-buttons t
  "Whether to show buffer history buttons in diff buffers."
  :package-version '(magit . "2.1.0")
  :group 'magit-diff
  :type 'boolean)

(defcustom magit-diff-options nil
  ""
  :group 'magit-popups
  :type 'sexp)

(put 'magit-diff-options 'permanent-local t)

;; This variable is only a temporary hack.  Eventually it will
;; be possible to set some of these arguments in the diff popup.
(defvar magit-diff-extra-options '("-M" "-C" "--no-prefix"))

(defcustom magit-diff-auto-show
  '(commit stage-all log-oneline log-select blame-follow)
  "Whether to automatically show relevant diff or commit.

When this option is non-nil certain operations cause the relevant
changes to be displayed automatically.

`commit'
`stage-all'
`log-oneline'
`log-follow'
`log-select'
`blame-follow'

In the event that expanding very large patches takes a long time
\\<global-map>\\[keyboard-quit] can be used to abort that step.
This is especially useful when you would normally not look at the
changes, e.g. because you are committing some binary files."
  :package-version '(magit . "2.1.0")
  :group 'magit-diff
  :type 'sexp)

(defcustom magit-diff-refine-hunk nil
  "Whether to show word-granularity differences within diff hunks.

nil    never show fine differences.
t      show fine differences for the selected diff hunk only.
`all'  show fine differences for all displayed diff hunks."
  :group 'magit-diff
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Selected only" t)
                 (const :tag "All" all)))

;;;;;; Commit

(defcustom magit-commit-buffer-name-format "*magit-commit: %a*"
  "Name format for buffers used to display a commit.

The following `format'-like specs are supported:
%a the absolute filename of the repository toplevel.
%b the basename of the repository toplevel."
  :package-version '(magit . "2.1.0")
  :group 'magit-commit
  :type 'string)

(defcustom magit-commit-show-diffstat t
  "Whether to show diffstat in commit buffers."
  :package-version '(magit . "2.1.0")
  :group 'magit-commit
  :type 'boolean)

(defcustom magit-commit-show-xref-buttons t
  "Whether to show buffer history buttons in commit buffers."
  :package-version '(magit . "2.1.0")
  :group 'magit-commit
  :type 'boolean)

;; This variable is only a temporary hack.
(defvar magit-commit-extra-options '("--decorate=full" "--pretty=medium"))

(defcustom magit-commit-ask-to-stage t
  "Whether to ask to stage everything when committing and nothing is staged."
  :package-version '(magit . "2.1.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-commit-extend-override-date nil
  "Whether using `magit-commit-extend' changes the committer date."
  :package-version '(magit . "2.1.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-commit-reword-override-date nil
  "Whether using `magit-commit-reword' changes the committer date."
  :package-version '(magit . "2.1.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-commit-squash-confirm t
  "Whether the commit targeted by squash and fixup has to be confirmed.
When non-nil then the commit at point (if any) is used as default
choice, otherwise it has to be confirmed.  This option only
affects `magit-commit-squash' and `magit-commit-fixup'.  The
\"instant\" variants always require confirmation because making
an error while using those is harder to recover from."
  :package-version '(magit . "2.1.0")
  :group 'magit-commands
  :type 'boolean)

;;;;;; Log

(defcustom magit-log-buffer-name-format "*magit-log: %a*"
  "Name format for buffers used to display log entries.

The following `format'-like specs are supported:
%a the absolute filename of the repository toplevel.
%b the basename of the repository toplevel."
  :package-version '(magit . "2.1.0")
  :group 'magit-log
  :type 'string)

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
                 (function-item magit-log-format-unicode-graph)
                 function))

(defcustom magit-log-format-unicode-graph-alist
  '((?/ . ?╱) (?| . ?│) (?\\ . ?╲) (?* . ?◆) (?o . ?◇))
  "Alist used by `magit-log-format-unicode-graph' to translate chars."
  :package-version '(magit . "2.1.0")
  :group 'magit-log
  :type '(repeat (cons :format "%v\n"
                       (character :format "replace %v ")
                       (character :format "with %v"))))

(defcustom magit-log-show-margin t
  "Whether to initially show the margin in log buffers.

When non-nil the author name and date are initially displayed in
the margin of log buffers.  The margin can be shown or hidden in
the current buffer using the command `magit-log-toggle-margin'.

When a log buffer contains a verbose log, then the margin is
never displayed.  In status buffers this option is ignored but
it is possible to show the margin using the mentioned command."
  :package-version '(magit . "2.1.0")
  :group 'magit-log
  :type 'boolean)

(put 'magit-log-show-margin 'permanent-local t)

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
Each element has the form (CHAR UNIT UNITS SECONDS).  UNIT is the
time unit, UNITS is the plural of that unit.  CHAR is a character
abbreviation.  And SECONDS is the number of seconds in one UNIT.
Also see option `magit-log-margin-spec'."
  :package-version '(magit . "2.1.0")
  :group 'magit-log
  :type '(repeat (list (character :tag "Unit character")
                       (string    :tag "Unit singular string")
                       (string    :tag "Unit plural string")
                       (integer   :tag "Seconds in unit"))))

(defcustom magit-log-margin-spec '(28 7 magit-duration-spec)
  "How to format the log margin.

The log margin is used to display each commit's author followed
by the commit's age.  This option controls the total width of the
margin and how time units are formatted, the value has the form:

  (WIDTH UNIT-WIDTH DURATION-SPEC)

WIDTH specifies the total width of the log margin.  UNIT-WIDTH is
either the integer 1, in which case time units are displayed as a
single characters, leaving more room for author names; or it has
to be the width of the longest time unit string in DURATION-SPEC.
DURATION-SPEC has to be a variable, its value controls which time
units, in what language, are being used."
  :package-version '(magit . "2.1.0")
  :group 'magit-log
  :set-after '(magit-duration-spec)
  :type '(list (integer  :tag "Margin width")
               (choice   :tag "Time unit style"
                         (const   :format "%t\n"
                                  :tag "abbreviate to single character" 1)
                         (integer :format "%t\n"
                                  :tag "show full name" 7))
               (variable :tag "Duration spec variable")))

(defcustom magit-log-section-commit-count 10
  "How many recent commits to show in certain log sections.
How many recent commits `magit-insert-recent-commits' and
`magit-insert-unpulled-or-recent-commits' (provided there
are no unpulled commits) show."
  :package-version '(magit . "2.1.0")
  :group 'magit-status
  :type 'number)

(defcustom magit-log-section-args nil
  "Additional Git arguments used when creating log sections.
Only `--graph', `--decorate', and `--show-signature' are
supported.  This option is only a temporary kludge and will
be removed again.  Note that due to an issue in Git the
use of `--graph' is very slow with long histories.  See
http://www.mail-archive.com/git@vger.kernel.org/msg51337.html"
  :package-version '(magit . "2.1.0")
  :group 'magit-status
  :type '(repeat (choice (const "--graph")
                         (const "--decorate")
                         (const "--show-signature"))))

;;;;;; Others

(defcustom magit-merge-warn-dirty-worktree t
  "Whether to warn before merging with a dirty worktree."
  :package-version '(magit . "2.1.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-set-upstream-on-push nil
  "Whether `magit-push' may set upstream when pushing a branch.
This only applies if the branch does not have an upstream set yet.

nil        don't use --set-upstream.
t          ask if --set-upstream should be used.
`dontask'  always use --set-upstream.
`refuse'   refuse to push unless a remote branch has already been set."
  :group 'magit-commands
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Ask" t)
                 (const :tag "Ask if not set" askifnotset)
                 (const :tag "Refuse" refuse)
                 (const :tag "Always" dontask)))

(defcustom magit-stash-snapshot-message-format
  "Snapshot taken at %Y-%m-%d %H:%M:%S"
  "Format for messages of snapshot stashes."
  :package-version '(magit . "2.1.0")
  :group 'magit-commands
  :type 'string)

(defcustom magit-refs-sections-hook
  '(magit-insert-branch-description
    magit-insert-local-branches
    magit-insert-remote-branches
    magit-insert-tags)
  "Hook run to insert sections into the references buffer."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type 'hook)

(defcustom magit-cherry-sections-hook
  '(magit-insert-cherry-headers
    magit-insert-cherry-commits)
  "Hook run to insert sections into the cherry buffer."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type 'hook)

(defcustom magit-reflog-buffer-name-format "*magit-reflog: %a*"
  "Name format for buffers used to display reflog entries.

The following `format'-like specs are supported:
%a the absolute filename of the repository toplevel.
%b the basename of the repository toplevel."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type 'string)

(defcustom magit-refs-buffer-name-format "*magit-refs: %a*"
  "Name format for buffers used to display and manage refs.

The following `format'-like specs are supported:
%a the absolute filename of the repository toplevel.
%b the basename of the repository toplevel."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type 'string)

(defcustom magit-cherry-buffer-name-format "*magit-cherry: %a*"
  "Name format for buffers used to display commits not merged upstream.

The following `format'-like specs are supported:
%a the absolute filename of the repository toplevel.
%b the basename of the repository toplevel."
  :group 'magit-modes
  :type 'string)

(defcustom magit-wip-commit-message "autosave %r"
  "Commit message for automatic work-in-progress commits.

The following `format'-like specs are supported:
%r the relative filename of the file being saved,
%a the absolute filename of the file being saved, and
%t the absolute filename of the repository toplevel."
  :group 'magit-wip
  :type 'string)

(defcustom magit-wip-save-message "Wrote %a (wip)"
  "Message shown after updating a work-in-progress ref.

The following `format'-like specs are supported:
%r the relative filename of the file being saved,
%a the absolute filename of the file being saved, and
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
  :group 'magit-wip
  :type 'string)

;;;; Custom Faces

(defface magit-file-heading
  '((t :weight bold))
  "Face for diff file headings."
  :group 'magit-faces)

(defface magit-hunk-heading
  '((((class color) (background light))
     :background "grey80"
     :foreground "grey30")
    (((class color) (background dark))
     :background "grey25"
     :foreground "grey70"))
  "Face for diff hunk headings."
  :group 'magit-faces)

(defface magit-hunk-heading-highlight
  '((((class color) (background light))
     :background "grey75"
     :foreground "grey30")
    (((class color) (background dark))
     :background "grey35"
     :foreground "grey70"))
  "Face for diff hunk headings."
  :group 'magit-faces)

(defface magit-conflict-heading
  '((t :inherit magit-hunk-heading))
  "Face for conflict markers."
  :group 'magit-faces)

(defface magit-diff-added
  '((((class color) (background light))
     :background "#ddffdd"
     :foreground "#22aa22")
    (((class color) (background dark))
     :background "#335533"
     :foreground "#aaccaa"))
  "Face for lines in a diff that have been added."
  :group 'magit-faces)

(defface magit-diff-removed
 '((((class color) (background light))
     :background "#ffdddd"
     :foreground "#aa2222")
    (((class color) (background dark))
     :background "#553333"
     :foreground "#ccaaaa"))
  "Face for lines in a diff that have been removed."
  :group 'magit-faces)

(defface magit-diff-context
  '((((class color) (background light)) :foreground "grey50")
    (((class color) (background  dark)) :foreground "grey70"))
  "Face for lines in a diff that are unchanged."
  :group 'magit-faces)

(defface magit-diff-added-highlight
  '((((class color) (background light))
     :background "#cceecc"
     :foreground "#22aa22")
    (((class color) (background dark))
     :background "#336633"
     :foreground "#bbddbb"))
  "Face for lines in a diff that have been added."
  :group 'magit-faces)

(defface magit-diff-removed-highlight
  '((((class color) (background light))
     :background "#eecccc"
     :foreground "#aa2222")
    (((class color) (background dark))
     :background "#663333"
     :foreground "#ddbbbb"))
  "Face for lines in a diff that have been removed."
  :group 'magit-faces)

(defface magit-diff-context-highlight
  '((((class color) (background light))
     :background "grey85"
     :foreground "grey50")
    (((class color) (background dark))
     :background "grey20"
     :foreground "grey70"))
  "Face for lines in a diff that have been removed."
  :group 'magit-faces)

(defface magit-diffstat-added
  '((((class color) (background light)) :foreground "#22aa22")
    (((class color) (background  dark)) :foreground "#448844"))
  "Face for plus sign in diffstats."
  :group 'magit-faces)

(defface magit-diffstat-removed
  '((((class color) (background light)) :foreground "#aa2222")
    (((class color) (background  dark)) :foreground "#aa4444"))
  "Face for minus sign in diffstats."
  :group 'magit-faces)

(defface magit-log-graph
  '((((class color) (background light)) :foreground "grey30")
    (((class color) (background  dark)) :foreground "grey80"))
  "Face for the graph part of the log output."
  :group 'magit-faces)

(defface magit-log-author
  '((((class color) (background light)) :foreground "firebrick")
    (((class color) (background  dark)) :foreground "tomato"))
  "Face for the author part of the log output."
  :group 'magit-faces)

(defface magit-log-date
  '((((class color) (background light)) :foreground "grey30")
    (((class color) (background  dark)) :foreground "grey80"))
  "Face for the date part of the log output."
  :group 'magit-faces)

(defface magit-bisect-good
  '((t :background "LightGreen"
       :foreground "DarkOliveGreen"))
  "Face for good bisect revisions."
  :group 'magit-faces)

(defface magit-bisect-skip
  '((t :background "LightGoldenrod"
       :foreground "DarkGoldenrod"))
  "Face for skipped bisect revisions."
  :group 'magit-faces)

(defface magit-bisect-bad
  '((t :background "IndianRed1"
       :foreground "IndianRed4"))
  "Face for bad bisect revisions."
  :group 'magit-faces)

(defface magit-tag
  '((t :background "LemonChiffon1"
       :foreground "goldenrod4"))
  "Face for tag labels shown in log buffer."
  :group 'magit-faces)

(defface magit-branch-local
  '((((class color) (background light))
     :background "grey80"
     :foreground "SkyBlue4")
    (((class color) (background dark))
     :background "grey30"
     :foreground "LightSkyBlue1"))
  "Face for local branches."
  :group 'magit-faces)

(defface magit-branch-remote
  '((((class color) (background light))
     :background "grey80"
     :foreground "DarkOliveGreen4")
    (((class color) (background dark))
     :background "grey30"
     :foreground "DarkSeaGreen2"))
  "Face for remote branch head labels shown in log buffer."
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

(defface magit-head
  '((((class color) (background light))
     :background "grey75"
     :foreground "grey15")
    (((class color) (background dark))
     :background "grey30"
     :foreground "grey90"))
  "Face for the symbolic ref \"HEAD\"."
  :group 'magit-faces)

(defface magit-refname
  '((((class color) (background light))
     :background "grey75"
     :foreground "grey30")
    (((class color) (background  dark))
     :background "grey30"
     :foreground "grey80"))
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

(defface magit-reflog-commit
  '((t :background "LemonChiffon1"
       :foreground "goldenrod4"))
  "Face for commit commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-amend
  '((t :inherit magit-reflog-commit))
  "Face for amend commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-merge
  '((t :inherit magit-reflog-commit))
  "Face for merge, checkout and branch commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-checkout
  '((((class color) (background light))
     :background "grey85"
     :foreground "LightSkyBlue4")
    (((class color) (background dark))
     :background "grey30"
     :foreground "LightSkyBlue1"))
  "Face for checkout commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-reset
  '((t :background "IndianRed1"
       :foreground "IndianRed4"))
  "Face for reset commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-rebase
  '((((class color) (background light))
     :background "grey85"
     :foreground "OliveDrab4")
    (((class color) (background dark))
     :background "grey30"
     :foreground "DarkSeaGreen2"))
  "Face for rebase commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-cherry-pick
  '((t :background "LightGreen"
       :foreground "DarkOliveGreen"))
  "Face for cherry-pick commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-remote
  '((t :background "grey50"))
  "Face for pull and clone commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-other
  '((t :background "grey50"))
  "Face for other commands in reflogs."
  :group 'magit-faces)

(defface magit-process-ok
  '((t :inherit magit-section-heading :foreground "green"))
  "Face for zero exit-status."
  :group 'magit-faces)

(defface magit-process-ng
  '((t :inherit magit-section-heading :foreground "red"))
  "Face for non-zero exit-status."
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
  "Face for unmatched cherry commits.")

(defface magit-cherry-equivalent
  '((t :foreground "magenta"))
  "Face for equivalent cherry commits.")

(defface magit-whitespace-warning
  '((t :inherit trailing-whitespace))
  "Face for highlighting whitespace errors in Magit diffs."
  :group 'magit-faces)

;;;; Keymaps

(define-key git-commit-mode-map
  (kbd "C-c C-d") 'magit-diff-while-committing)

(defvar magit-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "jz" 'magit-jump-to-stashes)
    (define-key map "jn" 'magit-jump-to-untracked)
    (define-key map "ju" 'magit-jump-to-unstaged)
    (define-key map "js" 'magit-jump-to-staged)
    (define-key map "jf" 'magit-jump-to-unpulled)
    (define-key map "jp" 'magit-jump-to-unpushed)
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
    (define-key map "j" 'magit-jump-to-diffstats)
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
    (define-key map "\C-c\C-b" 'magit-go-backward)
    (define-key map "\C-c\C-f" 'magit-go-forward)
    (define-key map "+" 'magit-log-show-more-entries)
    map)
  "Keymap for `magit-log-mode'.")

(defvar magit-log-select-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-log-mode-map)
    (define-key map "\C-c\C-b" 'undefined)
    (define-key map "\C-c\C-f" 'undefined)
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

(defvar magit-refs-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    map)
  "Keymap for `magit-refs-mode'.")

(defvar magit-process-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    map)
  "Keymap for `magit-process-mode'.")

(defvar magit-hunk-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-visit-file)
    (define-key map "a"  'magit-apply)
    (define-key map "C"  'magit-commit-add-log)
    (define-key map "k"  'magit-discard)
    (define-key map "s"  'magit-stage)
    (define-key map "u"  'magit-unstage)
    (define-key map "v"  'magit-reverse)
    map)
  "Keymap for `hunk' sections.")

(defvar magit-file-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-visit-file)
    (define-key map "a"  'magit-apply)
    (define-key map "k"  'magit-discard)
    (define-key map "s"  'magit-stage)
    (define-key map "u"  'magit-unstage)
    (define-key map "v"  'magit-reverse)
    map)
  "Keymap for `file' sections.")

(defvar magit-commit-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-show-commit)
    (define-key map "a"  'magit-cherry-apply)
    (define-key map "v"  'magit-revert-no-commit)
    map)
  "Keymap for `commit' sections.")

(defvar magit-mcommit-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-show-commit)
    map)
  "Keymap for `mcommit' (module commit) sections.")

(defvar magit-stash-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-diff-stash)
    (define-key map "a"  'magit-stash-apply)
    (define-key map "A"  'magit-stash-pop)
    (define-key map "k"  'magit-stash-drop)
    map)
  "Keymap for `stash' sections.")

(defvar magit-branch-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-show-commit)
    (define-key map "k"  'magit-branch-delete)
    (define-key map "R"  'magit-branch-rename)
    map)
  "Keymap for `branch' sections.")

(defvar magit-remote-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "k"  'magit-remote-remove)
    (define-key map "R"  'magit-remote-rename)
    map)
  "Keymap for `remote' sections.")

(defvar magit-staged-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-diff-staged)
    (define-key map "k"  'magit-discard)
    (define-key map "s"  'magit-stage)
    (define-key map "u"  'magit-unstage)
    (define-key map "v"  'magit-reverse)
    map)
  "Keymap for the `staged' section.")

(defvar magit-unstaged-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-diff-unstaged)
    (define-key map "k"  'magit-discard)
    (define-key map "s"  'magit-stage)
    (define-key map "u"  'magit-unstage)
    map)
  "Keymap for the `unstaged' section.")

(defvar magit-untracked-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "k"  'magit-discard)
    (define-key map "s"  'magit-stage)
    map)
  "Keymap for the `untracked' section.")

(defvar magit-unpushed-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-diff-unpushed)
    map)
  "Keymap for the `unpushed' section.")

(defvar magit-unpulled-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-diff-unpulled)
    map)
  "Keymap for the `unpulled' section.")

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
             (?S "Stage all"       magit-stage-modified)
             (?t "Tagging"         magit-tag-popup)
             (?U "Reset Index"     magit-reset-index)
             (?v "Show Commit"     magit-show-commit)
             (?V "Show File"       magit-find-file)
             (?y "Show Refs"       magit-show-refs)
             (?Y "Cherry"          magit-cherry)
             (?z "Stashing"        magit-stash-popup)
             (?! "Running"         magit-run-popup)
             (?$ "Show Process"    magit-display-process)))

;;; Utilities
;;;; Various Utilities

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

;;;; Buffer Margins

(defun magit-set-buffer-margin (enable)
  (make-local-variable 'magit-log-show-margin)
  (let ((width (and enable
                    (if (and (derived-mode-p 'magit-log-mode)
                             (eq (car magit-refresh-args) 'long))
                        0 ; temporarily hide redundant margin
                      (car magit-log-margin-spec)))))
    (setq magit-log-show-margin width)
    (-when-let (window (get-buffer-window))
      (with-selected-window window
        (set-window-margins nil (car (window-margins)) width)
        (if enable
            (add-hook  'window-configuration-change-hook
                       'magit-set-buffer-margin-1 nil t)
          (remove-hook 'window-configuration-change-hook
                       'magit-set-buffer-margin-1 t))))))

(defun magit-set-buffer-margin-1 ()
  (-when-let (window (get-buffer-window))
    (with-selected-window window
      (set-window-margins nil (car (window-margins)) magit-log-show-margin))))

(defun magit-make-margin-overlay (&rest strings)
  (let ((o (make-overlay (point) (line-end-position) nil t)))
    (overlay-put o 'evaporate t)
    (overlay-put o 'before-string
                 (propertize "o" 'display
                             (list '(margin right-margin)
                                   (apply #'concat strings))))))

;;;; Section Jumpers

(magit-define-section-jumper stashes   "Stashes")
(magit-define-section-jumper untracked "Untracked files")
(magit-define-section-jumper unstaged  "Unstaged changes")
(magit-define-section-jumper staged    "Staged changes")
(magit-define-section-jumper unpulled  "Unpulled commits")
(magit-define-section-jumper unpushed  "Unpushed commits")
(magit-define-section-jumper diffstats "Diffstats")

;;;; Diff Utilities

(defun magit-diff-section-for-diffstat (section)
  (let ((file (magit-section-value section)))
    (--first (and (eq (magit-section-type it) 'file)
                  (string-equal (magit-section-value it) file))
             (magit-section-children magit-root-section))))

(defun magit-section-diff-header (section)
  (when (eq (magit-section-type section) 'hunk)
    (setq section (magit-section-parent section)))
  (when (eq (magit-section-type section) 'file)
    (let* ((file (magit-section-value section))
           (orig (or (magit-section-source section) file)))
      (format "diff --git a/%s b/%s\n--- a/%s\n+++ b/%s\n" orig file orig file))))

(defun magit-diff-scope (&optional section singular)
  (--when-let (or section (magit-current-section))
    (pcase (list (magit-section-type it) (use-region-p) singular)
      (`(hunk   t  ,_) 'region)
      (`(hunk nil  ,_) 'hunk)
      (`(file nil  ,_) 'file)
      (`(file   t   t) 'file)
      (`(file   t nil) 'files)
      (`(,(or `staged `unstaged `untracked) nil ,_) 'list)
      (`(,(or `staged `unstaged `untracked)   t ,_)
       (if (= (point) (1- (magit-section-end it)))
           (if singular 'file 'files)
         'list)))))

;;; Magit Api
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
  (magit-section-when process
    (let ((process (magit-section-value it)))
      (if (eq (process-status process) 'run)
          (when (magit-confirm 'kill-process "Kill this process")
            (kill-process process))
        (user-error "Process isn't running")))))

;;;;; Process Mode

(define-derived-mode magit-process-mode magit-mode "Magit Process"
  "Mode for looking at Git process output."
  :group 'magit-process)

(defun magit-process-buffer (&optional topdir)
  (or (magit-mode-get-buffer magit-process-buffer-name-format
                             'magit-process-mode topdir)
      (with-current-buffer (magit-mode-get-buffer-create
                            magit-process-buffer-name-format
                            'magit-process-mode topdir)
        (magit-process-mode)
        (let ((inhibit-read-only t))
          (make-local-variable 'text-property-default-nonsticky)
          (magit-insert-section (processbuf)
            (insert "\n")))
        (current-buffer))))

;;;;; Synchronous Processes

(defun magit-run-git (&rest args)
  "Call Git synchronously in a separate process, and refresh.

Option `magit-git-executable' specifies the Git executable and
option `magit-git-standard-options' specifies constant arguments.
The remaining arguments ARGS specify arguments to Git, they are
flattened before use.

After Git returns, the current buffer (if it is a Magit buffer)
as well as the current repository's status buffer are refreshed.
Unmodified buffers visiting files that are tracked in the current
repository are reverted if `magit-auto-revert-mode' is active.

Process output goes into a new section in a buffer specified by
variable `magit-process-buffer-name-format'."
  (magit-call-git args)
  (magit-refresh))

(defun magit-call-git (&rest args)
  "Call Git synchronously in a separate process.

Option `magit-git-executable' specifies the Git executable and
option `magit-git-standard-options' specifies constant arguments.
The remaining arguments ARGS specify arguments to Git, they are
flattened before use.

Process output goes into a new section in a buffer specified by
variable `magit-process-buffer-name-format'."
  (run-hooks 'magit-pre-call-git-hook)
  (apply #'magit-call-process magit-git-executable
         (magit-process-git-arguments args)))

(defun magit-call-process (program &rest args)
  "Call PROGRAM synchronously in a separate process.
Process output goes into a new section in a buffer specified by
variable `magit-process-buffer-name-format'."
  (cl-destructuring-bind (process-buf . section)
      (magit-process-setup program args)
    (magit-process-finish
     (let ((inhibit-read-only t))
       (apply #'process-file program nil process-buf nil args))
     process-buf (current-buffer) default-directory section)))

(defun magit-run-git-with-input (input &rest args)
  "Call Git in a separate process.
ARGS is flattened and then used as arguments to Git.

The first argument, INPUT, should be a buffer or the name of
an existing buffer.  The content of that buffer is used as the
process' standard input.  It may also be nil in which case the
current buffer is used.

Option `magit-git-executable' specifies the Git executable and
option `magit-git-standard-options' specifies constant arguments.
The remaining arguments ARGS specify arguments to Git, they are
flattened before use.

After Git returns, the current buffer (if it is a Magit buffer)
as well as the current repository's status buffer are refreshed.
Unmodified buffers visiting files that are tracked in the current
repository are reverted if `magit-auto-revert-mode' is active.
When INPUT is nil then do not refresh any buffers.

This function actually starts a asynchronous process, but it then
waits for that process to return."
  (declare (indent 1))
  (magit-start-git (or input (current-buffer)) args)
  (magit-process-wait)
  (when input (magit-refresh)))

(defun magit-run-git-with-logfile (file &rest args)
  "Call Git in a separate process and log its output to FILE.
See `magit-run-git' for more information.
This function might have a short halflive."
  (magit-start-git nil args)
  (process-put magit-this-process 'logfile file)
  (set-process-filter magit-this-process 'magit-process-logfile-filter)
  (magit-process-wait)
  (magit-refresh))

;;;;; Asynchronous Processes

(defvar magit-this-process nil)

(defun magit-run-git-with-editor (&rest args)
  (with-editor "GIT_EDITOR"
    (let ((magit-process-popup-time -1))
      (magit-run-git-async args))))

(defun magit-run-git-sequencer (&rest args)
  (magit-server-visit-args (if (symbolp (car args)) (pop args) 'sequencer))
  (magit-run-git-with-editor args))

(defun magit-run-git-async (&rest args)
  "Start Git, prepare for refresh, and return the process object.
ARGS is flattened and then used as arguments to Git.

Display the command line arguments in the echo area.

After Git returns some buffers are refreshed: the buffer that was
current when `magit-start-process' was called (if it is a Magit
buffer and still alive), as well as the respective Magit status
buffer.  Unmodified buffers visiting files that are tracked in
the current repository are reverted if `magit-auto-revert-mode'
is active.

See `magit-start-process' for more information."
  (message "Running %s %s" magit-git-executable
           (mapconcat 'identity (-flatten args) " "))
  (magit-start-git nil args))

(defun magit-start-git (input &rest args)
  "Start Git, prepare for refresh, and return the process object.

If INPUT is non-nil, it has to be a buffer or the name of an
existing buffer.  The buffer content becomes the processes
standard input.

Option `magit-git-executable' specifies the Git executable and
option `magit-git-standard-options' specifies constant arguments.
The remaining arguments ARGS specify arguments to Git, they are
flattened before use.

After Git returns some buffers are refreshed: the buffer that was
current when `magit-start-process' was called (if it is a Magit
buffer and still alive), as well as the respective Magit status
buffer.  Unmodified buffers visiting files that are tracked in
the current repository are reverted if `magit-auto-revert-mode'
is active.

See `magit-start-process' for more information."
  (run-hooks 'magit-pre-start-git-hook)
  (apply #'magit-start-process magit-git-executable input
         (magit-process-git-arguments args)))

(defun magit-start-process (program &optional input &rest args)
  "Start PROGRAM, prepare for refresh, and return the process object.

If optional argument INPUT is non-nil, it has to be a buffer or
the name of an existing buffer.  The buffer content becomes the
processes standard input.

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
  (cl-destructuring-bind (process-buf . section)
      (magit-process-setup program args)
    (let* ((process-connection-type
            ;; Don't use a pty, because it would set icrnl
            ;; which would modify the input (issue #20).
            (and (not input) magit-process-connection-type))
           (process (apply #'start-file-process
                           (file-name-nondirectory program)
                           process-buf program args)))
      (with-editor-set-process-filter process #'magit-process-filter)
      (set-process-sentinel process #'magit-process-sentinel)
      (set-process-buffer   process process-buf)
      (process-put process 'section section)
      (process-put process 'command-buf (current-buffer))
      (process-put process 'default-dir default-directory)
      (when inhibit-magit-refresh
        (process-put process 'inhibit-refresh t))
      (setf (magit-section-process section) process)
      (with-current-buffer process-buf
        (set-marker (process-mark process) (point)))
      (when input
        (with-current-buffer input
          (process-send-region process (point-min) (point-max))
          (process-send-eof    process)))
      (setq magit-this-process process)
      (setf (magit-section-value section) process)
      (magit-process-display-buffer process)
      process)))

;;;;; Process Internals

(defun magit-process-setup (program args)
  (magit-process-set-mode-line program args)
  (let ((buf (magit-process-buffer)))
    (if  buf
        (magit-process-truncate-log buf)
      (setq buf (magit-process-buffer)))
    (when (and args (equal program magit-git-executable))
      (setq args (-split-at (length magit-git-standard-options) args)))
    (with-current-buffer buf
      (goto-char (1- (point-max)))
      (let* ((inhibit-read-only t)
             (magit-insert-section--parent magit-root-section)
             (section (magit-insert-section (process)
                        (magit-insert-heading "run " program " "
                          (and args (format "%c " magit-ellipsis))
                          (mapconcat 'identity (cadr args) " "))
                        (insert "\n"))))
        (backward-char 1)
        (cons (current-buffer) section)))))

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
                   (delete-region (magit-section-start section)
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
    (--when-let (and (not (process-get process 'inhibit-refresh))
                     (process-get process 'command-buf))
      (when (buffer-live-p it)
        (with-current-buffer it
          (magit-refresh))))))

(defun magit-process-filter (proc string)
  "Default filter used by `magit-start-process'."
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (magit-process-yes-or-no-prompt proc string)
      (magit-process-username-prompt  proc string)
      (magit-process-password-prompt  proc string)
      (goto-char (process-mark proc))
      (setq string (propertize string 'magit-section
                               (process-get proc 'section)))
      ;; Find last ^M in string.  If one was found, ignore
      ;; everything before it and delete the current line.
      (let ((ret-pos (length string)))
        (while (and (>= (cl-decf ret-pos) 0)
                    (/= ?\r (aref string ret-pos))))
        (if (< ret-pos 0)
            (insert string)
          (delete-region (line-beginning-position) (point))
          (insert (substring string (1+ ret-pos)))))
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
  "Forward Yes-or-No prompts to the user."
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
  (let ((str (concat " " program (and args (concat " " (car args))))))
    (dolist (buf (magit-mode-get-buffers))
      (with-current-buffer buf (setq mode-line-process str)))))

(defun magit-process-unset-mode-line ()
  (dolist (buf (magit-mode-get-buffers))
    (with-current-buffer buf (setq mode-line-process nil))))

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
            (marker (magit-section-start section)))
        (goto-char marker)
        (save-excursion
          (delete-char 3)
          (set-marker-insertion-type marker nil)
          (insert (propertize (format "%3s" arg) 'magit-section section))
          (set-marker-insertion-type marker t)
          (magit-put-face-property (- (point) 3) (point)
                                   (if (= arg 0)
                                       'magit-process-ok
                                     'magit-process-ng)))
        (if (= (magit-section-end section)
               (+ (line-end-position) 2))
            (save-excursion
              (goto-char (1+ (line-end-position)))
              (delete-char -1)
              (setf (magit-section-content section) nil))
          (when (= arg 0)
            (magit-section-hide section))))))
  (unless (= arg 0)
    (message ; `error' would prevent refresh
     "%s ... [%s buffer %s for details]"
     (or (and (buffer-live-p process-buf)
              (with-current-buffer process-buf
                (save-excursion
                  (goto-char (magit-section-end section))
                  (--when-let (magit-section-content section)
                    (when (re-search-backward
                           magit-process-error-message-re nil it)
                      (match-string 1))))))
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
(add-hook 'with-editor-filter-visit-hook 'magit-server-visit t)

;;; Modes (1)
;;;; Commit Mode
;;;;; Commit Core

(define-derived-mode magit-commit-mode magit-mode "Magit"
  "Mode for looking at a Git commit.
This mode is documented in info node `(magit)Commit Buffer'.

\\<magit-commit-mode-map>\
Type \\[magit-section-toggle] to expand or hide the section at point.
Type \\[magit-visit-file] to visit the hunk or file at point.
Type \\[magit-apply] to apply the change at point to the worktree.
Type \\[magit-reverse] to reverse the change at point in the worktree.
\n\\{magit-commit-mode-map}"
  :group 'magit-commit)

;;;###autoload
(defun magit-show-commit (commit &optional noselect module)
  "Show the commit at point.
If there is no commit at point or with a prefix argument prompt
for a commit."
  (interactive
   (let* ((mcommit (magit-section-when mcommit))
          (atpoint (or (and magit-blame-mode (magit-blame-chunk-get :hash))
                       mcommit (magit-branch-or-commit-at-point))))
     (list (or (and (not current-prefix-arg) atpoint)
               (magit-read-rev "Show commit" atpoint))
           nil (and mcommit (magit-section-parent-value
                             (magit-current-section))))))
  (let ((default-directory (if module
                               (file-name-as-directory
                                (expand-file-name module (magit-get-top-dir)))
                             default-directory)))
    (when (magit-git-failure "cat-file" "commit" commit)
      (user-error "%s is not a commit" commit))
    (magit-mode-setup magit-commit-buffer-name-format
                      (if noselect 'display-buffer 'pop-to-buffer)
                      #'magit-commit-mode
                      #'magit-refresh-commit-buffer
                      commit)))

(defun magit-show-or-scroll-up ()
  "Update the commit or diff buffer for the thing at point.

Either show the commit or stash at point in the appropriate
buffer, or if that buffer is already being displayed in the
current frame and contains information about that commit or
stash, then instead scroll the buffer up.  If there is no
commit or stash at point, then prompt for a commit."
  (interactive)
  (magit-show-or-scroll 'scroll-up))

(defun magit-show-or-scroll-down ()
  "Update the commit or diff buffer for the thing at point.

Either show the commit or stash at point in the appropriate
buffer, or if that buffer is already being displayed in the
current frame and contains information about that commit or
stash, then instead scroll the buffer down.  If there is no
commit or stash at point, then prompt for a commit."
  (interactive)
  (magit-show-or-scroll 'scroll-down))

(defun magit-show-or-scroll (fn)
  (let (rev cmd buf win)
    (if magit-blame-mode
        (setq rev (magit-blame-chunk-get :hash)
              cmd 'magit-show-commit
              buf (magit-mode-get-buffer
                   magit-commit-buffer-name-format 'magit-commit-mode))
      (magit-section-case
        (commit (setq rev (magit-section-value it)
                      cmd 'magit-show-commit
                      buf (magit-mode-get-buffer
                           magit-commit-buffer-name-format 'magit-commit-mode)))
        (stash  (setq rev (magit-section-value it)
                      cmd 'magit-diff-stash
                      buf (magit-mode-get-buffer
                           magit-diff-buffer-name-format 'magit-diff-mode)))))
    (if rev
        (if (and buf
                 (setq win (get-buffer-window buf))
                 (with-current-buffer buf
                   (equal (if (eq cmd 'magit-diff-stash)
                              (concat rev "^2^.." rev)
                            rev)
                          (car magit-refresh-args))))
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
  (magit-insert-section (commitbuf)
    (magit-git-wash #'magit-wash-commit
      "log" "-1" "-p" "--cc" "--no-prefix"
      (and magit-commit-show-diffstat "--stat")
      magit-diff-options
      magit-diff-extra-options
      magit-commit-extra-options
      commit)))

;;;;; Commit Washing

(defun magit-wash-commit (args)
  (looking-at "^commit \\([a-z0-9]+\\)\\(?: \\(.+\\)\\)?$")
  (magit-bind-match-strings (rev refs) nil
    (magit-delete-line)
    (magit-insert-section (headers)
      (magit-insert-heading
        (and refs (concat (magit-format-ref-labels refs) " "))
        (propertize rev 'face 'magit-hash))
      (while (re-search-forward "^\\([a-z]+\\): +\\(.+\\)$" nil t)
        (magit-bind-match-strings (keyword revs) nil
          (when (string-match-p keyword "Merge")
            (magit-delete-match 2)
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
    (magit-delete-line)
    (magit-insert-section (message)
      (insert summary ?\n)
      (magit-insert-heading)
      (cond ((re-search-forward "^---" bound t)
             (magit-delete-match))
            ((re-search-forward "^.[^ ]" bound t)
             (goto-char (1- (match-beginning 0)))))))
  (forward-line)
  (magit-diff-wash-diffs args))

(defun magit-insert-commit-button (hash)
  (magit-insert-section (commit hash)
    (insert-text-button hash
                        'help-echo "Visit commit"
                        'action (lambda (button)
                                  (save-excursion
                                    (goto-char button)
                                    (call-interactively #'magit-show-commit)))
                        'follow-link t
                        'mouse-face 'magit-section-highlight
                        'face 'magit-hash)))

;;;; Status Mode

(define-derived-mode magit-status-mode magit-mode "Magit"
  "Mode for looking at Git status.
This mode is documented in info node `(magit)Status'.

\\<magit-status-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-dispatch-popup] to see available action popups.
Type \\[magit-section-toggle] to expand or hide the section at point.
Type \\[magit-visit-file] to visit the thing at point.
Type \\[magit-stage] to stage the change at point; \\[magit-unstage] to unstage.
Type \\[magit-commit-popup] to create a commit.
\n\\{magit-status-mode-map}"
  :group 'magit-status)

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
                          (> (prefix-numeric-value current-prefix-arg) 4))
                       (or (magit-get-top-dir)
                           (magit-read-top-dir nil)))))
  (-when-let (default-directory
              (or (magit-get-top-dir dir)
                  (and (yes-or-no-p
                        (format "No repository in %s.  Create one? " dir))
                       (progn (magit-init dir)
                              (magit-get-top-dir dir)))))
    (magit-mode-setup magit-status-buffer-name-format
                      (or switch-function
                          magit-status-buffer-switch-function)
                      #'magit-status-mode
                      #'magit-refresh-status)))

(defun magit-refresh-status ()
  (magit-git-exit-code "update-index" "--refresh")
  (magit-insert-section (status)
    (run-hooks 'magit-status-sections-hook))
  (run-hooks 'magit-refresh-status-hook))

;;;; Status Sections

(defun magit-insert-stashes ()
  ;; #1427 Set log.date to work around an issue in Git <1.7.10.3.
  (--when-let (magit-git-lines "-c" "log.date=default" "stash" "list")
    (magit-insert-section (stashes)
      (magit-insert-heading "Stashes:")
      (dolist (stash it)
        (string-match "^\\(stash@{\\([0-9]+\\)}\\): \\(.+\\)$" stash)
        (magit-bind-match-strings (stash number message) stash
          (magit-insert-section (stash stash)
            (insert (propertize (format "stash@{%s}" number) 'face 'magit-hash)
                    " " message "\n"))))
      (insert "\n"))))

(defun magit-insert-untracked-files ()
  (--when-let (--mapcat (and (eq (aref it 0) ??) (list it))
                        (magit-git-lines "status" "--porcelain"))
    (magit-insert-section (untracked)
      (magit-insert-heading "Untracked files:")
      (dolist (file it)
        (setq file (magit-decode-git-path (substring file 3)))
        (magit-insert-section (file file)
          (insert "\t" file "\n")))
      (insert "\n"))))

(defun magit-insert-unstaged-changes ()
  (magit-insert-section (unstaged)
    (magit-insert-heading "Unstaged changes:")
    (magit-git-wash #'magit-diff-wash-diffs
      "diff" magit-diff-options magit-diff-extra-options)))

(defun magit-insert-staged-changes ()
  (magit-insert-section (staged)
    (magit-insert-heading "Staged changes:")
    (magit-git-wash #'magit-diff-wash-diffs
      "diff" "--cached" magit-diff-options magit-diff-extra-options)))

(defun magit-insert-unpulled-or-recent-commits ()
  (let ((tracked (magit-get-tracked-branch nil t)))
    (if (and tracked (not (equal (magit-rev-parse "HEAD")
                                 (magit-rev-parse tracked))))
        (magit-insert-unpulled-commits)
      (magit-insert-recent-commits))))

(defun magit-insert-recent-commits ()
  (magit-insert-section (recent)
    (magit-insert-heading "Recent commits:")
    (magit-insert-log nil (cons (format "-%d" magit-log-section-commit-count)
                                magit-log-section-args))))

(defun magit-insert-unpulled-commits ()
  (-when-let (tracked (magit-get-tracked-branch nil t))
    (magit-insert-section (unpulled)
      (magit-insert-heading "Unpulled commits:")
      (magit-insert-log (concat "HEAD.." tracked) magit-log-section-args))))

(defun magit-insert-unpushed-commits ()
  (-when-let (tracked (magit-get-tracked-branch nil t))
    (magit-insert-section (unpushed)
      (magit-insert-heading "Unpushed commits:")
      (magit-insert-log (concat tracked "..HEAD") magit-log-section-args))))

(defun magit-insert-unpulled-cherries ()
  (-when-let (tracked (magit-get-tracked-branch nil t))
    (magit-insert-section (unpulled)
      (magit-insert-heading "Unpulled commits:")
      (magit-git-wash (apply-partially 'magit-log-wash-log 'cherry)
        "cherry" "-v" (magit-abbrev-arg) (magit-get-current-branch) tracked))))

(defun magit-insert-unpushed-cherries ()
  (-when-let (tracked (magit-get-tracked-branch nil t))
    (magit-insert-section (unpushed)
      (magit-insert-heading "Unpushed commits:")
      (magit-git-wash (apply-partially 'magit-log-wash-log 'cherry)
        "cherry" "-v" (magit-abbrev-arg) tracked))))

(defun magit-insert-branch-description ()
  (let ((branch (magit-get-current-branch)))
    (--when-let (magit-git-lines
                 "config" (format "branch.%s.description" branch))
      (magit-insert-section (branchdesc branch t)
        (magit-insert-heading branch ": " (car it))
        (insert (mapconcat 'identity (cdr it) "\n"))
        (insert "\n\n")))))

(defun magit-insert-status-headers (&optional branch upstream)
  (unless branch
    (setq branch (magit-get-current-branch)))
  (-if-let  (hash (magit-rev-parse "--verify" "HEAD"))
      (let ((line (magit-rev-format "%h %s" "HEAD")))
        (string-match "^\\([^ ]+\\) \\(.+\\)" line)
        (magit-bind-match-strings (hash msg) line
          (magit-insert-section (branch (or branch hash))
            (magit-insert-heading
              (magit-string-pad "Head: " 10)
              (propertize hash 'face 'magit-hash) " "
              (if branch
                  (propertize branch 'face 'magit-branch-local)
                (propertize "HEAD" 'face 'magit-head))
              " " msg "\n")
            (when (or upstream (setq upstream (magit-get-tracked-branch branch)))
              (setq line (or (magit-rev-format "%h %s" upstream) ""))
              (string-match "^\\([^ ]+\\) \\(.+\\)" line)
              (magit-bind-match-strings (hash msg) line
                (magit-insert-section (branch upstream)
                  (magit-insert
                   (concat
                    (magit-string-pad "Upstream: " 10)
                    (if hash (propertize hash 'face 'magit-hash) "missing") " "
                    (and (magit-get-boolean "branch" branch "rebase") "onto ")
                    (propertize
                     upstream 'face
                     (if (string= (magit-get "branch" branch "remote") ".")
                         'magit-branch-local
                       'magit-branch-remote))
                    " " msg "\n")))))
            (run-hooks 'magit-status-headers-hook)))
        ;; This belongs to no section but `magit-root-section'.
        (insert "\n"))
    (insert "In the beginning there was darkness\n\n")))

(defun magit-insert-status-tags-line ()
  (let* ((current-tag (magit-get-current-tag t))
         (next-tag (magit-get-next-tag t))
         (both-tags (and current-tag next-tag t)))
    (when (or current-tag next-tag)
      (magit-insert-section (tag (or current-tag next-tag))
        (magit-insert
         (concat
          (magit-string-pad (if both-tags "Tags: " "Tag: ") 10)
          (and current-tag (magit-format-status-tag-sentence
                            (car current-tag) (cadr current-tag) nil))
          (and both-tags ", ")
          (and next-tag (magit-format-status-tag-sentence
                         (car next-tag) (cadr next-tag) t))
          "\n"))))))

(defun magit-format-status-tag-sentence (tag count next)
  (concat (propertize tag 'face 'magit-tag)
          (and (> count 0)
               (format " (%s)"
                       (propertize (format "%s" count) 'face
                                   (if next 'magit-tag 'magit-branch-local))))))

;;; Porcelain
;;;; Apply
;;;;; Stage

(defun magit-stage ()
  "Add the change at point to the staging area."
  (interactive)
  (--when-let (magit-current-section)
    (pcase (list (magit-diff-type) (magit-diff-scope))
      (`(untracked     ,_) (magit-stage-untracked))
      (`(unstaged  region) (magit-apply-region it "--cached"))
      (`(unstaged    hunk) (magit-apply-hunk   it "--cached"))
      (`(unstaged    file) (magit-run-git "add" "-u" "--"
                                          (magit-section-value it)))
      (`(unstaged   files) (magit-run-git "add" "-u" "--"
                                          (magit-section-region-siblings
                                           #'magit-section-value)))
      (`(unstaged    list) (magit-stage-modified))
      (`(staged        ,_) (user-error "Already staged"))
      (`(committed     ,_) (user-error "Cannot stage committed changes"))
      (`(undefined     ,_) (user-error "Cannot stage this change")))))

;;;###autoload
(defun magit-stage-file (file)
  "Stage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be staged.  Otherwise stage the file at point without
requiring confirmation."
  (interactive
   (let* ((atpoint (magit-section-when (file)))
          (current (magit-file-relative-name))
          (choices (nconc (magit-modified-files)
                          (magit-untracked-files)))
          (default (car (member (or atpoint current) choices))))
     (list (if (or current-prefix-arg (not default))
               (magit-completing-read "Stage file" choices
                                      nil t nil nil default)
             default))))
  (magit-run-git "add" file))

;;;###autoload
(defun magit-stage-modified (&optional all)
  "Stage all changes to files modified in the worktree.
Stage all new content of tracked files and remove tracked files
that no longer exist in the working tree from the index also.
With a prefix argument also stage previously untracked (but not
ignored) files.
\('git add --update|--all .')."
  (interactive
   (unless (or (not (magit-anything-staged-p))
               (magit-confirm 'stage-all "Stage all changes"))
     (user-error "Abort")))
  (magit-run-git "add" (if all "--all" "--update") "."))

(defun magit-stage-untracked ()
  (let ((section (magit-current-section)) files repos)
    (dolist (file (pcase (magit-diff-scope)
                    (`file  (list (magit-section-value section)))
                    (`files (magit-section-region-siblings 'magit-section-value))
                    (`list  (magit-untracked-files))))
      (if (magit-git-repo-p file t)
          (push file repos)
        (push file files)))
    (when files
      (magit-run-git "add" "--" files))
    (dolist (repo repos)
      (save-excursion
        (goto-char (magit-section-start repo))
        (call-interactively 'magit-submodule-add)))))

;;;;; Unstage

(defun magit-unstage ()
  "Remove the change at point from the staging area."
  (interactive)
  (--when-let (magit-current-section)
    (pcase (list (magit-diff-type) (magit-diff-scope))
      (`(untracked     ,_) (user-error "Cannot unstage untracked changes"))
      (`(unstaged      ,_) (user-error "Already unstaged"))
      (`(staged    region) (magit-apply-region it "--reverse" "--cached"))
      (`(staged      hunk) (magit-apply-hunk   it "--reverse" "--cached"))
      (`(staged      file) (magit-unstage-1 (magit-section-value it)))
      (`(staged     files) (magit-unstage-1 (magit-section-region-siblings
                                             #'magit-section-value)))
      (`(staged      list) (when (or (and (not (magit-anything-unstaged-p))
                                          (not (magit-untracked-files)))
                                     (magit-confirm 'unstage-all
                                                    "Unstage all changes"))
                             (magit-run-git "reset" "HEAD" "--")))
      (`(committed     ,_) (user-error "Cannot unstage committed changes"))
      (`(undefined     ,_) (user-error "Cannot unstage this change")))))

;;;###autoload
(defun magit-unstage-file (file)
  "Unstage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be unstaged.  Otherwise unstage the file at point
without requiring confirmation."
  (interactive
   (let* ((atpoint (magit-section-when (file)))
          (current (magit-file-relative-name))
          (choices (magit-staged-files))
          (default (car (member (or atpoint current) choices))))
     (list (if (or current-prefix-arg (not default))
               (magit-completing-read "Unstage file" choices
                                      nil t nil nil default)
             default))))
  (magit-unstage-1 file))

(defun magit-unstage-1 (args)
  (if (magit-no-commit-p)
      (magit-run-git "rm" "--cached" "--" args)
    (magit-run-git "reset" "HEAD" "--" args)))

;;;;; Discard

(defun magit-discard ()
  "Remove the change at point."
  (interactive)
  (--when-let (magit-current-section)
    (pcase (list (magit-diff-type) (magit-diff-scope))
      (`(committed ,_) (user-error "Cannot discard committed changes"))
      (`(undefined ,_) (user-error "Cannot discard this change"))
      (`(,_      list) (magit-discard-files (magit-section-children it)))
      (`(,_     files) (magit-discard-files (magit-section-region-siblings)))
      (`(,_      file) (magit-discard-files (list it)))
      (_               (magit-discard-apply it)))))

(defun magit-discard-apply (section)
  (let* ((type  (magit-diff-type  section))
         (scope (magit-diff-scope section t))
         (fn    (pcase scope
                  (`region 'magit-apply-region)
                  (`hunk   'magit-apply-hunk)
                  (`file   'magit-apply-diff))))
    (when (or (eq scope 'file)
              (magit-confirm 'discard (format "Discard %s" scope)))
      (if (eq type 'unstaged)
          (funcall fn section "--reverse")
        (if (magit-anything-unstaged-p
             (if (eq scope 'file)
                 (magit-section-value section)
               (magit-section-parent-value section)))
            (progn
              (let ((inhibit-magit-refresh t))
                (funcall fn section "--reverse" "--cached")
                (funcall fn section "--reverse"))
              (magit-refresh))
          (funcall fn section "--reverse" "--index"))))))

(defun magit-discard-files (sections)
  (let ((auto-revert-verbose nil)
        (inhibit-magit-refresh t)
        (status (magit-file-status))
        delete resurrect rename discard resolve)
    (dolist (section sections)
      (let ((file (magit-section-value section)))
        (pcase (cons (pcase (magit-diff-type section)
                       (`staged ?X)
                       (`unstaged ?Y)
                       (`untracked ?Z))
                     (magit-file-status file status))
          ((or `(?Z ?? ??) `(?Z ?! ?!)) (push file delete))
          ((or `(?Z ?D ? ) `(,_ ?D ?D)) (push file delete))
          ((or `(,_ ?U ,_) `(,_ ,_ ?U)) (push file resolve))
          (`(,_ ?A ?A)                  (push file resolve))
          (`(?X ?M ,(or ?  ?M ?D)) (push section discard))
          (`(?Y ,_         ?M    ) (push section discard))
          (`(?X ?A ,(or ?  ?M ?D)) (push file delete))
          (`(?X ?C ,(or ?  ?M ?D)) (push file delete))
          (`(?X ?D ,(or ?  ?M   )) (push file resurrect))
          (`(?Y ,_            ?D ) (push file resurrect))
          (`(?X ?R ,(or ?  ?M ?D)) (push section rename)))))
    (magit-discard-files--resurrect (nreverse resurrect))
    (magit-discard-files--delete    (nreverse delete))
    (magit-discard-files--rename    (nreverse rename))
    (magit-discard-files--discard   (nreverse discard))
    (when resolve
      (let ((inhibit-magit-refresh t))
        (dolist (file (nreverse resolve))
          (magit-checkout-stage file (magit-checkout-read-stage file))))))
  (magit-refresh))

(defun magit-discard-files--resurrect (files)
  (when (magit-confirm 'resurrect "Resurrect" files)
    (if (eq (magit-diff-type) 'staged)
        (magit-call-git "reset"  "--" files)
      (magit-call-git "checkout" "--" files))))

(defun magit-discard-files--delete (files)
  (when (if magit-delete-by-moving-to-trash
            (magit-confirm 'trash "Trash" files)
          (magit-confirm 'delete "Delete" files))
    (let ((delete-by-moving-to-trash magit-delete-by-moving-to-trash)
          (status (magit-file-status)))
      (dolist (file files)
        (if (memq (magit-diff-type) '(unstaged untracked))
            (delete-file file magit-delete-by-moving-to-trash)
          (pcase (nth 2 (assoc file status))
            (?  (delete-file file t)
                (magit-call-git "rm" "--cached" "--" file))
            (?M (let ((temp (magit-git-string "checkout-index" "--temp" file)))
                  (string-match
                   (format "\\(.+?\\)\t%s" (regexp-quote file)) temp)
                  (rename-file (match-string 1 temp)
                               (setq temp (concat file ".~{index}~")))
                  (delete-file temp t))
                (magit-call-git "rm" "--cached" "--force" "--" file))
            (?D (magit-call-git "checkout" "--" file)
                (delete-file file t)
                (magit-call-git "rm" "--cached" "--force" "--" file))))))))

(defun magit-discard-files--rename (sections)
  (when (magit-confirm 'rename
                       (if (= (length sections) 1)
                           "Undo rename"
                         "Undo renames")
                       (--map (format "%s -> %s"
                                      (magit-section-source it)
                                      (magit-section-value it))
                              sections))
    (dolist (section sections)
      (let ((file (magit-section-value section))
            (orig (magit-section-source section)))
        (if (file-exists-p file)
            (magit-call-git "mv" file orig)
          (magit-call-git "rm" "--cached" "--" file)
          (magit-call-git "reset" "--" orig))))))

(defun magit-discard-files--discard (sections)
  (when (magit-confirm 'discard
                       (format "Discard %s changes to" (magit-diff-type))
                       (mapcar 'magit-section-value sections))
    (mapc 'magit-discard-apply sections)))

;;;;; Reverse

(defun magit-reverse ()
  "Reverse the change at point in the working tree."
  (interactive)
  (--when-let (magit-current-section)
    (pcase (list (magit-diff-type) (magit-diff-scope))
      (`(untracked ,_) (user-error "Cannot reverse untracked changes"))
      (`(unstaged  ,_) (user-error "Cannot reverse unstaged changes"))
      (`(,_      list) (magit-reverse-files (magit-section-children it)))
      (`(,_     files) (magit-reverse-files (magit-section-region-siblings)))
      (`(,_      file) (magit-reverse-files (list it)))
      (_               (magit-reverse-apply it)))))

(defun magit-reverse-apply (section)
  (let ((scope (magit-diff-scope section t)))
    (when (or (eq scope 'file)
              (magit-confirm 'reverse (format "Reverse %s" scope)))
      (funcall (pcase scope
                 (`region 'magit-apply-region)
                 (`hunk   'magit-apply-hunk)
                 (`file   'magit-apply-diff))
               section "--reverse"))))

(defun magit-reverse-files (sections)
  (when (magit-confirm
         'reverse "Reverse" (mapcar 'magit-section-value sections))
    (mapc 'magit-reverse-apply sections)))

;;;;; Apply

(defun magit-apply ()
  "Apply the change at point."
  (interactive)
  (--when-let (magit-current-section)
    (pcase (list (magit-diff-type) (magit-diff-scope))
      (`(,(or `unstaged `staged) ,_)
       (user-error "Change is already in the working tree"))
      (`(,_ region) (magit-apply-region it))
      (`(,_   hunk) (magit-apply-hunk it))
      (`(,_   file) (magit-apply-diff it)))))

(defun magit-apply-diff (section &rest args)
  (when (member "-U0" magit-diff-options)
    (setq args (cons "--unidiff-zero" args)))
  (magit-apply-patch (concat (magit-section-diff-header section)
                             (buffer-substring (magit-section-content section)
                                               (magit-section-end section)))
                     args))

(defun magit-apply-hunk (section &rest args)
  (when (string-match "^diff --cc" (magit-section-parent-value section))
    (user-error "Cannot un-/stage resolution hunks.  Stage the whole file"))
  (when (member "-U0" magit-diff-options)
    (setq args (cons "--unidiff-zero" args)))
  (magit-apply-patch (concat (magit-section-diff-header section)
                             (buffer-substring (magit-section-start section)
                                               (magit-section-end section)))
                     args))

(defun magit-apply-region (section &rest args)
  (when (member "-U0" magit-diff-options)
    (user-error "Not enough context to apply region.  Increase the context"))
  (when (string-match "^diff --cc" (magit-section-parent-value section))
    (user-error "Cannot un-/stage resolution hunks.  Stage the whole file"))
  (let ((op (if (member "--reverse" args) "+" "-"))
        (rbeg (region-beginning))
        (rend (region-end))
        (sbeg (magit-section-start section))
        (send (magit-section-end section))
        (patch (list (magit-section-diff-header section))))
    (save-excursion
      (goto-char sbeg)
      (while (< (point) send)
        (looking-at "\\(.\\)\\([^\n]*\n\\)")
        (cond ((or (string-match-p "[@ ]" (match-string 1))
                   (and (>= (point) rbeg)
                        (<  (point) rend)))
               (push (match-string 0) patch))
              ((equal op (match-string 1))
               (push (concat " " (match-string 2)) patch)))
        (forward-line)))
    (with-temp-buffer
      (insert (mapconcat 'identity (reverse patch) ""))
      (diff-fixup-modifs (point-min) (point-max))
      (setq patch (buffer-string)))
    (magit-apply-patch patch args)))

(defun magit-apply-patch (patch args)
  (when magit-apply-backup
    (magit-apply-backup patch args))
  (with-temp-buffer
    (insert patch)
    (magit-run-git-with-input nil
      "apply" args "--ignore-space-change" "-"))
  (magit-refresh))

(defconst magit-apply-backup-file "magit/apply.diff")

(defun magit-apply-backup (patch args)
  (with-temp-buffer
    (insert "git apply " (mapconcat 'identity args " ") "\n---\n" patch)
    (let ((buffer-file-name (magit-git-dir magit-apply-backup-file))
          (make-backup-files t)
          (backup-directory-alist nil)
          (version-control t)
          (kept-old-versions 0)
          (kept-new-versions 10))
      (make-directory (file-name-directory buffer-file-name) t)
      (save-buffer 16))))

(defun magit-apply-undo ()
  "Apply the previously applied diff in reverse.

WARNING: Backups are only created when Magit used `git apply'.
That is not sufficient as not every action that might destroy
uncommitted changes eventually uses that Git command.  Don't rely
on this.  Reminder: you are using Magit's `next' branch and are
therefor using a development version.

Also see variable `magit-apply-backup'."
  (interactive)
  (let ((file (magit-git-dir magit-apply-backup-file)))
    (if (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (if (looking-at "^git apply \\(.+\\)")
              (let ((args (split-string (match-string 1))))
                (magit-apply-patch (buffer-string)
                                   (if (member "--reverse" args)
                                       (remove "--reverse" args)
                                     (cons "--reverse" args))))
            (error "Cannot undo patch")))
      (user-error "No backups exist"))))

;;;; Visit

;;;###autoload
(defun magit-find-file (rev file)
  (interactive (magit-find-file-read-args "Find file"))
  (switch-to-buffer (magit-find-file-noselect rev file)))

;;;###autoload
(defun magit-find-file-other-window (rev file)
  (interactive (magit-find-file-read-args "Find file in other window"))
  (switch-to-buffer-other-window (magit-find-file-noselect rev file)))

(defun magit-find-file-read-args (prompt)
  (let ((rev (magit-read-rev "Find file from revision"
                             (or (magit-branch-or-commit-at-point)
                                 (magit-get-current-branch)))))
    (list rev (magit-read-file-from-rev rev prompt))))

(defun magit-get-revision-buffer (rev file &optional create)
  (funcall (if create 'get-buffer-create 'get-buffer)
           (format "%s.~%s~" file (subst-char-in-string ?/ ?_ rev))))

(defun magit-get-revision-buffer-create (rev file)
  (magit-get-revision-buffer rev file t))

(defun magit-find-file-noselect (rev file)
  (with-current-buffer (magit-get-revision-buffer-create rev file)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (magit-git-insert "cat-file" "-p" (concat rev ":" file)))
    (setq magit-buffer-revision  (magit-rev-format "%H" rev)
          magit-buffer-refname   rev
          magit-buffer-file-name (expand-file-name file (magit-get-top-dir)))
    (let ((buffer-file-name magit-buffer-file-name))
      (normal-mode t))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (run-hooks 'magit-find-file-hook)
    (current-buffer)))

(defun magit-find-file-index-noselect (file)
  (let* ((bufname (concat file ".~{index}~"))
         (origbuf (get-buffer bufname)))
    (with-current-buffer (get-buffer-create bufname)
      (when (or (not origbuf)
                (y-or-n-p (format "%s already exists; revert it? " bufname)))
        (let ((inhibit-read-only t)
              (temp (car (split-string
                          (magit-git-string "checkout-index" "--temp" file)
                          "\t"))))
          (erase-buffer)
          (insert-file-contents temp nil nil nil t)
          (delete-file temp)))
      (setq magit-buffer-revision  "{index}"
            magit-buffer-refname   "{index}"
            magit-buffer-file-name (expand-file-name file (magit-get-top-dir)))
      (let ((buffer-file-name magit-buffer-file-name))
        (normal-mode t))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (run-hooks 'magit-find-index-hook)
      (current-buffer))))

(defun magit-update-index ()
  (interactive)
  (let ((file (magit-file-relative-name)))
    (unless (equal magit-buffer-refname "{index}")
      (user-error "%s isn't visiting the index" file))
    (unless (y-or-n-p (format "Update index with contents of %s" (buffer-name)))
      (user-error "Abort"))
    (let ((index (make-temp-file "index"))
          (buffer (current-buffer)))
      (with-temp-file index
        (insert-buffer-substring buffer))
      (magit-call-git "update-index" "--cacheinfo"
                      (substring (magit-git-string "ls-files" "-s" file) 0 6)
                      (magit-git-string "hash-object" "-t" "blob" "-w"
                                        (concat "--path=" file)
                                        "--" index)
                      file))
    (set-buffer-modified-p nil))
  (--when-let (magit-mode-get-buffer
               magit-status-buffer-name-format 'magit-status-mode)
    (with-current-buffer it (magit-refresh)))
  t)

(defun magit-visit-file (file &optional other-window)
  (interactive (list (magit-file-at-point) current-prefix-arg))
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
    (let ((pos (magit-section-when hunk
                 (magit-hunk-file-position it)))
          (buffer (or (get-file-buffer file)
                      (find-file-noselect file))))
      (if (or other-window (get-buffer-window buffer))
          (pop-to-buffer buffer)
        (switch-to-buffer buffer))
      (when pos
        (goto-char (point-min))
        (forward-line (1- (car pos)))
        (move-to-column (cdr pos))))
    (when (magit-anything-unmerged-p file)
      (smerge-start-session))))

(defun magit-hunk-file-position (section)
  (let* ((value (magit-section-value section))
         (hunk-line (line-number-at-pos (point)))
         (goto-line (car (last value)))
         (offset (- (length value) 2))
         (column (current-column)))
    (save-excursion
      (string-match "^\\+\\([0-9]+\\)" goto-line)
      (setq goto-line (string-to-number (match-string 1 goto-line)))
      (goto-char (magit-section-content section))
      (while (< (line-number-at-pos) hunk-line)
        (unless (string-match-p
                 "-" (buffer-substring (point) (+ (point) offset)))
          (cl-incf goto-line))
        (forward-line))
      (cons goto-line (if (looking-at "-") 0 (max 0 (- column offset)))))))

;;;###autoload
(defun magit-dired-jump (&optional other-window)
  "Visit file at point using Dired.
With a prefix argument, visit in other window.  If there
is no file at point then instead visit `default-directory'."
  (interactive "P")
  (dired-jump other-window
              (file-truename (or (magit-file-at-point) default-directory))))

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
       (list dir))))
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
  :sequence-actions   '((?c "Commit merge" magit-commit)
                        (?a "Abort merge"  magit-merge-abort))
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
      (when (magit-confirm 'abort-merge "Abort merge")
        (magit-run-git-async "merge" "--abort"))
    (user-error "No merge in progress")))

(defun magit-checkout-stage (file arg &optional restore-conflict)
  "During a conflict checkout and stage side, or restore conflict."
  (interactive
   (let ((default-directory (magit-get-top-dir))
         (file (magit-completing-read "Checkout file"
                                      (magit-tracked-files) nil nil nil
                                      'magit-read-file-hist
                                      (magit-current-file))))
     (cond
      ((member file (magit-unmerged-files))
       (list file (magit-checkout-read-stage file)))
      ((yes-or-no-p (format "Restore conflicts in %s? " file))
       (list file "--merge" t))
      (t
       (user-error "Quit")))))
  (if restore-conflict
      (progn
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
          (magit-run-git-with-input nil "checkout" arg file))
        (magit-refresh))
    (magit-call-git "checkout" arg file)
    (if (string= arg "--merge")
        (magit-refresh)
      (magit-run-git "add" file))))

(defun magit-merge-state ()
  (file-exists-p (magit-git-dir "MERGE_HEAD")))

(defun magit-merge-assert ()
  (or (not (magit-anything-modified-p))
      (not magit-merge-warn-dirty-worktree)
      (magit-confirm
       'merge-dirty
       "Running merge in a dirty worktree could cause data loss.  Continue")
      (user-error "Abort")))

(defun magit-merge-read-rev ()
  (magit-read-rev "Merge"
                  (or (magit-branch-or-commit-at-point)
                      (magit-get-previous-branch))))

(defun magit-checkout-read-stage (file)
  (magit-read-char-case (format "For %s checkout: " file) t
    (?o "[o]ur stage"   "--ours")
    (?t "[t]heir stage" "--theirs")
    (?c "[c]onflict"    "--merge")))

(defun magit-insert-merge-log ()
  (-when-let (heads (mapcar 'magit-get-shortname
                            (magit-file-lines (magit-git-dir "MERGE_HEAD"))))
    (magit-insert-section (commit (car heads))
      (magit-insert-heading
        (format "Merging %s:" (mapconcat 'identity heads ", ")))
      (magit-insert-log
       (concat (magit-git-string "merge-base" "--octopus" "HEAD" (car heads))
               ".." (car heads))
       (let ((args magit-log-section-args))
         (unless (member "--decorate=full" magit-log-section-args)
           (push "--decorate=full" args))
         args)))))

;;;;; Branching

(magit-define-popup magit-branch-popup
  "Popup console for branch commands."
  'magit-popups
  :man-page "git-branch"
  :switches '((?t "Set upstream configuration" "--track"))
  :actions  '((?b "Checkout"          magit-checkout)
              (?c "Create"            magit-branch)
              (?B "Create & Checkout" magit-branch-and-checkout)
              (?u "Set upstream"      magit-branch-set-upstream)
              (?r "Rename"            magit-branch-rename)
              (?k "Delete"            magit-branch-delete))
  :default-arguments '("--track")
  :default-action 'magit-checkout)

;;;###autoload
(defun magit-checkout (revision)
  "Checkout REVISION, updating the index and the working tree.
If REVISION is a local branch then that becomes the current
branch.  If it is something else then `HEAD' becomes detached.
Checkout fails if the working tree or the staging area contain
changes.
\n(git checkout REVISION)."
  (interactive
   (list (let ((current (magit-get-current-branch))
               (default (or (magit-branch-or-commit-at-point)
                            (magit-get-previous-branch))))
           (magit-read-rev "Checkout"
                           (unless (equal default current) default)
                           current))))
  (magit-run-git "checkout" revision))

(defun magit-branch (branch start-point &optional args)
  "Create BRANCH at branch or revision START-POINT.
\n(git branch [ARGS] BRANCH START-POINT)."
  (interactive (magit-branch-read-args "Create branch"))
  (magit-run-git "branch" args branch start-point))

;;;###autoload
(defun magit-branch-and-checkout (branch start-point &optional args)
  "Create and checkout BRANCH at branch or revision START-POINT.
\n(git checkout [ARGS] -b BRANCH START-POINT)."
  (interactive (magit-branch-read-args "Create and checkout branch"))
  (magit-run-git "checkout" args "-b" branch start-point))

(defun magit-branch-read-args (prompt)
  (let ((args magit-current-popup-args)
        (branch (magit-read-string prompt))
        (start  (magit-read-rev "Start point"
                                (or (magit-branch-or-commit-at-point)
                                    (magit-get-current-branch)))))
    (when (and (member "--track" args)
               (not (magit-branch-p start)))
      (setq args (delete "--track" args)))
    (list branch start args)))

;;;###autoload
(defun magit-branch-delete (branch &optional force)
  "Delete BRANCH.
Without a prefix argument deleting a branch that hasn't been
merged will fail.  With a prefix argument the deletion is forced.
When BRANCH is the current branch offer to first detach HEAD or
checkout the \"master\" branch.
\n(git branch -d|-D BRANCH || git push REMOTE :BRANCH)."
  (interactive (list (magit-read-rev (if current-prefix-arg
                                         "Force delete branch"
                                       "Delete branch")
                                     (or (magit-branch-at-point)
                                         (magit-get-previous-branch)))
                     current-prefix-arg))
  (let ((ref (magit-ref-fullname branch)))
    (unless ref
      (error "%s cannot be resolved" branch))
    (if (string-match "^refs/remotes/\\([^/]+\\)/\\(.+\\)" ref)
        (magit-run-git-async "push"      (match-string 1 ref)
                             (concat ":" (match-string 2 ref)))
      (cl-case (when (equal ref (magit-ref-fullname (magit-get-current-branch)))
                 (let ((msg (format "Branch %s is checked out.  " branch)))
                   (if (equal ref "refs/heads/master")
                       (magit-read-char-case msg nil
                         (?d "[d]etach HEAD & delete" 'detach)
                         (?a "[a]bort"                'abort))
                     (magit-read-char-case msg nil
                       (?d "[d]etach HEAD & delete"     'detach)
                       (?c "[c]heckout master & delete" 'master)
                       (?a "[a]bort"                    'abort)))))
        (detach (setq force t) (magit-call-git "checkout" "--detach"))
        (master (setq force t) (magit-call-git "checkout" "master"))
        (abort  (user-error "Branch %s not deleted" branch)))
      (magit-run-git "branch" (if force "-D" "-d") branch))))

;;;###autoload
(defun magit-branch-set-upstream (branch upstream)
  "Change the UPSTREAM branch of BRANCH."
  (interactive
   (let* ((atpoint (magit-branch-at-point))
          (current (magit-get-current-branch))
          (b (magit-read-rev "Change upstream of branch" (or atpoint current))))
     (list b (magit-read-rev "Change upstream to branch"
                             (or (unless (equal atpoint b) atpoint)
                                 (unless (equal current b) current))))))
  (if upstream
      (magit-run-git "branch" (concat "--set-upstream-to=" upstream) branch)
    (magit-run-git "branch" "--unset-upstream" branch)))

;;;###autoload
(defun magit-request-pull (url start)
  (interactive
   (let* ((remote (magit-read-remote "Remote"))
          (branch (magit-read-remote-branch "Branch" remote)))
     (list (magit-get "remote" remote "url")
           (magit-get-tracked-branch))))
  (let ((dir default-directory))
    ;; mu4e changes default-directory
    (compose-mail)
    (setq default-directory dir))
  (message-goto-body)
  (process-file magit-git-executable nil t nil "request-pull" start url)
  (set-buffer-modified-p nil))

;;;###autoload
(defun magit-branch-edit-description (branch)
  "Edit the description of BRANCH."
  (interactive (list (magit-read-rev "Edit branch description"
                                     (or (magit-branch-or-commit-at-point)
                                         (magit-get-current-branch)))))
  (magit-run-git-with-editor "branch" "--edit-description"))

;;;###autoload
(defun magit-branch-rename (old new &optional force)
  "Rename branch OLD to NEW.
With prefix, forces the rename even if NEW already exists.
\n(git branch -m|-M OLD NEW)."
  (interactive
   (let ((branch (magit-read-local-branch
                  "Rename branch" (or (magit-branch-at-point)
                                      (magit-get-current-branch)))))
     (list branch
           (magit-read-string (format "Rename branch '%s' to" branch))
           current-prefix-arg)))
  (unless (string= old new)
    (magit-run-git "branch" (if force "-M" "-m") old new)))

;;;;; Remoting

(magit-define-popup magit-remote-popup
  "Popup console for remote commands."
  'magit-popups
  :man-page "git-remote"
  :actions  '((?a "Add"    magit-remote-add)
              (?r "Rename" magit-remote-rename)
              (?k "Remove" magit-remote-remove)))

;;;###autoload
(defun magit-remote-add (remote url)
  "Add the REMOTE and fetch it.
\n(git remote add -f REMOTE URL)."
  (interactive (list (magit-read-string "Remote name")
                     (magit-read-string "Remote url")))
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
   (let  ((remote (magit-read-remote "Rename remote")))
     (list remote (magit-read-string (format "Rename remote '%s' to" remote)))))
  (unless (string= old new)
    (magit-run-git "remote" "rename" old new)))

;;;;; Rebasing

(magit-define-popup magit-rebase-popup
  "Key menu for rebasing."
  'magit-popups 'magit-popup-sequence-mode
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
             (magit-rebase-async upstream args)
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
             (magit-rebase-async "--onto" newbase upstream args)
             (message "Rebasing...done"))
    (magit-log-select
      `(lambda (commit)
         (magit-rebase-onto ,newbase (concat commit "^") ,args)))))

;;;###autoload
(defun magit-rebase-interactive (commit &optional args)
  "Start an interactive rebase operation.
\n(git rebase -i COMMIT[^] [ARGS])"
  (interactive (let ((commit (magit-commit-at-point)))
                 (list (and commit (concat commit "^"))
                       magit-current-popup-args)))
  (cond
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
    (user-error "No rebase in progress")))

;;;###autoload
(defun magit-rebase-skip ()
  "Skip the current commit and restart the current rebase operation."
  (interactive)
  (if (magit-rebase-in-progress-p)
      (magit-rebase-async "--skip")
    (user-error "No rebase in progress")))

;;;###autoload
(defun magit-rebase-edit ()
  "Edit the todo list of the current rebase operation."
  (interactive)
  (if (magit-rebase-in-progress-p)
      (magit-rebase-async "--edit-todo")
    (user-error "No rebase in progress")))

;;;###autoload
(defun magit-rebase-abort ()
  "Abort the current rebase operation, restoring the original branch."
  (interactive)
  (if (magit-rebase-in-progress-p)
      (magit-run-git "rebase" "--abort")
    (user-error "No rebase in progress")))

(defun magit-rebase-async (&rest args)
  (apply #'magit-run-git-sequencer 'rebase "rebase" args))

(defun magit-rebase-interactive-assert (commit)
  (when commit
    (if (magit-git-lines "rev-list" "--merges" (concat commit "..HEAD"))
        (magit-read-char-case "Proceed despite merge in rebase range?  " nil
          (?c "[c]ontinue" commit)
          (?s "[s]elect other" nil)
          (?a "[a]bort" (user-error "Quit")))
      commit)))

(defun magit-rebase-in-progress-p ()
  "Return t if a rebase is in progress."
  (or (file-exists-p (magit-git-dir "rebase-merge"))
      (file-exists-p (magit-git-dir "rebase-apply/onto"))))

(defun magit-insert-rebase-sequence ()
  (when (magit-rebase-in-progress-p)
    (let ((interactive (file-directory-p (magit-git-dir "rebase-merge"))))
      (magit-insert-section (rebase-sequence)
        (magit-insert-heading
          (if interactive
              (format "Rebasing %s:"
                      (-> "rebase-merge/head-name"
                        magit-git-dir magit-file-line magit-get-shortname))
            (format "Rebasing %s onto %s:"
                    (-> "rebase-apply/head-name"
                      magit-git-dir magit-file-line magit-get-shortname)
                    (-> "rebase-apply/onto"
                      magit-git-dir magit-file-line magit-get-shortname))))
        (if interactive
            (let* ((stop (magit-git-dir "rebase-merge/done"))
                   (stop (and (file-exists-p stop)
                              (-> stop
                                magit-file-lines last car split-string cadr))))
              (dolist (line (nreverse
                             (magit-file-lines
                              (magit-git-dir "rebase-merge/git-rebase-todo"))))
                (when (string-match "^\\([^# ]+\\) \\([^ ]+\\) \\(.*\\)$" line)
                  (magit-bind-match-strings (cmd hash msg) line
                    (magit-insert-section (commit hash)
                      (insert cmd " " (magit-format-rev-summary hash) ?\n)))))
              (when stop
                (magit-insert-section (commit stop)
                  (insert "stop " (magit-format-rev-summary stop) ?\n))))
          (magit-insert-rebase-apply-sequence))
        (insert ?\n)))))

(defun magit-insert-rebase-apply-sequence ()
  (let* ((files (nreverse (directory-files (magit-git-dir "rebase-apply")
                                           t "^[0-9]\\{4\\}$")))
         (stop (car (last files))))
    (dolist (file files)
      (let (hash)
        (with-temp-buffer
          (insert-file-contents file)
          (re-search-forward "^From \\([^ ]+\\)")
          (setq hash (match-string 1)))
        (magit-insert-section (commit hash)
          (insert (if (eq file stop) "stop " "pick ")
                  (magit-format-rev-summary hash) ?\n))))
    (insert ?\n)))

;;;;; AM

(magit-define-popup magit-am-popup
  "Popup console for mailbox commands."
  'magit-popups 'magit-popup-sequence-mode
  :man-page "git-am"
  :switches '((?3 "Fall back on 3way merge"           "--3way")
              (?s "Add Signed-off-by lines"           "--signoff")
              (?c "Remove text before scissors line"  "--scissors")
              (?k "Inhibit removal of email cruft"    "--keep")
              (?b "Limit removal of email cruft"      "--keep-non-patch")
              (?d "Use author date as committer date"
                  "--committer-date-is-author-date")
              (?D "Use committer date as author date" "--ignore-date"))
  :options  '((?p "Remove leading slashes from paths" "-p" read-number))
  :actions  '((?w "Apply patches" magit-am-apply-patches)
              (?m "Apply maildir" magit-am-apply-maildir))
  :default-arguments '("--3way")
  :default-actions 'magit-am-apply-patches
  :sequence-actions '((?w "Continue" magit-am-continue)
                      (?s "Skip"     magit-am-skip)
                      (?a "Abort"    magit-am-abort))
  :sequence-predicate 'magit-am-in-progress-p)

;;;###autoload
(defun magit-am-apply-patches (&optional files args)
  (interactive
   (let ((selection (if (use-region-p)
                        (magit-section-region-siblings)
                      (list (magit-current-section)))))
     (unless (eq (magit-section-type (car selection)) 'file)
       (setq selection nil))
     (list (if (or current-prefix-arg (not selection))
               (list (read-file-name "Apply patch(es): "
                                     nil (car (last selection))))
             (nreverse (mapcar 'magit-section-value selection)))
           magit-current-popup-args)))
  (magit-run-git-sequencer "am" args "--" (mapcar 'expand-file-name files)))

;;;###autoload
(defun magit-am-apply-maildir (&optional maildir args)
  (interactive (list (read-file-name "Apply mbox or Maildir: ")
                     magit-current-popup-args))
  (magit-run-git-sequencer "am" args (expand-file-name maildir)))

;;;###autoload
(defun magit-am-continue ()
  (interactive)
  (if (magit-am-in-progress-p)
      (if (magit-anything-unstaged-p)
          (error "Cannot continue due to unstaged changes")
        (magit-run-git-sequencer "am" "--continue"))
    (user-error "Not applying any patches")))

;;;###autoload
(defun magit-am-skip ()
  (interactive)
  (if (magit-am-in-progress-p)
      (magit-run-git-sequencer "am" "--skip")
    (user-error "Not applying any patches")))

;;;###autoload
(defun magit-am-abort ()
  (interactive)
  (if (magit-am-in-progress-p)
      (magit-run-git "am" "--abort")
    (user-error "Not applying any patches")))

(defun magit-am-in-progress-p ()
  (file-exists-p (magit-git-dir "rebase-apply/applying")))

(defun magit-insert-am-sequence ()
  (when (file-exists-p (magit-git-dir "rebase-apply/applying"))
    (let (msg file hash)
      (magit-insert-section (sequence)
        (magit-insert-heading "Applying patches:")
        (magit-insert-rebase-apply-sequence)))))

;;;;; Reset

;;;###autoload
(defun magit-reset-index (commit)
  "Reset the index to COMMIT.
Keep the head and working tree as-is, so if COMMIT revers to the
head this effectivley unstages all changes.
\n(git reset --mixed COMMIT)"
  (interactive
   (list (magit-read-rev "Reset index to"
                         (or (magit-branch-or-commit-at-point) "HEAD"))))
  (magit-run-git "reset" commit "--"))

;;;###autoload
(defun magit-reset (commit)
  "Reset the head and index to COMMIT, but not the working tree.
With a prefix argument also reset the working tree.
\n(git reset --mixed|--hard COMMIT)"
  (interactive
   (list (magit-read-rev (if current-prefix-arg
                             "Hard reset to"
                           "Reset head to")
                         (or (magit-branch-or-commit-at-point) "HEAD"))))
  (magit-run-git "reset" (if current-prefix-arg "--hard" "--mixed") commit))

;;;###autoload
(defun magit-reset-head (commit)
  "Reset the head and index to COMMIT, but not the working tree.
\n(git reset --mixed COMMIT)"
  (interactive
   (list (magit-read-rev "Reset head to"
                         (or (magit-branch-or-commit-at-point) "HEAD"))))
  (magit-run-git "reset" "--mixed" commit))

;;;###autoload
(defun magit-reset-soft (commit)
  "Reset the head to COMMIT, but not the index and working tree.
\n(git reset --soft REVISION)"
  (interactive
   (list (magit-read-rev "Soft reset to"
                         (or (magit-branch-or-commit-at-point) "HEAD"))))
  (magit-run-git "reset" "--soft" commit))

;;;###autoload
(defun magit-reset-hard (commit)
  "Reset the head, index, and working tree to COMMIT.
\n(git reset --hard REVISION)"
  (interactive
   (list (magit-read-rev "Hard reset to"
                         (or (magit-branch-or-commit-at-point) "HEAD"))))
  (magit-run-git "reset" "--hard" commit))

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
  (magit-run-git-async "remote" "update" magit-current-popup-args))

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
  "Pull changes from a remote repository.

If there is no default remote, the user is prompted for one and
the choosen values is saved.  If there is no default merge
branch, the user is prompted for one and the choosen values is
saved.

With a prefix argument, the default remote is not used and the
user is prompted for a remote.  With two prefix arguments, the
default merge branch is not used and the user is prompted for
a merge branch.  Values entered by the user because of prefix
arguments are not saved."
  (interactive)
  (let* ((branch (magit-get-current-branch))
         (branch-remote (magit-get-current-remote))
         (branch-merge (magit-get "branch" branch "merge"))
         (branch-merge-name (and branch-merge
                                 (save-match-data
                                   (string-match "^refs/heads/\\(.+\\)"
                                                 branch-merge)
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
                        chosen-branch-merge-name))))))

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
(defun magit-push-tags (remote &optional args)
  "Push all tags to a remote repository.
If only one remote exists, push to that.  Otherwise prompt for a
remote, offering the remote configured for the current branch as
default."
  (interactive (let ((remotes (magit-git-lines "remote")))
                 (list (if (= (length remotes) 1)
                           (car remotes)
                         (magit-read-remote "Push tags to remote"))
                       magit-current-popup-args)))
  (magit-run-git-async "push" remote "--tags" args))

;;;###autoload
(defun magit-push (arg)
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
         (auto-remote (magit-get-current-remote))
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

(defun magit-commit-log-buffer ()
  (let ((topdir (magit-get-top-dir)))
    (--first (equal topdir (with-current-buffer it
                             (and git-commit-mode (magit-get-top-dir))))
             (append (buffer-list (selected-frame))
                     (buffer-list)))))

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
  (interactive (list (magit-commit-at-point) magit-current-popup-args))
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
  (interactive (list (magit-commit-at-point) magit-current-popup-args))
  (magit-commit-squash-internal
   (lambda (c a)
     (when (setq c (magit-commit-squash c a))
       (magit-rebase-autosquash (concat c "^"))))
   "--squash" commit args t))

(defun magit-commit-squash-read-args ()
  (list (magit-commit-at-point) magit-current-popup-args
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
    (magit-commit-async nil "--continue")
    nil)
   ((and (file-exists-p (magit-git-dir "MERGE_MSG"))
         (not (magit-anything-unstaged-p)))
    (or args (list "--")))
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
  (apply #'magit-run-git-with-editor "commit" args))

(defun magit-read-file-trace (ignored)
  (let ((file  (magit-read-file-from-rev "HEAD" "File"))
        (trace (magit-read-string "Trace")))
    (if (string-match
         "^\\(/.+/\\|:[^:]+\\|[0-9]+,[-+]?[0-9]+\\)\\(:\\)?$" trace)
        (concat trace (or (match-string 2 trace) ":") file)
      (user-error "Trace is invalid, see man git-log"))))

(defvar magit-gpg-secret-key-hist nil)

(defun magit-read-gpg-secret-key (prompt &optional initial-input)
  (require 'epa)
  (let ((keys (--map (list (epg-sub-key-id (car (epg-key-sub-key-list it)))
                           (-when-let (id-obj (car (epg-key-user-id-list it)))
                             (let    ((id-str (epg-user-id-string id-obj)))
                               (if (stringp id-str)
                                   id-str
                                 (epg-decode-dn id-obj)))))
                     (epg-list-keys (epg-make-context epa-protocol) nil t))))
    (magit-completing-read prompt keys nil nil nil 'magit-gpg-secret-key-hist
                           (car (or magit-gpg-secret-key-hist keys)))))

(defvar magit-commit-add-log-insert-function 'magit-commit-add-log-insert)

(defun magit-commit-add-log ()
  "Add a stub for the current hunk into the commit message buffer.
If no commit is in progress, then initiate it.  Use the function
specified by variable `magit-commit-add-log-insert-function' to
actually insert the entry."
  (interactive)
  (let ((log (magit-commit-log-buffer)) buf pos)
    (save-window-excursion
      (call-interactively #'magit-visit-file)
      (setq buf (current-buffer)
            pos (point)))
    (unless log
      (unless (magit-commit-assert nil)
        (user-error "Abort"))
      (magit-commit)
      (while (not (setq log (magit-commit-log-buffer)))
        (sit-for 0.01)))
    (save-excursion
      (with-current-buffer buf
        (goto-char pos)
        (funcall magit-commit-add-log-insert-function log
                 (file-relative-name buffer-file-name (magit-get-top-dir))
                 (add-log-current-defun))))))

(defun magit-commit-add-log-insert (buffer file defun)
  (with-current-buffer buffer
    (goto-char (point-min))
    (cond ((not (re-search-forward (format "^\\* %s" (regexp-quote file))
                                   nil t))
           ;; No entry for file, create it.
           (goto-char (point-max))
           (forward-comment -1000)
           (unless (or (bobp) (looking-back "\\(\\*[^\n]+\\|\n\\)"))
             (insert "\n"))
           (insert (format "\n* %s" file))
           (when defun
             (insert (format " (%s)" defun)))
           (insert ": "))
          (defun
           ;; found entry for file, look for defun
           (let ((limit (save-excursion
                          (or (and (re-search-forward "^\\* " nil t)
                                   (match-beginning 0))
                              (progn (goto-char (point-max))
                                     (forward-comment -1000)
                                     (point))))))
             (cond ((re-search-forward
                     (format "(.*\\_<%s\\_>.*):" (regexp-quote defun))
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
                    (insert (format "(%s): " defun))))))
          (t
           ;; found entry for file, look for its beginning
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
                                     (or (magit-branch-or-commit-at-point) "HEAD"))
                     current-prefix-arg))
  (let ((args magit-current-popup-args))
    (when annotate
      (add-to-list 'args "--annotate"))
    (magit-run-git-with-editor "tag" args name rev)))

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
  (interactive (list (read-string "Stash message: ")
                     (magit-current-popup-args :not "--index")))
  (magit-run-git "stash" "save" args "--" description))

;;;###autoload
(defun magit-stash-snapshot (&optional args)
  "Create new stash of working tree and staging area; keep changes in place.
\n(git stash save [ARGS] \"Snapshot...\";
 git stash apply stash@{0})"
  (interactive (list (magit-current-popup-args :not "--index")))
  (magit-call-git "stash" "save" args (magit-stash-format-snapshot-message))
  (magit-stash-apply "stash@{0}" "--index"))

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
        (magit-run-git-with-input nil "apply"))
      (magit-refresh))))

(defun magit-stash-index-snapshot ()
  "Create a new stash of the index only; keep changes in place."
  (interactive)
  (magit-stash-index (magit-stash-format-snapshot-message) t))

(defun magit-stash-apply (stash &optional args)
  "Apply a stash on top of the current working tree state.
\n(git stash apply [ARGS] stash@{N})"
  (interactive (list (magit-read-stash "Apply stash" t)
                     (if magit-current-popup
                         (magit-current-popup-args :only "--index")
                       (--first (equal it "--index")
                                magit-stash-popup-defaults))))
  (if (or magit-current-popup (not (member "--index" args)))
      (magit-run-git "stash" "apply" args stash)
    (unless (magit-git-success "stash" "apply" args stash)
      (magit-run-git "stash" "apply" (remove "--index" args) stash))))

(defun magit-stash-pop (stash &optional args)
  "Apply a stash on top of working tree state and remove from stash list.
\n(git stash pop [ARGS] stash@{N})"
  (interactive (list (magit-read-stash "Pop stash" t)
                     (if magit-current-popup
                         (magit-current-popup-args :only "--index")
                       (--first (equal it "--index")
                                magit-stash-popup-defaults))))
  (magit-run-git "stash" "pop" args stash))

(defun magit-stash-drop (stash)
  "Remove a stash from the stash list.
When the region is active offer to drop all contained stashes.
\n(git stash drop stash@{N})"
  (interactive
   (if (use-region-p)
       (let ((stashes (magit-section-region-siblings 'magit-section-value)))
         (when (magit-confirm 'drop-stashes
                              (format "Drop %s through %s"
                                      (car stashes) (car (last stashes))))
           (deactivate-mark t)
           (list stashes)))
     (list (magit-read-stash "Drop stash"))))
  (if (listp stash)
      (mapc 'magit-stash-drop (nreverse stash))
    (magit-run-git "stash" "drop" stash)))

(defun magit-stash-branch (stash branchname)
  "Create and checkout a branch from STASH.
\n(git stash branch BRANCHNAME stash@{N})"
  (interactive (list (magit-read-stash  "Branch stash" t)
                     (magit-read-string "Branch name")))
  (magit-run-git "stash" "branch" branchname stash))

(defun magit-stash-format-snapshot-message ()
  (format-time-string magit-stash-snapshot-message-format (current-time)))

;;;;; Cherry-Pick & Revert

(magit-define-popup magit-cherry-pick-popup
  "Popup console for cherry-pick commands."
  'magit-popups 'magit-popup-sequence-mode
  :man-page "git-cherry-pick"
  :switches '((?s "Add Signed-off-by lines"            "--signoff")
              (?e "Edit commit messages"               "--edit")
              (?x "Reference cherry in commit message" "-x")
              (?F "Attempt fast-forward"               "--ff")
              (?m "Reply merge relative to parent"     "--mainline="))
  :options  '((?s "Strategy" "--strategy=" read-from-minibuffer))
  :actions  '((?A "Cherry Pick"  magit-cherry-pick)
              (?a "Cherry Apply" magit-cherry-apply))
  :sequence-actions '((?A "Continue" magit-sequencer-continue)
                      (?s "Skip"     magit-sequencer-skip)
                      (?a "Abort"    magit-sequencer-abort))
  :sequence-predicate 'magit-sequencer-in-progress-p
  :default-arguments '("--ff"))

;;;###autoload
(defun magit-cherry-pick (commit &optional args)
  (interactive (magit-sequencer-read-args 'cherry-pick "Cherry-pick"))
  (magit-assert-one-parent (car (if (listp commit)
                                    commit
                                  (split-string commit "\\.\\.")))
                           "cherry-pick")
  (magit-run-git-sequencer "cherry-pick" args commit))

;;;###autoload
(defun magit-cherry-apply (commit &optional args)
  (interactive (magit-sequencer-read-args 'cherry-pick "Apply commit"))
  (magit-assert-one-parent commit "cherry-pick")
  (magit-run-git-sequencer "cherry-pick" "--no-commit" args commit))


(magit-define-popup magit-revert-popup
  "Popup console for revert commands."
  'magit-popups 'magit-popup-sequence-mode
  :man-page "git-revert"
  :switches '((?s "Add Signed-off-by lines" "--signoff"))
  :options  '((?s "Strategy" "--strategy="  read-from-minibuffer))
  :actions  '((?V "Revert commit(s)" magit-revert)
              (?v "Revert changes"   magit-revert-no-commit))
  :sequence-actions '((?V "Continue" magit-sequencer-continue)
                      (?s "Skip"     magit-sequencer-skip)
                      (?a "Abort"    magit-sequencer-abort))
  :sequence-predicate 'magit-sequencer-in-progress-p)

;;;###autoload
(defun magit-revert (commit &optional args)
  (interactive (magit-sequencer-read-args 'revert "Revert commit"))
  (magit-assert-one-parent commit "revert")
  (magit-run-git "revert" args commit))

;;;###autoload
(defun magit-revert-no-commit (commit &optional args)
  (interactive (magit-sequencer-read-args 'revert "Revert changes"))
  (magit-assert-one-parent commit "revert")
  (magit-run-git "revert" "--no-commit" args commit))

;;;###autoload
(defun magit-sequencer-continue ()
  (interactive)
  (if (magit-sequencer-in-progress-p)
      (if (magit-anything-unstaged-p)
          (error "Cannot continue due to unstaged changes")
        (magit-run-git-sequencer
         (if (magit-revert-in-progress-p) "revert" "cherry-pick") "--continue"))
    (error "No cherry-pick or revert in progress")))

;;;###autoload
(defun magit-sequencer-skip ()
  (interactive)
  (if (magit-sequencer-in-progress-p)
      (magit-run-git-sequencer
       (if (magit-revert-in-progress-p) "revert" "cherry-pick") "--skip")
    (error "No cherry-pick or revert in progress")))

;;;###autoload
(defun magit-sequencer-abort ()
  (interactive)
  (if (magit-sequencer-in-progress-p)
      (magit-run-git-sequencer
       (if (magit-revert-in-progress-p) "revert" "cherry-pick") "--abort")
    (error "No cherry-pick or revert in progress")))

(defun magit-sequencer-read-args (command prompt)
  (let ((selection (if (use-region-p)
                       (magit-section-region-siblings)
                     (list (magit-current-section)))))
    (list (if (or current-prefix-arg
                  (not selection)
                  (not (eq (magit-section-type (car selection)) 'commit)))
              (let ((atpoint (magit-branch-or-commit-at-point)))
                (if (eq command 'cherry-pick)
                    (let ((current (magit-get-current-branch)))
                      (when (equal atpoint current)
                        (setq atpoint nil))
                      (magit-read-rev prompt atpoint current))
                  (magit-read-rev prompt atpoint)))
            (setq selection (mapcar 'magit-section-value selection))
            (if (eq command 'cherry-pick)
                (nreverse selection)
              selection))
          magit-current-popup-args)))

(defun magit-sequencer-in-progress-p ()
  (or (magit-cherry-pick-in-progress-p)
      (magit-revert-in-progress-p)))

(defun magit-cherry-pick-in-progress-p ()
  (file-regular-p (magit-git-dir "CHERRY_PICK_HEAD")))

(defun magit-revert-in-progress-p ()
  (file-regular-p (magit-git-dir "REVERT_HEAD")))

(defun magit-insert-sequencer-sequence ()
  (-when-let
      (heading (or (and (magit-cherry-pick-in-progress-p) "Cherry Picking:")
                   (and (magit-revert-in-progress-p)      "Reverting:")))
    (magit-insert-section (sequence)
      (magit-insert-heading heading)
      (let* ((lines (nreverse
                     (magit-file-lines (magit-git-dir "sequencer/todo"))))
             (stop (car (last lines))))
        (dolist (line lines)
          (when (string-match "^pick \\([^ ]+\\) \\(.*\\)$" line)
            (magit-bind-match-strings (hash msg) line
              (magit-insert-section (commit hash)
                (insert (if (eq line stop) "stop " "pick ")
                        (propertize hash 'face 'magit-hash))
                (insert " " msg "\n"))))))
      (insert "\n"))))

;;;;; Submoduling

(magit-define-popup magit-submodule-popup
  "Popup console for submodule commands."
  'magit-popups
  :man-page "git-submodule"
  :actions  '((?a "Add"    magit-submodule-add)
              (?b "Setup"  magit-submodule-setup)
              (?i "Init"   magit-submodule-init)
              (?u "Update" magit-submodule-update)
              (?s "Sync"   magit-submodule-sync)))

;;;###autoload
(defun magit-submodule-add (url &optional path)
  "Add the repository at URL as a submodule.
Optional PATH is the path to the submodule relative to the root
of the superproject. If it is nil then the path is determined
based on URL."
  (interactive
   (let* ((default-directory (magit-toplevel))
          (path (read-file-name
                 "Add submodule: " nil nil nil
                 (magit-section-when [file untracked]
                   (directory-file-name (magit-section-value it))))))
     (when path
       (setq path (file-name-as-directory (expand-file-name path)))
       (when (member path (list "" default-directory))
         (setq path nil)))
     (list (magit-read-string
            "Remote url"
            (and path (magit-git-repo-p path t)
                 (let ((default-directory path))
                   (magit-get "remote"
                              (or (magit-get-current-remote) "origin")
                              "url"))))
           (and path (directory-file-name (file-relative-name path))))))
  (magit-run-git "submodule" "add" url path))

;;;###autoload
(defun magit-submodule-setup ()
  "Clone and register missing submodules and checkout appropriate commits."
  (interactive)
  (magit-submodule-update t))

;;;###autoload
(defun magit-submodule-init ()
  "Register submodules listed in .gitmodules into .git/config."
  (interactive)
  (let ((default-directory (magit-get-top-dir)))
    (magit-run-git-async "submodule" "init")))

;;;###autoload
(defun magit-submodule-update (&optional init)
  "Clone missing submodules and checkout appropriate commits.
With a prefix argument also register submodules in .git/config."
  (interactive "P")
  (let ((default-directory (magit-get-top-dir)))
    (magit-run-git-async "submodule" "update" (and init "--init"))))

;;;###autoload
(defun magit-submodule-sync ()
  "Update each submodule's remote URL according to .gitmodules."
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
   (if (magit-bisect-in-progress-p)
       (user-error "Already bisecting")
     (list (magit-read-rev "Start bisect with known bad revision" "HEAD")
           (magit-read-rev "Good revision" (magit-branch-or-commit-at-point)))))
  (magit-bisect-async "start" (list bad good) t))

;;;###autoload
(defun magit-bisect-reset ()
  "After bisecting cleanup bisection state and return to original HEAD."
  (interactive)
  (when (magit-confirm 'reset-bisect "Reset bisect")
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
  (unless (or no-assert (magit-bisect-in-progress-p))
    (user-error "Not bisecting"))
  (let ((file (magit-git-dir "BISECT_CMD_OUTPUT"))
        (default-directory (magit-get-top-dir)))
    (ignore-errors (delete-file file))
    (magit-run-git-with-logfile file "bisect" subcommand args)
    (magit-process-wait)
    (magit-refresh)))

(defun magit-bisect-in-progress-p ()
  (file-exists-p (magit-git-dir "BISECT_LOG")))

(defun magit-insert-bisect-output ()
  (when (magit-bisect-in-progress-p)
    (let ((lines
           (or (magit-file-lines (magit-git-dir "BISECT_CMD_OUTPUT"))
               (list "Bisecting: (no saved bisect output)"
                     "It appears you have invoked `git bisect' from a shell."
                     "There is nothing wrong with that, we just cannot display"
                     "anything useful here.  Consult the shell output instead.")))
          (done-re "^[a-z0-9]\\{40\\} is the first bad commit$"))
      (magit-insert-section (bisect-output t)
        (magit-insert-heading
          (propertize (or (and (string-match done-re (car lines)) (pop lines))
                          (--first (string-match done-re it) lines)
                          (pop lines))
                      'face 'magit-section-heading))
        (dolist (line lines)
          (insert line "\n"))))
    (insert "\n")))

(defun magit-insert-bisect-rest ()
  (when (magit-bisect-in-progress-p)
    (magit-insert-section (bisect-view)
      (magit-insert-heading "Bisect Rest:")
      (magit-git-wash (apply-partially 'magit-log-wash-log 'bisect-vis)
        "bisect" "visualize" "git" "log"
        "--pretty=format:%h%d %s" "--decorate=full"))))

(defun magit-insert-bisect-log ()
  (when (magit-bisect-in-progress-p)
    (magit-insert-section (bisect-log)
      (magit-insert-heading "Bisect Log:")
      (magit-git-wash #'magit-wash-bisect-log "bisect" "log"))))

(defun magit-wash-bisect-log (args)
  (let (beg)
    (while (progn (setq beg (point-marker))
                  (re-search-forward "^\\(git bisect [^\n]+\n\\)" nil t))
      (magit-bind-match-strings (heading) nil
        (magit-delete-match)
        (save-restriction
          (narrow-to-region beg (point))
          (goto-char (point-min))
          (magit-insert-section (bisect-log nil t)
            (magit-insert-heading heading)
            (magit-wash-sequence
             (apply-partially 'magit-log-wash-line 'bisect-log
                              (magit-abbrev-length)))
            (insert ?\n)))))
    (when (re-search-forward
           "# first bad commit: \\[\\([a-z0-9]\\{40\\}\\)\\] [^\n]+\n" nil t)
      (magit-bind-match-strings (hash) nil
        (magit-delete-match)
        (magit-insert-section (bisect-log)
          (magit-insert (concat hash " is the first bad commit\n")))))))

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
              (?D "Show ref names"            "--decorate")
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
              (?f "File log"       magit-log-file)
              (?b "Oneline branch" magit-log)
              (?B "Verbose branch" magit-log-verbose)
              (?R "Reflog HEAD"    magit-reflog-head))
  :default-arguments '("--graph" "--decorate")
  :default-action 'magit-log-dwim
  :max-action-columns 4)

;;;###autoload
(defun magit-log (range &optional args)
  (interactive (magit-log-read-args nil nil))
  (magit-mode-setup magit-log-buffer-name-format nil
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
  (magit-mode-setup magit-log-buffer-name-format nil
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
          (if (--any? (string-match-p "^\\(-G\\|--grep=\\)" it)
                      magit-current-popup-args)
              (delete "--graph" magit-current-popup-args)
            magit-current-popup-args))))

;;;###autoload
(defun magit-log-file (file &optional use-graph)
  "Display the log for the currently visited file or another one.
With a prefix argument show the log graph."
  (interactive
   (list (magit-read-file-from-rev (magit-get-current-branch) "Log for file")
         current-prefix-arg))
  (magit-mode-setup magit-log-buffer-name-format nil
                    #'magit-log-mode
                    #'magit-refresh-log-buffer
                    'oneline "HEAD"
                    (cons "--follow"
                          (if use-graph
                              (cons "--graph" magit-current-popup-args)
                            (delete "--graph" magit-current-popup-args)))
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
  (magit-mode-setup magit-reflog-buffer-name-format nil
                    #'magit-reflog-mode
                    #'magit-refresh-reflog-buffer ref))

;;;###autoload
(defun magit-reflog-head ()
  "Display the HEAD reflog."
  (interactive)
  (magit-reflog "HEAD"))

;;;;; Miscellaneous

;;;###autoload
(defun magit-format-patch (range)
  (interactive
   (let ((revs (if (use-region-p)
                   (magit-section-region-siblings)
                 (list (magit-current-section)))))
     (unless (eq (magit-section-type (car revs)) 'commit)
       (setq revs nil))
     (setq revs (nreverse (mapcar 'magit-section-value revs)))
     (list (if (or current-prefix-arg (not revs))
               (magit-read-rev "Format range")
             (concat (car revs) "^.." (car (last revs)))))))
  (magit-run-git "format-patch" range))

(defun magit-copy-as-kill ()
  "Copy the thing at point into the kill ring."
  (interactive)
  (magit-section-when (branch commit mcommit file)
    (kill-new (message "%s" (magit-section-value it)))))

;;; Modes (2)
;;;; Log Mode
;;;;; Log Core

(define-derived-mode magit-log-mode magit-mode "Magit Log"
  "Mode for looking at Git log.
This mode is documented in info node `(magit)History'.

\\<magit-log-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-show-commit] or \\[magit-show-or-scroll-up]\
 to visit the commit at point.
Type \\[magit-merge-popup] to merge the commit at point.
Type \\[magit-cherry-pick] to cherry-pick the commit at point.
Type \\[magit-reset-head] to reset HEAD to the commit at point.
\n\\{magit-log-mode-map}"
  :group 'magit-log
  (magit-set-buffer-margin magit-log-show-margin))

(defun magit-refresh-log-buffer (style range args &optional file)
  (when (consp range)
    (setq range (concat (car range) ".." (cdr range))))
  (magit-insert-section (logbuf)
    (magit-insert-heading "Commits"
      (and file  (concat " for file " file))
      (and range (concat " in " range)))
    (if (eq style 'oneline)
        (magit-insert-log range args file)
      (magit-insert-log-long range args file)))
  (save-excursion
    (goto-char (point-min))
    (magit-format-log-margin)))

(defun magit-insert-log (range &optional args file)
  (--when-let (member "--decorate" args)
    (setcar it "--decorate=full"))
  (magit-git-wash (apply-partially 'magit-log-wash-log 'oneline)
    "log" (format "-%d" magit-log-cutoff-length) "--color"
    (format "--pretty=format:%%h%s %s[%%an][%%at]%%s"
            (if (member "--decorate=full" args) "%d" "")
            (if (member "--show-signature" args) "%G?" ""))
    (delete "--show-signature" args)
    range "--" file))

(defun magit-insert-log-long (range &optional args file)
  (--when-let (member "--decorate" args)
    (setcar it "--decorate=full"))
  (magit-git-wash (apply-partially 'magit-log-wash-log 'long)
    "log" (format "-%d" magit-log-cutoff-length)
    "--color" "--stat" "--abbrev-commit"
    args range "--" file))

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

(defun magit-log-wash-log (style args)
  (when (member "--color" args)
    (let ((ansi-color-apply-face-function
           (lambda (beg end face)
             (when face
               (put-text-property beg end 'font-lock-face face)))))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (when (eq style 'cherry)
    (reverse-region (point-min) (point-max)))
  (let ((magit-log-count 0))
    (magit-wash-sequence (apply-partially 'magit-log-wash-line style
                                          (magit-abbrev-length)))
    (if (derived-mode-p 'magit-log-mode)
        (when (= magit-log-count magit-log-cutoff-length)
          (magit-insert-section (longer)
            (insert-text-button
             (substitute-command-keys
              (format "Type \\<%s>\\[%s] to show more history"
                      'magit-log-mode-map
                      'magit-log-show-more-entries))
             'action (lambda (button)
                       (magit-log-show-more-entries))
             'follow-link t
             'mouse-face 'magit-section-highlight)))
      (unless (equal (car args) "cherry")
        (insert ?\n)))))

(defun magit-log-wash-line (style abbrev)
  (looking-at (cl-ecase style
                (oneline magit-log-oneline-re)
                (long    magit-log-long-re)
                (cherry  magit-log-cherry-re)
                (module  magit-log-module-re)
                (reflog  magit-log-reflog-re)
                (bisect-vis magit-log-bisect-vis-re)
                (bisect-log magit-log-bisect-log-re)))
  (magit-bind-match-strings
      (hash msg refs graph author date gpg cherry refsel refsub side) nil
    (magit-delete-match)
    (when cherry
      (unless (derived-mode-p 'magit-cherry-mode)
        (insert "  "))
      (magit-insert cherry (if (string= cherry "-")
                               'magit-cherry-equivalent
                             'magit-cherry-unmatched) ?\s))
    (when side
      (magit-insert side (if (string= side "<")
                             'magit-diff-removed
                           'magit-diff-added) ?\s))
    (unless (eq style 'long)
      (when (eq style 'bisect-log)
        (setq hash (magit-git-string "rev-parse" "--short" hash)))
      (if hash
          (insert (propertize hash 'face 'magit-hash) ?\s)
        (insert (make-string (1+ abbrev) ? ))))
    (when graph
      (if magit-log-format-graph-function
          (insert (funcall magit-log-format-graph-function graph))
        (insert graph)))
    (when (and hash (eq style 'long))
      (magit-insert (if refs hash (magit-rev-parse hash)) 'magit-hash ?\s))
    (when refs
      (magit-insert (magit-format-ref-labels refs))
      (insert ?\s))
    (when refsub
      (insert (format "%-2s " refsel))
      (magit-insert (magit-log-format-reflog refsub)))
    (when msg
      (magit-insert msg (cl-case (and gpg (aref gpg 0))
                          (?G 'magit-signature-good)
                          (?B 'magit-signature-bad)
                          (?U 'magit-signature-untrusted))))
    (goto-char (line-beginning-position))
    (when (memq style '(oneline reflog))
      (magit-format-log-margin author date))
    (if hash
        (magit-insert-section it (commit hash)
          (when (eq style 'module)
            (setf (magit-section-type it) 'mcommit))
          (when (derived-mode-p 'magit-log-mode)
            (cl-incf magit-log-count))
          (forward-line)
          (when (eq style 'long)
            (magit-wash-sequence
             (lambda ()
               (looking-at magit-log-long-re)
               (when (match-string 2)
                 (magit-log-wash-line 'long abbrev))))))
      (forward-line)))
  t)

(defun magit-log-format-unicode-graph (string)
  "Translate ascii characters to unicode characters.
Whether that actually is an improvment depends on the unicode
support of the font in use.  The translation is done using the
alist in `magit-log-format-unicode-graph-alist'."
  (replace-regexp-in-string
   "[/|\\*o ]"
   (lambda (str)
     (propertize
      (string (or (cdr (assq (aref str 0)
                             magit-log-format-unicode-graph-alist))
                  (aref str 0)))
      'face (get-text-property 0 'face str)))
   string))

(defun magit-format-log-margin (&optional author date)
  (cl-destructuring-bind (width unit-width duration-spec)
      magit-log-margin-spec
    (if author
        (magit-make-margin-overlay
         (propertize (truncate-string-to-width
                      author (- width 1 3 ; gap, digits
                                (if (= unit-width 1) 1 (1+ unit-width))
                                (if (derived-mode-p 'magit-log-mode)
                                    1 ; pseudo fringe
                                  0))
                      nil ?\s (make-string 1 magit-ellipsis))
                     'face 'magit-log-author)
         " "
         (propertize (magit-format-duration
                      (abs (truncate (- (float-time)
                                        (string-to-number date))))
                      (symbol-value duration-spec)
                      unit-width)
                     'face 'magit-log-date)
         (when (derived-mode-p 'magit-log-mode)
           (propertize " " 'face 'fringe)))
      (magit-make-margin-overlay
       (propertize (make-string (1- width) ?\s) 'face 'default)
       (propertize " " 'face 'fringe)))))

;;;;; Log Commands

(defun magit-log-toggle-margin ()
  "Show or hide the log margin."
  (interactive)
  (unless (derived-mode-p 'magit-log-mode 'magit-status-mode)
    (user-error "Buffer doesn't contain any logs"))
  (when (eq (car magit-refresh-args) 'long)
    (user-error "Log margin is redundant when showing verbose logs"))
  (magit-set-buffer-margin (not (cdr (window-margins)))))

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
    (magit-section-forward)))

(defun magit-log-maybe-show-commit (&optional section) ; TODO rename
  (--when-let
      (or (and section
               (eq (magit-section-type section) 'commit)
               (or (and (magit-diff-auto-show-p 'log-follow)
                        (get-buffer-window magit-commit-buffer-name-format))
                   (and (magit-diff-auto-show-p 'log-oneline)
                        (derived-mode-p 'magit-log-mode)
                        (eq (car magit-refresh-args) 'oneline)))
               (magit-section-value section))
          (and magit-blame-mode
               (magit-diff-auto-show-p 'blame-follow)
               (get-buffer-window magit-commit-buffer-name-format)
               (magit-blame-chunk-get :hash)))
    (magit-show-commit it t)))

(defun magit-log-goto-same-commit ()
  (--when-let
      (and magit-previous-section
           (derived-mode-p 'magit-log-mode)
           (-when-let (value (magit-section-value magit-previous-section))
             (--first (equal (magit-section-value it) value)
                      (magit-section-children magit-root-section))))
    (goto-char (magit-section-start it))))

;;;; Log Select Mode

(define-derived-mode magit-log-select-mode magit-log-mode "Magit Select"
  "Mode for selecting a commit from history."
  :group 'magit-log)

(defvar-local magit-log-select-pick-function nil)
(defvar-local magit-log-select-quit-function nil)

(defun magit-log-select (pick &optional quit desc branch args)
  (declare (indent defun))
  (magit-mode-setup magit-log-buffer-name-format nil
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
        (rev (magit-commit-at-point)))
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

\\<magit-cherry-mode-map>\
Type \\[magit-show-commit] or \\[magit-show-or-scroll-up]\
 to visit the commit at point.
Type \\[magit-cherry-pick] to cherry-pick the commit at point.
\n\\{magit-cherry-mode-map}"
  :group 'magit-modes)

;;;###autoload
(defun magit-cherry (head upstream)
  "Show commits in a branch that are not merged in the upstream branch."
  (interactive
   (let  ((head (magit-read-rev "Cherry head" (magit-get-current-branch))))
     (list head (magit-read-rev "Cherry upstream"
                                (magit-get-tracked-branch head)))))
  (magit-mode-setup magit-cherry-buffer-name-format nil
                    #'magit-cherry-mode
                    #'magit-refresh-cherry-buffer upstream head))

(defun magit-refresh-cherry-buffer (upstream head)
  (magit-insert-section (cherry)
    (run-hooks 'magit-cherry-sections-hook)))

(defun magit-insert-cherry-headers ()
  (magit-insert-status-headers (nth 1 magit-refresh-args)
                               (nth 0 magit-refresh-args)))

(defun magit-insert-cherry-commits ()
  (magit-insert-section (cherries)
    (magit-insert-heading "Cherry commits:")
    (apply 'magit-insert-cherry-commits-1 magit-refresh-args)))

(defun magit-insert-cherry-commits-1 (&rest args)
  (magit-git-wash (apply-partially 'magit-log-wash-log 'cherry)
    "cherry" "-v" "--abbrev" args))

;;;; Reflog Mode

(define-derived-mode magit-reflog-mode magit-log-mode "Magit Reflog"
  "Mode for looking at Git reflog.
This mode is documented in info node `(magit)Reflogs'.

\\<magit-reflog-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-show-commit] or \\[magit-show-or-scroll-up]\
 to visit the commit at point.
Type \\[magit-cherry-pick] to cherry-pick the commit at point.
Type \\[magit-reset-head] to reset HEAD to the commit at point.
\n\\{magit-reflog-mode-map}"
  :group 'magit-log)

(defun magit-refresh-reflog-buffer (ref)
  (magit-insert-section (reflogbuf)
    (magit-insert-heading "Local history of branch " ref)
    (magit-git-wash (apply-partially 'magit-log-wash-log 'reflog)
      "reflog" "show" "--format=format:%h [%an] %ct %gd %gs"
      (format "--max-count=%d" magit-log-cutoff-length) ref)))

(defvar magit-reflog-labels
  '(("commit"      . magit-reflog-commit)
    ("amend"       . magit-reflog-amend)
    ("merge"       . magit-reflog-merge)
    ("checkout"    . magit-reflog-checkout)
    ("branch"      . magit-reflog-checkout)
    ("reset"       . magit-reflog-reset)
    ("rebase"      . magit-reflog-rebase)
    ("cherry-pick" . magit-reflog-cherry-pick)
    ("initial"     . magit-reflog-commit)
    ("pull"        . magit-reflog-remote)
    ("clone"       . magit-reflog-remote)))

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
                            'magit-reflog-other)))))

;;;; Diff Mode
;;;;; Diff Core

(define-derived-mode magit-diff-mode magit-mode "Magit Diff"
  "Mode for looking at a Git diff.
This mode is documented in info node `(magit)Diffing'.

\\<magit-diff-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-section-toggle] to expand or hide the section at point.
Type \\[magit-visit-file] to visit the file at point.
Type \\[magit-apply] to apply the change at point to the worktree.
Type \\[magit-reverse] to reverse the change at point in the worktree.
\n\\{magit-diff-mode-map}"
  :group 'magit-diff)

(defun magit-diff-type (&optional section)
  (--when-let (or section (magit-current-section))
    (cond ((derived-mode-p 'magit-diff-mode)
           (or (and (not (nth 0 magit-refresh-args))
                    (not (nth 1 magit-refresh-args))
                    (cond ((not (nth 2 magit-refresh-args))
                           'unstaged)
                          ((member "--cached" (cddr magit-refresh-args))
                           'staged)))
               'undefined))
          ((derived-mode-p 'magit-commit-mode) 'committed)
          ((derived-mode-p 'magit-status-mode)
           (let ((stype (magit-section-type it)))
             (if (memq stype '(staged unstaged untracked))
                 stype
               (pcase stype
                 (`file (-> it magit-section-parent magit-section-type))
                 (`hunk (-> it magit-section-parent magit-section-parent
                            magit-section-type))))))
          (t 'undefined))))

;;;;; Diff Entry Commands

(magit-define-popup magit-diff-popup
  "Key menu for diffing."
  'magit-popups 'magit-popup-mode 'magit-diff-options
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
(defun magit-diff (range &optional args)
    "Show differences between two commits.
RANGE should be a range (A..B or A...B) but can also be a single
commit.  If one side of the range is omitted, then it defaults
to HEAD.  If just a commit is given, then changes in the working
tree relative to that commit are shown."
  (interactive (list (magit-read-rev "Diff for range")))
  (magit-mode-setup magit-diff-buffer-name-format
                    magit-diff-switch-buffer-function
                    #'magit-diff-mode
                    #'magit-refresh-diff-buffer range args))

;;;###autoload
(defun magit-diff-working-tree (&optional rev)
  "Show changes between the current working tree and the `HEAD' commit.
With a prefix argument show changes between the working tree and
a commit read from the minibuffer."
  (interactive
   (and current-prefix-arg
        (list (magit-read-rev "Diff working tree and commit"
                              (or (magit-branch-or-commit-at-point)
                                  (magit-get-current-branch) "HEAD")))))
  (magit-diff (or rev "HEAD")))

;;;###autoload
(defun magit-diff-staged (&optional commit)
  "Show changes between the index and the `HEAD' commit.
With a prefix argument show changes between the index and
a commit read from the minibuffer."
  (interactive
   (and current-prefix-arg
        (list (magit-read-rev "Diff index and commit"
                              (or (magit-branch-or-commit-at-point)
                                  (magit-get-current-branch) "HEAD")))))
  (magit-diff nil (cons "--cached" (and commit (list commit)))))

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
         (diff-buf (magit-mode-get-buffer magit-diff-buffer-name-format
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
  (magit-diff "HEAD^" (list "--cached")))

;;;###autoload
(defun magit-diff-paths (a b)
  "Show changes between any two files on disk."
  (interactive (list (read-file-name "First file: " nil nil t)
                     (read-file-name "Second file: " nil nil t)))
  (magit-diff nil (list "--no-index" "--" a b)))

;;;###autoload
(defun magit-diff-stash (stash &optional noselect)
  "Show changes in the stash at point.
If there is no stash at point or with a prefix argument prompt
for a stash.

A stash consist of more than just one commit.  This command uses
a special diff range so that the stashed changes appear as if it
actually were a single commit."
  (interactive (list (or (and (not current-prefix-arg)
                              (magit-stash-at-point t))
                         (magit-read-stash "Show stash"))))
  (magit-mode-setup magit-diff-buffer-name-format
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
  (--if-let (--first (string-match "^-U\\([0-9]+\\)$" it) magit-diff-options)
      (progn (setq magit-diff-options (delete it magit-diff-options))
             (string-to-number (match-string 1 it)))
    3))

(defun magit-refresh-diff-buffer (range &optional args)
  (magit-insert-section (diffbuf)
    (magit-insert-heading
      (if range
          (if (string-match-p "\\.\\." range)
              (format "Changes in %s" range)
            (format "Changes from %s to working tree" range))
        (if (member "--cached" args)
            "Staged changes"
          "Unstaged changes")))
    (magit-git-wash #'magit-diff-wash-diffs
      "diff" "-p" (and magit-diff-show-diffstat "--stat")
      magit-diff-extra-options
      range args magit-diff-options "--")))

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

(defconst magit-diff-submodule-re
  (concat "^Submodule \\([^ ]+\\) \\(?:"
          "\\([^ ]+ (new submodule)\\)\\|"
          "\\(contains modified content\\)\\|"
          "\\([^:]+\\):\\)$"))

(defun magit-diff-wash-diffs (args)
  (let ((diffstats (magit-diff-wash-diffstats)))
    (when (re-search-forward magit-diff-headline-re nil t)
      (goto-char (line-beginning-position))
      (magit-wash-sequence
       (lambda ()
         (magit-diff-wash-diff args (pop diffstats))))
      (insert ?\n)))
  (goto-char (point-max))
  (magit-xref-insert-buttons))

(defun magit-diff-wash-diffstats ()
  (let (heading diffstats (beg (point)))
    (when (re-search-forward "^ ?\\([0-9]+ +files? change[^\n]*\n\\)" nil t)
      (setq heading (match-string 1))
      (magit-delete-match)
      (goto-char beg)
      (magit-insert-section it (diffstats)
        (insert heading)
        (magit-insert-heading)
        (magit-wash-sequence
         (lambda ()
           (when (looking-at magit-diff-statline-re)
             (magit-bind-match-strings (file sep cnt add del) nil
               (magit-delete-line)
               (magit-insert-section (file file)
                 (insert " " file sep cnt " ")
                 (when add
                   (magit-insert (propertize add 'face 'magit-diffstat-added)))
                 (when del
                   (magit-insert (propertize del 'face 'magit-diffstat-removed)))
                 (insert "\n"))))))
        (setq diffstats (magit-section-children it))))
    diffstats))

(defun magit-diff-wash-diff (args diffstat)
  (cond
   ((looking-at magit-diff-submodule-re)
    (magit-bind-match-strings (module new dirty range) nil
      (magit-delete-line)
      (when (and dirty
                 (looking-at magit-diff-submodule-re)
                 (string= (match-string 1) module))
        (setq range (match-string 4))
        (magit-delete-line))
      (while (looking-at "^  \\([<>]\\) \\(.+\\)$")
        (magit-delete-line))
      (if range
          (let ((default-directory
                  (file-name-as-directory
                   (expand-file-name module (magit-get-top-dir)))))
            (setf (magit-section-value
                   (magit-insert-section (file module t)
                     (magit-insert-heading
                       (concat (propertize (concat "modified   " module)
                                           'face 'magit-file-heading)
                               " ("
                               (and range "new commits")
                               (and dirty ", modified content")
                               ")"))
                     (magit-git-wash (apply-partially 'magit-log-wash-log 'module)
                       "log" "--oneline" "--left-right" range)
                     (delete-char -1)))
                  module))
        (magit-insert-section (file module)
          (magit-insert
           (concat (propertize (if new
                                   (concat "new module " module)
                                 (concat "modified   " module))
                               'face 'magit-file-heading)
                   (and dirty " (modified content)"))
           nil ?\n)))))
   ((looking-at "^\\* Unmerged path \\(.*\\)")
    (let ((file (magit-decode-git-path (match-string 1))))
      (magit-delete-line)
      (unless (and (derived-mode-p 'magit-status-mode)
                   (not (member "--cached" args)))
        (magit-insert-section (file file)
          (magit-insert (propertize (format "unmerged   %s" file)
                                    'face 'magit-file-heading) nil ?\n))))
    t)
   ((looking-at "^diff --\\(git\\|cc\\|combined\\) \\(?:\\(.+?\\) \\2\\)?")
    (let ((status (cond ((equal (match-string 1) "git")      "modified")
                        ((derived-mode-p 'magit-commit-mode) "resolved")
                        (t                                   "unmerged")))
          (orig (match-string 2))
          (file (match-string 2))
          modes)
      (magit-delete-line)
      (while (not (or (eobp) (looking-at magit-diff-headline-re)))
        (if (looking-at "^old mode \\([^\n]+\\)\nnew mode \\([^\n]+\\)\n")
            (progn (setq modes (match-string 0))
                   (magit-delete-match))
          (cond
           ((looking-at "^--- \\([^/].*?\\)\t?$") ; i.e. not /dev/null
            (setq orig (match-string 1)))
           ((looking-at "^\\+\\+\\+ \\([^/].*?\\)\t?$")
            (setq file (match-string 1)))
           ((looking-at "^\\(copy\\|rename\\) from \\(.+\\)$")
            (setq orig (match-string 2)))
           ((looking-at "^\\(copy\\|rename\\) to \\(.+\\)$")
            (setq file (match-string 2))
            (setq status (if (equal (match-string 1) "copy") "new file" "renamed")))
           ((looking-at "^\\(new file\\|deleted\\)")
            (setq status (match-string 1))))
          (magit-delete-line)))
      (setq orig (magit-decode-git-path orig))
      (setq file (magit-decode-git-path file))
      (when diffstat
        (setf (magit-section-value diffstat) file))
      (magit-insert-section it
        (file file (or (equal status "deleted")
                      (derived-mode-p 'magit-status-mode)))
        (magit-insert-heading
          (propertize
           (format "%-10s %s\n" status
                   (if (equal orig file) file (format "%s -> %s" orig file)))
           'face 'magit-file-heading))
        (unless (equal orig file)
          (setf (magit-section-source it) orig))
        (when modes
          (magit-insert-section (hunk)
            (insert modes)))
        (magit-wash-sequence #'magit-diff-wash-hunk))))))

(defun magit-diff-wash-hunk ()
  (when (looking-at "^@\\{2,\\} \\(.+?\\) @\\{2,\\}\\(?: \\(.*\\)\\)?")
    (let ((heading (match-string 0))
          (value (cons (match-string 2) (split-string (match-string 1)))))
      (magit-delete-line)
      (magit-insert-section it (hunk value)
        (insert (propertize (concat heading "\n")
                            'face 'magit-hunk-heading))
        (setf (magit-section-content it) (point-marker))
        (while (not (or (eobp) (looking-at magit-diff-headline-re)))
          (forward-line))
        (setf (magit-section-end it) (point))
        (magit-diff-paint-hunk it nil)
        (when (eq magit-diff-refine-hunk 'all)
          (magit-diff-refine-hunk it))))
    t))

(defun magit-diff-paint-hunk (section highlight)
  (when (magit-section-value section)
    (let ((beg (magit-section-start   section))
          (cnt (magit-section-content section))
          (end (magit-section-end     section))
          merging)
      (save-restriction
        (goto-char beg)
        (setq merging (looking-at "@@@"))
        (goto-char cnt)
        (narrow-to-region cnt end)
        (while (not (eobp))
          (put-text-property
           (point) (1+ (line-end-position)) 'face
           (cond
            ((looking-at "^\\+\\+[<=|>]\\{7\\}") 'magit-conflict-heading)
            ((looking-at (if merging  "^\\(\\+\\| \\+\\)" "^\\+"))
             (magit-diff-highlight-whitespace merging)
             (if highlight 'magit-diff-added-highlight 'magit-diff-added))
            ((looking-at (if merging  "^\\(-\\| -\\)" "^-"))
             (if highlight 'magit-diff-removed-highlight 'magit-diff-removed))
            (t
             (if highlight 'magit-diff-context-highlight 'magit-diff-context))))
          (forward-line))))))

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
              (cdr (--first (string-match-p (car it) default-directory)
                            (nreverse
                             (default-value 'magit-highlight-indentation))))))))
      (when (and magit-highlight-trailing-whitespace
                 (looking-at (concat prefix ".*?\\([ \t]+\\)$")))
        (magit-put-face-property (match-beginning 1) (match-end 1)
                                 'magit-whitespace-warning))
      (when (or (and (eq indent 'tabs)
                     (looking-at (concat prefix "\\( *\t[ \t]*\\)")))
                (and (integerp indent)
                     (looking-at (format "%s\\([ \t]* \\{%s,\\}[ \t]*\\)"
                                         prefix indent))))
        (magit-put-face-property (match-beginning 1) (match-end 1)
                                 'magit-whitespace-warning)))))

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
  (let ((old magit-diff-refine-hunk))
    (setq-local magit-diff-refine-hunk
                (if other
                    (if (eq old 'all) t 'all)
                  (not old)))
    (if (or (eq old 'all)
            (eq magit-diff-refine-hunk 'all))
        (magit-refresh)
      (--when-let magit-current-section
        (when (eq (magit-section-type it) 'hunk)
          (if  magit-diff-refine-hunk
              (magit-diff-refine-hunk it)
            (magit-diff-unrefine-hunk it)))))
    (message "magit-diff-refine-hunk: %s" magit-diff-refine-hunk)))

(defun magit-diff-refine-hunk (hunk)
  (save-excursion
    (goto-char (magit-section-start hunk))
    ;; `diff-refine-hunk' does not handle combined diffs.
    (unless (looking-at "@@@")
      (diff-refine-hunk))))

(defun magit-diff-unrefine-hunk (hunk)
  (remove-overlays (magit-section-start hunk)
                   (magit-section-end hunk)
                   'diff-mode 'fine))

;;;; Refs Mode

(define-derived-mode magit-refs-mode magit-mode "Magit Branch"
  "Mode which lists and compares references.
This mode is documented in info node `(magit)Branches and Remotes'.

\\<magit-refs-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-branch-popup] to see available branch commands.
Type \\[magit-show-commit] or \\[magit-show-or-scroll-up]\
 to visit the commit at point.
Type \\[magit-merge-popup] to merge the branch or commit at point.
Type \\[magit-cherry-pick] to cherry-pick the commit at point.
Type \\[magit-reset-head] to reset HEAD to the commit at point.
\n\\{magit-refs-mode-map}"
  :group 'magit-modes)

;;;###autoload
(defun magit-show-refs (&optional head)
  "List and compare references in a dedicated buffer."
  (interactive (when current-prefix-arg
                 (list (magit-read-rev "Compare branch"
                                       (magit-get-current-branch)))))
  (magit-mode-setup magit-refs-buffer-name-format nil
                    #'magit-refs-mode
                    #'magit-refresh-refs-buffer head))

(defun magit-refresh-refs-buffer (&optional head)
  (magit-insert-section (branchbuf)
    (run-hooks 'magit-refs-sections-hook)))

(defconst magit-wash-branch-line-re
  (concat "^"
          "\\(?:[ \\*]\\) "
          "\\(?1:[^ ]+?\\)"                 ; branch
          "\\(?: +\\)"
          "\\(?2:[0-9a-fA-F]+\\) "          ; sha1
          "\\(?:\\["
          "\\(?4:[^:\n]+?\\)\\(?:: \\)?"    ; upstream
          "\\(?:ahead \\(?5:[0-9]+\\)\\)?"  ; ahead
          "\\(?:, \\)?"
          "\\(?:behind \\(?6:[0-9]+\\)\\)?" ; behind
          "\\] \\)?"
          "\\(?3:.*\\)"))                   ; message

(defvar magit-local-branch-format "%c %-25n %U%m\n")
(defvar magit-remote-branch-format "%c %-25n %m\n")
(defvar magit-tags-format "    %n\n")

(defun magit-insert-local-branches ()
  (magit-insert-section (local nil)
    (magit-insert-heading "Branches:")
    (let ((current  (magit-get-current-branch))
          (branches (magit-list-local-branch-names)))
      (dolist (line (magit-git-lines "branch" "-vv"))
        (string-match magit-wash-branch-line-re line)
        (magit-bind-match-strings
            (branch hash message upstream ahead behind) line
          (magit-insert-branch
           branch current branches
           magit-local-branch-format 'magit-branch-local
           hash message upstream ahead behind))))
    (insert ?\n)))

(defun magit-insert-remote-branches ()
  (dolist (remote (magit-git-lines "remote"))
    (magit-insert-section (remote remote)
      (magit-insert-heading
        (let ((pull (magit-get "remote" remote "url"))
              (push (magit-get "remote" remote "pushurl")))
          (format "%s (%s):" (capitalize remote)
                  (concat pull (and pull push ", ") push))))
      (let ((current  (magit-get-current-branch))
            (branches (magit-list-local-branch-names)))
        (dolist (line (magit-git-lines "branch" "-vvr"))
          (when (string-match magit-wash-branch-line-re line)
            (magit-bind-match-strings (branch hash message) line
              (when (string-match-p (format "^%s/" remote) branch)
                (magit-insert-branch
                 branch current branches
                 magit-remote-branch-format 'magit-branch-remote hash message))))))
      (insert ?\n))))

(defun magit-insert-branch
    (branch current branches format face
            &optional hash message upstream ahead behind)
  (magit-insert-section it (branch branch t)
    (let* ((head  (or (car magit-refresh-args) current "HEAD"))
           (count (string-to-number
                   (magit-git-string
                    "rev-list" "--count" "--right-only"
                    (concat head "..." branch)))))
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
           (?c . ,(cond
                   ((equal branch (car magit-refresh-args))
                    (format "%3s" (if (equal branch current) "@" "#")))
                   ((> count 0)
                    (propertize (format "%3s" (number-to-string count))
                                'face 'magit-dimmed))
                   (t "   ")))
           (?h . ,(or (propertize hash 'face 'magit-hash) ""))
           (?m . ,(or message ""))
           (?n . ,(propertize branch 'face face))
           (?u . ,(or upstream ""))
           (?U . ,(if upstream
                      (format
                       (propertize "[%s%s] " 'face 'magit-dimmed)
                       upstream
                       (if (or ahead behind)
                           (concat ": " (and ahead (format "ahead %s" ahead))
                                   (and ahead behind ", ")
                                   (and behind (format "behind %s" behind)))
                         ""))
                    "")))))
      (when (> count 0)
        (if (magit-section-hidden it)
            (setf (magit-section-washer it)
                  `(lambda ()
                     (let ((magit-insert-section--parent ,it))
                       (magit-insert-cherry-commits-1 ,head ,branch))
                     (insert ?\n)))
          (magit-insert-cherry-commits-1 head branch)
          (insert ?\n))))))

(defun magit-insert-tags ()
  (magit-insert-section (tags)
    (magit-insert-heading "Tags:")
    (dolist (tag (magit-git-lines "tag"))
      (magit-insert-section (tag)
        (magit-insert
         (format-spec magit-tags-format
                      `((?n . ,(propertize tag 'face 'magit-tag)))))))
    (insert ?\n)))

;;; Miscellaneous
;;;; Git Popup

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
  (magit-mode-display-buffer (magit-process-buffer)
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
    (list (magit-read-string (format "Git subcommand (in %s)"
                                     (abbreviate-file-name dir))
                             nil 'magit-git-command-history)
          dir)))

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
                        (--map (cons (concat
                                      key "\\"
                                      (file-name-nondirectory
                                       (directory-file-name
                                        (substring it 0 (- (length key))))))
                                     it)
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
             (magit-file-tracked-p (buffer-file-name))
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
                       (index-file (make-temp-name (expand-file-name "index" toplevel))))
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

;;; magit.el ends soon

(defconst magit-font-lock-keywords
  (eval-when-compile
    `((,(concat "(\\(magit-define-section-jumper\\)\\_>"
                "[ \t'\(]*"
                "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      (,(concat "(" (regexp-opt '("magit-insert-section"
                                  "magit-section-case"
                                  "magit-section-when"
                                  "magit-bind-match-strings"
                                  "magit-with-blob") t)
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
                     (or (and (fboundp 'package-desc-vers) ; < 24.3.50
                              (package-version-join
                               (package-desc-vers
                                (cdr (assq 'magit package-alist)))))
                         (and (fboundp 'package-desc-version) ; >= 24.3.50
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

(define-obsolete-variable-alias 'magit-mode-refresh-buffer-hook 'magit-refresh-buffer-hook)

(provide 'magit)

(require 'magit-blame)
(require 'magit-ediff)
(require 'magit-extras)
(require 'magit-wip)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; magit.el ends here
