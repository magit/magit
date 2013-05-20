;;; magit.el --- control Git from Emacs

;; Copyright (C) 2010 Aaron Culich.
;; Copyright (C) 2010, 2011 Alan Falloon.
;; Copyright (C) 2008, 2010 Alex Ott.
;; Copyright (C) 2008, 2009, 2010 Alexey Voinov.
;; Copyright (C) 2011 Andreas Fuchs.
;; Copyright (C) 2012 Andreas Liljeqvist.
;; Copyright (C) 2011 Andreas Rottmann.
;; Copyright (C) 2011 Andrew Kirkpatrick.
;; Copyright (C) 2011 Andrey Smirnov.
;; Copyright (C) 2011 Bastian Beischer.
;; Copyright (C) 2010 Ben Walton.
;; Copyright (C) 2012 Bradley Wright.
;; Copyright (C) 2012 Brandon W Maister.
;; Copyright (C) 2010, 2011 Brian Warner.
;; Copyright (C) 2012 Bryan Shell.
;; Copyright (C) 2010 Chris Bernard.
;; Copyright (C) 2012 Chris Done.
;; Copyright (C) 2011 Chris Moore.
;; Copyright (C) 2012 Christian Dietrich.
;; Copyright (C) 2010 Christian Kluge.
;; Copyright (C) 2012 Christopher Monsanto.
;; Copyright (C) 2011 Craig Andera.
;; Copyright (C) 2012 Dale Hagglund.
;; Copyright (C) 2012 Damien Cassou.
;; Copyright (C) 2010 Daniel Brockman.
;; Copyright (C) 2008 Daniel Farina.
;; Copyright (C) 2012 Daniel Hackney.
;; Copyright (C) 2010, 2011 Dave Abrahams.
;; Copyright (C) 2010 David Abrahams.
;; Copyright (C) 2009 David Wallin.
;; Copyright (C) 2011 Dominique Quatravaux.
;; Copyright (C) 2011 Eli Barzilay.
;; Copyright (C) 2011, 2012 Eric Davis.
;; Copyright (C) 2011 George Kadianakis.
;; Copyright (C) 2011 Graham Clark.
;; Copyright (C) 2009, 2010, 2011 Hannu Koivisto.
;; Copyright (C) 2012 Hans-Peter Deifel.
;; Copyright (C) 2009 Ian Eure.
;; Copyright (C) 2011 Jasper St. Pierre.
;; Copyright (C) 2011 Jeff Bellegarde.
;; Copyright (C) 2009, 2012 Jesse Alama.
;; Copyright (C) 2009, 2010, 2012 John Wiegley.
;; Copyright (C) 2012, 2013 Jonas Bernoulli.
;; Copyright (C) 2012 Jonathan Roes.
;; Copyright (C) 2011 Julien Danjou.
;; Copyright (C) 2012 Justin Caratzas.
;; Copyright (C) 2011 Kimberly Wolk.
;; Copyright (C) 2010, 2011 Leo.
;; Copyright (C) 2012 Leonardo Etcheverry.
;; Copyright (C) 2011 Lluís Vilanova.
;; Copyright (C) 2011 Luke Amdor.
;; Copyright (C) 2011 Luís Borges de Oliveira.
;; Copyright (C) 2010, 2011 Marc Herbert.
;; Copyright (C) 2008, 2009 Marcin Bachry.
;; Copyright (C) 2011 Marco Craveiro.
;; Copyright (C) 2008, 2009 Marius Vollmer.
;; Copyright (C) 2010 Mark Hepburn.
;; Copyright (C) 2012 Miles Bader.
;; Copyright (C) 2010, 2011, 2012 Moritz Bunkus.
;; Copyright (C) 2010 Nathan Weizenbaum.
;; Copyright (C) 2012 Nic Ferier.
;; Copyright (C) 2012 Nick Alcock.
;; Copyright (C) 2011, 2012 Nicolas Dudebout.
;; Copyright (C) 2011 Ole Arndt.
;; Copyright (C) 2010 Oscar Fuentes.
;; Copyright (C) 2010 Paul Stadig.
;; Copyright (C) 2009 Pavel Holejsovsky.
;; Copyright (C) 2011, 2012 Peter J. Weisberg.
;; Copyright (C) 2009, 2010, 2011, 2013 Phil Jackson.
;; Copyright (C) 2010 Philip Weaver.
;; Copyright (C) 2011 Pieter Praet.
;; Copyright (C) 2012 Raimon Grau.
;; Copyright (C) 2010, 2011, 2012 Ramkumar Ramachandra.
;; Copyright (C) 2010 Remco van 't Veer.
;; Copyright (C) 2009 René Stadler.
;; Copyright (C) 2010 Robert Boone.
;; Copyright (C) 2010 Robin Green.
;; Copyright (C) 2010, 2011 Roger Crew.
;; Copyright (C) 2012 Romain Francoise.
;; Copyright (C) 2012 Ron Parker.
;; Copyright (C) 2012 Ryan C. Thompson.
;; Copyright (C) 2009-2013 Rémi Vanicat.
;; Copyright (C) 2011, 2012 Rüdiger Sonderfeld.
;; Copyright (C) 2012 Samuel Bronson.
;; Copyright (C) 2010 Sean Bryant.
;; Copyright (C) 2009, 2011 Steve Purcell.
;; Copyright (C) 2012 Steven Chow.
;; Copyright (C) 2012 Suhail Shergill.
;; Copyright (C) 2012, 2013 Takafumi Arakaki.
;; Copyright (C) 2010 Thomas Jost.
;; Copyright (C) 2011 Tibor Simko.
;; Copyright (C) 2010, 2012 Timo Juhani Lindfors.
;; Copyright (C) 2010, 2011 Tom Feist.
;; Copyright (C) 2010, 2011, 2012, 2013 Yann Hodique.
;; Copyright (C) 2010 oscar.
;; Copyright (C) 2012 rabio.
;; Copyright (C) 2010, 2011 Ævar Arnfjörð Bjarmason.
;; Copyright (C) 2010 Óscar Fuentes.
;; Copyright (C) 2011 Štěpán Němec.

;; Original Author: Marius Vollmer <marius.vollmer@nokia.com>
;; Former Maintainer: Phil Jackson <phil@shellarchive.co.uk>
;; Maintenance Group: https://github.com/organizations/magit/teams/53130
;;   Currently composed of:
;;   - Phil Jackson
;;   - Peter J Weisberg
;;   - Yann Hodique
;;   - Rémi Vanicat
;; Version: @GIT_DEV_VERSION@
;; Keywords: tools

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

(require 'cl-lib)

(require 'log-edit)
(require 'easymenu)
(require 'diff-mode)
(require 'ansi-color)
(require 'thingatpt)
(require 'ring)
(require 'grep)

;; Silences byte-compiler warnings
(eval-and-compile
  (unless (fboundp 'declare-function)
    (defmacro declare-function (&rest args))))

(eval-when-compile (require 'view))
(declare-function view-mode 'view)
(eval-when-compile (require 'iswitchb))
(declare-function iswitchb-read-buffer 'iswitchb)
(eval-when-compile (require 'ido))
(declare-function ido-completing-read 'ido)
(eval-when-compile (require 'ediff))
(declare-function ediff-cleanup-mess 'ediff)
(eval-when-compile (require 'eshell))
(declare-function eshell-parse-arguments 'eshell)
(eval-when-compile (require 'server))
(declare-function server-running-p 'server)

;; Dummy to be used by the defcustoms when first loading the file.
(cl-eval-when (load eval)
  (defalias 'magit-set-variable-and-refresh 'set-default))


(defgroup magit nil
  "Controlling Git from Emacs."
  :prefix "magit-"
  :group 'tools)

(defcustom magit-git-executable "git"
  "The name of the Git executable."
  :group 'magit
  :type 'string)

(defcustom magit-gitk-executable (concat (file-name-directory magit-git-executable)
                                         "gitk")
  "The name of the Gitk executable."
  :group 'magit
  :type 'string)

(defcustom magit-git-standard-options '("--no-pager")
  "Standard options when running Git."
  :group 'magit
  :type '(repeat string))

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

(defcustom magit-commit-all-when-nothing-staged 'ask
  "Determine what \\[magit-log-edit] does when nothing is staged.

Setting this to nil will make it do nothing, setting it to t will
arrange things so that the actual commit command will use the
\"--all\" option, setting it to `ask' will first ask for
confirmation whether to do this, and setting it to `ask-stage'
will cause all changes to be staged, after a confirmation."
  :group 'magit
  :type '(choice (const :tag "No" nil)
                 (const :tag "Always" t)
                 (const :tag "Ask" ask)
                 (const :tag "Ask to stage everything" ask-stage)))

(defcustom magit-commit-signoff nil
  "Add the \"Signed-off-by:\" line when committing."
  :group 'magit
  :type 'boolean)

(defcustom magit-commit-gpgsign nil
  "Use GPG to sign commits."
  :group 'magit
  :type 'boolean)

(defcustom magit-commit-no-verify nil
  "Bypass the pre-commit and commit-msg hooks when committing."
  :group 'magit
  :type 'boolean)

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

(defcustom magit-log-show-gpg-status nil
  "Display signature verification information as part of the log."
  :group 'magit
  :type 'boolean)

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

(defcustom magit-log-edit-confirm-cancellation nil
  "Require acknowledgment before canceling the log edit buffer."
  :group 'magit
  :type 'boolean)

(defcustom magit-remote-ref-format 'branch-then-remote
  "What format to use for autocompleting refs, in pariticular for remotes.

Autocompletion is used by functions like `magit-checkout',
`magit-interactive-rebase' and others which offer branch name
completion.

The value 'name-then-remote means remotes will be of the form
\"name (remote)\", while the value 'remote-slash-name means that
they'll be of the form \"remote/name\".  I.e. something that's
listed as \"remotes/upstream/next\" by \"git branch -l -a\" will
be \"upstream/next\"."
  :group 'magit
  :type '(choice (const :tag "name (remote)" branch-then-remote)
                 (const :tag "remote/name" remote-slash-branch)))

(defcustom magit-process-connection-type (not (eq system-type 'cygwin))
  "Connection type used for the git process.

If nil, use pipes: this is usually more efficient, and works on Cygwin.
If t, use ptys: this enables magit to prompt for passphrases when needed."
  :group 'magit
  :type '(choice (const :tag "pipe" nil)
                 (const :tag "pty" t)))

(defcustom magit-completing-read-function 'magit-builtin-completing-read
  "Function to be called when requesting input from the user."
  :group 'magit
  :type '(radio (function-item magit-iswitchb-completing-read)
                (function-item magit-ido-completing-read)
                (function-item magit-builtin-completing-read)
                (function :tag "Other")))

(defcustom magit-create-branch-behaviour 'at-head
  "Where magit will create a new branch if not supplied a branchname or ref.

The value 'at-head means a new branch will be created at the tip
of your current branch, while the value 'at-point means magit
will try to find a valid reference at point..."
  :group 'magit
  :type '(choice (const :tag "at HEAD" at-head)
                 (const :tag "at point" at-point)))

(defcustom magit-status-buffer-switch-function 'pop-to-buffer
  "Function for `magit-status' to use for switching to the status buffer.

The function is given one argument, the status buffer."
  :group 'magit
  :type '(radio (function-item switch-to-buffer)
                (function-item pop-to-buffer)
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

(defcustom magit-diff-refine-hunk nil
  "Show fine (word-granularity) differences within diff hunks.

There are three possible settings:

  nil means to never show fine differences

  t means to only show fine differences for the currently
  selected diff hunk

  `all' means to always show fine differences for all displayed diff hunks"
  :group 'magit
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Selected only" t)
                 (const :tag "All" all))
  :set 'magit-set-variable-and-refresh)


(defgroup magit-faces nil
  "Customize the appearance of Magit."
  :prefix "magit-"
  :group 'faces
  :group 'magit)

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
  "Face for lines in a diff that have been added."
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

(defface magit-diff-none
  '((t :inherit diff-context))
  "Face for lines in a diff that are unchanged."
  :group 'magit-faces)

(defface magit-diff-del
  '((t :inherit diff-removed))
  "Face for lines in a diff that have been deleted."
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
  '((t :inherit highlight))
  "Face for highlighting the current item."
  :group 'magit-faces)

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

(defvar magit-custom-options '()
  "List of custom options to pass to Git.
Do not customize this (used in the `magit-key-mode' implementation).")

(defvar magit-read-rev-history nil
  "The history of inputs to `magit-read-rev'.")

(defvar magit-buffer-internal nil
  "Track associated *magit* buffers.
Do not customize this (used in the `magit-log-edit-mode' implementation
to switch back to the *magit* buffer associated with a given commit
operation after commit).")

(defvar magit-back-navigation-history nil
  "History items that will be visited by successively going \"back\".")
(make-variable-buffer-local 'magit-back-navigation-history)
(put 'magit-back-navigation-history 'permanent-local t)

(defvar magit-forward-navigation-history nil
  "History items that will be visited by successively going \"forward\".")
(make-variable-buffer-local 'magit-forward-navigation-history)
(put 'magit-forward-navigation-history 'permanent-local t)

(defvar magit-omit-untracked-dir-contents nil
  "When non-nil magit will only list an untracked directory, not its contents.")

(defvar magit-tmp-buffer-name " *magit-tmp*")

(defvar magit-read-file-hist nil)

(defvar magit-current-indentation nil
  "Indentation highlight used in the current buffer.
This is calculated from `magit-highlight-indentation'.")
(make-variable-buffer-local 'magit-current-indentation)

(defvar magit-bug-report-url
  "http://github.com/magit/magit/issues")

(defconst magit-version "@GIT_DEV_VERSION@"
  "The version of Magit that you're using.")


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
    (define-key map (kbd "!") 'magit-key-mode-popup-running)
    (define-key map (kbd ":") 'magit-git-command)
    (define-key map (kbd "C-x 4 a") 'magit-add-change-log-entry-other-window)
    (define-key map (kbd "L") 'magit-add-change-log-entry-no-option)
    (define-key map (kbd "RET") 'magit-visit-item)
    (define-key map (kbd "SPC") 'magit-show-item-or-scroll-up)
    (define-key map (kbd "DEL") 'magit-show-item-or-scroll-down)
    (define-key map (kbd "C-w") 'magit-copy-item-as-kill)
    (define-key map (kbd "R") 'magit-rebase-step)
    (define-key map (kbd "t") 'magit-key-mode-popup-tagging)
    (define-key map (kbd "r") 'magit-key-mode-popup-rewriting)
    (define-key map (kbd "P") 'magit-key-mode-popup-pushing)
    (define-key map (kbd "f") 'magit-key-mode-popup-fetching)
    (define-key map (kbd "b") 'magit-key-mode-popup-branching)
    (define-key map (kbd "M") 'magit-key-mode-popup-remoting)
    (define-key map (kbd "B") 'magit-key-mode-popup-bisecting)
    (define-key map (kbd "F") 'magit-key-mode-popup-pulling)
    (define-key map (kbd "l") 'magit-key-mode-popup-logging)
    (define-key map (kbd "o") 'magit-key-mode-popup-submodule)
    (define-key map (kbd "$") 'magit-display-process)
    (define-key map (kbd "c") 'magit-log-edit)
    (define-key map (kbd "E") 'magit-interactive-rebase)
    (define-key map (kbd "e") 'magit-ediff)
    (define-key map (kbd "w") 'magit-wazzup)
    (define-key map (kbd "q") 'magit-quit-window)
    (define-key map (kbd "m") 'magit-key-mode-popup-merging)
    (define-key map (kbd "x") 'magit-reset-head)
    (define-key map (kbd "v") 'magit-revert-item)
    (define-key map (kbd "a") 'magit-apply-item)
    (define-key map (kbd "A") 'magit-cherry-pick-item)
    (define-key map (kbd "d") 'magit-diff-working-tree)
    (define-key map (kbd "D") 'magit-diff)
    (define-key map (kbd "-") 'magit-diff-smaller-hunks)
    (define-key map (kbd "+") 'magit-diff-larger-hunks)
    (define-key map (kbd "0") 'magit-diff-default-hunks)
    (define-key map (kbd "h") 'magit-toggle-diff-refine-hunk)
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
    (define-key map (kbd "C") 'magit-add-log)
    (define-key map (kbd "X") 'magit-reset-working-tree)
    (define-key map (kbd "z") 'magit-key-mode-popup-stashing)
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
    (define-key map (kbd "r") 'magit-move-item)
    (define-key map (kbd "k") 'magit-discard-item)
    (define-key map (kbd "T") 'magit-change-what-branch-tracks)
    map))

(defvar magit-diffstat-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") 'magit-diffstat-ediff)
    map))

(defconst magit-bug-report-buffer "*magit-bug-report*"
  "The buffer in which Magit output bug report messages.")

(defun magit-bug-report (str)
  "Asks the user to submit a bug report about the error described
in STR."
  (with-current-buffer (get-buffer-create magit-bug-report-buffer)
    (erase-buffer)
    (insert (format
             (concat
              "Magit unknown error: %s\n Please, with as much"
              " information as possible, file a bug at\n %s\n"
              "- Magit: %s\n"
              "- Emacs: %s")
             str magit-bug-report-url
             magit-version (emacs-version))))
  (switch-to-buffer-other-window magit-bug-report-buffer))

(defun magit-buffer-switch (buf)
  (if (string-match "magit" (buffer-name))
      (switch-to-buffer buf)
    (pop-to-buffer buf)))

;;; Macros

(defmacro magit-with-refresh (&rest body)
  (declare (indent 0))
  `(magit-refresh-wrapper (lambda () ,@body)))

;;; Git features

(defvar magit-have-graph 'unset)
(defvar magit-have-decorate 'unset)
(defvar magit-have-abbrev 'unset)
(make-variable-buffer-local 'magit-have-graph)
(put 'magit-have-graph 'permanent-local t)
(make-variable-buffer-local 'magit-have-decorate)
(put 'magit-have-decorate 'permanent-local t)
(make-variable-buffer-local 'magit-have-abbrev)
(put 'magit-have-abbrev 'permanent-local t)

(defun magit-configure-have-graph ()
  (if (eq magit-have-graph 'unset)
      (let ((res (magit-git-exit-code "log" "--graph" "--max-count=0")))
        (setq magit-have-graph (eq res 0)))))

(defun magit-configure-have-decorate ()
  (if (eq magit-have-decorate 'unset)
      (let ((res (magit-git-exit-code "log" "--decorate=full" "--max-count=0")))
        (setq magit-have-decorate (eq res 0)))))

(defun magit-configure-have-abbrev ()
  (if (eq magit-have-abbrev 'unset)
      (let ((res (magit-git-exit-code "log" "--no-abbrev-commit" "--max-count=0")))
        (setq magit-have-abbrev (eq res 0)))))

;;; Compatibilities

(eval-and-compile
  (defun magit-max-args-internal (function)
    "Return the maximum number of arguments accepted by FUNCTION."
    (if (symbolp function)
        (setq function (symbol-function function)))
    (if (subrp function)
        (let ((max (cdr (subr-arity function))))
          (if (eq 'many max)
              most-positive-fixnum
            max))
      (if (eq 'macro (car-safe function))
          (setq function (cdr function)))
      (let ((arglist (if (byte-code-function-p function)
                         (aref function 0)
                       (cadr function))))
        (if (memq '&rest arglist)
            most-positive-fixnum
          (length (remq '&optional arglist))))))

  (if (functionp 'start-file-process)
      (defalias 'magit-start-process 'start-file-process)
    (defalias 'magit-start-process 'start-process))

  (unless (fboundp 'string-match-p)
    (defun string-match-p (regexp string &optional start)
      "Same as `string-match' but don't change the match data."
      (let ((inhibit-changing-match-data t))
        (string-match regexp string start))))

  (if (fboundp 'with-silent-modifications)
      (defalias 'magit-with-silent-modifications 'with-silent-modifications)
    (defmacro magit-with-silent-modifications (&rest body)
      "Execute body without changing `buffer-modified-p'.
Also, do not record undo information."
      `(set-buffer-modified-p
        (prog1 (buffer-modified-p)
          (let ((buffer-undo-list t)
                before-change-functions
                after-change-functions)
            ,@body)))))

  (if (>= (magit-max-args-internal 'delete-directory) 2)
      (defalias 'magit-delete-directory 'delete-directory)
    (defun magit-delete-directory (directory &optional recursive)
      "Delete a directory named DIRECTORY.
If RECURSIVE is non-nil, recursively delete all of DIRECTORY's
contents as well.  Don't follow symlinks."
      (if (or (file-symlink-p directory)
              (not (file-directory-p directory)))
          (delete-file directory)
        (if recursive
            ;; `directory-files-no-dot-files-regex' borrowed from Emacs 23
            (dolist (file (directory-files directory 'full "\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"))
              (magit-delete-directory file recursive)))
        (delete-directory directory)))))

;;; Utilities

(defun magit-set-variable-and-refresh (symbol value)
  "Set SYMBOL to VALUE and call `magit-refresh-all'."
  (set-default symbol value)
  (magit-refresh-all))

(defun magit-iswitchb-completing-read (prompt choices &optional predicate require-match
                                              initial-input hist def)
  "iswitchb-based completing-read almost-replacement."
  (require 'iswitchb)
  (let ((iswitchb-make-buflist-hook
         (lambda ()
           (setq iswitchb-temp-buflist (if (consp (car choices))
                                           (mapcar #'car choices)
                                         choices)))))
    (iswitchb-read-buffer prompt (or initial-input def) require-match)))

(defun magit-ido-completing-read (prompt choices &optional predicate require-match initial-input hist def)
  "ido-based completing-read almost-replacement."
  (require 'ido)
  (let ((selected (ido-completing-read prompt (if (consp (car choices))
                                                  (mapcar #'car choices)
                                                choices)
                                       predicate require-match initial-input hist def)))
    (if (consp (car choices))
        (or (cdr (assoc selected choices))
            selected)
      selected)))

(defun magit-builtin-completing-read (prompt choices &optional predicate require-match
                                             initial-input hist def)
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

(defun magit-completing-read (prompt collection &optional predicate require-match
                                     initial-input hist def)
  "Call function in `magit-completing-read-function' to read user input.

Read `completing-read' documentation for the meaning of the argument."
  (funcall magit-completing-read-function prompt collection predicate require-match
           initial-input hist def))

(defun magit-use-region-p ()
  (if (fboundp 'use-region-p)
      (use-region-p)
    (and transient-mark-mode mark-active)))

(defun magit-goto-line (line)
  "Like `goto-line' but doesn't set the mark."
  (save-restriction
    (widen)
    (goto-char 1)
    (forward-line (1- line))))

(defun magit-trim-line (str)
  (if (string= str "")
      nil
    (if (equal (elt str (- (length str) 1)) ?\n)
        (substring str 0 (- (length str) 1))
      str)))

(defun magit-split-lines (str)
  (if (string= str "")
      nil
    (let ((lines (nreverse (split-string str "\n"))))
      (if (string= (car lines) "")
          (setq lines (cdr lines)))
      (nreverse lines))))

(defun magit-git-insert (args)
  (insert (magit-git-output args)))

(defun magit-git-output (args)
  (magit-cmd-output magit-git-executable (append magit-git-standard-options args)))

(defun magit-cmd-insert (cmd args)
  (insert (magit-cmd-output cmd args)))

(defun magit-cmd-output (cmd args)
  (with-output-to-string
    (with-current-buffer standard-output
      (apply #'process-file
             cmd
             nil (list t nil) nil
             args))))

(defun magit-git-string (&rest args)
  (magit-trim-line (magit-git-output args)))

(defun magit-git-lines (&rest args)
  (magit-split-lines (magit-git-output args)))

(defun magit-git-exit-code (&rest args)
  (apply #'process-file magit-git-executable nil nil nil
         (append magit-git-standard-options args)))

(defun magit-file-lines (file)
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((rev (nreverse (split-string (buffer-string) "\n"))))
        (nreverse (if (equal (car rev) "")
                      (cdr rev)
                    rev))))))

(defun magit-write-file-lines (file lines)
  (with-temp-buffer
    (dolist (l lines)
      (insert l "\n"))
    (write-file file)))

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

(defun magit-remove-conflicts (alist)
  (let ((dict (make-hash-table :test 'equal))
        (result nil))
    (dolist (a alist)
      (puthash (car a) (cons (cdr a) (gethash (car a) dict))
               dict))
    (maphash (lambda (key value)
               (if (= (length value) 1)
                   (push (cons key (car value)) result)
                 (let ((sub (magit-remove-conflicts
                             (mapcar (lambda (entry)
                                       (let ((dir (directory-file-name
                                                   (substring entry 0 (- (length key))))))
                                         (cons (concat (file-name-nondirectory dir) "/" key)
                                               entry)))
                                     value))))
                   (setq result (append result sub)))))
             dict)
    result))

(defun magit-git-repo-p (dir)
  (file-exists-p (expand-file-name ".git" dir)))

(defun magit-git-dir ()
  "Return the .git directory for the current repository."
  (concat (expand-file-name (magit-git-string "rev-parse" "--git-dir")) "/"))

(defun magit-no-commit-p ()
  "Return non-nil if there is no commit in the current git repository."
  (not (magit-git-string
        "rev-list" "HEAD" "--max-count=1")))

(defun magit-list-repos* (dir level)
  (if (magit-git-repo-p dir)
      (list dir)
    (apply #'append
           (mapcar (lambda (entry)
                     (unless (or (string= (substring entry -3) "/..")
                                 (string= (substring entry -2) "/."))
                       (magit-list-repos* entry (+ level 1))))
                   (and (file-directory-p dir)
                        (< level magit-repo-dirs-depth)
                        (directory-files dir t nil t))))))

(defun magit-list-repos (dirs)
  (magit-remove-conflicts
   (apply #'append
          (mapcar (lambda (dir)
                    (mapcar #'(lambda (repo)
                                (cons (file-name-nondirectory repo)
                                      repo))
                            (magit-list-repos* dir 0)))
                  dirs))))

(defun magit-get-top-dir (cwd)
  (let ((cwd (expand-file-name (file-truename cwd))))
    (when (file-directory-p cwd)
      (let* ((default-directory (file-name-as-directory cwd))
             (cdup (magit-git-string "rev-parse" "--show-cdup")))
        (when cdup
          (file-name-as-directory (expand-file-name cdup cwd)))))))

(defun magit-get-ref (ref)
  (magit-git-string "symbolic-ref" "-q" ref))

(defun magit-get-current-branch ()
  (let* ((head (magit-get-ref "HEAD"))
         (pos (and head (string-match "^refs/heads/" head))))
    (if pos
        (substring head 11)
      nil)))

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
  (let ((tag (magit-git-string
              "describe" "--long" "--tags" "--contains" "HEAD"))
        cnt)
    (save-match-data
      (when tag
        (string-match "\\(.+?\\)\\(?:\\^0\\|~\\([0-9]+\\)\\)?$" tag)
        (setq cnt (match-string 2 tag)
              tag (match-string 1 tag))
        (unless (equal tag (car (magit-get-current-tag t)))
          (if with-distance-p
              (list tag (string-to-number cnt))
            tag))))))

(defun magit-get-remote (branch)
  "Return the name of the remote for BRANCH.
If branch is nil or it has no remote, but a remote named
\"origin\" exists, return that.  Otherwise, return nil."
  (let ((remote (or (and branch (magit-get "branch" branch "remote"))
                    (and (magit-get "remote" "origin" "url") "origin"))))
    (if (string= remote "") nil remote)))

(defun magit-get-current-remote ()
  "Return the name of the remote for the current branch.
If there is no current branch, or no remote for that branch,
but a remote named \"origin\" is configured, return that.
Otherwise, return nil."
  (magit-get-remote (magit-get-current-branch)))

(defun magit-ref-exists-p (ref)
  (= (magit-git-exit-code "show-ref" "--verify" ref) 0))

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
                          (or (magit-get-top-dir default-directory)
                              default-directory)))))

(defun magit-rev-parse (ref)
  "Return the SHA hash for REF."
  (magit-git-string "rev-parse" ref))

(defun magit-ref-ambiguous-p (ref)
  "Return whether or not REF is ambiguous."
  ;; If REF is ambiguous, rev-parse just prints errors,
  ;; so magit-git-string returns nil.
  (not (magit-git-string "rev-parse" "--abbrev-ref" ref)))

(defun magit-name-rev (rev &optional no-trim)
  "Return a human-readable name for REV.
Unlike git name-rev, this will remove tags/ and remotes/ prefixes
if that can be done unambiguously (unless optional arg NO-TRIM is
non-nil).  In addition, it will filter out revs involving HEAD."
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

(defun magit-highlight-line-whitespace ()
  (when (and magit-highlight-whitespace
             (or (derived-mode-p 'magit-status-mode)
                 (not (eq magit-highlight-whitespace 'status))))
    (if (and magit-highlight-trailing-whitespace
             (looking-at "^[-+].*?\\([ \t]+\\)$"))
        (overlay-put (make-overlay (match-beginning 1) (match-end 1))
                     'face 'magit-whitespace-warning-face))
    (if (or (and (eq magit-current-indentation 'tabs)
                 (looking-at "^[-+]\\( *\t[ \t]*\\)"))
            (and (integerp magit-current-indentation)
                 (looking-at (format "^[-+]\\([ \t]* \\{%s,\\}[ \t]*\\)"
                                     magit-current-indentation))))
        (overlay-put (make-overlay (match-beginning 1) (match-end 1))
                     'face 'magit-whitespace-warning-face))))

(defun magit-put-line-property (prop val)
  (put-text-property (line-beginning-position) (line-beginning-position 2)
                     prop val))

(defun magit-format-commit (commit format)
  (magit-git-string "log" "--max-count=1"
                    (concat "--pretty=format:" format)
                    commit))

(defun magit-current-line ()
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defun magit-insert-region (beg end buf)
  (let ((text (buffer-substring-no-properties beg end)))
    (with-current-buffer buf
      (insert text))))

(defun magit-insert-current-line (buf)
  (let ((text (buffer-substring-no-properties
               (line-beginning-position) (line-beginning-position 2))))
    (with-current-buffer buf
      (insert text))))

(defun magit-file-uptodate-p (file)
  (eq (magit-git-exit-code "diff" "--quiet" "--" file) 0))

(defun magit-anything-staged-p ()
  (not (eq (magit-git-exit-code "diff" "--quiet" "--cached") 0)))

(defun magit-everything-clean-p ()
  (and (not (magit-anything-staged-p))
       (eq (magit-git-exit-code "diff" "--quiet") 0)))

(defun magit-commit-parents (commit)
  (cdr (split-string (magit-git-string "rev-list" "-1" "--parents" commit))))

;; XXX - let the user choose the parent

(defun magit-choose-parent-id (commit op)
  (let* ((parents (magit-commit-parents commit)))
    (if (> (length parents) 1)
        (error "Can't %s merge commits" op)
      nil)))

;;; Revisions and ranges

(defvar magit-current-range nil
  "The range described by the current buffer.
This is only non-nil in diff and log buffers.

This has three possible (non-nil) forms.  If it's a string REF or
a singleton list (REF), then the range is from REF to the current
working directory state (or HEAD in a log buffer).  If it's a
pair (START . END), then the range is START..END.")
(make-variable-buffer-local 'magit-current-range)

(defun magit-list-interesting-refs (&optional uninteresting)
  "Return interesting references as given by `git show-ref'.
Removes references matching UNINTERESTING from the
results. UNINTERESTING can be either a function taking a single
argument or a list of strings used as regexps."
  (let ((refs ()))
    (dolist (line (magit-git-lines "show-ref"))
      (if (string-match "[^ ]+ +\\(.*\\)" line)
          (let ((ref (match-string 1 line)))
            (cond ((and (functionp uninteresting)
                        (funcall uninteresting ref)))
                  ((and (not (functionp uninteresting))
                        (cl-loop for i in uninteresting thereis (string-match i ref))))
                  (t
                   (let ((fmt-ref (magit-format-ref ref)))
                     (when fmt-ref
                       (push (cons fmt-ref
                                   (replace-regexp-in-string "^refs/heads/"
                                                             "" ref))
                             refs))))))))
    (nreverse refs)))

(defun magit-format-ref (ref)
  "Convert fully-specified ref REF into its displayable form
according to `magit-remote-ref-format'"
  (cond
   ((null ref)
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
      (format "%s/%s"
              (match-string 1 ref)
              (match-string 2 ref))))))

(defun magit-tree-contents (treeish)
  "Return a list of all files under TREEISH.
TREEISH can be a tree, a commit, or any reference to one of
those."
  (let ((return-value nil))
    (with-temp-buffer
      (magit-git-insert (list "ls-tree" "-r" treeish))
      (if (eql 0 (buffer-size))
          (error "%s is not a commit or tree." treeish))
      (goto-char (point-min))
      (while (search-forward-regexp "\t\\(.*\\)" nil 'noerror)
        (push (match-string 1) return-value)))
    return-value))

(defvar magit-uninteresting-refs '("refs/remotes/\\([^/]+\\)/HEAD$" "refs/stash"))

(defun magit-read-file-from-rev (revision)
  (magit-completing-read (format "Retrieve file from %s: " revision)
                         (magit-tree-contents revision)
                         nil
                         'require-match
                         nil
                         'magit-read-file-hist
                         (if buffer-file-name
                             (let ((topdir-length (length (magit-get-top-dir default-directory))))
                               (substring (buffer-file-name) topdir-length)))))

(defun magit-read-rev (prompt &optional default uninteresting)
  (let* ((interesting-refs (magit-list-interesting-refs
                            (or uninteresting magit-uninteresting-refs)))
         (reply (magit-completing-read (concat prompt ": ") interesting-refs
                                       nil nil nil 'magit-read-rev-history default))
         (rev (or (cdr (assoc reply interesting-refs)) reply)))
    (if (string= rev "")
        nil
      rev)))

(defun magit-read-rev-with-default (prompt &optional no-trim uninteresting)
  "Ask user for revision like `magit-read-rev' but default is set
appropriately depending on context.  If NO-TRIM is non-nil, strip
off prefixes such as \"ref/remotes/\" (see `magit-name-rev').
PROMPT and UNINTERESTING are passed to `magit-read-rev'."
  (magit-read-rev prompt (magit-default-rev no-trim) uninteresting))

(defun magit-read-rev-range (op &optional def-beg def-end)
  (let ((beg (magit-read-rev (format "%s start" op)
                             def-beg)))
    (if (not beg)
        nil
      (save-match-data
        (if (string-match "^\\(.+\\)\\.\\.\\(.+\\)$" beg)
            (cons (match-string 1 beg) (match-string 2 beg))
          (let ((end (magit-read-rev (format "%s end" op) def-end)))
            (cons beg end)))))))

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

(defun magit-default-rev (&optional no-trim)
  (or (magit-name-rev (magit-commit-at-point t) no-trim)
      (let ((branch (magit-guess-branch)))
        (if branch
            (if (string-match "^refs/\\(.*\\)" branch)
                (match-string 1 branch)
              branch)))))

(defun magit-read-remote (&optional prompt def require-match)
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
         (branches (delete nil
                           (mapcar
                            (lambda (b)
                              (and (not (string-match " -> " b))
                                   (string-match (format "^ *%s/\\(.*\\)$"
                                                         (regexp-quote remote)) b)
                                   (match-string 1 b)))
                            (magit-git-lines "branch" "-r"))))
         (reply (magit-completing-read (concat prompt ": ") branches
                                       nil nil nil nil default)))
    (if (string= reply "") nil reply)))

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

(defvar magit-top-section nil
  "The top section of the current buffer.")
(make-variable-buffer-local 'magit-top-section)
(put 'magit-top-section 'permanent-local t)

(defvar magit-old-top-section nil)

(defvar magit-section-hidden-default nil)

(defvar magit-show-diffstat t
  "If non-nil, diff and commit log will display diffstat.")

(defvar magit-diffstat-cached-sections)
(make-variable-buffer-local 'magit-diffstat-cached-sections)
(put 'magit-diffstat-cached-sections 'permanent-local t)


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
    (if old
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
  (declare (indent 2))
  (let ((s (make-symbol "*section*")))
    `(let* ((,s (magit-new-section ,title ,type))
            (magit-top-section ,s))
       (setf (magit-section-beginning ,s) (point))
       ,@body
       (setf (magit-section-end ,s) (point))
       (setf (magit-section-children ,s)
             (nreverse (magit-section-children ,s)))
       ,s)))

(defun magit-set-section (title type start end)
  "Create a new section of title TITLE and type TYPE.
Use the specified START and END positions."
  (let ((section (magit-new-section title type)))
    (setf (magit-section-beginning section) start)
    (setf (magit-section-end section) end)
    section))

(defun magit-set-section-info (info &optional section)
  (setf (magit-section-info (or section magit-top-section)) info))

(defun magit-set-section-needs-refresh-on-show (flag &optional section)
  (setf (magit-section-needs-refresh-on-show
         (or section magit-top-section))
        flag))

(defmacro magit-create-buffer-sections (&rest body)
  "Empty current buffer of text and Magit's sections, and then evaluate BODY."
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
  "Add text-property needed for SECTION."
  (put-text-property (magit-section-beginning section)
                     (magit-section-end section)
                     'magit-section section)
  (dolist (s (magit-section-children section))
    (magit-propertize-section s)))

(defun magit-find-section (path top)
  "Find the section at the path PATH in subsection of section TOP."
  (if (null path)
      top
    (let ((secs (magit-section-children top)))
      (while (and secs (not (equal (car path)
                                   (magit-section-title (car secs)))))
        (setq secs (cdr secs)))
      (and (car secs)
           (magit-find-section (cdr path) (car secs))))))

(defun magit-section-path (section)
  "Return the path of SECTION."
  (if (not (magit-section-parent section))
      '()
    (append (magit-section-path (magit-section-parent section))
            (list (magit-section-title section)))))

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
             (next (if (not (magit-section-hidden current))
                       (magit-find-section-before* pos (magit-section-children current)))
                   (if (not (magit-section-hidden current))
                       (magit-find-section-before* pos (magit-section-children current)))))
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

(defun magit-insert-section (section-title-and-type
                             buffer-title washer cmd &rest args)
  "Run CMD and put its result in a new section.

SECTION-TITLE-AND-TYPE is either a string that is the title of the section
or (TITLE . TYPE) where TITLE is the title of the section and TYPE is its type.

If there is no type, or if type is nil, the section won't be highlighted.

BUFFER-TITLE is the inserted title of the section

WASHER is a function that will be run after CMD.
The buffer will be narrowed to the inserted text.
It should add sectioning as needed for Magit interaction.

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
            (if buffer-title
                (insert (propertize buffer-title 'face 'magit-section-title)
                        "\n"))
            (setq body-beg (point))
            (magit-cmd-insert cmd args)
            (if (not (eq (char-before) ?\n))
                (insert "\n"))
            (if washer
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
      (if (called-interactively-p 'interactive)
          (message "No diffstats section found")))))

(defun magit-for-all-sections (func &optional top)
  "Run FUNC on TOP and recursively on all its children.
Default value for TOP is `magit-top-section'"
  (let ((section (or top magit-top-section)))
    (when section
      (funcall func section)
      (dolist (c (magit-section-children section))
        (magit-for-all-sections func c)))))

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
      (if (< beg end)
          (put-text-property beg end 'invisible hidden)))
    (if (not hidden)
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

(defvar magit-highlighted-section nil)

(defun magit-refine-section (section)
  "Apply temporary refinements to the display of SECTION.
Refinements can be undone with `magit-unrefine-section'."
  (let ((type (and section (magit-section-type section))))
    (cond ((and (eq type 'hunk)
                magit-diff-refine-hunk
                (not (eq magit-diff-refine-hunk 'all)))
           ;; Refine the current hunk to show fine details, using
           ;; diff-mode machinery.
           (save-excursion
             (goto-char (magit-section-beginning magit-highlighted-section))
             (magit-maybe-diff-refine-hunk))))))

(defun magit-unrefine-section (section)
  "Remove refinements to the display of SECTION done by `magit-refine-section'."
  (let ((type (and section (magit-section-type section))))
    (cond ((and (eq type 'hunk)
                magit-diff-refine-hunk
                (not (eq magit-diff-refine-hunk 'all)))
           ;; XXX this should be in some diff-mode function, like
           ;; `diff-unrefine-hunk'
           (remove-overlays (magit-section-beginning section)
                            (magit-section-end section)
                            'diff-mode 'fine)))))

(defvar magit-highlight-overlay nil)

(defun magit-highlight-section ()
  "Highlight current section if it has a type."
  (let ((section (magit-current-section)))
    (when (not (eq section magit-highlighted-section))
      (when magit-highlighted-section
        ;; remove any refinement from previous hunk
        (magit-unrefine-section magit-highlighted-section))
      (setq magit-highlighted-section section)
      (if (not magit-highlight-overlay)
          (let ((ov (make-overlay 1 1)))
            (overlay-put ov 'face 'magit-item-highlight)
            (setq magit-highlight-overlay ov)))
      (if (and section (magit-section-type section))
          (progn
            (magit-refine-section section)
            (move-overlay magit-highlight-overlay
                          (magit-section-beginning section)
                          (magit-section-end section)
                          (current-buffer)))
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
  "Return non-nil if PREFIX is a prefix of LIST.

PREFIX and LIST should both be lists.  If the car of PREFIX is
the symbol `*', then return non-nil if the cdr of PREFIX is a
sublist of LIST (as if `*' matched zero or more arbitrary
elements of LIST)"
  ;;; Very schemish...
  (or (null prefix)
      (if (eq (car prefix) '*)
          (or (magit-prefix-p (cdr prefix) list)
              (and (not (null list))
                   (magit-prefix-p prefix (cdr list))))
        (and (not (null list))
             (equal (car prefix) (car list))
             (magit-prefix-p (cdr prefix) (cdr list))))))

(defmacro magit-section-case (head &rest clauses)
  "Make different action depending of current section.

HEAD is (SECTION INFO &optional OPNAME),
  SECTION will be bind to the current section,
  INFO will be bind to the info's of the current section,
  OPNAME is a string that will be used to describe current action,

CLAUSES is a list of CLAUSE, each clause is (SECTION-TYPE &BODY)
where SECTION-TYPE describe section where BODY will be run.

This returns non-nil if some section matches. If the
corresponding body return a non-nil value, it is returned,
otherwise it returns t.

If no section matches, this returns nil if no OPNAME was given
and throws an error otherwise."
  (declare (indent 1))
  (let ((section (car head))
        (info (cadr head))
        (type (make-symbol "*type*"))
        (context (make-symbol "*context*"))
        (opname (car (cddr head))))
    `(let* ((,section (magit-current-section))
            (,info (and ,section (magit-section-info ,section)))
            (,type (and ,section (magit-section-type ,section)))
            (,context (magit-section-context-type ,section)))
       (cond ,@(mapcar (lambda (clause)
                         (if (eq (car clause) t)
                             `(t (or (progn ,@(cdr clause))
                                     t))
                           (let ((prefix (reverse (car clause)))
                                 (body (cdr clause)))
                             `((magit-prefix-p ',prefix ,context)
                               (or (progn ,@body)
                                   t)))))
                       clauses)
             ,@(when opname
                 `(((run-hook-with-args-until-success
                     ',(intern (format "magit-%s-action-hook" opname))))
                   ((not ,type)
                    (error "Nothing to %s here" ,opname))
                   (t
                    (error "Can't %s a %s"
                           ,opname
                           (or (get ,type 'magit-description)
                               ,type)))))))))

(defmacro magit-section-action (head &rest clauses)
  (declare (indent 1))
  `(magit-with-refresh
     (magit-section-case ,head ,@clauses)))

(defmacro magit-add-action (head &rest clauses)
  "Add additional actions to a pre-existing operator.
The syntax is identical to `magit-section-case', except that
OPNAME is mandatory and specifies the operation to which to add
the actions."
  (declare (indent 1))
  (let ((section (car head))
        (info (nth 1 head))
        (type (nth 2 head)))
    `(add-hook ',(intern (format "magit-%s-action-hook" type))
               (lambda ()
                 ,(macroexpand
                   ;; Don't pass in the opname so we don't recursively
                   ;; run the hook again, and so we don't throw an
                   ;; error if no action matches.
                   `(magit-section-case (,section ,info)
                      ,@clauses))))))

(defun magit-wash-sequence (func)
  "Run FUNC until end of buffer is reached.
FUNC should leave point at the end of the modified region"
  (while (and (not (eobp))
              (funcall func))))

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
                           [&optional stringp]        ; Match the doc string, if present.
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
         (or (run-hook-with-args-until-success
              ',hook ,@(remq '&optional (remq '&rest arglist)))
             ,@instr))
       (put ',fun 'definition-name ',sym)
       (put ',hook 'definition-name ',sym))))

;;; Running commands

(defun magit-set-mode-line-process (str)
  (let ((pr (if str (concat " " str) "")))
    (save-excursion
      (magit-for-all-buffers (lambda ()
                               (setq mode-line-process pr))))))

(defun magit-process-indicator-from-command (comps)
  (if (magit-prefix-p (cons magit-git-executable magit-git-standard-options)
                      comps)
      (setq comps (nthcdr (+ (length magit-git-standard-options) 1) comps)))
  (cond ((or (null (cdr comps))
             (not (member (car comps) '("remote"))))
         (car comps))
        (t
         (concat (car comps) " " (cadr comps)))))

(defvar magit-process nil)
(defvar magit-process-client-buffer nil)
(defvar magit-process-buffer-name "*magit-process*"
  "Buffer name for running git commands.")

(defun magit-run* (cmd-and-args
                   &optional logline noerase noerror nowait input)
  (if (and magit-process
           (get-buffer magit-process-buffer-name))
      (error "Git is already running"))
  (let ((cmd (car cmd-and-args))
        (args (cdr cmd-and-args))
        (dir default-directory)
        (buf (get-buffer-create magit-process-buffer-name))
        (successp nil))
    (magit-set-mode-line-process
     (magit-process-indicator-from-command cmd-and-args))
    (setq magit-process-client-buffer (current-buffer))
    (with-current-buffer buf
      (view-mode 1)
      (set (make-local-variable 'view-no-disable-on-exit) t)
      (setq view-exit-action
            (lambda (buffer)
              (with-current-buffer buffer
                (bury-buffer))))
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (setq default-directory dir)
        (if noerase
            (goto-char (point-max))
          (erase-buffer))
        (insert "$ " (or logline
                         (mapconcat 'identity cmd-and-args " "))
                "\n")
        (cond (nowait
               (setq magit-process
                     (let ((process-connection-type magit-process-connection-type))
                       (apply 'magit-start-process cmd buf cmd args)))
               (set-process-sentinel magit-process 'magit-process-sentinel)
               (set-process-filter magit-process 'magit-process-filter)
               (when input
                 (with-current-buffer input
                   (process-send-region magit-process
                                        (point-min) (point-max)))
                 (process-send-eof magit-process)
                 (sit-for 0.1 t))
               (cond ((= magit-process-popup-time 0)
                      (pop-to-buffer (process-buffer magit-process)))
                     ((> magit-process-popup-time 0)
                      (run-with-timer
                       magit-process-popup-time nil
                       (function
                        (lambda (buf)
                          (with-current-buffer buf
                            (when magit-process
                              (display-buffer (process-buffer magit-process))
                              (goto-char (point-max))))))
                       (current-buffer))))
               (setq successp t))
              (input
               (with-current-buffer input
                 (setq default-directory dir)
                 (setq magit-process
                       ;; Don't use a pty, because it would set icrnl
                       ;; which would modify the input (issue #20).
                       (let ((process-connection-type nil))
                         (apply 'magit-start-process cmd buf cmd args)))
                 (set-process-filter magit-process 'magit-process-filter)
                 (process-send-region magit-process
                                      (point-min) (point-max))
                 (process-send-eof magit-process)
                 (while (equal (process-status magit-process) 'run)
                   (sit-for 0.1 t))
                 (setq successp
                       (equal (process-exit-status magit-process) 0))
                 (setq magit-process nil))
               (magit-set-mode-line-process nil)
               (magit-need-refresh magit-process-client-buffer))
              (t
               (setq successp
                     (equal (apply 'process-file cmd nil buf nil args) 0))
               (magit-set-mode-line-process nil)
               (magit-need-refresh magit-process-client-buffer))))
      (or successp
          noerror
          (error
           "%s ... [Hit %s or see buffer %s for details]"
           (or (with-current-buffer (get-buffer magit-process-buffer-name)
                 (when (re-search-backward
                        (concat "^error: \\(.*\\)" paragraph-separate) nil t)
                   (match-string 1)))
               "Git failed")
           (with-current-buffer magit-process-client-buffer
             (key-description (car (where-is-internal
                                    'magit-display-process))))
           magit-process-buffer-name))
      successp)))

(autoload 'dired-uncache "dired")
(defun magit-process-sentinel (process event)
  (let ((msg (format "%s %s." (process-name process) (substring event 0 -1)))
        (successp (string-match "^finished" event))
        (key (if (buffer-live-p magit-process-client-buffer)
                 (with-current-buffer magit-process-client-buffer
                   (key-description (car (where-is-internal
                                          'magit-display-process))))
               "M-x magit-display-process")))
    (with-current-buffer (process-buffer process)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert msg "\n")
        (message (if successp msg
                   (format "%s Hit %s or see buffer %s for details."
                           msg key (current-buffer)))))
      (unless (memq (process-status process) '(run open))
        (dired-uncache default-directory)))
    (setq magit-process nil)
    (magit-set-mode-line-process nil)
    (when (and (buffer-live-p magit-process-client-buffer)
               (with-current-buffer magit-process-client-buffer
                 (derived-mode-p 'magit-mode)))
      (magit-refresh-buffer magit-process-client-buffer))))

(defun magit-password (proc string)
  "Check if git/ssh asks for a password and ask the user for it."
  (let (ask)
    (cond ((or (string-match "^Enter passphrase for key '\\\(.*\\\)': $" string)
               (string-match "^\\\(.*\\\)'s password:" string)
               (string-match "^Password for '\\\(.*\\\)':" string))
           (setq ask (format "Password for '%s': " (match-string 1 string))))
          ((string-match "^[pP]assword:" string)
           (setq ask "Password:")))
    (when ask
      (process-send-string proc (concat (read-passwd ask nil) "\n")))))

(defun magit-username (proc string)
  "Check if git asks for a username and ask the user for it."
  (when (string-match "^Username for '\\\(.*\\\)':" string)
    (process-send-string proc
                         (concat
                          (read-string (format "Username for '%s': "
                                               (match-string 1 string))
                                       nil nil (user-login-name))
                          "\n"))))

(defun magit-process-filter (proc string)
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (let ((inhibit-read-only t))
      (magit-username proc string)
      (magit-password proc string)
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

(defun magit-run (cmd &rest args)
  (magit-with-refresh
    (magit-run* (cons cmd args))))

(defun magit-run-git (&rest args)
  (magit-with-refresh
    (magit-run* (append (cons magit-git-executable
                              magit-git-standard-options)
                        args))))

(defun magit-run-git-with-input (input &rest args)
  (magit-with-refresh
    (magit-run* (append (cons magit-git-executable
                              magit-git-standard-options)
                        args)
                nil nil nil nil input)))

(defun magit-run-git-async (&rest args)
  (message "Running %s %s" magit-git-executable (mapconcat 'identity args " "))
  (magit-run* (append (cons magit-git-executable
                            magit-git-standard-options)
                      args)
              nil nil nil t))

(defun magit-run-async-with-input (input cmd &rest args)
  (magit-run* (cons cmd args) nil nil nil t input))

(defun magit-display-process ()
  "Display output from most recent git command."
  (interactive)
  (unless (get-buffer magit-process-buffer-name)
    (error "No Git commands have run"))
  (display-buffer magit-process-buffer-name))

;;; Mode

;; We define individual functions (instead of using lambda etc) so
;; that the online help can show something meaningful.

(magit-define-section-jumper untracked "Untracked files")
(magit-define-section-jumper unstaged  "Unstaged changes")
(magit-define-section-jumper staged    "Staged changes")
(magit-define-section-jumper unpushed  "Unpushed commits")

(magit-define-level-shower 1)
(magit-define-level-shower 2)
(magit-define-level-shower 3)
(magit-define-level-shower 4)

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
    ["Commit" magit-log-edit t]
    ["Add log entry" magit-add-log t]
    ["Tag" magit-tag t]
    ["Annotated tag" magit-annotated-tag t]
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
    ["Merge" magit-manual-merge t]
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

(defvar magit-mode-hook nil "Hook run by `magit-mode'.")

(put 'magit-mode 'mode-class 'special)

(defvar magit-refresh-function nil)
(make-variable-buffer-local 'magit-refresh-function)
(put 'magit-refresh-function 'permanent-local t)

(defvar magit-refresh-args nil)
(make-variable-buffer-local 'magit-refresh-args)
(put 'magit-refresh-args 'permanent-local t)

(defvar last-point)

(defun magit-remember-point ()
  (setq last-point (point)))

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
  (if (invisible-p (point))
      (let ((end (magit-invisible-region-end (point))))
        (goto-char (if (= end last-point)
                       (magit-invisible-region-start (point))
                     end))))
  (setq disable-point-adjustment t))

(defun magit-post-command-hook ()
  (magit-correct-point-after-command)
  (magit-highlight-section))

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
  (add-hook 'post-command-hook #'magit-post-command-hook t t)
  (use-local-map magit-mode-map)
  (setq magit-current-indentation (magit-indentation-for default-directory))
  ;; Emacs' normal method of showing trailing whitespace gives weird
  ;; results when `magit-whitespace-warning-face' is different from
  ;; `trailing-whitespace'.
  (if (and magit-highlight-whitespace magit-highlight-trailing-whitespace)
      (setq show-trailing-whitespace nil))
  (run-mode-hooks 'magit-mode-hook))

(defun magit-mode-init (dir submode refresh-func &rest refresh-args)
  (setq default-directory dir
        magit-refresh-function refresh-func
        magit-refresh-args refresh-args)
  (funcall submode)
  (magit-refresh-buffer))

(defun magit-indentation-for (dir)
  (let (result)
    (dolist (pair magit-highlight-indentation)
      (if (string-match-p (car pair) dir)
          (setq result (cdr pair))))
    result))

(defun magit-find-buffer (submode &optional dir)
  (let ((topdir (magit-get-top-dir (or dir default-directory))))
    (cl-find-if (lambda (buf)
                  (with-current-buffer buf
                    (and (eq major-mode submode)
                         default-directory
                         (equal (expand-file-name default-directory)
                                topdir))))
                (buffer-list))))

(defun magit-find-status-buffer (&optional dir)
  (magit-find-buffer 'magit-status-mode dir))

(defun magit-for-all-buffers (func &optional dir)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (if (and (derived-mode-p 'magit-mode)
               (or (null dir)
                   (equal default-directory dir)))
          (funcall func)))))

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
        (if magit-refresh-function
            (apply magit-refresh-function
                   magit-refresh-args))
        (magit-refresh-marked-commits-in-buffer)
        (let ((s (and old-path (magit-find-section old-path magit-top-section))))
          (cond (s
                 (goto-char (magit-section-beginning s))
                 (forward-line section-line)
                 (forward-char line-char))
                (t
                 (magit-goto-line old-line)))
          (dolist (w (get-buffer-window-list (current-buffer)))
            (set-window-point w (point)))
          (magit-highlight-section))))))

(defun magit-string-has-prefix-p (string prefix)
  (eq (compare-strings string nil (length prefix) prefix nil nil) t))

(defun magit-revert-buffers (dir &optional ignore-modtime)
  (dolist (buffer (buffer-list))
    (when (and buffer
               (buffer-file-name buffer)
               ;; don't revert indirect buffers, as the parent will be reverted
               (not (buffer-base-buffer buffer))
               (magit-string-has-prefix-p (buffer-file-name buffer) dir)
               (file-readable-p (buffer-file-name buffer))
               (or ignore-modtime (not (verify-visited-file-modtime buffer)))
               (not (buffer-modified-p buffer)))
      (with-current-buffer buffer
        (condition-case var
            (revert-buffer t t nil)
          (error (let ((signal-data (cadr var)))
                   (cond (t (magit-bug-report signal-data))))))))))

(defun magit-update-vc-modeline (dir)
  "Update the modeline for buffers representable by magit."
  (dolist (buffer (buffer-list))
    (when (and buffer
               (buffer-file-name buffer)
               (magit-string-has-prefix-p (buffer-file-name buffer) dir))
      (with-current-buffer buffer
        (condition-case var
            (vc-find-file-hook)
          (error (let ((signal-data (cadr var)))
                   (cond (t (magit-bug-report signal-data))))))))))

(defvar magit-refresh-needing-buffers nil)
(defvar magit-refresh-pending nil)

(defun magit-refresh-wrapper (func)
  (if magit-refresh-pending
      (funcall func)
    (let* ((dir default-directory)
           (status-buffer (magit-find-status-buffer dir))
           (magit-refresh-needing-buffers nil)
           (magit-refresh-pending t))
      (unwind-protect
          (funcall func)
        (when magit-refresh-needing-buffers
          (magit-revert-buffers dir)
          (dolist (b (if (not (memq status-buffer magit-refresh-needing-buffers))
                         (cons status-buffer magit-refresh-needing-buffers)
                       magit-refresh-needing-buffers))
            (magit-refresh-buffer b)))))))

(defun magit-need-refresh (&optional buffer)
  "Mark BUFFER as needing to be refreshed.
If optional BUFFER is nil, use the current buffer."
  (cl-pushnew (or buffer (current-buffer)) magit-refresh-needing-buffers :test 'eq))

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
  (magit-for-all-buffers #'magit-refresh-buffer default-directory))

;;; Untracked files

(defun magit-wash-untracked-file ()
  (if (looking-at "^? \\(.*\\)$")
      (let ((file (match-string-no-properties 1)))
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

(defun magit-insert-untracked-files ()
  (unless (string= (magit-get "status" "showUntrackedFiles") "no")
    (apply 'magit-git-section
           `(untracked
             "Untracked files:"
             magit-wash-untracked-files
             "ls-files" "--others" "-t" "--exclude-standard"
             ,@(when magit-omit-untracked-dir-contents
                 '("--directory"))))))

;;; Diffs and Hunks

(defvar magit-diff-context-lines 3)

(defun magit-diff-U-arg ()
  (format "-U%d" magit-diff-context-lines))

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
  (interactive "")
  (setq magit-diff-context-lines 3)
  (magit-refresh))

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
  (let* ((old magit-diff-refine-hunk)
         (new
          (if other
              (if (eq old 'all) t 'all)
            (not old))))

    ;; remove any old refining in currently highlighted section
    (when (and magit-highlighted-section old (not (eq old 'all)))
      (magit-unrefine-section magit-highlighted-section))

    ;; set variable to new value locally
    (set (make-local-variable 'magit-diff-refine-hunk) new)

    ;; if now highlighting in "selected only" mode, turn refining back
    ;; on in the current section
    (when (and magit-highlighted-section new (not (eq new 'all)))
      (magit-refine-section magit-highlighted-section))

    ;; `all' mode being turned on or off needs a complete refresh
    (when (or (eq old 'all) (eq new 'all))
      (magit-refresh))))

(defun magit-diff-line-file ()
  (cond ((looking-at "^diff --git ./\\(.*\\) ./\\(.*\\)$")
         (match-string-no-properties 2))
        ((looking-at "^diff --cc +\\(.*\\)$")
         (match-string-no-properties 1))
        (t
         nil)))

(defun magit-wash-diffs ()
  (magit-wash-diffstats)
  (magit-wash-sequence #'magit-wash-diff-or-other-file))

(defun magit-wash-diff-or-other-file ()
  (or (magit-wash-diff)
      (magit-wash-other-file)))

(defun magit-diffstat-ediff ()
  (interactive)
  (magit-goto-diff-section-at-file
   (magit-diff-item-file (magit-current-section)))
  (call-interactively 'magit-ediff))

(defun magit-wash-diffstat (&optional guess)
  (let ((entry-regexp "^ ?\\(.*?\\)\\( +| +.*\\)$"))
    (when (looking-at entry-regexp)
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
            (magit-put-line-property 'keymap magit-diffstat-keymap)

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
          )))))

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
              ;;(magit-put-line-property 'face 'magit-section-title)
              (insert "\n")

              (set (make-local-variable 'magit-diffstat-cached-sections)
                   nil)

              (magit-wash-sequence #'magit-wash-diffstat))
            (setq magit-diffstat-cached-sections
                  (nreverse magit-diffstat-cached-sections))
            (insert "\n")))))))

(defun magit-wash-diffstats-postwork (file &optional section)
  (let ((sec (or section
                 (and (boundp 'magit-diffstat-cached-sections)
                      (pop magit-diffstat-cached-sections)))))
    (when sec
      (let* ((info (magit-section-info sec))
             (begin (nth 3 info))
             (end (nth 4 info)))
        (put-text-property begin end
                          'face 'magit-diff-file-header)
        (magit-set-section-info (list 'diffstat file 'completed)
                                sec)))))

(defun magit-diffstat-item-kind (diffstat)
  (car (magit-section-info diffstat)))

(defun magit-diffstat-item-file (diffstat)
  (let ((file (cadr (magit-section-info diffstat))))
    ;; Git diffstat may shorten long pathname with the prefix "..."
    ;; (e.g. ".../long/sub/dir/file" or "...longfilename")
    (save-match-data
      (if (string-match "\\`\\.\\.\\." file)
          nil
        file))))

(defun magit-diffstat-item-status (diffstat)
  "Return 'completed or 'incomplete depending on the processed status"
  (car (cddr (magit-section-info diffstat))))

(defun magit-wash-other-file ()
  (if (looking-at "^? \\(.*\\)$")
      (let ((file (match-string-no-properties 1)))
        (magit-wash-diffstats-postwork file)
        (delete-region (point) (+ (line-end-position) 1))
        (magit-with-section file 'file
          (magit-set-section-info file)
          (insert "\tNew      " file "\n"))
        t)
    nil))

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
                             (point-marker))))
    (let ((magit-indentation-level (+ magit-indentation-level 1)))
      (save-restriction
        (narrow-to-region first-start second-start)
        (goto-char (point-min))
        (magit-with-section file 'diff
          (magit-wash-diff-section)))
      (save-restriction
        (narrow-to-region second-start (point-max))
        (goto-char (point-min))
        (magit-with-section file 'diff
          (magit-wash-diff-section))))))

(defun magit-wash-diff-section ()
  (cond ((looking-at "^\\* Unmerged path \\(.*\\)")
         (let ((file (match-string-no-properties 1)))
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
             (when (search-forward-regexp "\\(--- \\(.*\\)\n\\+\\+\\+ \\(.*\\)\n\\)" () t)
               (when (match-string 1)
                 (add-text-properties (match-beginning 1) (match-end 1)
                                      '(face magit-diff-hunk-header))
                 (add-text-properties (match-beginning 2) (match-end 2)
                                      '(face magit-diff-file-header))
                 (add-text-properties (match-beginning 3) (match-end 3)
                                      '(face magit-diff-file-header))))
             (goto-char end)
             (let ((magit-section-hidden-default nil))
               (magit-wash-sequence #'magit-wash-hunk))))
         t)
        (t
         nil)))

(defun magit-wash-diff ()
  (let ((magit-section-hidden-default magit-hide-diffs))
    (magit-with-section (magit-current-line) 'diff
      (magit-wash-diff-section))))

(defun magit-diff-item-kind (diff)
  (car (magit-section-info diff)))

(defun magit-diff-item-file (diff)
  (cadr (magit-section-info diff)))

(defun magit-diff-item-file2 (diff)
  (car (cddr (magit-section-info diff))))

(defun magit-diff-item-range (diff)
  (nth 3 (magit-section-info diff)))

(defun magit-wash-hunk ()
  (cond ((looking-at "\\(^@+\\)[^@]*@+.*")
         (let ((n-columns (1- (length (match-string 1))))
               (head (match-string 0))
               (hunk-start-pos (point)))
           (magit-with-section head 'hunk
             (add-text-properties (match-beginning 0) (match-end 0)
                                  '(face magit-diff-hunk-header))
             (forward-line)
             (while (not (or (eobp)
                             (looking-at "^diff\\|^@@")))
               (magit-highlight-line-whitespace)
               (let ((prefix (buffer-substring-no-properties
                              (point) (min (+ (point) n-columns) (point-max))))
                     (line (buffer-substring-no-properties (point) (line-end-position))))
                 (cond ((string-match "^[\\+]+<<<<<<< " line)
                        (magit-put-line-property 'face 'magit-diff-merge-current))
                       ((string-match "^[\\+]+=======" line)
                        (magit-put-line-property 'face 'magit-diff-merge-separator))
                       ((string-match "^[\\+]+|||||||" line)
                        (magit-put-line-property 'face 'magit-diff-merge-diff3-separator))
                       ((string-match "^[\\+]+>>>>>>> " line)
                        (magit-put-line-property 'face 'magit-diff-merge-proposed))
                       ((string-match "\\+" prefix)
                        (magit-put-line-property 'face 'magit-diff-add))
                       ((string-match "-" prefix)
                        (magit-put-line-property 'face 'magit-diff-del))
                       (t
                        (magit-put-line-property 'face 'magit-diff-none))))
               (forward-line)))

           (when (eq magit-diff-refine-hunk 'all)
             (save-excursion
               (goto-char hunk-start-pos)
               (magit-maybe-diff-refine-hunk))))
         t)
        (t
         nil)))

(defun magit-looking-at-combined-diff-p ()
  (looking-at "@@@"))

(defun magit-maybe-diff-refine-hunk ()
  ;; diff-refine-hunk can't handle git's combined diff output (--cc)
  (unless (magit-looking-at-combined-diff-p)
    (diff-refine-hunk)))

(defvar magit-diff-options nil)

(defun magit-insert-diff (file status)
  (let ((cmd magit-git-executable)
        (args (append (list "diff")
                      (list (magit-diff-U-arg))
                      magit-diff-options
                      (list "--" file))))
    (let ((p (point)))
      (magit-git-insert args)
      (if (not (eq (char-before) ?\n))
          (insert "\n"))
      (save-restriction
        (narrow-to-region p (point))
        (goto-char p)
        (cond
         ((eq status 'typechange)
          (magit-insert-diff-title status file file)
          (magit-wash-typechange-section file))
         (t
          (magit-wash-diff-section)))
        (goto-char (point-max))))))

(defvar magit-last-raw-diff nil)
(defvar magit-ignore-unmerged-raw-diffs nil)

(defun magit-wash-raw-diffs ()
  (let ((magit-last-raw-diff nil))
    (magit-wash-sequence #'magit-wash-raw-diff)))

(defun magit-wash-raw-diff ()
  (if (looking-at
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
            (file (match-string-no-properties 4)))
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
        t)
    nil))

(defun magit-hunk-item-diff (hunk)
  (let ((diff (magit-section-parent hunk)))
    (or (eq (magit-section-type diff) 'diff)
        (error "Huh?  Parent of hunk not a diff"))
    diff))

(defun magit-diff-item-insert-header (diff buf)
  (let ((beg (save-excursion
               (goto-char (magit-section-beginning diff))
               (forward-line)
               (point)))
        (end (if (magit-section-children diff)
                 (magit-section-beginning (car (magit-section-children diff)))
               (magit-section-end diff))))
    (magit-insert-region beg end buf)))

(defun magit-insert-diff-item-patch (diff buf)
  (let ((beg (save-excursion
               (goto-char (magit-section-beginning diff))
               (forward-line)
               (point)))
        (end (magit-section-end diff)))
    (magit-insert-region beg end buf)))

(defun magit-insert-hunk-item-patch (hunk buf)
  (magit-diff-item-insert-header (magit-hunk-item-diff hunk) buf)
  (magit-insert-region (magit-section-beginning hunk) (magit-section-end hunk)
                       buf))

(defun magit-insert-hunk-item-region-patch (hunk reverse beg end buf)
  (magit-diff-item-insert-header (magit-hunk-item-diff hunk) buf)
  (save-excursion
    (goto-char (magit-section-beginning hunk))
    (magit-insert-current-line buf)
    (forward-line)
    (let ((copy-op (if reverse "+" "-")))
      (while (< (point) (magit-section-end hunk))
        (if (and (<= beg (point)) (< (point) end))
            (magit-insert-current-line buf)
          (cond ((looking-at " ")
                 (magit-insert-current-line buf))
                ((looking-at copy-op)
                 (let ((text (buffer-substring-no-properties
                              (+ (point) 1) (line-beginning-position 2))))
                   (with-current-buffer buf
                     (insert " " text))))))
        (forward-line))))
  (with-current-buffer buf
    (diff-fixup-modifs (point-min) (point-max))))

(defun magit-hunk-item-is-conflict-p (hunk)
  ;;; XXX - Using the title is a bit too clever...
  (string-match "^diff --cc"
                (magit-section-title (magit-hunk-item-diff hunk))))

(defun magit-hunk-item-target-line (hunk)
  (save-excursion
    (beginning-of-line)
    (let ((line (line-number-at-pos)))
      (goto-char (magit-section-beginning hunk))
      (if (not (looking-at "@@+ .* \\+\\([0-9]+\\)\\(,[0-9]+\\)? @@+"))
          (error "Hunk header not found"))
      (let ((target (string-to-number (match-string 1))))
        (forward-line)
        (while (< (line-number-at-pos) line)
          ;; XXX - deal with combined diffs
          (if (not (looking-at "-"))
              (setq target (+ target 1)))
          (forward-line))
        target))))

(defvar magit-file-name ()
  "Name of file the buffer shows a different version of.")
(make-variable-buffer-local 'magit-file-name)

(defvar magit-show-current-version ()
  "Which version of MAGIT-FILE-NAME is shown in this buffer.")
(make-variable-buffer-local 'magit-show-current-version)

(defun magit-save-index ()
  "Add the content of current file as if it was the index."
  (interactive)
  (unless (eq magit-show-current-version 'index)
    (error "Current buffer doesn't visit the index version of a file"))
  (when (y-or-n-p (format "Stage current version of %s" magit-file-name))
    (let ((buf (current-buffer))
          (name (concat (magit-git-dir) "magit-add-index")))
      (with-temp-file name
        (insert-buffer-substring buf))
      (let ((hash
             (magit-git-string "hash-object" "-t" "blob" "-w" (concat "--path=" magit-file-name) "--" name))
            (perm (substring (magit-git-string "ls-files" "-s" magit-file-name)
                             0 6)))
        (magit-run-git "update-index" "--cacheinfo" perm hash magit-file-name)))))

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
  (interactive (let* ((revision (magit-read-rev "Retrieve file from revision"))
                      (filename (magit-read-file-from-rev revision)))
                 (list revision filename t current-prefix-arg)))
  (if (eq commit 'working)
      (find-file-noselect filename)
    (let ((buffer (create-file-buffer (format "%s.%s" filename (replace-regexp-in-string ".*/" "" (prin1-to-string commit t))))))
      (cond
       ((eq commit 'index)
        (let ((checkout-string (magit-git-string "checkout-index"
                                                 "--temp"
                                                 filename)))
          (string-match "^\\(.*\\)\t" checkout-string)
          (with-current-buffer buffer
            (let ((tmpname (match-string 1 checkout-string)))
              (magit-with-silent-modifications
               (insert-file-contents tmpname nil nil nil t))
              (delete-file tmpname)))))
       (t
        (with-current-buffer buffer
          (magit-with-silent-modifications
           (magit-git-insert (list "cat-file" "-p"
                                   (concat commit ":" filename)))))))
      (with-current-buffer buffer
        (let ((buffer-file-name filename))
          (normal-mode)
          (setq magit-file-name filename)
          (setq magit-show-current-version commit))
        (goto-char (point-min)))
      (if select
          (if prefix
              (switch-to-buffer buffer)
            (switch-to-buffer-other-window buffer))
        buffer))))

(defmacro with-magit-tmp-buffer (var &rest body)
  (declare (indent 1)
           (debug (symbolp &rest form)))
  `(let ((,var (generate-new-buffer magit-tmp-buffer-name)))
     (unwind-protect
         (progn ,@body)
       (kill-buffer ,var))))

(defun magit-apply-diff-item (diff &rest args)
  (when (zerop magit-diff-context-lines)
    (setq args (cons "--unidiff-zero" args)))
  (with-magit-tmp-buffer tmp
    (magit-insert-diff-item-patch diff tmp)
    (apply #'magit-run-git-with-input tmp
           "apply" (append args (list "-")))))

(defun magit-apply-hunk-item* (hunk reverse &rest args)
  "Apply single hunk or part of a hunk to the index or working file.

This function is the core of magit's stage, unstage, apply, and
revert operations.  HUNK (or the portion of it selected by the
region) will be applied to either the index, if \"--cached\" is a
member of ARGS, or to the working file otherwise."
  (let ((zero-context (zerop magit-diff-context-lines))
        (use-region (magit-use-region-p)))
    (when zero-context
      (setq args (cons "--unidiff-zero" args)))
    (when reverse
      (setq args (cons "--reverse" args)))
    (when (and use-region zero-context)
      (error (concat "Not enough context to partially apply hunk.  "
                     "Use `+' to increase context.")))
    (with-magit-tmp-buffer tmp
      (if use-region
          (magit-insert-hunk-item-region-patch
           hunk reverse (region-beginning) (region-end) tmp)
        (magit-insert-hunk-item-patch hunk tmp))
      (apply #'magit-run-git-with-input tmp
             "apply" (append args (list "-"))))))

(defun magit-apply-hunk-item (hunk &rest args)
  (apply #'magit-apply-hunk-item* hunk nil args))

(defun magit-apply-hunk-item-reverse (hunk &rest args)
  (apply #'magit-apply-hunk-item* hunk t args))

(magit-define-inserter unstaged-changes (title)
  (let ((magit-hide-diffs t)
        (magit-current-diff-range (cons 'index 'working)))
    (let ((magit-diff-options (append '() magit-diff-options)))
      (magit-git-section 'unstaged title 'magit-wash-raw-diffs
                         "diff-files"))))

(magit-define-inserter staged-changes (staged no-commit)
  (let ((magit-current-diff-range (cons "HEAD" 'index)))
    (when staged
      (let ((magit-hide-diffs t)
            (base (if no-commit
                      (magit-git-string "mktree")
                    "HEAD")))
        (let ((magit-diff-options (append '("--cached") magit-diff-options))
              (magit-ignore-unmerged-raw-diffs t))
          (magit-git-section 'staged "Staged changes:" 'magit-wash-raw-diffs
                             "diff-index" "--cached"
                             base))))))

;;; Logs and Commits

;; Note: making this a plain defcustom would probably let users break
;; the parser too easily
(defvar magit-git-log-options
  (list
   "--pretty=format:* %h %s"
   (format "--abbrev=%s" magit-sha1-abbrev-length)))
;; --decorate=full otherwise some ref prefixes are stripped
;;  '("--pretty=format:* %H%d %s" "--decorate=full"))

(defconst magit-unpushed-or-unpulled-commit-re
  (concat "^\\* "
          "\\([0-9a-fA-F]+\\) " ;; sha
          "\\(.*\\)$"           ;; message
          )
  "Regexp for parsing format in `magit-git-log-options'.")

;;
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
   "\\([BG]\\)?"                                    ; gpg     (4)
   "\\(\\[.*?\\]\\)"                                ; author  (5)
   "\\(\\[.*?\\]\\)"                                ; date    (6)
   "\\)?"
   "\\(.+\\)?$"                                     ; msg     (7)
   ))

(defconst magit-log-longline-re
  (concat
   ;; use \0 delimiter (from -z option) to identify commits. this prevents
   ;; commit messages containing lines like "commit 00000" from polluting the
   ;; display
   "\\(?:\\`\\|\0\\)"
   "\\(\\(?:[---_\\*|/.] ?\\)+ *\\)"               ; graph   (1)
   "\\(?:"
   "commit "
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
   "\\(.+\\)?$"                                    ; msg     (4)
   ))

(defvar magit-present-log-line-function 'magit-present-log-line
  "The function to use when generating a log line.
It takes four args: CHART, SHA1, REFS and MESSAGE.  The function
must return a string which will represent the log line.")

(defun magit-log-get-bisect-state-color (suffix)
  (if (string= suffix "bad")
      (list suffix 'magit-log-head-label-bisect-bad)
    (list suffix 'magit-log-head-label-bisect-good)))

(defun magit-log-get-patches-color (suffix)
  (list (and (string-match ".+/\\(.+\\)" suffix)
             (match-string 1 suffix))
        'magit-log-head-label-patches))

(defvar magit-log-remotes-color-hook nil)

(defun magit-log-get-remotes-color (suffix)
  (or
   (run-hook-with-args-until-success
    'magit-log-remotes-color-hook suffix)
   (list suffix 'magit-log-head-label-remote)))

(defvar magit-refs-namespaces
  '(("tags" . magit-log-head-label-tags)
    ("remotes" magit-log-get-remotes-color)
    ("heads" . magit-log-head-label-local)
    ("patches" magit-log-get-patches-color)
    ("bisect" magit-log-get-bisect-state-color)))

(defun magit-ref-get-label-color (r)
  (let ((uninteresting (cl-loop for re in magit-uninteresting-refs
                                thereis (string-match re r))))
    (if uninteresting (list nil nil)
      (let* ((ref-re "\\(?:tag: \\)?refs/\\(?:\\([^/]+\\)/\\)?\\(.+\\)")
             (label (and (string-match ref-re r)
                         (match-string 2 r)))
             (res (let ((colorizer
                         (cdr (assoc (match-string 1 r)
                                     magit-refs-namespaces))))
                    (cond ((null colorizer)
                           (list r 'magit-log-head-label-default))
                          ((symbolp colorizer)
                           (list label colorizer))
                          ((listp colorizer)
                           (funcall (car colorizer)
                                    (match-string 2 r)))
                          (t
                           (list r 'magit-log-head-label-default))))))
        res))))

(defun magit-present-log-line (line)
  "The default log line generator."
  (let ((graph (magit-log-line-chart line))
        (sha1 (magit-log-line-sha1 line))
        (refs (magit-log-line-refs line))
        (author (magit-log-line-author line))
        (date (magit-log-line-date line))
        (message (magit-log-line-msg line))
        (gpg-status (magit-log-line-gpg line)))
    (let* ((string-refs
            (when refs
              (let ((colored-labels
                     (delete nil
                             (mapcar (lambda (r)
                                       (cl-destructuring-bind (label face)
                                           (magit-ref-get-label-color r)
                                         (and label
                                              (propertize label 'face face))))
                                     refs))))
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
      (if (and magit-log-show-author-date
               author date)
          (magit-log-make-author-date-overlay author date))
      lhs)))

(defcustom magit-log-author-date-max-length 25
  "max of author-date margin length."
  :type 'integer
  :group 'magit)

(defvar magit-log-author-date-string-length nil
  "only use in `*magit-log*' buffer.")
(make-variable-buffer-local 'magit-log-author-date-string-length)

(defvar magit-log-author-string-length nil
  "only use in `*magit-log*' buffer.")
(make-variable-buffer-local 'magit-log-author-string-length)

(defvar magit-log-date-string-length nil
  "only use in `*magit-log*' buffer.")
(make-variable-buffer-local 'magit-log-date-string-length)

(defvar magit-log-author-date-overlay nil
  "only use in `*magit-log*' buffer.")
(make-variable-buffer-local 'magit-log-author-date-overlay)

(defun magit-log-make-author-date-overlay (author date)
  (let ((overlay (make-overlay (point) (1+ (point)))))
    (setq author (propertize author 'face 'magit-log-author)
          date (delete "ago" (split-string date "[ ,]+"))
          date (propertize (concat (format "%2s %5s"
                                           (nth 0 date)
                                           (nth 1 date))
                                   (if (nth 2 date)
                                       (format " %2s %1.1s "
                                               (nth 2 date)
                                               (nth 3 date))))
                           'face 'magit-log-date))
    (overlay-put overlay 'magit-log-overlay (cons author date))
    (overlay-put overlay 'evaporate t)
    (setq magit-log-author-date-overlay
          (cons overlay magit-log-author-date-overlay))
    (if (> (length author) magit-log-author-string-length)
        (setq magit-log-author-string-length (length author)))
    (if (> (length date) magit-log-date-string-length)
        (setq magit-log-date-string-length (length date)))))

(defun magit-log-set-author-date-overlays ()
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
      (mapc
       #'(lambda (overlay)
           (let* ((data (overlay-get overlay 'magit-log-overlay))
                  (author (car data))
                  (date (cdr data))
                  (author-date
                   (format format-string
                           (if (< author-length (length author))
                               (concat
                                (substring author
                                           0 (1- author-length))
                                (propertize "-" 'face
                                            'magit-log-author-date-cutoff))
                             author)
                           date)))
             (overlay-put overlay 'before-string
                          (propertize " " 'display
                                      (list '(margin right-margin)
                                            author-date)))))
       magit-log-author-date-overlay)
      (setq magit-log-author-date-string-length max-length))))

(defvar magit-log-buffer-name "*magit-log*"
  "Buffer name for display of log entries.")

(defun magit-log-display-author-date ()
  (when (derived-mode-p 'magit-log-mode)
    (set-window-margins nil
                            (car (window-margins))
                            magit-log-author-date-string-length)))

(defun magit-log-initialize-author-date-overlay ()
  (when (derived-mode-p 'magit-log-mode)
    (setq magit-log-author-date-string-length 0
          magit-log-author-string-length 0
          magit-log-date-string-length 0
          magit-log-author-date-overlay nil)
    (remove-hook 'window-configuration-change-hook
                 'magit-log-display-author-date t)))

(defun magit-log-create-author-date-overlay ()
  (when (derived-mode-p 'magit-log-mode)
    (magit-log-set-author-date-overlays)
    (magit-log-display-author-date)
    (when magit-log-author-date-overlay
      (add-hook 'window-configuration-change-hook
                'magit-log-display-author-date
                nil t))))

(defvar magit-log-count ()
  "Internal var used to count the number of logs actually added in a buffer.")

(defmacro magit-create-log-buffer-sections (&rest body)
  "Empty current buffer of text and magit's section, and then evaluate BODY.

if the number of logs inserted in the buffer is `magit-log-cutoff-length'
insert a line to tell how to insert more of them"
  (declare (indent 0))
  `(let ((magit-log-count 0) (inhibit-read-only t))
     (magit-create-buffer-sections
       (magit-with-section 'log nil
         ,@body
         (if (= magit-log-count magit-log-cutoff-length)
             (magit-with-section "longer"  'longer
               (insert "type \"e\" to show more logs\n")))))))

(cl-defstruct magit-log-line
  chart sha1 author date msg refs gpg)

(defun magit-parse-log-line (line style)
  (let ((remove-surrounding-braces
         (lambda (string)
           (when string
             (replace-regexp-in-string "\\(^\\[\\)\\|\\(\\]$\\)" "" string))))
        (match-style-string
         (lambda (short-pos long-pos)
           (match-string (if (eq style 'long) long-pos short-pos) line)))
        (line-re (cond ((eq style 'long) magit-log-longline-re)
                         (t magit-log-oneline-re))))
    (when (string-match line-re line)
      (make-magit-log-line
       :chart (funcall match-style-string 1 1)
       :sha1 (funcall match-style-string 2 2)
       :author (funcall remove-surrounding-braces
                        (when (not (eq style 'long)) (match-string 5 line)))
       :date (funcall remove-surrounding-braces
                      (when (not (eq style 'long)) (match-string 6 line)))
       :gpg (when (not (eq style 'long))
              (match-string 4 line))
       :msg (funcall match-style-string 7 4)
       :refs (when (funcall match-style-string 3 3)
               (delq nil
                     (mapcar
                      (lambda (s)
                        (and (not
                              (or (string= s "tag:")
                                  (string= s "HEAD"))) ; as of 1.6.6
                             s))
                      (split-string (funcall match-style-string 3 3)
                                    "[(), ]" t))))))))

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
    (magit-log-initialize-author-date-overlay)
    (magit-wash-sequence (apply-partially 'magit-wash-log-line style))
    (magit-log-create-author-date-overlay)))

(defun magit-wash-color-log (&optional style)
  (let ((ansi-color-apply-face-function
         (lambda (beg end face)
           (when face
             (put-text-property beg end 'font-lock-face face)))))
    (ansi-color-apply-on-region (point-min) (point-max)))
  (magit-wash-log style))

(defvar magit-currently-shown-commit nil)

(defun magit-wash-commit ()
  (let ((magit-current-diff-range)
        (merge-commit))
    (when (looking-at "^commit \\([0-9a-fA-F]\\{40\\}\\)")
      (setq magit-current-diff-range (match-string 1))
      (add-text-properties (match-beginning 1) (match-end 1)
                           '(face magit-log-sha1)))
    (cond
     ((search-forward-regexp "^Merge: \\([0-9a-fA-F]+\\) \\([0-9a-fA-F]+\\)$" nil t)
      (setq magit-current-diff-range (cons (cons (match-string 1)
                                                 (match-string 2))
                                           magit-current-diff-range)
            merge-commit t)
      (let ((first (magit-set-section nil 'commit (match-beginning 1) (match-end 1)))
            (second (magit-set-section nil 'commit (match-beginning 2) (match-end 2))))
        (magit-set-section-info (match-string 1) first)
        (magit-set-section-info (match-string 2) second))
      (make-commit-button (match-beginning 1) (match-end 1))
      (make-commit-button (match-beginning 2) (match-end 2)))
     (t
      (setq magit-current-diff-range (cons (concat magit-current-diff-range "^")
                                           magit-current-diff-range)
            merge-commit nil)))

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
            (search-forward-regexp "\\(\\b[0-9a-fA-F]\\{4,40\\}\\b\\)\\|\\(^diff\\)" nil 'noerror)
            (not (match-string 2)))
      (let ((sha1 (match-string 1))
            (start (match-beginning 1))
            (end (match-end 1)))
        (when (string-equal "commit" (magit-git-string "cat-file" "-t" sha1))
          (make-commit-button start end)
          (let ((section (magit-set-section sha1 'commit start end)))
            (magit-set-section-info sha1 section)))))
    (beginning-of-line)
    (when (looking-at "^diff")
      (magit-wash-diffs))
    (goto-char (point-max))
    (insert "\n")
    (if magit-back-navigation-history
        (magit-with-section "[back]" 'button
          (insert-text-button "[back]"
                              'help-echo "Previous commit"
                              'action 'magit-show-commit-backward
                              'follow-link t
                              'mouse-face 'magit-item-highlight)))
    (insert " ")
    (if magit-forward-navigation-history
        (magit-with-section "[forward]" 'button
          (insert-text-button "[forward]"
                              'help-echo "Next commit"
                              'action 'magit-show-commit-forward
                              'follow-link t
                              'mouse-face 'magit-item-highlight)))))

(defun make-commit-button (start end)
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
  (magit-configure-have-abbrev)
  (magit-configure-have-decorate)
  (magit-create-buffer-sections
    (apply #'magit-git-section nil nil
           'magit-wash-commit
           "log"
           "--max-count=1"
           "--pretty=medium"
           `(,@(if magit-have-abbrev (list "--no-abbrev-commit"))
             ,@(if magit-have-decorate (list "--decorate=full"))
             ,@(if magit-show-diffstat (list "--stat"))
             "--cc"
             "-p" ,commit))))

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
  (unless (eql 0 (magit-git-exit-code "cat-file" "commit" commit))
    (error "%s is not a commit" commit))
  (let ((dir default-directory)
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
    (if select
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

(defvar magit-marked-commit nil)

(defvar magit-mark-overlay nil)
(make-variable-buffer-local 'magit-mark-overlay)
(put 'magit-mark-overlay 'permanent-local t)

(defun magit-refresh-marked-commits ()
  (magit-for-all-buffers #'magit-refresh-marked-commits-in-buffer))

(defun magit-refresh-marked-commits-in-buffer ()
  (if (not magit-mark-overlay)
      (let ((ov (make-overlay 1 1)))
        (overlay-put ov 'face 'magit-item-mark)
        (setq magit-mark-overlay ov)))
  (delete-overlay magit-mark-overlay)
  (magit-for-all-sections
   (lambda (section)
     (when (and (eq (magit-section-type section) 'commit)
                (equal (magit-section-info section)
                       magit-marked-commit))
       (move-overlay magit-mark-overlay
                     (magit-section-beginning section)
                     (magit-section-end section)
                     (current-buffer))))))

(defun magit-set-marked-commit (commit)
  (setq magit-marked-commit commit)
  (magit-refresh-marked-commits))

(defun magit-marked-commit ()
  (or magit-marked-commit
      (error "No commit marked")))

(defun magit-remote-branch-name (remote branch)
  "Get the name of the branch BRANCH on remote REMOTE."
  (if (string= remote ".")
      branch
    (concat remote "/" branch)))


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


(magit-define-inserter unpulled-commits (remote branch)
  (when remote
    (apply #'magit-git-section
           'unpulled "Unpulled commits:" #'magit-wash-unpulled-or-unpushed "log"
           (append magit-git-log-options
                   (list
                    (format "HEAD..%s" (magit-remote-branch-name remote branch)))))))

(magit-define-inserter unpushed-commits (remote branch)
  (when remote
    (apply #'magit-git-section
           'unpushed "Unpushed commits:" #'magit-wash-unpulled-or-unpushed "log"
           (append magit-git-log-options
                   (list
                    (format "%s..HEAD" (magit-remote-branch-name remote branch)))))))

(defun magit-remote-branch-for (local-branch &optional fully-qualified-name)
  "Guess the remote branch name that LOCAL-BRANCH is tracking.
Gives a fully qualified name (e.g., refs/remotes/origin/master)
if FULLY-QUALIFIED-NAME is non-nil."
  (let ((merge  (magit-get "branch" local-branch "merge"))
        (remote (magit-get "branch" local-branch "remote")))
    (save-match-data
      (when (and merge remote
                 (string-match "^refs/heads/\\(.+\\)" merge))
        (concat (when fully-qualified-name
                  (if (string= "." remote)
                      "refs/heads/"
                    (concat "refs/remotes/" remote "/")))
                (match-string 1 merge))))))

;;; Status

(defvar magit-remote-string-hook nil)

(defun magit-remote-string (remote remote-branch remote-rebase)
  (cond
   ((and (string= "." remote) remote-branch)
    (concat
     (when remote-rebase "onto ")
     "branch "
     (propertize remote-branch 'face 'magit-branch)))
   ((and remote remote-branch)
    (concat
     (when remote-rebase "onto ")
     (propertize remote-branch 'face 'magit-branch)
     " @ " remote
     " (" (magit-get "remote" remote "url") ")"))
   (t
    (run-hook-with-args-until-success 'magit-remote-string-hook))))

(declare-function magit--bisect-info-for-status "magit-bisect" (branch))

(defvar magit-status-line-align-to 9)

(defun magit-insert-status-line (keyword string &rest args)
  (insert keyword ":"
          (make-string (max 1 (- magit-status-line-align-to
                                 (length keyword))) ?\ )
          (apply 'format string args) "\n"))

(defun magit-refresh-status ()
  (magit-create-buffer-sections
    (magit-with-section 'status nil
      (let* ((branch (magit-get-current-branch))
             (remote (and branch (magit-get "branch" branch "remote")))
             (remote-rebase (and branch (magit-get-boolean "branch" branch "rebase")))
             (remote-branch (or (and branch (magit-remote-branch-for branch)) branch))
             (remote-string (magit-remote-string remote remote-branch remote-rebase))
             (head (magit-git-string
                    "log"
                    "--max-count=1"
                    "--abbrev-commit"
                    (format "--abbrev=%s" magit-sha1-abbrev-length)
                    "--pretty=oneline"))
             (no-commit (not head))
             (merge-heads (magit-file-lines (concat (magit-git-dir) "MERGE_HEAD")))
             (current-tag (magit-get-current-tag t))
             (next-tag (magit-get-next-tag t))
             (both-tags (and current-tag next-tag t))
             (rebase (magit-rebase-info)))
        (when remote-string
          (magit-insert-status-line "Remote" remote-string))
        (magit-insert-status-line
         "Local" "%s %s"
         (propertize (magit--bisect-info-for-status branch)
                     'face 'magit-branch)
         (abbreviate-file-name default-directory))
        (magit-insert-status-line
         "Head" (if no-commit "nothing commited (yet)" head))
        (when (or current-tag next-tag)
          (magit-insert-status-line
           (if both-tags "Tags" "Tag")
           (concat
            (and current-tag
                 (concat
                  (propertize (car current-tag) 'face 'magit-tag)
                  (and (> (cadr current-tag) 0)
                       (concat " ("
                               (propertize (format "%s" (cadr current-tag))
                                           'face 'magit-branch)
                               " behind)"))))
            (and both-tags ", ")
            (and next-tag
                 (concat
                  (propertize (car next-tag) 'face 'magit-tag)
                  (and (> (cadr next-tag) 0)
                       (concat " ("
                               (propertize (format "%s" (cadr next-tag))
                                           'face 'magit-tag)
                               " ahead)")))))))
        (when merge-heads
          (magit-insert-status-line
           "Merging"
           (mapconcat 'identity (mapcar 'magit-name-rev merge-heads) ", ")))
        (when rebase
          (apply 'magit-insert-status-line
                 "Rebasing"
                 " onto %s (%s of %s); Press \"R\" to Abort, Skip, or Continue\n"
                 rebase))
        (insert "\n")
        (magit-git-exit-code "update-index" "--refresh")
        (magit-insert-stashes)
        (magit-insert-untracked-files)
        (magit-insert-pending-changes)
        (magit-insert-pending-commits)
        (magit-insert-unpulled-commits remote remote-branch)
        (let ((staged (or no-commit (magit-anything-staged-p))))
          (magit-insert-unstaged-changes
           (if staged "Unstaged changes:" "Changes:"))
          (magit-insert-staged-changes staged no-commit))
        (magit-insert-unpushed-commits remote remote-branch))))
  (run-hooks 'magit-refresh-status-hook))

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
        (magit-run* (list magit-git-executable "init"))))))

(define-derived-mode magit-status-mode magit-mode "Magit"
  "Mode for looking at git status.

\\{magit-status-mode-map}"
  :group 'magit)

(defvar magit-default-directory nil)

(defun magit-save-some-buffers (&optional msg pred)
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
        (magit-default-directory default-directory))
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
                       (or (magit-get-top-dir default-directory)
                           (magit-read-top-dir nil)))))
  (let ((topdir (magit-get-top-dir dir)))
    (unless topdir
      (when (y-or-n-p (format "There is no Git repository in %S.  Create one? "
                              dir))
        (magit-init dir)
        (setq topdir (magit-get-top-dir dir))))
    (when topdir
      (let ((default-directory topdir))
        (magit-save-some-buffers))
      (let ((buf (or (magit-find-status-buffer topdir)
                     (generate-new-buffer
                      (concat "*magit: "
                              (file-name-nondirectory
                               (directory-file-name topdir)) "*")))))
        (funcall magit-status-buffer-switch-function buf)
        (magit-mode-init topdir 'magit-status-mode #'magit-refresh-status)))))

(magit-define-command automatic-merge (revision)
  "Merge REVISION into the current 'HEAD'; commit unless merge fails.
\('git merge REVISION')."
  (interactive (list (magit-read-rev "Merge" (magit-guess-branch))))
  (if revision
      (magit-run-git "merge" (magit-rev-to-git revision))))

(magit-define-command manual-merge (revision)
  "Merge REVISION into the current 'HEAD'; commit unless merge fails.
\('git merge REVISION')."
  (interactive (list (magit-read-rev "Merge" (magit-guess-branch))))
  (when revision
    (apply 'magit-run-git
           "merge" "--no-commit"
           (magit-rev-to-git revision)
           magit-custom-options)
    (when (file-exists-p ".git/MERGE_MSG")
        (magit-log-edit))))

;;; Staging and Unstaging

(defun magit-stage-item (&optional ask)
  "Add the item at point to the staging area.
If ASK is set, ask for the file name rather than picking the one
at point."
  (interactive "P")
  (if ask
      (magit-run-git "add" (read-file-name "File to stage: "))
    (magit-section-action (item info "stage")
      ((untracked file)
       (magit-run-git "add" info))
      ((untracked)
       (apply #'magit-run-git "add" "--"
              (magit-git-lines "ls-files" "--other" "--exclude-standard")))
      ((unstaged diff hunk)
       (if (magit-hunk-item-is-conflict-p item)
           (error (concat "Can't stage individual resolution hunks.  "
                          "Please stage the whole file.")))
       (magit-apply-hunk-item item "--cached"))
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
     (if (eq (car info) 'unmerged)
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

(defun magit-stage-all (&optional also-untracked-p)
  "Add all remaining changes in tracked files to staging area.
With prefix argument, add remaining untracked files as well.
\('git add -u .' or 'git add .', respectively)."
  (interactive "P")
  (if also-untracked-p
      (magit-run-git "add" ".")
    (magit-run-git "add" "-u" ".")))

(defun magit-unstage-all ()
  "Remove all changes from staging area.
\('git reset --mixed HEAD')."
  (interactive)
  (magit-run-git "reset" "HEAD"))

;;; Branches

(defun escape-branch-name (branch)
  "Escape branch name BRANCH to remove problematic characters."
  (replace-regexp-in-string "[/]" "-" branch))

(defun magit-default-tracking-name-remote-plus-branch (remote branch)
  "Use the remote name plus a hyphen plus the escaped branch name for tracking branches."
  (concat remote "-" (escape-branch-name branch)))

(defun magit-default-tracking-name-branch-only (remote branch)
  "Use just the escaped branch name for tracking branches."
  (escape-branch-name branch))

(defun magit-get-tracking-name (remote branch)
  "Given a REMOTE and a BRANCH name, ask the user for a local
tracking brach name suggesting a sensible default."
  (when (yes-or-no-p
         (format "Create local tracking branch for %s? " branch))
    (let* ((default-name
             (funcall magit-default-tracking-name-function remote branch))
           (chosen-name (read-string (format "Call local branch (%s): " default-name)
                                     nil
                                     nil
                                     default-name)))
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
           (magit-read-rev "Switch to"
                           (unless (string= current-branch default)
                             default)
                           (if current-branch
                               (cons (concat "refs/heads/" current-branch "$")
                                     magit-uninteresting-refs)
                             magit-uninteresting-refs)))))
  (if revision
      (when (not (magit-maybe-create-local-tracking-branch revision))
        (magit-save-some-buffers)
        (magit-run-git "checkout" (magit-rev-to-git revision))
        (magit-update-vc-modeline default-directory))))

(defun magit-read-create-branch-args ()
  (let* ((cur-branch (magit-get-current-branch))
         (cur-point (magit-default-rev))
         (branch (read-string "Create branch: "))
         (parent (magit-read-rev "Parent"
                                 (cond
                                  ((eq magit-create-branch-behaviour 'at-point) cur-point)
                                  ((eq magit-create-branch-behaviour 'at-head) cur-branch)
                                  (t cur-branch)))))
    (list branch parent)))

(magit-define-command create-branch (branch parent)
  "Switch 'HEAD' to new BRANCH at revision PARENT and update working tree.
Fails if working tree or staging area contain uncommitted changes.
\('git checkout -b BRANCH REVISION')."
  (interactive (magit-read-create-branch-args))
  (when (and branch (not (string= branch ""))
             parent)
    (magit-save-some-buffers)
    (apply #'magit-run-git
           "checkout" "-b"
           branch
           (append magit-custom-options (list (magit-rev-to-git parent))))
    (magit-update-vc-modeline default-directory)))

(defun magit-delete-branch (branch &optional force)
  "Delete the BRANCH.
If the branch is the current one, offers to switch to `master' first.
With prefix, forces the removal even if it hasn't been merged.
Works with local or remote branches.
\('git branch [-d|-D] BRANCH' or 'git push <remote-part-of-BRANCH> :refs/heads/BRANCH')."
  (interactive (list (magit-read-rev-with-default "Branch to delete" 'notrim)
                     current-prefix-arg))
  (let* ((remote (magit-remote-part-of-branch branch))
         (is-current (string= branch (magit-get-current-branch)))
         (args (list "branch"
                     (if force "-D" "-d")
                     branch)))
    (cond
     (remote
      (magit-run-git-async "push" remote (concat ":refs/heads/" (magit-branch-no-remote branch))))
     (is-current
      (when (y-or-n-p "Cannot delete current branch. Switch to master first? ")
          (progn
            (magit-checkout "master")
            (apply 'magit-run-git args))
          (message "The current branch was not deleted.")))
     (t
            (apply 'magit-run-git args)))))

(defun magit-move-branch (old new &optional force)
  "Rename or move branch OLD to NEW.
With prefix, forces the move even if NEW already exists.
\('git branch [-m|-M] OLD NEW')."
  (interactive (list (magit-read-rev-with-default "Old name")
                     (read-string "New name: ")
                     current-prefix-arg))
  (magit-run-git "branch" (if force
                              "-M"
                            "-m")
                 (magit-rev-to-git old) new))

(defun magit-guess-branch ()
  "Return a branch name depending on the context of cursor.
If no branch is found near the cursor return nil."
  (let ((branch                  ; may be t.  see `magit-section-case'
         (magit-section-case (item info)
           ((branch)
            (magit-section-info (magit-current-section)))
           ((wazzup commit)
            (magit-section-info (magit-section-parent item)))
           ((commit)
            (magit-name-rev (substring info 0 magit-sha1-abbrev-length)))
           ((wazzup) info)
           (t (let ((lines (magit-git-lines "reflog")))
                (while (and lines
                            (not (string-match "moving from \\(.+?\\) to"
                                               (car lines))))
                  (setq lines (cdr lines)))
                (when lines
                  (match-string 1 (car lines))))))))
    (when (stringp branch)
      branch)))

;;; Remotes

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

;;; Merging

(defun magit-merge (revision)
  "Merge REVISION into the current 'HEAD'; leave changes uncommitted.
With a prefix-arg, the merge will be squashed.
\('git merge --no-commit [--squash|--no-ff] REVISION')."
  (interactive
   (list (magit-read-rev-with-default "Merge")))
  (if revision
      (apply 'magit-run-git
             "merge"
             (magit-rev-to-git revision)
             magit-custom-options)))

;;; Rebasing

(defun magit-rebase-info ()
  "Return a list indicating the state of an in-progress rebase.
If there is no rebase in progress return nil."
  (let ((git-dir (magit-git-dir)))
    (cond ((file-exists-p (concat git-dir "rebase-merge"))
           (list
            ;; The commit we're rebasing onto, i.e. git rebase -i <onto>
            (magit-name-rev (car (magit-file-lines (concat git-dir "rebase-merge/onto"))))

            ;; How many commits we've gone through
            (length (magit-file-lines (concat git-dir "rebase-merge/done")))

            ;; How many commits we have in total, without the comments
            ;; at the end of git-rebase-todo.backup
            (let ((todo-lines-with-comments (magit-file-lines (concat git-dir "rebase-merge/git-rebase-todo.backup"))))
              (cl-loop for i in todo-lines-with-comments
                       until (string= "" i)
                       count i))))
          ((and (file-exists-p (concat git-dir "rebase-apply"))
                (file-exists-p (concat git-dir "rebase-apply/onto")))
           ;; we might be here because a non-interactive rebase failed: the
           ;; patches didn't apply cleanly
           (list
            ;; The commit we're rebasing onto, i.e. git rebase -i <onto>
            (magit-name-rev (car (magit-file-lines (concat git-dir "rebase-apply/onto"))))

            ;; How many commits we've gone through
            (- (string-to-number (car (magit-file-lines (concat git-dir "rebase-apply/next")))) 1)

            ;; How many commits we have in total
            (string-to-number (car (magit-file-lines (concat git-dir "rebase-apply/last"))))
            ))
          (t nil))))

(defun magit-rebase-step ()
  (interactive)
  (let ((info (magit-rebase-info)))
    (if (not info)
        (let* ((current-branch (magit-get-current-branch))
               (rev (magit-read-rev "Rebase to"
                                    (magit-format-ref (magit-remote-branch-for current-branch t))
                                    (if current-branch
                                        (cons (concat "refs/heads/" current-branch)
                                              magit-uninteresting-refs)
                                      magit-uninteresting-refs))))
          (if rev
              (magit-run-git "rebase" (magit-rev-to-git rev))))
      (let ((cursor-in-echo-area t)
            (message-log-max nil))
        (message "Rebase in progress. [A]bort, [S]kip, or [C]ontinue? ")
        (let ((reply (read-event)))
          (cl-case reply
            ((?A ?a)
             (magit-run-git-async "rebase" "--abort"))
            ((?S ?s)
             (magit-run-git-async "rebase" "--skip"))
            ((?C ?c)
             (magit-run-git-async "rebase" "--continue"))))))))

;;; Resetting

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
                                     (or (magit-default-rev)
                                         "HEAD^"))
                     current-prefix-arg))
  (when revision
    (magit-run-git "reset" (if hard "--hard" "--soft")
                   (magit-rev-to-git revision))
    (magit-update-vc-modeline default-directory)))

(magit-define-command reset-head-hard (revision)
  "Switch 'HEAD' to REVISION, losing all changes.
Uncomitted changes in both working tree and staging area are lost.
\('git reset --hard REVISION')."
  (interactive (list (magit-read-rev (format "Hard reset head to")
                                     (or (magit-default-rev)
                                         "HEAD"))))
  (magit-reset-head revision t))

(magit-define-command reset-working-tree (&optional arg)
  "Revert working tree and clear changes from staging area.
\('git reset --hard HEAD').

With a prefix arg, also remove untracked files.  With two prefix args, remove ignored files as well."
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
      (if include-untracked
          (magit-run-git "clean" "-fd" (if include-ignored
                                           "-x"
                                         ""))))))

;;; Rewriting

(defun magit-read-rewrite-info ()
  (when (file-exists-p (concat (magit-git-dir) "magit-rewrite-info"))
    (with-temp-buffer
      (insert-file-contents (concat (magit-git-dir) "magit-rewrite-info"))
      (goto-char (point-min))
      (read (current-buffer)))))

(defun magit-write-rewrite-info (info)
  (with-temp-file (concat (magit-git-dir) "magit-rewrite-info")
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
                       "log" "--max-count=1"
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
  (magit-section-action (item info)
    ((pending commit)
     (magit-rewrite-set-commit-property info 'used t))))

(defun magit-rewrite-set-unused ()
  (interactive)
  (magit-section-action (item info)
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
         (base
          (if
              (or
               (eq magit-rewrite-inclusive t)
               (and
                (eq magit-rewrite-inclusive 'ask)
                (y-or-n-p "Include selected revision in rewrite? ")))
              (or
               (car (magit-commit-parents from))
               (error "Can't rewrite a parentless commit."))
            from))
         (pending (magit-git-lines "rev-list" (concat base ".."))))
    (magit-write-rewrite-info `((orig ,orig)
                                (pending ,@(mapcar #'list pending))))
    (magit-run-git "reset" "--hard" base)))

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
      (magit-run-git "reset" "--hard" orig))))

(defun magit-rewrite-finish ()
  (interactive)
  (magit-with-refresh
    (magit-rewrite-finish-step t)))

(defun magit-rewrite-finish-step (first-p)
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
            ((magit-apply-commit commit t (not first-p))
             (magit-rewrite-set-commit-property commit 'used t)
             (magit-rewrite-finish-step nil))))))

;;; Updating, pull, and push

(magit-define-command fetch (remote)
  "Fetch from REMOTE."
  (interactive (list (magit-read-remote)))
  (apply 'magit-run-git-async "fetch" remote magit-custom-options))

(magit-define-command fetch-current ()
  "Run fetch for default remote.

If there is no default remote, ask for one."
  (interactive)
  (magit-fetch (or (magit-get-current-remote)
                   (magit-read-remote))))

(magit-define-command remote-update ()
  "Update all remotes."
  (interactive)
  (apply 'magit-run-git-async "remote" "update" magit-custom-options))

(magit-define-command pull ()
  "Run git pull.

If there is no default remote, the user is prompted for one and its values is saved with git config.
If there is no default merge branch, the user is prompted for one and its values is saved with git config.
With a prefix argument, the default remote is not used and the user is prompted for a remote.
With two prefix arguments, the default merge branch is not used and the user is prompted for a merge branch.
Values entered by the user because of prefix arguments are not saved with git config."

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
         (chosen-branch-remote (if remote-needed
                                   (magit-read-remote "Pull from remote" branch-remote)
                                 branch-remote))
         (chosen-branch-merge-name (if branch-needed
                                       (magit-read-remote-branch chosen-branch-remote (format "Pull branch from remote %s" chosen-branch-remote))
                                   branch-merge-name)))
    (when (and (not branch-remote)
               (not choose-remote))
      (magit-set chosen-branch-remote "branch" branch "remote"))
    (when (and (not branch-merge-name)
               (not choose-branch))
      (magit-set (format "%s" chosen-branch-merge-name) "branch" branch "merge"))
    (apply 'magit-run-git-async "pull" "-v"
           (append
            magit-custom-options
            (when choose-remote
              (list chosen-branch-remote))
            (when choose-branch
               (list (format "refs/heads/%s:refs/remotes/%s/%s" chosen-branch-merge-name chosen-branch-remote chosen-branch-merge-name)))))))

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
      (magit-run* (append (cons magit-git-executable
                                magit-git-standard-options)
                          args)
                  nil nil nil t))))

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
      (setq remote (magit-read-remote "Push to remote: ")))
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
         (ref-branch (or (and (>= (prefix-numeric-value current-prefix-arg) 16)
                              (concat "refs/heads/"
                                      (magit-read-remote-branch
                                       push-remote
                                       (format "Push %s as branch" branch))))
                         (and (equal branch-remote push-remote)
                              (magit-get "branch" branch "merge")))))
    (if (and (not ref-branch)
             (eq magit-set-upstream-on-push 'refuse))
        (error "Not pushing since no upstream has been set")
      (let ((set-upstream-on-push (and (not ref-branch)
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

;;; Log edit mode

(defvar magit-log-edit-buffer-name "*magit-edit-log*"
  "Buffer name for composing commit messages.")

(defvar magit-log-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'magit-log-edit-commit)
    (define-key map (kbd "C-x #") 'magit-log-edit-commit)
    (define-key map (kbd "C-c C-a") 'magit-log-edit-toggle-amending)
    (define-key map (kbd "C-c C-s") 'magit-log-edit-toggle-signoff)
    (define-key map (kbd "C-c C-v") 'magit-log-edit-toggle-gpgsign)
    (define-key map (kbd "C-c C-n") 'magit-log-edit-toggle-no-verify)
    (define-key map (kbd "C-c C-t") 'magit-log-edit-toggle-author)
    (define-key map (kbd "C-c C-e") 'magit-log-edit-toggle-allow-empty)
    (define-key map (kbd "M-p") 'log-edit-previous-comment)
    (define-key map (kbd "M-n") 'log-edit-next-comment)
    (define-key map (kbd "C-c C-k") 'magit-log-edit-cancel-log-message)
    (define-key map (kbd "C-c C-]") 'magit-log-edit-cancel-log-message)
    (define-key map (kbd "C-x C-s") (lambda ()
                                      (interactive)
                                      (message "Not saved. Use C-c C-c to finalize this commit message.")))
    map))

(defvar magit-pre-log-edit-window-configuration nil)

(easy-menu-define magit-log-edit-mode-menu magit-log-edit-mode-map
  "Log Edit menu"
  '("Log Edit"
    ["Previous" log-edit-previous-comment t]
    ["Next" log-edit-next-comment t]
    "-"
    ["Amend" magit-log-edit-toggle-amending
     :style toggle
     :selected (string= (magit-log-edit-get-field 'amend) "yes")
     :help "If selected this commit will be an amendment to the previous commit."]
    ["Sign-Off" magit-log-edit-toggle-signoff
     :style toggle
     :selected (let ((sign-off-field (magit-log-edit-get-field 'sign-off)))
                 (if sign-off-field
                     (equal sign-off-field "yes")
                   magit-commit-signoff))
     :help "If selected a Signed-off-by line will be added."]
    ["GPG Sign" magit-log-edit-toggle-gpgsign
     :style toggle
     :selected (let ((gpg-sign-field (magit-log-edit-get-field 'gpg-sign)))
                 (if gpg-sign-field
                     (equal gpg-sign-field "yes")
                   magit-commit-gpgsign))
     :help "If selected the commit will be signed."]
    ["No Verify" magit-log-edit-toggle-no-verify
     :style toggle
     :selected (let ((no-verify-field (magit-log-edit-get-field 'no-verify)))
                 (if no-verify-field
                     (equal no-verify-field "yes")
                   magit-commit-no-verify))
     :help "If selected the commit will bypass the pre-commit and commit-msg hooks."]
    ["Author" magit-log-edit-toggle-author
     :style toggle
     :selected (magit-log-edit-get-field 'author)
     :help "If selected this commit will include an author."]
    ["Allow Empty" magit-log-edit-toggle-allow-empty
     :style toggle
     :selected (string= (magit-log-edit-get-field 'allow-empty) "yes")
     :help "If selected the commit is allowed to be empty."]
    "-"
    ["Cancel" magit-log-edit-cancel-log-message t]
    ["Commit" magit-log-edit-commit t]))

(define-derived-mode magit-log-edit-mode text-mode "Magit Log Edit"
  ;; Recognize changelog-style paragraphs
  (set (make-local-variable 'paragraph-start)
       (concat paragraph-start "\\|*\\|(")))

(defun magit-log-edit-cleanup ()
  (save-excursion
    (goto-char (point-min))
    (goto-char (point-min))
    (if (re-search-forward "[ \t\n]*\\'" nil t)
        (replace-match "\n" nil nil))))

(defun magit-log-edit-append (str)
  (with-current-buffer (get-buffer-create magit-log-edit-buffer-name)
    (goto-char (point-max))
    (insert str "\n")))

(defconst magit-log-header-end "-- End of Magit header --\n")

(defun magit-log-edit-get-fields ()
  (let ((buf (get-buffer magit-log-edit-buffer-name))
        (result nil))
    (if buf
        (with-current-buffer buf
          (goto-char (point-min))
          (while (looking-at "^\\([A-Za-z0-9-_]+\\): *\\(.+\\)?$")
            (setq result (cons (cons (intern (downcase (match-string 1)))
                                     (read (or (match-string 2) "nil")))
                               result))
            (forward-line))
          (if (not (looking-at (regexp-quote magit-log-header-end)))
              (setq result nil))))
    (nreverse result)))

(defun magit-log-edit-set-fields (fields)
  (let ((buf (get-buffer-create magit-log-edit-buffer-name)))
    (with-current-buffer buf
      (goto-char (point-min))
      (if (search-forward-regexp (format "^\\([A-Za-z0-9-_]+:.*\n\\)*%s"
                                         (regexp-quote magit-log-header-end))
                                 nil t)
          (delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (when fields
        (while fields
          (insert (capitalize (symbol-name (caar fields))) ": "
                  (prin1-to-string (cdar fields)) "\n")
          (setq fields (cdr fields)))
        (insert magit-log-header-end)))))

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

(defun magit-log-edit-get-field (name)
  (cdr (assq name (magit-log-edit-get-fields))))

(defun magit-log-edit-toggle-field (name default)
  "Toggle the log-edit field named NAME.
If it's currently unset, set it to DEFAULT (t or nil).

Return nil if the field is toggled off, and non-nil if it's
toggled on.  When it's toggled on for the first time, return
'first."
  (let* ((fields (magit-log-edit-get-fields))
         (cell (assq name fields)) yesp)
    (if cell
        (progn
          (setq yesp (equal (cdr cell) "yes"))
          (rplacd cell (if yesp "no" "yes")))
      (setq fields (cons (cons name (if default "yes" "no")) fields))
      (setq yesp (if default 'first)))
    (magit-log-edit-set-fields fields)
    yesp))

(defun magit-log-edit-toggle-input (name default)
  "Toggle the log-edit input named NAME.
If it's currently unset, set it to DEFAULT (a string). If it is
set remove it.

Return nil if the input is toggled off, and its valud if it's
toggled on."
  (let* ((fields (magit-log-edit-get-fields))
         (cell (assq name fields))
         result)
    (if cell
        (progn
          (setq fields (assq-delete-all name fields)
                result (cdr cell)))
      (setq fields (cons (cons name default) fields)))
    (magit-log-edit-set-fields fields)
    result))

(defun magit-log-edit-setup-author-env (author)
  "Set GIT_AUTHOR_* variables from AUTHOR spec.
If AUTHOR is nil, honor default values from
environment (potentially empty)."
  (when author
    ;; XXX - this is a bit strict, probably.
    (or (string-match "\\(.*\\) <\\(.*\\)>\\(?:,\\s-*\\(.+\\)\\)?" author)
        (error "Can't parse author string"))
    ;; Shucks, setenv destroys the match data.
    (let ((name (match-string 1 author))
          (email (match-string 2 author))
          (date  (match-string 3 author)))
      (make-local-variable 'process-environment)
      (setenv "GIT_AUTHOR_NAME" name)
      (setenv "GIT_AUTHOR_EMAIL" email)
      (if date
          (setenv "GIT_AUTHOR_DATE" date)))))

(defun magit-log-edit-push-to-comment-ring (comment)
  (when (or (ring-empty-p log-edit-comment-ring)
            (not (equal comment (ring-ref log-edit-comment-ring 0))))
    (ring-insert log-edit-comment-ring comment)))

(defun magit-log-edit-commit ()
  "Finish edits and create new commit object.
\('git commit ...')"
  (interactive)
  (let* ((fields (magit-log-edit-get-fields))
         (amend (equal (cdr (assq 'amend fields)) "yes"))
         (allow-empty (equal (cdr (assq 'allow-empty fields)) "yes"))
         (commit-all (equal (cdr (assq 'commit-all fields)) "yes"))
         (sign-off-field (assq 'sign-off fields))
         (sign-off (if sign-off-field
                       (equal (cdr sign-off-field) "yes")
                     magit-commit-signoff))
         (gpg-sign-field (assq 'gpg-sign fields))
         (gpg-sign (if gpg-sign-field
                       (equal (cdr gpg-sign-field) "yes")
                     magit-commit-gpgsign))
         (no-verify-field (assq 'no-verify fields))
         (no-verify (if no-verify-field
                       (equal (cdr no-verify-field) "yes")
                     magit-commit-no-verify))
         (tag-rev (cdr (assq 'tag-rev fields)))
         (tag-name (cdr (assq 'tag-name fields)))
         (author (cdr (assq 'author fields)))
         (tag-options (cdr (assq 'tag-options fields))))

    (unless (or (magit-anything-staged-p)
                allow-empty
                amend
                tag-name
                (file-exists-p (concat (magit-git-dir) "MERGE_HEAD"))
                (and commit-all
                     (not (magit-everything-clean-p))))
      (error "Refusing to create empty commit. Maybe you want to amend (%s) or allow-empty (%s)?"
             (key-description (car (where-is-internal
                                    'magit-log-edit-toggle-amending)))
             (key-description (car (where-is-internal
                                    'magit-log-edit-toggle-allow-empty)))))

    (magit-log-edit-push-to-comment-ring (buffer-string))
    (magit-log-edit-setup-author-env author)
    (magit-log-edit-set-fields nil)
    (magit-log-edit-cleanup)
    (if (= (buffer-size) 0)
        (insert "(Empty description)\n"))
    (let ((env process-environment)
          (commit-buf (current-buffer)))
      (with-current-buffer (magit-find-status-buffer default-directory)
        (let ((process-environment env))
          (cond (tag-name
                 (apply #'magit-run-git-with-input commit-buf
                        "tag" (append tag-options
                                      (list tag-name "-a" "-F" "-" tag-rev))))
                (t
                 (apply #'magit-run-async-with-input commit-buf
                        magit-git-executable
                        (append magit-git-standard-options
                                '("commit")
                                magit-custom-options
                                '("-F" "-")
                                (when (and commit-all (not allow-empty))
                                  '("--all"))
                                (when amend '("--amend"))
                                (when allow-empty '("--allow-empty"))
                                (when sign-off '("--signoff"))
                                (when gpg-sign '("-S"))
                                (when no-verify '("--no-verify")))))))))
    ;; shouldn't we kill that buffer altogether?
    (erase-buffer)
    ;; potentially the local environment has been altered with settings that
    ;; were specific to this commit. Let's revert it
    (kill-local-variable 'process-environment)
    (let ((magit-buf magit-buffer-internal))
      (bury-buffer)
      (set-buffer magit-buf))
    (when (file-exists-p (concat (magit-git-dir) "MERGE_MSG"))
      (delete-file (concat (magit-git-dir) "MERGE_MSG")))
    (magit-update-vc-modeline default-directory)
    (when magit-pre-log-edit-window-configuration
      (set-window-configuration magit-pre-log-edit-window-configuration)
      (setq magit-pre-log-edit-window-configuration nil))))

(defun magit-log-edit-cancel-log-message ()
  "Abort edits and erase commit message being composed."
  (interactive)
  (when (or (not magit-log-edit-confirm-cancellation)
            (yes-or-no-p
             "Really cancel editing the log (any changes will be lost)?"))
    (erase-buffer)
    (bury-buffer)
    (when magit-pre-log-edit-window-configuration
      (set-window-configuration magit-pre-log-edit-window-configuration)
      (setq magit-pre-log-edit-window-configuration nil))))

(defun magit-log-edit-toggle-amending ()
  "Toggle whether this will be an amendment to the previous commit.
\(i.e., whether eventual commit does 'git commit --amend')"
  (interactive)
  (when (eq (magit-log-edit-toggle-field 'amend t) 'first)
    (magit-log-edit-append
     (magit-trim-line (magit-format-commit "HEAD" "%s%n%n%b")))))

(defun magit-log-edit-toggle-signoff ()
  "Toggle whether this commit will include a signoff.
\(i.e., whether eventual commit does 'git commit --signoff')"
  (interactive)
  (magit-log-edit-toggle-field 'sign-off (not magit-commit-signoff)))

(defun magit-log-edit-toggle-gpgsign ()
  "Toggle whether this commit will be GPG-signed.
\(i.e., whether eventual commit does 'git commit -S')"
  (interactive)
  (magit-log-edit-toggle-field 'gpg-sign (not magit-commit-gpgsign)))

(defun magit-log-edit-toggle-no-verify ()
  "Toggle whether this commit will bypass the pre-commit and commit-msg hooks.
\(i.e., whether eventual commit does 'git commit --no-verify')"
  (interactive)
  (magit-log-edit-toggle-field 'no-verify (not magit-commit-no-verify)))

(defun magit-log-edit-toggle-author ()
  "Toggle whether this commit will include an author.
\(i.e., whether eventual commit is run with GIT_AUTHOR_NAME and
GIT_AUTHOR_EMAIL set)"
  (interactive)
  (magit-log-edit-toggle-input 'author (format "%s <%s>"
                                               (or (magit-get "user" "name") "Author Name")
                                               (or (magit-get "user" "email") "author@email"))))

(defun magit-log-edit-toggle-allow-empty ()
  "Toggle whether this commit is allowed to be empty.
This means that the eventual commit does 'git commit --allow-empty'."
  (interactive)
  (magit-log-edit-toggle-field 'allow-empty t))

(defun magit-pop-to-log-edit (operation)
  (let ((dir default-directory)
        (magit-buf (current-buffer))
        (buf (get-buffer-create magit-log-edit-buffer-name)))
    (setq magit-pre-log-edit-window-configuration
          (current-window-configuration))
    (pop-to-buffer buf)
    (setq default-directory dir)
    (when (file-exists-p (concat (magit-git-dir) "MERGE_MSG"))
      (insert-file-contents (concat (magit-git-dir) "MERGE_MSG")))
    (magit-log-edit-mode)
    (make-local-variable 'magit-buffer-internal)
    (setq magit-buffer-internal magit-buf)
    (message "Type C-c C-c to %s (C-c C-k to cancel)." operation)))

(defun magit-log-edit (&optional arg)
  "Bring up a buffer to allow editing of commit messages.

Giving a simple prefix arg will amend a previous commit, while
a double prefix arg will allow creating an empty one.

If there is a rebase in progress, offer the user the option to
continue it.

\\{magit-log-edit-mode-map}"
  (interactive "P")
  ;; If repository is dirty there is no point in trying to
  ;; suggest to continue the rebase. Git will rebuke you and exit with
  ;; error code, so suggest it only if theres absolutely nothing else
  ;; to do and rebase is ongoing.
  (if (and (magit-everything-clean-p)
           (magit-rebase-info)
           (y-or-n-p "Rebase in progress.  Continue it? "))
      (magit-run-git-async "rebase" "--continue")

    ;; If there's nothing staged, set commit flag to `nil', thus
    ;; avoiding unnescessary popping up of the log edit buffer in case
    ;; when user chose to forgo commiting all unstaged changes
    (let ((amend-p (= (prefix-numeric-value arg) 4))
          (empty-p (= (prefix-numeric-value arg) 16)))
      (when (and magit-commit-all-when-nothing-staged
                 (not (magit-everything-clean-p))
                 (not (magit-anything-staged-p)))
        (cond ((eq magit-commit-all-when-nothing-staged 'ask-stage)
               (when (y-or-n-p "Nothing staged.  Stage everything now? ")
                 (magit-stage-all)))
              ((not (magit-log-edit-get-field 'commit-all))
               (when (or (eq magit-commit-all-when-nothing-staged t)
                         (y-or-n-p
                          "Nothing staged.  Commit all unstaged changes? "))
                 (magit-log-edit-set-field 'commit-all "yes")))))
      (when amend-p
        (magit-log-edit-toggle-amending))
      (when empty-p
        (magit-log-edit-toggle-allow-empty))
      (let ((author-email (or (getenv "GIT_AUTHOR_EMAIL") ""))
            (author-name (or (getenv "GIT_AUTHOR_NAME") ""))
            (author-date (or (getenv "GIT_AUTHOR_DATE") "")))
        (if (not (string= author-email ""))
            (magit-log-edit-set-field 'author (format "%s <%s>%s"
                                                      (if (string= "" author-name) author-email author-name)
                                                      author-email
                                                      (if (string= "" author-date) "" (format ", %s" author-date))))))
      (magit-pop-to-log-edit "commit"))))

(defun magit-add-log ()
  (interactive)
  (cond ((magit-rebase-info)
         (if (y-or-n-p "Rebase in progress.  Continue it? ")
             (magit-run-git-async "rebase" "--continue")))
        (t
         (let ((section (magit-current-section)))
           (let ((fun (if (eq (magit-section-type section) 'hunk)
                          (save-window-excursion
                            (save-excursion
                              (magit-visit-item)
                              (add-log-current-defun)))
                        nil))
                 (file (magit-diff-item-file
                        (cond ((eq (magit-section-type section) 'hunk)
                               (magit-hunk-item-diff section))
                              ((eq (magit-section-type section) 'diff)
                               section)
                              (t
                               (error "No change at point"))))))
             (magit-log-edit nil)
             (goto-char (point-min))
             (cond ((not (search-forward-regexp
                          (format "^\\* %s" (regexp-quote file)) nil t))
                    ;; No entry for file, create it.
                    (goto-char (point-max))
                    (insert (format "\n* %s" file))
                    (if fun
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
                      (forward-char 2)))))))))

;;; Tags

(magit-define-command tag (name rev)
  "Create a new lightweight tag with the given NAME at REV.
\('git tag NAME')."
  (interactive
   (list
    (read-string "Tag name: ")
    (magit-read-rev "Place tag on: " (or (magit-default-rev) "HEAD"))))
  (apply #'magit-run-git "tag" (append magit-custom-options (list name rev))))

(magit-define-command annotated-tag (name rev)
  "Start composing an annotated tag with the given NAME.
Tag will point to the current 'HEAD'."
  (interactive
   (list
    (read-string "Tag name: ")
    (magit-read-rev "Place tag on: " (or (magit-default-rev) "HEAD"))))
  (magit-log-edit-set-field 'tag-name name)
  (magit-log-edit-set-field 'tag-rev rev)
  (magit-log-edit-set-field 'tag-options magit-custom-options)
  (magit-pop-to-log-edit "tag"))

;;; Stashing

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
  (interactive (list (read-string "Stash description: "
                                  nil
                                  'magit-read-stash-history)))
  (apply 'magit-run-git `("stash" "save" ,@magit-custom-options "--" ,description)))

(magit-define-command stash-snapshot ()
  "Create new stash of working tree and staging area; keep changes in place.
\('git stash save \"Snapshot...\"; git stash apply stash@{0}')"
  (interactive)
  (magit-with-refresh
    (apply 'magit-run-git `("stash" "save" ,@magit-custom-options
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
             (set-buffer buf)
             (goto-char (point-min))
             (let* ((range (cons (concat stash "^2^") stash))
                    (magit-current-diff-range range)
                    (args (magit-rev-range-to-git range)))
               (magit-mode-init dir 'magit-diff-mode #'magit-refresh-diff-buffer
                                range args)))))))
;;; Commits

(defun magit-commit-at-point (&optional nil-ok-p)
  (let* ((section (magit-current-section))
         (commit (if (and section
                          (eq (magit-section-type section) 'commit))
                     (magit-section-info section)
                 (get-text-property (point) 'revision))))
    (if nil-ok-p
        commit
      (or commit
          (error "No commit at point")))))

(defun magit-apply-commit (commit &optional docommit noerase revert)
  (let* ((parent-id (magit-choose-parent-id commit "cherry-pick"))
         (success (magit-run* `(,magit-git-executable
                                ,@magit-git-standard-options
                                ,(if revert "revert" "cherry-pick")
                                ,@(if parent-id
                                      (list "-m" (number-to-string parent-id)))
                                ,@(if (not docommit) (list "--no-commit"))
                                ,commit)
                              nil noerase)))
    (when (and (not docommit) success)
      (cond (revert
             (magit-log-edit-append
              (magit-format-commit commit "Reverting \"%s\"")))
            (t
             (magit-log-edit-append
              (magit-format-commit commit "%s%n%n%b"))
             (magit-log-edit-set-field
              'author
              (magit-format-commit commit "%an <%ae>, %ai")))))
    success))

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

(defun magit-cherry-pick-item ()
  (interactive)
  (magit-section-action (item info "cherry-pick")
    ((pending commit)
     (magit-apply-commit info t)
     (magit-rewrite-set-commit-property info 'used t))
    ((commit)
     (magit-apply-commit info t))
    ((stash)
     (magit-run-git "stash" "pop" info))))

(defmacro magit-with-revert-confirmation (&rest body)
  `(when (or (not magit-revert-item-confirm)
             (yes-or-no-p "Really revert this item? "))
     ,@body))

(defun magit-revert-item ()
  (interactive)
  (magit-section-action (item info "revert")
    ((pending commit)
     (magit-with-revert-confirmation
      (magit-apply-commit info nil nil t)
      (magit-rewrite-set-commit-property info 'used nil)))
    ((commit)
     (magit-with-revert-confirmation
      (magit-apply-commit info nil nil t)))
    ;; Reverting unstaged changes cannot be undone
    ((unstaged *)
     (magit-discard-item))
    ((hunk)
     (magit-with-revert-confirmation
      (magit-apply-hunk-item-reverse item)))
    ((diff)
     (magit-with-revert-confirmation
      (magit-apply-diff-item item "--reverse")))))

(defun magit-log-show-more-entries (&optional arg)
  "Grow the number of log entries shown.

With no prefix optional ARG, show twice as many log entries.
With a numerical prefix ARG, add this number to the number of shown log entries.
With a non numeric prefix ARG, show all entries"
  (interactive "P")
  (make-local-variable 'magit-log-cutoff-length)
  (cond
   ((numberp arg)
    (setq magit-log-cutoff-length (+ magit-log-cutoff-length arg)))
   (arg
    (setq magit-log-cutoff-length magit-log-infinite-length))
   (t (setq magit-log-cutoff-length (* magit-log-cutoff-length 2))))
  (let ((old-point (point)))
    (magit-refresh)
    (goto-char old-point)))

(defun magit-refresh-log-buffer (range style args)
  (magit-configure-have-graph)
  (magit-configure-have-decorate)
  (magit-configure-have-abbrev)
  (setq magit-current-range range)
  (magit-create-log-buffer-sections
    (apply #'magit-git-section nil
           (magit-rev-range-describe range "Commits")
           (apply-partially 'magit-wash-color-log style)
           `("log"
             ,(format "--max-count=%s" magit-log-cutoff-length)
             "--abbrev-commit"
             ,(format "--abbrev=%s" magit-sha1-abbrev-length)
             ,@(cond ((eq style 'long) (append
                                        (list "--stat" "-z")
                                        (when magit-log-show-gpg-status
                                          (list "--show-signature"))))
                     ((eq style 'oneline)
                      (let ((fmt
                             (if magit-log-show-gpg-status
                                 "%h%d %G?[%an][%ar]%s"
                               "%h%d [%an][%ar]%s")))
                        (list (format "--pretty=format:%s" fmt))))
                     (t nil))
             ,@(if magit-have-decorate (list "--decorate=full"))
             ,@(if magit-have-graph (list "--graph"))
             "--color"
             ,@args
             "--"))))

(define-derived-mode magit-log-mode magit-mode "Magit Log"
  "Mode for looking at git log.

\\{magit-log-mode-map}"
  :group 'magit)

(magit-define-command log-ranged ()
  (interactive)
  (magit-log t))
(define-obsolete-function-alias 'magit-display-log-ranged 'magit-log-ranged)

(magit-define-command log (&optional ask-for-range &rest extra-args)
  (interactive)
  (let* ((log-range (if ask-for-range
                        (magit-read-rev-range "Log" "HEAD")
                      "HEAD"))
         (topdir (magit-get-top-dir default-directory))
         (args (nconc (list (magit-rev-range-to-git log-range))
                      magit-custom-options
                      extra-args)))
    (magit-buffer-switch magit-log-buffer-name)
    (magit-mode-init topdir 'magit-log-mode #'magit-refresh-log-buffer log-range
                     'oneline args)))

(define-obsolete-function-alias 'magit-display-log 'magit-log)

(magit-define-command log-long-ranged ()
  (interactive)
  (magit-log-long t))

(magit-define-command log-long (&optional ranged)
  (interactive)
  (let* ((range (if ranged
                    (magit-read-rev-range "Long log" "HEAD")
                  "HEAD"))
         (topdir (magit-get-top-dir default-directory))
         (args (append (list (magit-rev-range-to-git range))
                       magit-custom-options)))
    (magit-buffer-switch magit-log-buffer-name)
    (magit-mode-init topdir 'magit-log-mode #'magit-refresh-log-buffer range
                     'long args)))

;;; Reflog

(defvar magit-reflog-head nil
  "The HEAD of the reflog in the current buffer.
This is only non-nil in reflog buffers.")
(make-variable-buffer-local 'magit-reflog-head)

(defun magit-refresh-reflog-buffer (head args)
  (setq magit-reflog-head head)
  (magit-create-log-buffer-sections
    (apply #'magit-git-section
           'reflog (format "Local history of head %s" head) 'magit-wash-log "log"
           (append magit-git-log-options
                   (list
                    "--walk-reflogs"
                    (format "--max-count=%s" magit-log-cutoff-length)
                    args)))))

(define-derived-mode magit-reflog-mode magit-log-mode "Magit Reflog"
  "Mode for looking at git reflog.

\\{magit-reflog-mode-map}"
  :group 'magit)

(magit-define-command reflog (&optional ask-for-range)
  (interactive)
  (let ((at (or (if ask-for-range
                    (magit-read-rev "Reflog of" (or (magit-guess-branch) "HEAD")))
                "HEAD")))
    (let* ((topdir (magit-get-top-dir default-directory))
           (args (magit-rev-to-git at)))
      (magit-buffer-switch "*magit-reflog*")
      (magit-mode-init topdir 'magit-reflog-mode
                       #'magit-refresh-reflog-buffer at args))))

(magit-define-command reflog-ranged ()
  (interactive)
  (magit-reflog t))

;;; Diffing

(defvar magit-ediff-buffers nil
  "List of buffers that may be killed by `magit-ediff-restore'.")

(defvar magit-ediff-windows nil
  "The window configuration that will be restored when Ediff is finished.")

(defun magit-ediff ()
  "View the current DIFF section in ediff."
  (interactive)
  (let ((diff (magit-current-section)))
    (when (magit-section-hidden diff)
      ;; Range is not set until the first time the diff is visible.
      ;; This somewhat hackish code makes sure it's been visible at least once.
      (magit-toggle-section)
      (magit-toggle-section)
      (setq diff (magit-current-section)))
    (if (eq 'hunk (magit-section-type diff))
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
             (magit-rev-range-describe range "Changes")
             'magit-wash-diffs
             "diff" (magit-diff-U-arg)
             `(,@(if magit-show-diffstat (list "--patch-with-stat"))
               ,args "--")))))

(define-derived-mode magit-diff-mode magit-mode "Magit Diff"
  "Mode for looking at a git diff.

\\{magit-diff-mode-map}"
  :group 'magit)

(magit-define-command diff (range)
  (interactive (list (magit-read-rev-range "Diff")))
  (if range
      (let* ((dir default-directory)
             (args (magit-rev-range-to-git range))
             (buf (get-buffer-create "*magit-diff*")))
        (display-buffer buf)
        (with-current-buffer buf
          (magit-mode-init dir 'magit-diff-mode #'magit-refresh-diff-buffer range args)))))

(magit-define-command diff-working-tree (rev)
  (interactive (list (magit-read-rev-with-default "Diff with")))
  (magit-diff (or rev "HEAD")))

(defun magit-diff-with-mark ()
  (interactive)
  (magit-diff (cons (magit-marked-commit)
                    (magit-commit-at-point))))

;;; Wazzup

(defvar magit-wazzup-head nil
  "The integration head for the current wazzup buffer.
This is only non-nil in wazzup buffers.")
(make-variable-buffer-local 'magit-wazzup-head)

(defvar magit-wazzup-all-p nil
  "Non-nil if the current wazzup buffer displays excluded branches.
This is only meaningful in wazzup buffers.")
(make-variable-buffer-local 'magit-wazzup-all-p)

(defun magit-wazzup-toggle-ignore (branch edit)
  (let ((ignore-file (concat (magit-git-dir) "info/wazzup-exclude")))
    (if edit
        (setq branch (read-string "Branch to ignore for wazzup: " branch)))
    (let ((ignored (magit-file-lines ignore-file)))
      (cond ((member branch ignored)
             (when (or (not edit)
                       (y-or-n-p "Branch %s is already ignored.  Unignore? "))
               (setq ignored (delete branch ignored))))
            (t
             (setq ignored (append ignored (list branch)))))
      (magit-write-file-lines ignore-file ignored)
      (magit-need-refresh))))

(defun magit-refresh-wazzup-buffer (head all)
  (setq magit-wazzup-head head)
  (setq magit-wazzup-all-p all)
  (let ((branch-desc (or head "(detached) HEAD")))
    (unless head (setq head "HEAD"))
    (magit-create-buffer-sections
      (magit-with-section 'wazzupbuf nil
        (insert (format "Wazzup, %s\n\n" branch-desc))
        (let* ((excluded (magit-file-lines (concat (magit-git-dir) "info/wazzup-exclude")))
               (all-branches (magit-list-interesting-refs))
               (branches (if all all-branches
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
                           "log"
                           (format "--max-count=%s" magit-log-cutoff-length)
                           "--abbrev-commit"
                           (format "--abbrev=%s" magit-sha1-abbrev-length)
                           "--graph"
                           "--pretty=oneline"
                           (format "%s..%s" head ref)
                           "--"))))
                  (magit-set-section-info ref section))))))))))

(define-derived-mode magit-wazzup-mode magit-mode "Magit Wazzup"
  "Mode for looking at commits that could be merged from other branches.

\\{magit-wazzup-mode-map}"
  :group 'magit)

(defun magit-wazzup (&optional all)
  (interactive "P")
  (let ((topdir (magit-get-top-dir default-directory))
        (current-branch (magit-get-current-branch)))
    (magit-buffer-switch "*magit-wazzup*")
    (magit-mode-init topdir 'magit-wazzup-mode
                     #'magit-refresh-wazzup-buffer
                     current-branch all)))

(defun magit-filename (filename)
  "Return the path of FILENAME relative to its git repository.

If FILENAME is absolute, return a path relative to the git
repository containing it. Otherwise, return a path relative to
the current git repository."
  (let ((topdir (expand-file-name
                 (magit-get-top-dir (or (file-name-directory filename)
                                        default-directory))))
        (file (file-truename filename)))
    (when (and (not (string= topdir ""))
               ;; FILE must start with the git repository path
               (zerop (string-match-p (concat "\\`" topdir) file)))
      (substring file (length topdir)))))

;; This variable is used to keep track of the current file in the
;; *magit-log* buffer when this one is dedicated to showing the log of
;; just 1 file.
(defvar magit-file-log-file nil)
(make-variable-buffer-local 'magit-file-log-file)

(defun magit-refresh-file-log-buffer (file range style)
  "Refresh the current file-log buffer by calling git.

FILE is the path of the file whose log must be displayed.

`magit-current-range' will be set to the value of RANGE.

STYLE controls the display. It is either `'long',  `'oneline', or something else."
  (magit-configure-have-graph)
  (magit-configure-have-decorate)
  (magit-configure-have-abbrev)
  (setq magit-current-range range)
  (setq magit-file-log-file file)
  (magit-create-log-buffer-sections
    (apply #'magit-git-section nil
           (magit-rev-range-describe range (format "Commits for file %s" file))
           (apply-partially 'magit-wash-log style)
           `("log"
             ,(format "--max-count=%s" magit-log-cutoff-length)
             ,"--abbrev-commit"
             ,(format "--abbrev=%s" magit-sha1-abbrev-length)
             ,@(cond ((eq style 'long) (list "--stat" "-z"))
                     ((eq style 'oneline) (list "--pretty=oneline"))
                     (t nil))
             ,@(if magit-have-decorate (list "--decorate=full"))
             ,@(if magit-have-graph (list "--graph"))
             "--"
             ,file))))

(defun magit-file-log (&optional all)
  "Display the log for the currently visited file or another one.

With a prefix argument or if no file is currently visited, ask
for the file whose log must be displayed."
  (interactive "P")
  (let ((topdir (magit-get-top-dir default-directory))
        (current-file (magit-filename
                       (if (or current-prefix-arg (not buffer-file-name))
                           (magit-read-file-from-rev (magit-get-current-branch))
                        buffer-file-name)))
        (range "HEAD"))
    (magit-buffer-switch magit-log-buffer-name)
    (magit-mode-init topdir 'magit-log-mode
                     #'magit-refresh-file-log-buffer
                     current-file range 'oneline)))

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

;;; Miscellaneous

(defun magit-edit-ignore-string (file)
  "Prompt the user for the string to be ignored.
A list of predefined values with wildcards is derived from the
filename FILE."
  (let* ((extension (concat "*." (file-name-extension file)))
         (extension-in-dir (concat (file-name-directory file) extension))
         (filename (file-name-nondirectory file))
         (completions (list extension extension-in-dir filename file)))
    (magit-completing-read "File to ignore: " completions () () () () file)))

(defun magit-ignore-file (file &optional edit-ignore-string local)
  "Add FILE to the list of files to ignore.
If EDIT-IGNORE-STRING is non-nil, prompt the user for the string
to be ignored instead of using FILE.  The changes are written to
.gitignore except if LOCAL is non-nil in which case they are
written to .git/info/exclude."
  (let* ((local-ignore-dir (concat (magit-git-dir) "info/"))
         (ignore-file (if local
                          (concat local-ignore-dir "exclude")
                        ".gitignore")))
    (if edit-ignore-string
      (setq file (magit-edit-ignore-string file)))
    (if (and local (not (file-exists-p local-ignore-dir)))
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

(defun magit--ignore-item (arg &optional local)
  (interactive)
  (magit-section-action (item info "ignore")
    ((untracked file)
     (magit-ignore-file (concat "/" info) current-prefix-arg local))
    ((wazzup)
     (magit-wazzup-toggle-ignore info current-prefix-arg))))

(defun magit-ignore-item ()
  "Ignore the item at point."
  (interactive)
  (magit--ignore-item current-prefix-arg))

(defun magit-ignore-item-locally ()
  "Ignore the item at point locally only."
  (interactive)
  (magit--ignore-item current-prefix-arg t))

(defun magit-discard-diff (diff stagedp)
  (let ((kind (magit-diff-item-kind diff))
        (file (magit-diff-item-file diff)))
    (cond ((eq kind 'deleted)
           (when (yes-or-no-p (format "Resurrect %s? " file))
             (magit-run-git "reset" "-q" "--" file)
             (magit-run-git "checkout" "--" file)))
          ((eq kind 'new)
           (if (yes-or-no-p (format "Delete %s? " file))
               (magit-run-git "rm" "-f" "--" file)))
          (t
           (if (yes-or-no-p (format "Discard changes to %s? " file))
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
           (magit-delete-directory info 'recursive)
         (delete-file info))
       (magit-refresh-buffer)))
    ((untracked)
     (if (yes-or-no-p "Delete all untracked files and directories? ")
         (magit-run-git "clean" "-df")))
    ((unstaged diff hunk)
     (when (yes-or-no-p (if (magit-use-region-p)
                            "Discard changes in region? "
                          "Discard hunk? "))
       (magit-apply-hunk-item-reverse item)))
    ((staged diff hunk)
     (if (magit-file-uptodate-p (magit-diff-item-file
                                 (magit-hunk-item-diff item)))
         (when (yes-or-no-p (if (magit-use-region-p)
                                "Discard changes in region? "
                              "Discard hunk? "))
           (magit-apply-hunk-item-reverse item "--index"))
       (error "Can't discard this hunk.  Please unstage it first")))
    ((unstaged diff)
     (magit-discard-diff item nil))
    ((staged diff)
     (if (magit-file-uptodate-p (magit-diff-item-file item))
         (magit-discard-diff item t)
       (error "Can't discard staged changes to this file.  Please unstage it first")))
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

(defun magit-move-item ()
  (interactive)
  (magit-section-action (item info "move")
    ((branch)
     (call-interactively 'magit-move-branch))
    ((remote)
     (call-interactively 'magit-rename-remote))))

(defmacro magit-visiting-file-item (&rest body)
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
The name of the change log file is set by variable change-log-default-name."
  (interactive "P")
  (if other-window
      (magit-visiting-file-item (add-change-log-entry-other-window))
    (magit-visiting-file-item (add-change-log-entry))))

(defun magit-add-change-log-entry-other-window ()
  (interactive)
  (magit-visiting-file-item (call-interactively 'add-change-log-entry-other-window)))

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
                                   (magit-hunk-item-diff item))))))))

(defun magit-visit-file-item (&optional other-window)
  "Visit current file associated with item.
With a prefix argument, visit in other window."
  (interactive "P")
  (magit-section-action (item info "visit-file")
    ((untracked file)
     (funcall
      (if other-window 'find-file-other-window 'find-file)
      info))
    ((diff)
     (let ((file (magit-diff-item-file item)))
       (cond ((not (file-exists-p file))
              (error "Can't visit deleted file: %s" file))
             ((file-directory-p file)
              (magit-status file))
             (t
              (funcall
               (if other-window 'find-file-other-window 'find-file)
               file)))))
    ((diffstat)
     (let ((file (magit-diffstat-item-file item)))
       (cond ((null file)
              (error "Can't get pathname for this file"))
             ((not (file-exists-p file))
              (error "Can't visit deleted file: %s" file))
             ((file-directory-p file)
              (magit-status file))
             (t
              (funcall
               (if other-window 'find-file-other-window 'find-file)
               file)))))
    ((hunk)
     (let ((file (magit-diff-item-file (magit-hunk-item-diff item)))
           (line (magit-hunk-item-target-line item))
           (column (current-column)))
       (if (not (file-exists-p file))
           (error "Can't visit deleted file: %s" file))
       (funcall
        (if other-window 'find-file-other-window 'find-file)
        file)
       (goto-char (point-min))
       (forward-line (1- line))
       (when (> column 0)
         (move-to-column (1- column)))))))

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

(defun magit-show-item-or-scroll-up ()
  (interactive)
  (magit-section-action (item info)
    ((commit)
     (magit-show-commit info #'scroll-up))
    ((stash)
     (magit-show-stash info #'scroll-up))
    (t
     (scroll-up))))

(defun magit-show-item-or-scroll-down ()
  (interactive)
  (magit-section-action (item info)
    ((commit)
     (magit-show-commit info #'scroll-down))
    ((stash)
     (magit-show-stash info #'scroll-down))
    (t
     (scroll-down))))

(defun magit-mark-item (&optional unmark)
  (interactive "P")
  (if unmark
      (magit-set-marked-commit nil)
    (magit-section-action (item info "mark")
      ((commit)
       (magit-set-marked-commit (if (eq magit-marked-commit info)
                                    nil
                                  info))))))

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

(defun magit-server-running-p ()
  "Test whether server is running (works with < 23 as well).

Return values:
  nil              the server is definitely not running.
  t                the server seems to be running.
  something else   we cannot determine whether it's running without using
                   commands which may have to wait for a long time."
  (require 'server)
  (if (functionp 'server-running-p)
      (server-running-p)
    (condition-case nil
        (if server-use-tcp
            (with-temp-buffer
              (insert-file-contents-literally (expand-file-name server-name server-auth-dir))
              (or (and (looking-at "127\\.0\\.0\\.1:[0-9]+ \\([0-9]+\\)")
                       (assq 'comm
                             (process-attributes
                              (string-to-number (match-string 1))))
                       t)
                  :other))
          (delete-process
           (make-network-process
            :name "server-client-test" :family 'local :server nil :noquery t
            :service (expand-file-name server-name server-socket-dir)))
          t)
      (file-error nil))))

(defun magit-interactive-rebase ()
  "Start a git rebase -i session, old school-style."
  (interactive)
  (unless (magit-server-running-p)
    (server-start))
  (let* ((section (get-text-property (point) 'magit-section))
         (commit (and (member 'commit (magit-section-context-type section))
                      (magit-section-info section)))
         (old-editor (getenv "GIT_EDITOR")))
    (if (executable-find "emacsclient")
        (setenv "GIT_EDITOR"
                (cond ((string= server-name "server")
                       (executable-find "emacsclient"))
                      ((eq system-type 'windows-nt)
                       (message "We don't know how to deal with non-default server name on windows")
                       ())
                      (t (concat (executable-find "emacsclient")
                                 " -s " server-name))))
        (message "Cannot find emacsclient, using default git editor, please check your PATH"))
    (unwind-protect
        (magit-run-git-async "rebase" "-i"
                             (or (and commit (concat commit "^"))
                                 (magit-read-rev "Interactively rebase to" (magit-guess-branch))))
      (if old-editor
          (setenv "GIT_EDITOR" old-editor)))))

(define-derived-mode magit-branch-manager-mode magit-mode "Magit Branch"
  "Magit Branches")

(defun magit-quit-window (&optional kill-buffer)
  "Bury the buffer and delete its window.
With a prefix argument, kill the buffer instead."
  (interactive "P")
  (quit-window kill-buffer (selected-window)))

(defun magit--branch-name-at-point ()
  "Get the branch name in the line at point."
  (let ((branch (magit-section-info (magit-current-section))))
    (or branch (error "No branch at point"))))

(defun magit--branches-for-remote-repo (remote)
  "Return a list of remote branch names for REMOTE.
These are the branch names with the remote name stripped."
  (remq nil
        (mapcar (lambda (line)
                  (save-match-data
                    (if (and (not (string-match-p " -> " line))
                             (string-match (concat "^ +" remote "/\\([^ $]+\\)")
                                           line))
                        (match-string 1 line))))
                (magit-git-lines "branch" "-r"))))

(defvar magit-branches-buffer-name "*magit-branches*")

(defun magit--is-branch-at-point-remote ()
  "Return non-nil if the branch at point is a remote tracking branch."
  (magit-remote-part-of-branch (magit--branch-name-at-point)))

(defun magit-remote-part-of-branch (branch)
  (when (string-match-p "^\\(?:refs/\\)?remotes\\/" branch)
    (cl-loop for remote in (magit-git-lines "remote")
             when (string-match-p (format "^\\(?:refs/\\)?remotes\\/%s\\/" (regexp-quote remote)) branch) return remote)))

(defun magit-branch-no-remote (branch)
  (let ((remote (magit-remote-part-of-branch branch)))
    (if remote
        (progn
          ;; This has to match if remote is non-nil
          (cl-assert (string-match (format "^\\(?:refs/\\)?remotes\\/%s\\/\\(.*\\)" (regexp-quote remote)) branch)
                     'show-args "Unexpected string-match failure: %s %s")
          (match-string 1 branch))
      branch)))

(defun magit-wash-branch-line (&optional remote-name)
  (looking-at (concat
               "^\\([ *] \\)"                 ; 1: current branch marker
               "\\(.+?\\) +"                  ; 2: branch name

               "\\(?:"

               "\\([0-9a-fA-F]+\\)"           ; 3: sha1
               " "
               "\\(?:\\["
               "\\([^:\n]+?\\)"               ; 4: tracking (non-greedy + to avoid matching \n)
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
                (equal (magit-remote-branch-for branch t)
                       (concat "refs/remotes/" tracking)))
           (concat " ["
                   ;; getting rid of the tracking branch name if it is
                   ;; the same as the branch name
                   (let* ((tracking-remote (magit-get "branch" branch "remote"))
                          (tracking-branch (substring tracking (+ 1 (length tracking-remote)))))
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
      (insert-before-markers (propertize (format "%s (%s):" remote-name urls) 'face 'magit-section-title) "\n")
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
         (remote-groups (cl-loop for remote in remotes
                              for end-markers on (cdr markers)
                              for marker = (cl-loop for x in end-markers thereis x)
                              collect (list remote marker))))

    ;; actual displaying of information
    (magit-with-section "local" nil
      (insert-before-markers (propertize "Local:" 'face 'magit-section-title) "\n")
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

(defun magit-refresh-branch-manager ()
  (magit-create-buffer-sections
    (apply #'magit-git-section
           "branches" nil 'magit-wash-branches
           "branch"
           "-vva"
           (format "--abbrev=%s" magit-sha1-abbrev-length)
           magit-custom-options)))

(magit-define-command branch-manager ()
  (interactive)
  (let ((topdir (magit-get-top-dir default-directory)))
    (magit-buffer-switch magit-branches-buffer-name)
    (magit-mode-init topdir 'magit-branch-manager-mode #'magit-refresh-branch-manager)))

(defun magit-change-what-branch-tracks ()
  "Change which remote branch the current branch tracks."
  (interactive)
  (if (magit--is-branch-at-point-remote)
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
    (if (string= (magit-get-current-branch) local-branch)
        (magit-refresh-buffer (magit-find-status-buffer default-directory)))))

(defvar magit-ediff-file)

(defun magit-interactive-resolve (file)
  (require 'ediff)
  (let ((merge-status (magit-git-string "ls-files" "-u" "--" file))
        (base-buffer (generate-new-buffer (concat file ".base")))
        (our-buffer (generate-new-buffer (concat file ".current")))
        (their-buffer (generate-new-buffer (concat file ".merged")))
        (windows (current-window-configuration)))
    (if (null merge-status)
        (error "Cannot resolve %s" file))
    (with-current-buffer base-buffer
      (if (string-match "^[0-9]+ [0-9a-f]+ 1" merge-status)
          (insert (magit-git-output `("cat-file" "blob" ,(concat ":1:" file))))))
    (with-current-buffer our-buffer
      (if (string-match "^[0-9]+ [0-9a-f]+ 2" merge-status)
          (insert (magit-git-output `("cat-file" "blob" ,(concat ":2:" file)))))
      (let ((buffer-file-name file))
        (normal-mode)))
    (with-current-buffer their-buffer
      (if (string-match "^[0-9]+ [0-9a-f]+ 3" merge-status)
          (insert (magit-git-output `("cat-file" "blob" ,(concat ":3:" file)))))
      (let ((buffer-file-name file))
        (normal-mode)))
    ;; We have now created the 3 buffer with ours, theirs and the ancestor files
    (with-current-buffer (ediff-merge-buffers-with-ancestor our-buffer their-buffer base-buffer () () file)
      (setq ediff-show-clashes-only t)
      (make-local-variable 'magit-ediff-windows)
      (setq magit-ediff-windows windows)
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
                    (when (bufferp buffer-Ancestor) (kill-buffer buffer-Ancestor))
                    (set-window-configuration windows)))))))

(defun magit-interactive-resolve-item ()
  (interactive)
  (magit-section-action (item info "resolv")
    ((diff)
     (magit-interactive-resolve (cadr info)))))

(defun magit-submodule-update (&optional init)
  "Update the submodule of the current git repository.
With a prefix arg, do a submodule update --init."
  (interactive "P")
  (let ((default-directory (magit-get-top-dir default-directory)))
    (apply #'magit-run-git-async "submodule" "update" (if init '("--init") ()))))

(defun magit-submodule-update-init ()
  "Update and init the submodule of the current git repository."
  (interactive)
  (magit-submodule-update t))

(defun magit-submodule-init ()
  "Initialize the submodules."
  (interactive)
  (let ((default-directory (magit-get-top-dir default-directory)))
    (magit-run-git-async "submodule" "init")))

(defun magit-submodule-sync ()
  "Synchronizes submodule's remote URL configuration."
  (interactive)
  (let ((default-directory (magit-get-top-dir default-directory)))
    (magit-run-git-async "submodule" "sync")))

(defun magit-run-git-gui ()
  "Run `git gui' for the current git repository."
  (interactive)
  (let* ((default-directory (magit-get-top-dir default-directory)))
    (magit-start-process "Git Gui" nil magit-git-executable "gui")))

(defun magit-run-gitk ()
  "Run `gitk --all' for the current git repository."
  (interactive)
  (let ((default-directory (magit-get-top-dir default-directory)))
    (cond
     ((eq system-type 'windows-nt)
      ;; Gitk is a shell script, and Windows doesn't know how to
      ;; "execute" it.  The Windows version of Git comes with an
      ;; implementation of "sh" and everything else it needs, but
      ;; Windows users might not have added the directory where it's
      ;; installed to their path
      (let ((git-bin-dir (file-name-directory magit-gitk-executable))
            (exec-path exec-path)
            (process-environment process-environment))
        (when git-bin-dir
          ;; Adding it onto the end so that anything the user
          ;; specified will get tried first.  Emacs looks in
          ;; exec-path; PATH is the environment variable inherited by
          ;; the process.  I need to change both.
          (setq exec-path (append exec-path (list git-bin-dir)))
          (push (format "PATH=%s;%s"
                        (getenv "PATH")
                        (replace-regexp-in-string "/" "\\\\" git-bin-dir))
                process-environment))
        (magit-start-process "Gitk" nil "sh" magit-gitk-executable "--all")))
     (t
      (magit-start-process "Gitk" nil magit-gitk-executable "--all")))))

(defun magit-load-config-extensions ()
  "Try to load magit extensions that are defined at git config layer.
This can be added to `magit-mode-hook' for example"
  (dolist (ext (magit-get-all "magit.extension"))
    (let ((sym (intern (format "magit-%s-mode" ext))))
      (when (and (fboundp sym)
                 (not (eq sym 'magit-wip-save-mode)))
        (funcall sym 1)))))

(magit-define-command grep (&optional pattern)
  (interactive)
  (let ((pattern (or pattern
                     (read-string "git grep: "
                                  (shell-quote-argument (grep-tag-default))))))
    (with-current-buffer (generate-new-buffer "*Magit Grep*")
      (let ((default-directory (magit-get-top-dir default-directory)))
        (insert magit-git-executable " "
                (mapconcat 'identity magit-git-standard-options " ")
                " grep -n "
                (shell-quote-argument pattern) "\n\n")
        (magit-git-insert (list "grep" "--line-number" pattern))
        (grep-mode)
        (pop-to-buffer (current-buffer))))))

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
                       "magit-with-silent-modifications"
                       "magit-with-section"
                       "magit-create-buffer-sections"
                       "magit-section-action"
                       "magit-add-action-clauses"
                       "with-magit-tmp-buffer"
                       "magit-create-log-buffer-sections"
                       "magit-with-revert-confirmation"
                       "magit-visiting-file-item") t)
                "\\>")
       . 1)))
  "Magit expressions to highlight in Emacs-Lisp mode.
To highlight Magit expressions add something like this to your
init file:

  (require 'magit)
  (font-lock-add-keywords 'emacs-lisp-mode
                          magit-font-lock-keywords)")

(provide 'magit)

;; rest of magit core
(require 'magit-key-mode)
(require 'magit-bisect)

;;; magit.el ends here
