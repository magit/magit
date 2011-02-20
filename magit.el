;;; magit.el -- control Git from Emacs.

;; Copyright (C) 2010 Aaron Culich.
;; Copyright (C) 2010 Alan Falloon.
;; Copyright (C) 2008, 2010 Alex Ott.
;; Copyright (C) 2008, 2009, 2010 Alexey Voinov.
;; Copyright (C) 2010 Ben Walton.
;; Copyright (C) 2010 Chris Bernard.
;; Copyright (C) 2010 Christian Kluge.
;; Copyright (C) 2008 Daniel Farina.
;; Copyright (C) 2010 David Abrahams.
;; Copyright (C) 2009 David Wallin.
;; Copyright (C) 2009, 2010 Hannu Koivisto.
;; Copyright (C) 2009 Ian Eure.
;; Copyright (C) 2009 Jesse Alama.
;; Copyright (C) 2009 John Wiegley.
;; Copyright (C) 2010 Leo.
;; Copyright (C) 2008, 2009 Marcin Bachry.
;; Copyright (C) 2008, 2009 Marius Vollmer.
;; Copyright (C) 2010 Mark Hepburn.
;; Copyright (C) 2010 Moritz Bunkus.
;; Copyright (C) 2010 Nathan Weizenbaum.
;; Copyright (C) 2010 Oscar Fuentes.
;; Copyright (C) 2009 Pavel Holejsovsky.
;; Copyright (C) 2009, 2010 Phil Jackson.
;; Copyright (C) 2010 Philip Weaver.
;; Copyright (C) 2010 Ramkumar Ramachandra.
;; Copyright (C) 2010 Remco van 't Veer.
;; Copyright (C) 2009 René Stadler.
;; Copyright (C) 2010 Robin Green.
;; Copyright (C) 2010 Roger Crew.
;; Copyright (C) 2009, 2010, 2011 Rémi Vanicat.
;; Copyright (C) 2010 Sean Bryant.
;; Copyright (C) 2009 Steve Purcell.
;; Copyright (C) 2010 Timo Juhani Lindfors.
;; Copyright (C) 2010 Yann Hodique.
;; Copyright (C) 2010 oscar.
;; Copyright (C) 2010 Ævar Arnfjörð Bjarmason.
;; Copyright (C) 2010 Óscar Fuentes.

;; Original Author: Marius Vollmer <marius.vollmer@nokia.com>
;; Lead developer: Phil Jackson <phil@shellarchive.co.uk>
;; Version: 0.8.2
;; Keywords: tools

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

;; magit core
(require 'magit-key-mode)

(eval-when-compile (require 'cl))
(require 'log-edit)
(require 'easymenu)
(require 'diff-mode)

;;; Code:
(defgroup magit nil
  "Controlling Git from Emacs."
  :prefix "magit-"
  :group 'tools)

(defcustom magit-git-executable "git"
  "The name of the Git executable."
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
When looking for a Git repository below the directories in `magit-repo-dirs',
Magit will only descend this many levels deep."
  :group 'magit
  :type 'integer)

(defcustom magit-save-some-buffers t
  "Non-nil means that \\[magit-status] will save modified buffers before running.
Setting this to t will ask which buffers to save, setting it to 'dontask will
save all modified buffers without asking."
  :group 'magit
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Ask" t)
		 (const :tag "Save without asking" dontask)))

(defcustom magit-save-some-buffers-predicate
  'magit-save-buffers-predicate-tree-only
  "Specifies a predicate function on \\[magit-save-some-buffers] to determine which
   unsaved buffers should be prompted for saving."

  :group 'magit
  :type '(radio (function-item magit-save-buffers-predicate-tree-only)
                (function-item magit-save-buffers-predicate-all)
		(function :tag "Other")))

(defcustom magit-commit-all-when-nothing-staged 'ask
  "Determines what \\[magit-log-edit] does when nothing is staged.
Setting this to nil will make it do nothing, setting it to t will
arrange things so that the actual commit command will use the \"--all\" option,
setting it to 'ask will first ask for confirmation whether to do this,
and setting it to 'ask-stage will cause all changes to be staged,
after a confirmation."
  :group 'magit
  :type '(choice (const :tag "No" nil)
		 (const :tag "Always" t)
		 (const :tag "Ask" ask)
		 (const :tag "Ask to stage everything" ask-stage)))

(defcustom magit-commit-signoff nil
  "When performing git commit adds --signoff."
  :group 'magit
  :type 'boolean)

(defcustom magit-log-cutoff-length 100
  "The maximum number of commits to show in the log and whazzup buffers."
  :group 'magit
  :type 'integer)

(defcustom magit-log-infinite-length 99999
  "Number of log used to show as maximum for `magit-log-cutoff-length'."
  :group 'magit
  :type 'integer)

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

The value 'name-then-remote means remotes will be of the
form \"name (remote)\", while the value 'remote-slash-name
means that they'll be of the form \"remote/name\". I.e. something that's
listed as \"remotes/upstream/next\" by \"git branch -l -a\"
will be \"upstream/next\"."
  :group 'magit
  :type '(choice (const :tag "name (remote)" branch-then-remote)
                 (const :tag "remote/name" remote-slash-branch)))

(defcustom magit-process-connection-type (not (eq system-type 'cygwin))
  "Connection type used for the git process.

nil mean pipe, it is usually faster and more efficient, and work on cygwin.
t mean pty, it enable magit to prompt for passphrase when needed."
  :group 'magit
  :type 'boolean)

(defcustom magit-completing-read-function 'magit-builtin-completing-read
  "Function to be called when requesting input from the user."
  :group 'magit
  :type '(radio (function-item magit-iswitchb-completing-read)
		(function-item magit-builtin-completing-read)
		(function :tag "Other")))


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

(defface magit-log-graph
  '((((class color) (background light))
     :foreground "grey11")
    (((class color) (background dark))
     :foreground "grey80"))
  "Face for the graph element of the log output."
  :group 'magit)

(defface magit-log-sha1
  '((((class color) (background light))
     :foreground "firebrick")
    (((class color) (background dark))
     :foreground "tomato"))
  "Face for the sha1 element of the log output."
  :group 'magit)

(defface magit-log-message
  '((t))
  "Face for the message element of the log output."
  :group 'magit)

(defface magit-item-highlight
  '((((class color) (background light))
     :background "gray95")
    (((class color) (background dark))
     :background "dim gray"))
  "Face for highlighting the current item."
  :group 'magit)

(defface magit-item-mark
  '((((class color) (background light))
     :foreground "red")
    (((class color) (background dark))
     :foreground "orange"))
  "Face for highlighting marked item."
  :group 'magit)

(defface magit-log-tag-label
  '((((class color) (background light))
     :background "LightGoldenRod")
    (((class color) (background dark))
     :background "DarkGoldenRod"))
  "Face for git tag labels shown in log buffer."
  :group 'magit)

(defface magit-log-head-label-bisect-good
  '((((class color) (background light))
     :box t
     :background "light green"
     :foreground "dark olive green")
    (((class color) (background dark))
     :box t
     :background "light green"
     :foreground "dark olive green"))
  "Face for good bisect refs"
  :group 'magit)

(defface magit-log-head-label-bisect-bad
  '((((class color) (background light))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4")
    (((class color) (background dark))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4"))
  "Face for bad bisect refs"
  :group 'magit)

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
  :group 'magit)

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
  :group 'magit)

(defface magit-log-head-label-patches
  '((((class color) (background light))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4")
    (((class color) (background dark))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4"))
  "Face for Stacked Git patches"
  :group 'magit)

(defvar magit-custom-options '()
  "List of custom options to pass git. Do not customise this.")

(defvar magit-read-rev-history nil
  "The history of inputs to `magit-read-rev'.")

(defvar magit-omit-untracked-dir-contents nil
  "When non-nil magit will only list an untracked directory, not its contents.")

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
  :group 'magit)

(defface magit-menu-selected-option
  '((((class color) (background light))
     :foreground "red")
    (((class color) (background dark))
     :foreground "orange"))
  "Face for selected options on magit's menu"
  :group 'magit)

(defvar magit-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "n") 'magit-goto-next-section)
    (define-key map (kbd "p") 'magit-goto-previous-section)
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
    (define-key map (kbd "<M-left>") 'magit-goto-parent-section)
    (define-key map (kbd "g") 'magit-refresh)
    (define-key map (kbd "G") 'magit-refresh-all)
    (define-key map (kbd "?") 'magit-describe-item)
    (define-key map (kbd "!") 'magit-key-mode-popup-running)
    (define-key map (kbd ":") 'magit-git-command)
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
    (define-key map (kbd "F") 'magit-key-mode-popup-pulling)
    (define-key map (kbd "l") 'magit-key-mode-popup-logging)
    (define-key map (kbd "$") 'magit-display-process)
    (define-key map (kbd "c") 'magit-log-edit)
    (define-key map (kbd "E") 'magit-interactive-rebase)
    (define-key map (kbd "w") 'magit-wazzup)
    (define-key map (kbd "q") 'quit-window)
    map))

(defvar magit-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'magit-apply-item)
    (define-key map (kbd "A") 'magit-cherry-pick-item)
    (define-key map (kbd "v") 'magit-revert-item)
    map))

(defvar magit-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "-") 'magit-diff-smaller-hunks)
    (define-key map (kbd "+") 'magit-diff-larger-hunks)
    (define-key map (kbd "0") 'magit-diff-default-hunks)
    (define-key map (kbd "s") 'magit-stage-item)
    (define-key map (kbd "S") 'magit-stage-all)
    (define-key map (kbd "u") 'magit-unstage-item)
    (define-key map (kbd "U") 'magit-unstage-all)
    (define-key map (kbd "i") 'magit-ignore-item)
    (define-key map (kbd "I") 'magit-ignore-item-locally)
    (define-key map (kbd ".") 'magit-mark-item)
    (define-key map (kbd "=") 'magit-diff-with-mark)
    (define-key map (kbd "d") 'magit-diff-working-tree)
    (define-key map (kbd "D") 'magit-diff)
    (define-key map (kbd "a") 'magit-apply-item)
    (define-key map (kbd "A") 'magit-cherry-pick-item)
    (define-key map (kbd "v") 'magit-revert-item)
    (define-key map (kbd "b") 'magit-key-mode-popup-branching)
    (define-key map (kbd "m") 'magit-key-mode-popup-merging)
    (define-key map (kbd "M") 'magit-key-mode-popup-submodule)
    (define-key map (kbd "k") 'magit-discard-item)
    (define-key map (kbd "e") 'magit-interactive-resolve-item)
    (define-key map (kbd "C") 'magit-add-log)
    (define-key map (kbd "x") 'magit-reset-head)
    (define-key map (kbd "X") 'magit-reset-working-tree)
    (define-key map (kbd "z") 'magit-key-mode-popup-stashing)
    map))

(defvar magit-stash-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "-") 'magit-diff-smaller-hunks)
    (define-key map (kbd "+") 'magit-diff-larger-hunks)
    (define-key map (kbd "0") 'magit-diff-default-hunks)
    (define-key map (kbd "a") 'magit-apply-item)
    (define-key map (kbd "A") 'magit-cherry-pick-item)
    (define-key map (kbd "v") 'magit-revert-item)
    map))

(defvar magit-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ".") 'magit-mark-item)
    (define-key map (kbd "=") 'magit-diff-with-mark)
    (define-key map (kbd "d") 'magit-diff-working-tree)
    (define-key map (kbd "D") 'magit-diff)
    (define-key map (kbd "a") 'magit-apply-item)
    (define-key map (kbd "A") 'magit-cherry-pick-item)
    (define-key map (kbd "v") 'magit-revert-item)
    (define-key map (kbd "b") 'magit-key-mode-popup-branching)
    (define-key map (kbd "m") 'magit-key-mode-popup-merging)
    (define-key map (kbd "x") 'magit-reset-head)
    (define-key map (kbd "e") 'magit-log-show-more-entries)
    (define-key map (kbd "l") 'magit-key-mode-popup-logging)
    (define-key map (kbd "t") 'magit-key-mode-popup-tagging)
    map))

(defvar magit-reflog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ".") 'magit-mark-item)
    (define-key map (kbd "=") 'magit-diff-with-mark)
    (define-key map (kbd "d") 'magit-diff-working-tree)
    (define-key map (kbd "D") 'magit-diff)
    (define-key map (kbd "a") 'magit-apply-item)
    (define-key map (kbd "A") 'magit-cherry-pick-item)
    (define-key map (kbd "v") 'magit-revert-item)
    (define-key map (kbd "x") 'magit-reset-head)
    map))

(defvar magit-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "-") 'magit-diff-smaller-hunks)
    (define-key map (kbd "+") 'magit-diff-larger-hunks)
    (define-key map (kbd "0") 'magit-diff-default-hunks)
    (define-key map (kbd "a") 'magit-apply-item)
    (define-key map (kbd "A") 'magit-cherry-pick-item)
    (define-key map (kbd "v") 'magit-revert-item)
    map))

(defvar magit-wazzup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ".") 'magit-mark-item)
    (define-key map (kbd "=") 'magit-diff-with-mark)
    (define-key map (kbd "d") 'magit-diff-working-tree)
    (define-key map (kbd "D") 'magit-diff)
    (define-key map (kbd "a") 'magit-apply-item)
    (define-key map (kbd "A") 'magit-cherry-pick-item)
    (define-key map (kbd "v") 'magit-revert-item)
    (define-key map (kbd "b") 'magit-key-mode-popup-branching)
    (define-key map (kbd "m") 'magit-key-mode-popup-merging)
    (define-key map (kbd "x") 'magit-reset-head)
    (define-key map (kbd "i") 'magit-ignore-item)
    map))

(defvar magit-show-branches-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'magit-branches-window-checkout)
    (define-key map (kbd "b") 'magit-branches-window-checkout)
    (define-key map (kbd "k") 'magit-remove-branch)
    (define-key map (kbd "K") 'magit-remove-branch-in-remote-repo)
    (define-key map (kbd "$") 'magit-display-process)
    (define-key map (kbd "q") 'magit-quit-branches-window)
    (define-key map (kbd "g") 'magit-show-branches)
    (define-key map (kbd "v") 'magit-show-branches)
    (define-key map (kbd "T") 'magit-change-what-branch-tracks)
    (define-key map (kbd "t") 'magit-key-mode-popup-tagging)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    map))

(defvar magit-bug-report-url "http://github.com/philjackson/magit/issues")
(defun magit-bug-report (str)
  (message "Unknown error: %s\nPlease file a bug at %s"
	   str magit-bug-report-url))

;;; Macros

(defmacro magit-with-refresh (&rest body)
  (declare (indent 0))
  `(magit-refresh-wrapper (lambda () ,@body)))

(eval-when-compile
  (when (< emacs-major-version 23)
    (defvar line-move-visual nil)))

;;; Compatibilities

(if (functionp 'start-file-process)
    (defalias 'magit-start-process 'start-file-process)
    (defalias 'magit-start-process 'start-process))

;;; Utilities

(defvar magit-submode nil)
(make-variable-buffer-local 'magit-submode)
(put 'magit-submode 'permanent-local t)

(defun magit-iswitchb-completing-read (prompt choices &optional predicate require-match
                                       initial-input hist def)
  "iswitchb-based completing-read almost-replacement."
  (require 'iswitchb)
  (let ((iswitchb-make-buflist-hook
         (lambda ()
           (setq iswitchb-temp-buflist (if (consp (first choices))
                                           (mapcar #'car choices)
                                         choices)))))
    (iswitchb-read-buffer prompt (or initial-input def) require-match)))

(defun magit-builtin-completing-read (prompt choices &optional predicate require-match
                                      initial-input hist def)
  "Magit wrapper for standard completing-read function."
  (completing-read (if (and def (> (length prompt) 2)
                            (string-equal ": " (substring prompt -2)))
                       (format "%s (default %s): " (substring prompt 0 -2) def)
                     prompt)
                   choices predicate require-match initial-input hist def))

(defun magit-completing-read (prompt choices &optional predicate require-match
                              initial-input hist def)
  (funcall magit-completing-read-function prompt choices predicate require-match
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
  (apply #'process-file
	 magit-git-executable
	 nil (list t nil) nil
	 (append magit-git-standard-options args)))

(defun magit-git-output (args)
  (with-output-to-string
    (with-current-buffer
        standard-output
      (magit-git-insert args))))

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

(defun magit-concat-with-delim (delim seqs)
  (cond ((null seqs)
	 nil)
	((null (cdr seqs))
	 (car seqs))
	(t
	 (concat (car seqs) delim (magit-concat-with-delim delim (cdr seqs))))))

(defun magit-get (&rest keys)
  (magit-git-string "config" (magit-concat-with-delim "." keys)))

(defun magit-get-all (&rest keys)
  (magit-git-lines "config" "--get-all" (magit-concat-with-delim "." keys)))

(defun magit-set (val &rest keys)
  (if val
      (magit-git-string "config" (magit-concat-with-delim "." keys) val)
    (magit-git-string "config" "--unset" (magit-concat-with-delim "." keys))))

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

(defun magit-get-remote (branch)
  "Return the name of the remote for BRANCH.
If branch is nil or it has no remote, but a remote named
\"origin\" exists, return that. Otherwise, return nil."
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

(defun magit-read-top-dir (rawp)
  (if (and (not rawp) magit-repo-dirs)
      (let* ((repos (magit-list-repos magit-repo-dirs))
	     (reply (magit-completing-read "Git repository: "
                                           (magit-list-repos magit-repo-dirs))))
	(file-name-as-directory
	 (cdr (assoc reply repos))))
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

(defun magit-name-rev (rev)
  "Return a human-readable name for REV.
Unlike git name-rev, this will remove tags/ and remotes/ prefixes
if that can be done unambiguously.  In addition, it will filter
out revs involving HEAD."
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
          (unless (magit-ref-ambiguous-p plain-name)
            (setq rev plain-name))))
      rev)))

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
                        (loop for i in uninteresting thereis (string-match i ref))))
                  ((string-match "refs/heads/\\(.*\\)" ref)
                   (let ((branch (match-string 1 ref)))
                     (push (cons branch branch) refs)))
                  ((string-match "refs/tags/\\(.*\\)" ref)
                   (push (cons (format
                                (if (eq magit-remote-ref-format 'branch-then-remote)
                                    "%s (tag)" "%s")
                                (match-string 1 ref))
                               ref)
                         refs))
                  ((string-match "refs/remotes/\\([^/]+\\)/\\(.+\\)" ref)
                   (push (cons (if (eq magit-remote-ref-format 'branch-then-remote)
                                   (format "%s (%s)"
                                           (match-string 2 ref)
                                           (match-string 1 ref))
                                 (format "%s/%s"
                                         (match-string 1 ref)
                                         (match-string 2 ref)))
                               ref)
                         refs))))))
    (nreverse refs)))

(defvar magit-uninteresting-refs '("refs/remotes/\\([^/]+\\)/HEAD$"))

;; TODO: fix this so that def can (must?) be git rev instead of, say, "master (origin)"
;; which involves a particular display strategy and shouldn't be visible to callers
;; of magit-read-rev
(defun magit-read-rev (prompt &optional def uninteresting)
  (let* ((interesting-refs (magit-list-interesting-refs
                            (or uninteresting magit-uninteresting-refs)))
	 (reply (magit-completing-read (concat prompt ": ") interesting-refs
                                       nil nil nil 'magit-read-rev-history def))
	 (rev (or (cdr (assoc reply interesting-refs)) reply)))
    (if (string= rev "")
	nil
      rev)))

(defun magit-read-rev-range (op &optional def-beg def-end)
  (let ((beg (magit-read-rev (format "%s start" op)
			     def-beg)))
    (if (not beg)
	nil
      (let ((end (magit-read-rev (format "%s end" op) def-end)))
	(cons beg end)))))

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
  (or (magit-name-rev (magit-commit-at-point t))
      (let ((branch (magit-guess-branch)))
	(if branch
	    (if (string-match "^refs/\\(.*\\)" branch)
		(match-string 1 branch)
		branch)))))

(defun magit-read-remote (&optional prompt def)
  "Read the name of a remote.
PROMPT is used as the prompt, and defaults to \"Remote\".
DEF is the default value, and defaults to the value of `magit-get-current-branch'."
  (let* ((prompt (or prompt "Remote: "))
         (def (or def (magit-get-current-remote)))
	 (remotes (magit-git-lines "remote"))
	 (reply (magit-completing-read prompt remotes
                                       nil nil nil nil def)))
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

(defstruct magit-section
  parent title beginning end children hidden type info
  needs-refresh-on-show)

(defvar magit-top-section nil
  "The top section of the current buffer.")
(make-variable-buffer-local 'magit-top-section)
(put 'magit-top-section 'permanent-local t)

(defvar magit-old-top-section nil)

(defvar magit-section-hidden-default nil)

(defun magit-new-section (title type)
  "Create a new section with title TITLE and type TYPE in current buffer.

If not `magit-top-section' exist, the new section will be the new top-section
otherwise, the new-section will be a child of the current top-section.

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

Sections create into BODY will be child of the new section.
BODY must leave point at the end of the created section.

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

(defun magit-set-section-info (info &optional section)
  (setf (magit-section-info (or section magit-top-section)) info))

(defun magit-set-section-needs-refresh-on-show (flag &optional section)
  (setf (magit-section-needs-refresh-on-show
	 (or section magit-top-section))
	flag))

(defmacro magit-create-buffer-sections (&rest body)
  "Empty current buffer of text and magit's section, and then evaluate BODY."
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

;; Dead code:
(defun magit-find-section-at (pos secs)
  "Return the section at POS in SECS."
  ;; Could use the text-property
  (while (and secs
	      (not (and (<= (magit-section-beginning (car secs)) pos)
			(<  pos (magit-section-end (car secs))))))
    (setq secs (cdr secs)))
  (if secs
      (or (magit-find-section-at pos (magit-section-children (car secs)))
	  (car secs))
    nil))

(defun magit-find-section-after (pos secs)
  "Find the first section that begins after POS in the list SECS."
  (while (and secs
	      (not (> (magit-section-beginning (car secs)) pos)))
    (setq secs (cdr secs)))
  (car secs))

(defun magit-find-section-before (pos secs)
  "Find the last section that begins before POS in the list SECS."
  (let ((prev nil))
    (while (and secs
		(not (> (magit-section-beginning (car secs)) pos)))
      (setq prev (car secs))
      (setq secs (cdr secs)))
    prev))

(defun magit-current-section ()
  "Return the magit section at point."
  (or (get-text-property (point) 'magit-section)
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
It should add sectioning as needed for magit interaction

CMD is an external command that will be run with ARGS as arguments"
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
	    (apply 'process-file cmd nil t nil args)
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
  "Run git and put its result in a new section.

see `magit-insert-section' for meaning of the arguments"
  (apply #'magit-insert-section
	 section-title-and-type
	 buffer-title
	 washer
	 magit-git-executable
	 (append magit-git-standard-options args)))

(defun magit-next-section (section)
  "Return the section that is after SECTION."
  (let ((parent (magit-section-parent section)))
    (if parent
	(let ((next (cadr (memq section
				(magit-section-children parent)))))
	  (or next
	      (magit-next-section parent))))))

(defun magit-goto-next-section ()
  "Go to the next magit section."
  (interactive)
  (let* ((section (magit-current-section))
	 (next (or (and (not (magit-section-hidden section))
			(magit-section-children section)
			(magit-find-section-after (point)
						  (magit-section-children
						   section)))
		   (magit-next-section section))))

    (if next
	(progn
	  (goto-char (magit-section-beginning next))
	  (if (memq magit-submode '(log reflog))
	      (magit-show-commit next))
	  (if (not (magit-section-hidden next))
	      (let ((offset (- (line-number-at-pos
				(magit-section-beginning next))
			       (line-number-at-pos
				(magit-section-end next)))))
		(if (< offset (window-height))
		    (recenter offset)))))
      (message "No next section"))))

(defun magit-prev-section (section)
  "Return the section that is before SECTION."
  (let ((parent (magit-section-parent section)))
    (if parent
	(let ((prev (cadr (memq section
				(reverse (magit-section-children parent))))))
	  (cond (prev
		 (while (and (not (magit-section-hidden prev))
			     (magit-section-children prev))
		   (setq prev (car (reverse (magit-section-children prev)))))
		 prev)
		(t
		 parent))))))

(defun magit-goto-previous-section ()
  "Goto the previous magit section."
  (interactive)
  (let ((section (magit-current-section)))
    (cond ((= (point) (magit-section-beginning section))
	   (let ((prev (magit-prev-section (magit-current-section))))
	     (if prev
		 (progn
		   (if (memq magit-submode '(log reflog))
		       (magit-show-commit (or prev section)))
		   (goto-char (magit-section-beginning prev)))
	       (message "No previous section"))))
	  (t
	   (let ((prev (magit-find-section-before (point)
						  (magit-section-children
						   section))))
	     (if (memq magit-submode '(log reflog))
		 (magit-show-commit (or prev section)))
	     (goto-char (magit-section-beginning (or prev section))))))))

(defun magit-goto-parent-section ()
  "Goto the parent section."
  (interactive)
  (let ((parent (magit-section-parent (magit-current-section))))
    (when parent
      (goto-char (magit-section-beginning parent)))))

(defun magit-goto-section (path)
  "Goto the section describe by PATH."
  (let ((sec (magit-find-section path magit-top-section)))
    (if sec
	(goto-char (magit-section-beginning sec))
      (message "No such section"))))

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
      (put-text-property beg end 'invisible hidden))
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

If FLAG-OR-FUNC is a function, it will be ran on current section
IF FLAG-OR-FUNC is a Boolean value, the section will be hidden if its true, shown otherwise"
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
  (when (eq 'hunk (first (magit-section-context-type (magit-current-section))))
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
	   ((notany #'magit-section-hidden (magit-section-children s))
	    (magit-section-set-hidden s t))
	   (t
	    (magit-section-expand s))))))

(defun magit-section-lineage (s)
  "Return list of parent, grand-parents... for section S."
  (when s
    (cons s (magit-section-lineage (magit-section-parent s)))))

(defun magit-section-show-level (section level threshold path)
  (magit-section-set-hidden section (>= level threshold))
  (when (< level threshold)
    (if path
	(magit-section-show-level (car path) (1+ level) threshold (cdr path))
	(dolist (c (magit-section-children section))
	  (magit-section-show-level c (1+ level) threshold nil)))))

(defun magit-show-level (level all)
  "Show section whose level is less than LEVEL, hide the others.
If ALL is non nil, do this in all sections,
otherwise do it only on ancestors and descendants of current section."
  (magit-with-refresh
    (if all
	(magit-section-show-level magit-top-section 0 level nil)
      (let ((path (reverse (magit-section-lineage (magit-current-section)))))
	(magit-section-show-level (car path) 0 level (cdr path))))))

(defun magit-show-only-files ()
  "Show section that are files, but not there subsection.

Do this in on ancestors and descendants of current section."
  (interactive)
  (if (eq magit-submode 'status)
      (call-interactively 'magit-show-level-2)
    (call-interactively 'magit-show-level-1)))

(defun magit-show-only-files-all ()
  "Show section that are files, but not there subsection.

Do this for all sections"
  (interactive)
  (if (eq magit-submode 'status)
      (call-interactively 'magit-show-level-2-all)
    (call-interactively 'magit-show-level-1-all)))

(defmacro magit-define-level-shower-1 (level all)
  "Define an interactive function to show function of level LEVEL.

If ALL is non nil, this function will affect all section,
otherwise it will affect only ancestors and descendants of current section."
  (let ((fun (intern (format "magit-show-level-%s%s"
			     level (if all "-all" ""))))
	(doc (format "Show sections on level %s." level)))
    `(defun ,fun ()
       ,doc
       (interactive)
       (magit-show-level ,level ,all))))

(defmacro magit-define-level-shower (level)
  "Define two interactive function to show function of level LEVEL.
one for all, one for current lineage."
  `(progn
     (magit-define-level-shower-1 ,level nil)
     (magit-define-level-shower-1 ,level t)))

(defmacro magit-define-section-jumper (sym title)
  "Define an interactive function to go to section SYM.

TITLE is the displayed title of the section."
  (let ((fun (intern (format "magit-jump-to-%s" sym)))
	(doc (format "Jump to section `%s'." title)))
    `(defun ,fun ()
       ,doc
       (interactive)
       (magit-goto-section '(,sym)))))

(defmacro magit-define-inserter (sym arglist &rest body)
  (declare (indent defun))
  (let ((fun (intern (format "magit-insert-%s" sym)))
        (before (intern (format "magit-before-insert-%s-hook" sym)))
        (after (intern (format "magit-after-insert-%s-hook" sym)))
        (doc (format "Insert items for `%s'." sym)))
    `(defun ,fun ,arglist
       ,doc
       (run-hooks ',before)
       ,@body
       (run-hooks ',after))))

(defvar magit-highlight-overlay nil)

(defvar magit-highlighted-section nil)

(defun magit-highlight-section ()
  "Highlight current section if it have a type."
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
otherwise it roturn t.

If no section matches, this returns nil if no OPNAME was given
and throws an error otherwise."
  (declare (indent 1))
  (let ((section (car head))
        (info (cadr head))
        (type (make-symbol "*type*"))
        (context (make-symbol "*context*"))
        (opname (caddr head)))
    `(let* ((,section (magit-current-section))
            (,info (magit-section-info ,section))
            (,type (magit-section-type ,section))
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
        (info (cadr head))
        (type (caddr head)))
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
  (declare (indent defun))
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
    `(defun ,fun ,arglist
       ,doc
       ,inter
       (or (run-hook-with-args-until-success
            ',hook ,@(remq '&optional (remq '&rest arglist)))
           ,@instr))))

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
			 (magit-concat-with-delim " " cmd-and-args))
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
	   (or (save-excursion
		 (set-buffer (get-buffer magit-process-buffer-name))
		 (when (re-search-backward
			(concat "^error: \\(.*\\)" paragraph-separate) nil t)
		   (match-string 1)))
	       "Git failed")))
      successp)))

(autoload 'dired-uncache "dired")
(defun magit-process-sentinel (process event)
  (let ((msg (format "Git %s." (substring event 0 -1)))
	(successp (string-match "^finished" event)))
    (with-current-buffer (process-buffer process)
      (let ((inhibit-read-only t))
	(goto-char (point-max))
	(insert msg "\n")
	(message msg))
      (unless (memq (process-status process) '(run open))
        (dired-uncache default-directory)))
    (setq magit-process nil)
    (magit-set-mode-line-process nil)
    (magit-refresh-buffer magit-process-client-buffer)))

(defun magit-password (proc string)
  "Checks if git/ssh asks for a password and ask the user for it."
  (when (or (string-match "^Enter passphrase for key '\\\(.*\\\)': $" string)
	    (string-match "^\\\(.*\\\)'s password:" string))
    (process-send-string proc
                         (concat (read-passwd
                                  (format "Password for '%s': " (match-string 1 string))
                                  nil) "\n"))))

(defun magit-process-filter (proc string)
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (let ((inhibit-read-only t))
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

(defun magit-run-with-input (input cmd &rest args)
  (magit-with-refresh
    (magit-run* (cons cmd args) nil nil nil nil input)))

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

(defun magit-true (prompt)
  "Dummy function for turning on options on menu."
  t)

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
     ["Short Log" magit-display-log t]
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
    ["Merge" magit-automatic-merge t]
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
    ["Quit Magit" quit-window t]))

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
  (setq buffer-read-only t)
  (make-local-variable 'line-move-visual)
  (setq major-mode 'magit-mode
	mode-name "Magit"
	mode-line-process ""
	truncate-lines t
	line-move-visual nil)
  (add-hook 'pre-command-hook #'magit-remember-point nil t)
  (add-hook 'post-command-hook #'magit-post-command-hook t t)
  (use-local-map magit-mode-map)
  (run-mode-hooks 'magit-mode-hook))

(defun magit-mode-init (dir submode refresh-func &rest refresh-args)
  (setq default-directory dir
	magit-submode submode
	magit-refresh-function refresh-func
	magit-refresh-args refresh-args)
  (magit-mode)
  (magit-refresh-buffer))

(defun magit-find-buffer (submode &optional dir)
  (let ((topdir (magit-get-top-dir (or dir default-directory))))
    (dolist (buf (buffer-list))
      (if (with-current-buffer buf
	    (and default-directory
		 (equal (expand-file-name default-directory) topdir)
		 (eq major-mode 'magit-mode)
		 (eq magit-submode submode)))
	  (return buf)))))

(defun magit-find-status-buffer (&optional dir)
  (magit-find-buffer 'status dir))

(defun magit-for-all-buffers (func &optional dir)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (if (and (eq major-mode 'magit-mode)
	       (or (null dir)
		   (equal default-directory dir)))
	  (funcall func)))))

(defun magit-refresh-buffer (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let* ((old-line (line-number-at-pos))
	   (old-section (magit-current-section))
	   (old-path (and old-section
			  (magit-section-path (magit-current-section))))
	   (section-line (and old-section
			      (count-lines
			       (magit-section-beginning old-section)
			       (point)))))
      (if magit-refresh-function
	  (apply magit-refresh-function
		 magit-refresh-args))
      (magit-refresh-marked-commits-in-buffer)
      (let ((s (and old-path (magit-find-section old-path magit-top-section))))
	(cond (s
	       (goto-char (magit-section-beginning s))
	       (forward-line section-line))
	      (t
	       (magit-goto-line old-line)))
	(dolist (w (get-buffer-window-list (current-buffer)))
	  (set-window-point w (point)))
	(magit-highlight-section)))))

(defun magit-string-has-prefix-p (string prefix)
  (eq (compare-strings string nil (length prefix) prefix nil nil) t))

(defun magit-revert-buffers (dir &optional ignore-modtime)
  (dolist (buffer (buffer-list))
    (when (and buffer
	       (buffer-file-name buffer)
	       (file-readable-p (buffer-file-name buffer))
	       (magit-string-has-prefix-p (buffer-file-name buffer) dir)
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
	   (status-buffer (magit-find-buffer 'status dir))
	   (magit-refresh-needing-buffers nil)
	   (magit-refresh-pending t))
      (unwind-protect
	  (funcall func)
	(when magit-refresh-needing-buffers
	  (magit-revert-buffers dir)
	  (dolist (b (adjoin status-buffer
			     magit-refresh-needing-buffers))
	    (magit-refresh-buffer b)))))))

(defun magit-need-refresh (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (when (not (memq buffer magit-refresh-needing-buffers))
      (setq magit-refresh-needing-buffers
	    (cons buffer magit-refresh-needing-buffers)))))

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

(defun magit-diff-line-file ()
  (cond ((looking-at "^diff --git ./\\(.*\\) ./\\(.*\\)$")
	 (match-string-no-properties 2))
	((looking-at "^diff --cc +\\(.*\\)$")
	 (match-string-no-properties 1))
	(t
	 nil)))

(defun magit-wash-diffs ()
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

(defvar magit-hide-diffs nil)

(defun magit-insert-diff-title (status file file2)
  (let ((status-text (case status
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
		       (t
			(format "?        %s" file)))))
    (insert "\t" status-text "\n")))

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
	     (magit-set-section-info (list status file file2))
	     (magit-insert-diff-title status file file2)
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
  (caddr (magit-section-info diff)))

(defun magit-wash-hunk ()
  (cond ((looking-at "\\(^@+\\)[^@]*@+")
	 (let ((n-columns (1- (length (match-string 1))))
	       (head (match-string 0)))
	   (magit-with-section head 'hunk
	     (add-text-properties (match-beginning 0) (match-end 0)
				  '(face magit-diff-hunk-header))
	     (forward-line)
	     (while (not (or (eobp)
			     (looking-at "^diff\\|^@@")))
	       (let ((prefix (buffer-substring-no-properties
			      (point) (min (+ (point) n-columns) (point-max)))))
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

(defvar magit-diff-options nil)

(defun magit-insert-diff (file)
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
	(magit-wash-diff-section)
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
	    (status (case (string-to-char (match-string-no-properties 3))
		      (?A 'new)
		      (?D 'deleted)
		      (?M 'modified)
		      (?U 'unmerged)
		      (?T 'new-type)
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
		  (magit-insert-diff file)
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
      (if (looking-at "-")
	  (error "Can't visit removed lines"))
      (goto-char (magit-section-beginning hunk))
      (if (not (looking-at "@@+ .* \\+\\([0-9]+\\),[0-9]+ @@+"))
	  (error "Hunk header not found"))
      (let ((target (string-to-number (match-string 1))))
	(forward-line)
	(while (< (line-number-at-pos) line)
	  ;; XXX - deal with combined diffs
	  (if (not (looking-at "-"))
	      (setq target (+ target 1)))
	  (forward-line))
	target))))

(defvar magit-tmp-buffer-name " *magit-tmp*")

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
  (when (zerop magit-diff-context-lines)
    (setq args (cons "--unidiff-zero" args)))
  (with-magit-tmp-buffer tmp
    (if (magit-use-region-p)
	(magit-insert-hunk-item-region-patch
	 hunk reverse (region-beginning) (region-end) tmp)
	(magit-insert-hunk-item-patch hunk tmp))
    (apply #'magit-run-git-with-input tmp
	   "apply" (append args (list "-")))))

(defun magit-apply-hunk-item (hunk &rest args)
  (apply #'magit-apply-hunk-item* hunk nil args))

(defun magit-apply-hunk-item-reverse (hunk &rest args)
  (apply #'magit-apply-hunk-item* hunk t (cons "--reverse" args)))

(magit-define-inserter unstaged-changes (title)
  (let ((magit-hide-diffs t))
    (let ((magit-diff-options (append '() magit-diff-options)))
      (magit-git-section 'unstaged title 'magit-wash-raw-diffs
                         "diff-files"))))

(magit-define-inserter staged-changes (staged no-commit)
  (when staged
    (let ((magit-hide-diffs t)
          (base (if no-commit
                    (magit-git-string "mktree")
                  "HEAD")))
      (let ((magit-diff-options (append '("--cached") magit-diff-options))
            (magit-ignore-unmerged-raw-diffs t))
        (magit-git-section 'staged "Staged changes:" 'magit-wash-raw-diffs
                           "diff-index" "--cached"
                           base)))))

;;; Logs and Commits

(defvar magit-log-oneline-re
  (concat
   "^\\([_\\*|/ -.]+\\)?"                                  ; graph   (1)
   "\\(?:"
   "\\([0-9a-fA-F]\\{40\\}\\) "                            ; sha1    (2)
   "\\(?:\\((\\(?:[^ ]+?\\(?:[:,] \\)?\\)+)\\) \\)?"       ; refs    (3)
   "\\(.*\\)"                                              ; msg     (4)
   "\\)?$")
  "Regexp used to extract elements of git log output.
Those output are generated by --pretty=oneline with graph, or --pretty=format:* %H %s")

(defvar magit-present-log-line-function 'magit-present-log-line
  "The function to use when generating a log line.
It takes four args: CHART, SHA1, REFS and MESSAGE.  The function
must return a string which will represent the log line.")

(defun magit-present-log-line (graph sha1 refs message)
  "The default log line generator."
  (let* ((ref-re "\\(?:tag: \\)?refs/\\(bisect\\|tags\\|remotes\\|patches/[^/]*\\|heads\\)/\\(.+\\)")
	 (string-refs
	  (when refs
	    (concat (mapconcat
		     (lambda (r)
		       (propertize
			(if (string-match ref-re r)
			    (match-string 2 r)
			  r)
			'face (cond
			       ((string= r "refs/stash")
				'magit-log-head-label-local)
			       ((string= (match-string 1 r) "remotes")
				'magit-log-head-label-remote)
			       ((string-match "^patches/[^/]*$" (match-string 1 r)) ; Stacked Git
				'magit-log-head-label-patches)
			       ((string= (match-string 1 r) "bisect")
				(if (string= (match-string 2 r) "bad")
				    'magit-log-head-label-bisect-bad
				  'magit-log-head-label-bisect-good))
			       ((string= (match-string 1 r) "tags")
				'magit-log-head-label-tags)
			       ((string= (match-string 1 r) "heads")
				'magit-log-head-label-local))))
		     refs
		     " ")
		    " "))))
    (concat
     (if sha1
	 (propertize (substring sha1 0 8) 'face 'magit-log-sha1)
       (insert-char ? 8))
     " "
     (when graph
       (propertize graph 'face 'magit-log-graph))
     string-refs
     (when message
       (propertize message 'face 'magit-log-message)))))

(defvar magit-log-count ()
  "Internal var used to count the number of logs actually added in a buffer.")

(defmacro magit-create-log-buffer-sections (&rest body)
  "Empty current buffer of text and magit's section, and then evaluate BODY.

if the number of logs inserted in the buffer is `magit-log-cutoff-length'
insert a line to tell how to insert more of them"
  (declare (indent 0))
  `(let ((magit-log-count 0) (inhibit-read-only t))
     (magit-create-buffer-sections
       ,@body
       (if (= magit-log-count magit-log-cutoff-length)
	   (magit-with-section "longer"  'longer
	     (insert "type \"e\" to show more logs\n"))))))


(defun magit-wash-log-line ()
  (beginning-of-line)
  (let ((line-re magit-log-oneline-re))
    (cond
     ((looking-at magit-log-oneline-re)
      (let ((chart (match-string 1))
            (sha1 (match-string 2))
            (msg (match-string 4))
            (refs (when (match-string 3)
		    (delq nil
			  (mapcar
			   (lambda (s)
			     (and (not
				   (or (string= s "tag:")
				       (string= s "HEAD"))) ; as of 1.6.6
				  s))
			   (split-string (match-string 3) "[(), ]" t))))))
        (delete-region (point-at-bol) (point-at-eol))
        (insert (funcall magit-present-log-line-function chart sha1 refs msg))
        (goto-char (point-at-bol))
        (if sha1
            (magit-with-section sha1 'commit
              (when magit-log-count (setq magit-log-count (1+ magit-log-count)))
              (magit-set-section-info sha1)
              (forward-line))
          (forward-line))))
     (t
      (forward-line)))
    t))

(defun magit-wash-log ()
  (let ((magit-old-top-section nil))
    (magit-wash-sequence #'magit-wash-log-line)))

(defvar magit-currently-shown-commit nil)

(defun magit-wash-commit ()
  (when (looking-at "^commit \\([0-9a-fA-F]\\{40\\}\\)")
    (add-text-properties (match-beginning 1) (match-end 1)
			 '(face magit-log-sha1)))
  (when (search-forward-regexp "^diff" nil t)
    (goto-char (match-beginning 0))
    (magit-wash-diffs)))

(defun magit-refresh-commit-buffer (commit)
  (magit-create-buffer-sections
    (magit-git-section nil nil
		       'magit-wash-commit
		       "log"
		       "--decorate=full"
		       "--max-count=1"
		       "--pretty=medium"
		       "--cc"
		       "-p" commit)))

(define-minor-mode magit-commit-mode
    "Minor mode to view a git commit.

\\{magit-commit-mode-map}"
  :group magit
  :init-value ()
  :lighter ()
  :keymap magit-commit-mode-map)

(defvar magit-commit-buffer-name "*magit-commit*"
  "Buffer name for displaying commit log messages.")

(defun magit-show-commit (commit &optional scroll)
  (when (magit-section-p commit)
    (setq commit (magit-section-info commit)))
  (let ((dir default-directory)
	(buf (get-buffer-create magit-commit-buffer-name)))
    (cond ((and (equal magit-currently-shown-commit commit)
		;; if it's empty then the buffer was killed
		(with-current-buffer buf
		  (> (length (buffer-string)) 1)))
	   (let ((win (get-buffer-window buf)))
	     (cond ((not win)
		    (display-buffer buf))
		   (scroll
		    (with-selected-window win
		      (funcall scroll))))))
	  (t
	   (setq magit-currently-shown-commit commit)
	   (display-buffer buf)
	   (with-current-buffer buf
	     (set-buffer buf)
	     (goto-char (point-min))
	     (magit-mode-init dir 'commit
			      #'magit-refresh-commit-buffer commit)
	     (magit-commit-mode t))))))

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
  "Get the name of the branch BRANCH on remote REMOTE"
  (if (string= remote ".")
      branch
    (concat remote "/" branch)))

(magit-define-inserter unpulled-commits (remote branch)
  (when remote
    (magit-git-section 'unpulled
		       "Unpulled commits:" 'magit-wash-log
		       "log" "--pretty=format:* %H %s"
		       (format "HEAD..%s"
			     (magit-remote-branch-name remote branch)))))

(magit-define-inserter unpushed-commits (remote branch)
  (when remote
    (magit-git-section 'unpushed
		       "Unpushed commits:" 'magit-wash-log
		       "log" "--pretty=format:* %H %s"
		       (format "%s..HEAD"
			     (magit-remote-branch-name remote branch)))))

(defun magit-remote-branch-for (local-branch &optional prepend-remote-name)
  "Guess the remote branch name that LOCAL-BRANCH is tracking.
Prepend \"remotes/\", the remote's name and \"/\" if
PREPEND-REMOTE-NAME is non-nil."
  (let ((merge (magit-get "branch" local-branch "merge")))
    (save-match-data
      (if (and merge (string-match "^refs/heads/\\(.+\\)" merge))
	  (concat (if prepend-remote-name
                      (concat "remotes/"
                              (magit-get "branch" local-branch "remote")
                              "/"))
                  (match-string 1 merge))))))

;;; Status

(defvar magit-remote-string-hook nil)

(defun magit-remote-string (remote remote-branch)
  (cond
   ((string= "." remote)
    (format "branch %s"
	    (propertize remote-branch 'face 'magit-branch)))
   (remote
      (concat
       (propertize remote-branch 'face 'magit-branch)
       " @ "
       remote
       " ("
       (magit-get "remote" remote "url")
       ")"))
   (t
    (run-hook-with-args-until-success 'magit-remote-string-hook))))

(defun magit-refresh-status ()
  (magit-create-buffer-sections
    (magit-with-section 'status nil
      (let* ((branch (magit-get-current-branch))
	     (remote (and branch (magit-get "branch" branch "remote")))
	     (remote-branch (or (and branch (magit-remote-branch-for branch)) branch))
	     (remote-string (magit-remote-string remote remote-branch))
	     (head (magit-git-string
		    "log" "--max-count=1" "--abbrev-commit" "--pretty=oneline"))
	     (no-commit (not head)))
	(when remote-string
	  (insert "Remote:   " remote-string "\n"))
	(insert (format "Local:    %s %s\n"
			(propertize (or branch "(detached)")
				    'face 'magit-branch)
			(abbreviate-file-name default-directory)))
	(insert (format "Head:     %s\n"
			(if no-commit "nothing commited (yet)" head)))
	(let ((merge-heads (magit-file-lines ".git/MERGE_HEAD")))
	  (if merge-heads
	      (insert (format "Merging:   %s\n"
			      (magit-concat-with-delim
			       ", "
			       (mapcar 'magit-name-rev merge-heads))))))
	(let ((rebase (magit-rebase-info)))
	  (if rebase
	      (insert (apply 'format "Rebasing: onto %s (%s of %s); Press \"R\" to Abort, Skip, or Continue\n" rebase))))
	(insert "\n")
	(magit-git-exit-code "update-index" "--refresh")
	(magit-insert-untracked-files)
	(magit-insert-stashes)
	(magit-insert-pending-changes)
	(magit-insert-pending-commits)
	(magit-insert-unpulled-commits remote remote-branch)
	(let ((staged (or no-commit (magit-anything-staged-p))))
	  (magit-insert-unstaged-changes
	   (if staged "Unstaged changes:" "Changes:"))
	  (magit-insert-staged-changes staged no-commit))
	(magit-insert-unpushed-commits remote remote-branch)
	(run-hooks 'magit-refresh-status-hook)))))

(defun magit-init (dir)
  "Initialize git repository in the DIR directory."
  (interactive (list (read-directory-name "Directory for Git repository: ")))
  (let ((topdir (magit-get-top-dir dir)))
    (when (or (not topdir)
	      (yes-or-no-p
	       (format
		(if (string-equal topdir (expand-file-name dir))
		    "There is already a Git repository in %s. Reinitialize? "
		  "There is a Git repository in %s. Create another in %s? ")
		topdir dir)))
      (unless (file-directory-p dir)
	(and (y-or-n-p (format "Directory %s does not exists.  Create it? " dir))
	     (make-directory dir)))
      (let ((default-directory dir))
	(magit-run* (list "git" "init"))))))

(define-minor-mode magit-status-mode
    "Minor mode for looking at git status.

\\{magit-status-mode-map}"
  :group magit
  :init-value ()
  :lighter ()
  :keymap magit-status-mode-map)

(defun magit-save-some-buffers (&optional msg pred)
  "Save some buffers if variable `magit-save-some-buffers' is non-nil.
If variable `magit-save-some-buffers' is set to 'dontask then
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
  (let ((predicate-function (or pred magit-save-some-buffers-predicate)))
    
    (if magit-save-some-buffers
        (save-some-buffers
         (eq magit-save-some-buffers 'dontask)
         predicate-function)
      (when msg
        (message msg)))))


(defun magit-save-buffers-predicate-all ()
  "Prompt to save all buffers with unsaved changes"
  t)

(defun magit-save-buffers-predicate-tree-only ()
  "Only prompt to save buffers which are within the current git project (as
  determined by the dir passed to `magit-status'."
  (let ((current-buf-dir
         (file-name-directory (buffer-file-name (current-buffer)))))
    (let ((invoked-git-root-dir (magit-get-top-dir default-directory)))
      (let ((save-this-buffer
             (and
              invoked-git-root-dir
              (eq 0
                  (string-match
                   (regexp-quote invoked-git-root-dir)
                   current-buf-dir)))))
        save-this-buffer))))

;;;###autoload
(defun magit-status (dir)
  (interactive (list (or (and (not current-prefix-arg)
			      (magit-get-top-dir default-directory))
			 (magit-read-top-dir (and (consp current-prefix-arg)
						  (> (car current-prefix-arg) 4))))))
  (magit-save-some-buffers)
  (let ((topdir (magit-get-top-dir dir)))
    (unless topdir
      (when (y-or-n-p (format "There is no Git repository in %S.  Create one? "
			      dir))
	(magit-init dir)
	(setq topdir (magit-get-top-dir dir))))
    (when topdir
      (let ((buf (or (magit-find-buffer 'status topdir)
		     (generate-new-buffer
		      (concat "*magit: "
			      (file-name-nondirectory
			       (directory-file-name topdir)) "*")))))
        (switch-to-buffer buf)
        (magit-mode-init topdir 'status #'magit-refresh-status)
        (magit-status-mode t)))))

(magit-define-command automatic-merge (revision)
  "Merge REVISION into the current 'HEAD'; commit unless merge fails.
\('git merge REVISION')."
  (interactive (list (magit-read-rev "Merge" (magit-guess-branch))))
  (if revision
      (magit-run-git "merge" (magit-rev-to-git revision))))

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
      ((staged *)
       (error "Already staged"))
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
	 (error "Can't unstage a unmerged file.  Resolve it first"))
     (magit-run-git "reset" "-q" "HEAD" "--" (magit-diff-item-file item)))
    ((unstaged *)
     (error "Already unstaged"))
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

(defun magit-get-tracking-name (remote branch)
  "Given a REMOTE and a BRANCH name, ask the user for a local
tracking brach name suggesting a sensible default."
  (when (yes-or-no-p
         (format "Create local tracking branch for %s? " branch))
    (let* ((default-name (concat remote
                                 "-"
                                 (replace-regexp-in-string "[/]" "-" branch)))
           (chosen-name (read-string (format "Call local branch (%s): " default-name)
                                     nil
                                     nil
                                     default-name)))
      (when (magit-ref-exists-p (concat "refs/heads/" chosen-name))
        (error "'%s' already exists." chosen-name))
      chosen-name)))

(defun magit-maybe-create-local-tracking-branch (rev)
  "Depending on the users wishes, create a tracking branch for
rev... maybe."
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
	 (branch (read-string "Create branch: "))
	 (parent (magit-read-rev "Parent" cur-branch)))
    (list branch parent)))

(magit-define-command create-branch (branch parent)
  "Switch 'HEAD' to new BRANCH at revision PARENT and update working tree.
Fails if working tree or staging area contain uncommitted changes.
\('git checkout -b BRANCH REVISION')."
  (interactive (magit-read-create-branch-args))
  (when (and branch (not (string= branch ""))
	     parent)
    (magit-save-some-buffers)
    (magit-run-git "checkout" "-b"
		   branch
		   (append
		    magit-custom-options
		    (magit-rev-to-git parent)))
    (magit-update-vc-modeline default-directory)))

(defun magit-delete-branch (branch)
  "Asks for a branch and deletes it.
If the branch is the current one, offers to switch to `master' first.
\('git branch -d BRANCH')."
  (interactive (list (magit-read-rev "Branch to delete" (magit-default-rev))))
  (when (and branch (string= branch (magit-get-current-branch)))
    (if (y-or-n-p "Cannot delete current branch. Switch to master first? ")
	(magit-checkout "master")
      (setq branch nil)))
  (when branch
    (magit-run-git "branch" "-d" (append magit-custom-options
					 (magit-rev-to-git branch)))))

(defun magit-move-branch (old new)
  "Renames or moves a branch.
If the branch is the current one, offers to switch to `master' first.
\('git branch -m OLD NEW')."
  (interactive (list (magit-read-rev "Old name" (magit-default-rev))
		     (magit-read-rev "New name" (magit-default-rev))))
  (magit-run-git "branch" "-m" (magit-rev-to-git old) new))

;;; Merging

(defun magit-guess-branch ()
  (magit-section-case (item info)
    ((wazzup commit)
     (magit-section-info (magit-section-parent item)))
    ((commit) (magit-name-rev (substring info 0 8)))
    ((wazzup) info)))

(defun magit-merge (revision)
  "Merge REVISION into the current 'HEAD'; leave changes uncommitted.
With a prefix-arg, the merge will be squashed.
\('git merge --no-commit [--squash|--no-ff] REVISION')."
  (interactive
   (list (magit-read-rev "Merge" (magit-default-rev))))
  (if revision
      (apply 'magit-run-git
             "merge"
             (magit-rev-to-git revision)
             magit-custom-options)))

;;; Rebasing

(defun magit-rebase-info ()
  "Returns a list indicating the state of an in-progress rebase,
if any."
  (cond ((file-exists-p ".git/rebase-merge")
         (list
          ;; The commit we're rebasing onto, i.e. git rebase -i <onto>
          (magit-name-rev (car (magit-file-lines ".git/rebase-merge/onto")))

          ;; How many commits we've gone through
          (length (magit-file-lines ".git/rebase-merge/done"))

          ;; How many commits we have in total, without the comments
          ;; at the end of git-rebase-todo.backup
          (let ((todo-lines-with-comments (magit-file-lines ".git/rebase-merge/git-rebase-todo.backup")))
            (loop for i in todo-lines-with-comments
                  until (string= "" i)
                  count i))))
	(t nil)))

(defun magit-rebase-step ()
  (interactive)
  (let ((info (magit-rebase-info)))
    (if (not info)
	(let* ((current-branch (magit-get-current-branch))
               (remote (when current-branch
                         (magit-get "branch" current-branch "remote")))
               (remote-branch (when remote
                                (magit-get "branch" current-branch "merge")))
               (rev (magit-read-rev "Rebase to"
                                    (when (and remote-branch
                                               (string-match "refs/heads/\\(.*\\)" remote-branch))
                                      (concat (match-string 1 remote-branch)
                                              " (" remote ")"))
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
          (case reply
            ((?A ?a)
             (magit-run-git "rebase" "--abort"))
            ((?S ?s)
             (magit-run-git "rebase" "--skip"))
            ((?C ?c)
             (magit-run-git "rebase" "--continue"))))))))

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

(magit-define-command reset-working-tree ()
  "Revert working tree and clear changes from staging area.
\('git reset --hard HEAD')."
  (interactive)
  (when (yes-or-no-p "Discard all uncommitted changes? ")
    (magit-reset-head-hard "HEAD")))

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
  (interactive (list (magit-read-rev "Rewrite from" (magit-default-rev))))
  (or (magit-everything-clean-p)
      (error "You have uncommitted changes"))
  (or (not (magit-read-rewrite-info))
      (error "Rewrite in progress"))
  (let* ((orig (magit-rev-parse "HEAD"))
	 (base (or (car (magit-commit-parents from))
		   (error "Can't rewrite a commit without a parent, sorry")))
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

(magit-define-command fetch ()
  "Run fetch."
  (interactive)
  (magit-run-git-async "fetch" (magit-read-remote)))

(magit-define-command fetch-current ()
  "Run fetch."
  (interactive)
  (magit-run-git-async "fetch"))

(magit-define-command remote-update ()
  "Update all remotes."
  (interactive)
  (magit-run-git-async "remote" "update"))

(magit-define-command pull ()
  "Run git pull against the current remote."
  (interactive)
  (let* ((branch (magit-get-current-branch))
	 (config-branch (and branch (magit-get "branch" branch "merge")))
	 (merge-branch (or (and config-branch (not current-prefix-arg))
			   (magit-read-rev (format "Pull from")))))
    (if (and branch (not config-branch))
	(magit-set merge-branch "branch" branch "merge"))
    (apply 'magit-run-git-async "pull" "-v" magit-custom-options)))

(eval-when-compile (require 'pcomplete))

(defun magit-shell-command (command)
  "Perform arbitrary shell COMMAND."
  (interactive "sCommand: ")
  (require 'pcomplete)
  (let ((args (car (with-temp-buffer
		     (insert command)
		     (pcomplete-parse-buffer-arguments))))
	(magit-process-popup-time 0))
    (magit-run* args nil nil nil t)))

(defun magit-git-command (command)
  "Perform arbitrary Git COMMAND.

Similar to `magit-shell-command', but involves slightly less
typing and automatically refreshes the status buffer."
  (interactive "sRun git like this: ")
  (require 'pcomplete)
  (let ((args (car (with-temp-buffer
		     (insert command)
		     (pcomplete-parse-buffer-arguments))))
	(magit-process-popup-time 0))
    (magit-with-refresh
      (magit-run* (append (cons magit-git-executable
                                magit-git-standard-options)
                          args)
                  nil nil nil t))))

(magit-define-command push-tags ()
  "Push tags."
  (interactive)
  (magit-run-git-async "push" "--tags"))

(magit-define-command push ()
  (interactive)
  (let* ((branch (or (magit-get-current-branch)
		     (error "Don't push a detached head.  That's gross")))
	 (branch-remote (magit-get-remote branch))
	 (push-remote (if (or current-prefix-arg
			      (not branch-remote))
			  (magit-read-remote (format "Push %s to: " branch)
					     branch-remote)
			branch-remote))
	 (ref-branch (magit-get "branch" branch "merge")))
    (if (and (not branch-remote)
	     (not current-prefix-arg))
	(magit-set push-remote "branch" branch "remote"))
    (apply 'magit-run-git-async "push" "-v" push-remote
           (if ref-branch
               (format "%s:%s" branch ref-branch)
             branch)
           magit-custom-options)
    ;; Although git will automatically set up the remote,
    ;; it doesn't set up the branch to merge (at least as of Git 1.6.6.1),
    ;; so we have to do that manually.
    (unless ref-branch
      (magit-set (concat "refs/heads/" branch) "branch" branch "merge"))))

;;; Log edit mode

(defvar magit-log-edit-mode-hook nil
  "Hook run by `magit-log-edit-mode'.")

(defvar magit-log-edit-buffer-name "*magit-edit-log*"
  "Buffer name for composing commit messages.")

(defvar magit-log-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'magit-log-edit-commit)
    (define-key map (kbd "C-c C-a") 'magit-log-edit-toggle-amending)
    (define-key map (kbd "C-c C-s") 'magit-log-edit-toggle-signoff)
    (define-key map (kbd "C-c C-e") 'magit-log-edit-toggle-allow-empty)
    (define-key map (kbd "M-p") 'log-edit-previous-comment)
    (define-key map (kbd "M-n") 'log-edit-next-comment)
    (define-key map (kbd "C-c C-k") 'magit-log-edit-cancel-log-message)
    (define-key map (kbd "C-c C-]") 'magit-log-edit-cancel-log-message)
    (define-key map (kbd "C-x C-s") 'ignore)
    map))

(defvar magit-pre-log-edit-window-configuration nil)

(defun magit-log-fill-paragraph (&optional justify)
  "Fill the paragraph, but preserve open parentheses at beginning of lines.
Prefix arg means justify as well."
  (interactive "P")
  ;; Add lines starting with a left paren or an asterisk.
  (let ((paragraph-start (concat paragraph-start "\\|*\\|(")))
    (let ((end (progn (forward-paragraph) (point)))
	  (beg (progn (backward-paragraph) (point)))
	  (adaptive-fill-mode nil))
      (fill-region beg end justify)
      t)))

(define-derived-mode magit-log-edit-mode text-mode "Magit Log Edit"
  (set (make-local-variable 'fill-paragraph-function)
       'magit-log-fill-paragraph))

(defun magit-log-edit-cleanup ()
  (save-excursion
    (goto-char (point-min))
    (flush-lines "^#")
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
	  (while (looking-at "^\\([A-Za-z0-9-_]+\\): *\\(.*\\)$")
	    (setq result (acons (intern (downcase (match-string 1)))
				(match-string 2)
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
		  (cdar fields) "\n")
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
      (setq fields (acons name (if default "yes" "no") fields))
      (setq yesp (if default 'first)))
    (magit-log-edit-set-fields fields)
    yesp))

(defun magit-log-edit-setup-author-env (author)
  (cond (author
	 ;; XXX - this is a bit strict, probably.
	 (or (string-match "\\(.*\\) <\\(.*\\)>" author)
	     (error "Can't parse author string"))
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
	 (tag-rev (cdr (assq 'tag-rev fields)))
	 (tag-name (cdr (assq 'tag-name fields)))
	 (author (cdr (assq 'author fields))))
    (magit-log-edit-push-to-comment-ring (buffer-string))
    (magit-log-edit-setup-author-env author)
    (magit-log-edit-set-fields nil)
    (magit-log-edit-cleanup)
    (if (= (buffer-size) 0)
	(insert "(Empty description)\n"))
    (let ((commit-buf (current-buffer)))
      (with-current-buffer (magit-find-buffer 'status default-directory)
	(cond (tag-name
	       (magit-run-git-with-input commit-buf "tag" tag-name "-a" "-F" "-" tag-rev))
	      (t
	       (apply #'magit-run-async-with-input commit-buf
		      magit-git-executable
		      (append magit-git-standard-options
			      (list "commit" "-F" "-")
			      (if (and commit-all (not allow-empty)) '("--all") '())
			      (if amend '("--amend") '())
			      (if allow-empty '("--allow-empty"))
			      (if sign-off '("--signoff") '())))))))
    (erase-buffer)
    (bury-buffer)
    (when (file-exists-p ".git/MERGE_MSG")
      (delete-file ".git/MERGE_MSG"))
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
     (magit-format-commit "HEAD" "%s%n%n%b"))))

(defun magit-log-edit-toggle-signoff ()
  "Toggle whether this commit will include a signoff.
\(i.e., whether eventual commit does 'git commit --signoff')"
  (interactive)
  (magit-log-edit-toggle-field 'sign-off (not magit-commit-signoff)))

(defun magit-log-edit-toggle-allow-empty ()
  "Toggle whether this commit is allowed to be empty.
This means that the eventual commit does 'git commit --allow-empty'."
  (interactive)
  (magit-log-edit-toggle-field 'allow-empty t))

(defun magit-pop-to-log-edit (operation)
  (let ((dir default-directory)
	(buf (get-buffer-create magit-log-edit-buffer-name)))
    (setq magit-pre-log-edit-window-configuration
	  (current-window-configuration))
    (pop-to-buffer buf)
    (when (file-exists-p ".git/MERGE_MSG")
      (insert-file-contents ".git/MERGE_MSG"))
    (setq default-directory dir)
    (magit-log-edit-mode)
    (message "Type C-c C-c to %s (C-c C-k to cancel)." operation)))

(defun magit-log-edit (amend-p)
  "Brings up a buffer to allow editing of commit messages. Given
a prefix arg will set the amend flag for the commit buffer.

If there is a rebase in progress offer the user the option to
continue it.

\\{magit-log-edit-mode-map}"
  (interactive "P")
  (cond ((magit-rebase-info)
	 (if (y-or-n-p "Rebase in progress.  Continue it? ")
	     (magit-run-git "rebase" "--continue")))
	(t
	 (when (and magit-commit-all-when-nothing-staged
		    (not (magit-anything-staged-p)))
	   (cond ((eq magit-commit-all-when-nothing-staged 'ask-stage)
		  (if (and (not (magit-everything-clean-p))
			   (y-or-n-p "Nothing staged.  Stage everything now? "))
		      (magit-stage-all)))
		 ((not (magit-log-edit-get-field 'commit-all))
		  (magit-log-edit-set-field
		   'commit-all
		   (if (or (eq magit-commit-all-when-nothing-staged t)
			   (y-or-n-p
			    "Nothing staged.  Commit all unstaged changes? "))
		       "yes" "no")))))
	 (when amend-p (magit-log-edit-toggle-amending))
	 (magit-pop-to-log-edit "commit"))))

(defun magit-add-log ()
  (interactive)
  (cond ((magit-rebase-info)
	 (if (y-or-n-p "Rebase in progress.  Continue it? ")
	     (magit-run-git "rebase" "--continue")))
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
			     (insert (format "(%s): " fun))))))))))))

;;; Tags

(magit-define-command tag (name rev)
  "Create a new lightweight tag with the given NAME at REV.
\('git tag NAME')."
  (interactive
   (list
    (read-string "Tag name: ")
    (magit-read-rev "Place tag on: " (or (magit-default-rev) "HEAD"))))
  (magit-run-git "tag" name rev))

(magit-define-command annotated-tag (name rev)
  "Start composing an annotated tag with the given NAME.
Tag will point to the current 'HEAD'."
  (interactive
   (list
    (read-string "Tag name: ")
    (magit-read-rev "Place tag on: " (or (magit-default-rev) "HEAD"))))
  (magit-log-edit-set-field 'tag-name name)
  (magit-log-edit-set-field 'tag-rev rev)
  (magit-pop-to-log-edit "tag"))

;;; Stashing

(defun magit-wash-stash ()
  (if (search-forward-regexp "stash@{\\(.*\\)}" (line-end-position) t)
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

(magit-define-command stash (description)
  "Create new stash of working tree and staging area named DESCRIPTION.
Working tree and staging area revert to the current 'HEAD'.
With prefix argument, changes in staging area are kept.
\('git stash save [--keep-index] DESCRIPTION')"
  (interactive "sStash description: ")
  (apply 'magit-run-git `("stash" "save" ,@magit-custom-options "--" ,description)))

(magit-define-command stash-snapshot ()
  "Create new stash of working tree and staging area; keep changes in place.
\('git stash save \"Snapshot...\"; git stash apply stash@{0}')"
  (interactive)
  (magit-with-refresh
    (magit-run-git "stash" "save"
		   (format-time-string "Snapshot taken at %Y-%m-%d %H:%M:%S"
				       (current-time)))
    (magit-run-git "stash" "apply" "stash@{0}")))

(defvar magit-currently-shown-stash nil)

(define-minor-mode magit-stash-mode
    "Minor mode for looking at a git stash.

\\{magit-stash-mode-map}"
  :group magit
  :init-value ()
  :lighter ()
  :keymap magit-stash-mode-map)

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
                    (args (magit-rev-range-to-git range)))
               (magit-mode-init dir 'diff #'magit-refresh-diff-buffer
                                range args)
               (magit-stash-mode t)))))))

;;; Commits

(defun magit-commit-at-point (&optional nil-ok-p)
  (let* ((section (magit-current-section))
         (commit (or (and (not section)                          ; Places without a magit-section
                          (get-text-property (point) 'revision)) ; but with a text property 'revision
                     (and (eq (magit-section-type section) 'commit)
                          (magit-section-info section)))))
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

(defun magit-revert-item ()
  (interactive)
  (when (or (not magit-revert-item-confirm)
	    (yes-or-no-p
	     "Really revert this item (cannot be undone)? "))
    (magit-section-action (item info "revert")
      ((pending commit)
       (magit-apply-commit info nil nil t)
       (magit-rewrite-set-commit-property info 'used nil))
      ((commit)
       (magit-apply-commit info nil nil t))
      ((hunk)
       (magit-apply-hunk-item-reverse item))
      ((diff)
       (magit-apply-diff-item item "--reverse")))))

(defvar magit-have-graph 'unset)
(defvar magit-have-decorate 'unset)
(make-variable-buffer-local 'magit-have-graph)
(put 'magit-have-graph 'permanent-local t)
(make-variable-buffer-local 'magit-have-decorate)
(put 'magit-have-decorate 'permanent-local t)

(defun magit-configure-have-graph ()
  (if (eq magit-have-graph 'unset)
      (let ((res (magit-git-exit-code "log" "--graph" "--max-count=0")))
	(setq magit-have-graph (eq res 0)))))

(defun magit-configure-have-decorate ()
  (if (eq magit-have-decorate 'unset)
      (let ((res (magit-git-exit-code "log" "--decorate=full" "--max-count=0")))
	(setq magit-have-decorate (eq res 0)))))

(defun magit-log-show-more-entries (&optional arg)
  "Grow the number of log entries shown.

With no prefix optional ARG, show twice as much log entries.
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
  (magit-refresh))


(defun magit-refresh-log-buffer (range style args)
  (magit-configure-have-graph)
  (magit-configure-have-decorate)
  (setq magit-current-range range)
  (magit-create-log-buffer-sections
    (apply #'magit-git-section nil
	   (magit-rev-range-describe range "Commits")
	   'magit-wash-log
	   `("log"
	     ,(format "--max-count=%s" magit-log-cutoff-length)
	     ,style
	     ,@(if magit-have-decorate (list "--decorate=full"))
	     ,@(if magit-have-graph (list "--graph"))
	     ,@args
	     "--"))))

(define-minor-mode magit-log-mode
    "Minor mode for looking at git log.

\\{magit-log-mode-map}"
  :group magit
  :init-value ()
  :lighter ()
  :keymap magit-log-mode-map)

(defvar magit-log-buffer-name "*magit-log*"
  "Buffer name for display of log entries.")
(defvar magit-log-grep-buffer-name "*magit-grep-log*"
  "Buffer name for display of log grep results.")

(magit-define-command display-log-ranged ()
  (interactive)
  (magit-display-log t))

(magit-define-command display-log (&optional ask-for-range &rest extra-args)
  (interactive)
  (let* ((log-range (if ask-for-range
                        (magit-read-rev-range "Log" "HEAD")
                      "HEAD"))
	 (topdir (magit-get-top-dir default-directory))
	 (args (nconc (list (magit-rev-range-to-git log-range))
                      magit-custom-options
                      extra-args)))
    (switch-to-buffer magit-log-buffer-name)
    (magit-mode-init topdir 'log #'magit-refresh-log-buffer log-range
		     "--pretty=oneline" args)
    (magit-log-mode t)))

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
    (switch-to-buffer magit-log-buffer-name)
    (magit-mode-init topdir 'log #'magit-refresh-log-buffer range
		     "--stat" args)
    (magit-log-mode t)))

;;; Reflog

(defvar magit-reflog-head nil
  "The HEAD of the reflog in the current buffer.
This is only non-nil in reflog buffers.")
(make-variable-buffer-local 'magit-reflog-head)

(defun magit-refresh-reflog-buffer (head args)
  (setq magit-reflog-head head)
  (magit-create-log-buffer-sections
    (magit-git-section 'reflog
		       (format "Local history of head %s" head)
		       'magit-wash-log
		       "log" "--walk-reflogs"
		       (format "--max-count=%s" magit-log-cutoff-length)
		       "--pretty=format:* %H %s"
		       args)))

(define-minor-mode magit-reflog-mode
    "Minor mode for looking at git reflog.

\\{magit-reflog-mode-map}"
  :group magit
  :init-value ()
  :lighter ()
  :keymap magit-reflog-mode-map)

(magit-define-command reflog (&optional ask-for-range)
  (interactive)
  (let ((at (or (if ask-for-range
                    (magit-read-rev "Reflog of" (or (magit-guess-branch) "HEAD")))
                "HEAD")))
    (let* ((topdir (magit-get-top-dir default-directory))
           (args (magit-rev-to-git at)))
      (switch-to-buffer "*magit-reflog*")
      (magit-mode-init topdir 'reflog
                       #'magit-refresh-reflog-buffer at args)
      (magit-reflog-mode t))))

(magit-define-command reflog-ranged ()
  (interactive)
  (magit-reflog t))

;;; Diffing

(defun magit-refresh-diff-buffer (range args)
  (setq magit-current-range range)
  (magit-create-buffer-sections
    (magit-git-section 'diffbuf
		       (magit-rev-range-describe range "Changes")
		       'magit-wash-diffs
		       "diff" (magit-diff-U-arg) args)))

(define-minor-mode magit-diff-mode
    "Minor mode for looking at a git diff.

\\{magit-diff-mode-map}"
  :group magit
  :init-value ()
  :lighter ()
  :keymap magit-diff-mode-map)

(magit-define-command diff (range)
  (interactive (list (magit-read-rev-range "Diff")))
  (if range
      (let* ((dir default-directory)
             (args (magit-rev-range-to-git range))
             (buf (get-buffer-create "*magit-diff*")))
        (display-buffer buf)
        (with-current-buffer buf
          (magit-mode-init dir 'diff #'magit-refresh-diff-buffer range args)
          (magit-diff-mode t)))))

(magit-define-command diff-working-tree (rev)
  (interactive (list (magit-read-rev "Diff with" (magit-default-rev))))
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
  (let ((ignore-file ".git/info/wazzup-exclude"))
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
	(let* ((excluded (magit-file-lines ".git/info/wazzup-exclude"))
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
			   "--graph"
			   "--pretty=oneline"
			   (format "%s..%s" head ref)
			   "--"))))
		  (magit-set-section-info ref section))))))))))

(define-minor-mode magit-wazzup-mode
    "Minor mode for looking at commits that could be merged from other branches.

\\{magit-wazzup-mode-map}"
  :group magit
  :init-value ()
  :lighter ()
  :keymap magit-wazzup-mode-map)

(defun magit-wazzup (&optional all)
  (interactive "P")
  (let ((topdir (magit-get-top-dir default-directory))
	(current-branch (magit-get-current-branch)))
    (switch-to-buffer "*magit-wazzup*")
    (magit-mode-init topdir 'wazzup
		     #'magit-refresh-wazzup-buffer
		     current-branch all)
    (magit-wazzup-mode t)))

;;; Miscellaneous

(defun magit-ignore-file (file edit local)
  (let ((ignore-file (if local ".git/info/exclude" ".gitignore")))
    (if edit
	(setq file (read-string "File to ignore: " file)))
    (with-temp-buffer
      (when (file-exists-p ignore-file)
        (insert-file-contents ignore-file))
      (goto-char (point-max))
      (unless (bolp)
	(insert "\n"))
      (insert "/" file "\n")
      (write-region nil nil ignore-file))
    (magit-need-refresh)))

(defun magit-ignore-item ()
  (interactive)
  (magit-section-action (item info "ignore")
    ((untracked file)
     (magit-ignore-file info current-prefix-arg nil))
    ((wazzup)
     (magit-wazzup-toggle-ignore info current-prefix-arg))))

(defun magit-ignore-item-locally ()
  (interactive)
  (magit-section-action (item info "ignore")
    ((untracked file)
     (magit-ignore-file info current-prefix-arg t))))

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
     (if (yes-or-no-p (format "Delete %s? " info))
	 (magit-run "rm" info)))
    ((untracked)
     (if (yes-or-no-p "Delete all untracked files and directories? ")
	 (magit-run "git" "clean" "-df")))
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
    ((hunk)
     (error "Can't discard this hunk"))
    ((diff)
     (error "Can't discard this diff"))
    ((stash)
     (when (yes-or-no-p "Discard stash? ")
       (magit-run-git "stash" "drop" info)))))

(defun magit-visit-item ()
  (interactive)
  (magit-section-action (item info "visit")
    ((untracked file)
     (find-file info))
    ((diff)
     (find-file (magit-diff-item-file item)))
    ((hunk)
     (let ((file (magit-diff-item-file (magit-hunk-item-diff item)))
	   (line (magit-hunk-item-target-line item)))
       (find-file file)
       (goto-char (point-min))
       (forward-line (1- line))))
    ((commit)
     (magit-show-commit info)
     (pop-to-buffer magit-commit-buffer-name))
    ((stash)
     (magit-show-stash info)
     (pop-to-buffer magit-stash-buffer-name))
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

(eval-when-compile (require 'server))

(defun magit-server-running-p ()
  "Test whether server is running (works with < 23 as well).

Return values:
  nil		   the server is definitely not running.
  t		   the server seems to be running.
  something else   we cannot determine whether it's running without using
		   commands which may have to wait for a long time."
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
    (setenv "GIT_EDITOR" (locate-file "emacsclient" exec-path))
    (unwind-protect
	(magit-run-git-async "rebase" "-i"
			     (or (and commit (concat commit "^"))
				 (magit-read-rev "Interactively rebase to" (magit-guess-branch))))
      (if old-editor
	  (setenv "GIT_EDITOR" old-editor)))))

(define-derived-mode magit-show-branches-mode fundamental-mode
  "Magit Branches")

(defun magit-quit-branches-window ()
  "Bury the branches buffer and delete its window."
  (interactive)
  (quit-window)
  (delete-window))

(defun magit--branch-name-from-line (line)
  "Extract the branch name from line LINE of 'git branch' output."
  (get-text-property 0 'branch-name line))

(defun magit--branch-name-at-point ()
  "Get the branch name in the line at point."
  (let ((branch (magit--branch-name-from-line (thing-at-point 'line))))
    (or branch (error "No branch at point"))))

(defun magit-branches-window-checkout ()
  "Check out the branch in the line at point."
  (interactive)
  (magit-checkout (magit--branch-name-at-point))
  (magit-show-branches))

(defun magit-remove-remote (ref)
  "Return REF with any remote part removed."
  (if (string-match "^remotes/" ref)
      (substring ref 8)
    ref))

(defun magit-remove-branch (&optional force)
  "Remove the branch in the line at point.
With prefix force the removal even it it hasn't been merged."
  (interactive "P")
  (let ((args (list "branch"
		    (if force "-D" "-d")
		    (when (magit--is-branch-at-point-remote) "-r")
		    ;; remove the remotes part
		    (magit-remove-remote
                     (magit--branch-name-at-point)))))
    (apply 'magit-run-git (remq nil args))
    (if (and (magit--is-branch-at-point-remote)
             (yes-or-no-p "Remove branch in remote repository as well? "))
        (magit-remove-branch-in-remote-repo (magit--branch-name-at-point)))
    (magit-show-branches)))

(defun magit--remotes ()
  "Return a list of names for known remotes."
  (magit-git-lines "remote"))

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

(defun magit-remove-branch-in-remote-repo (&optional branch-name-at-local)
  "Remove a branch in a remote repository by pushing nothing into it.
If BRANCH-NAME-AT-LOCAL is not given then ask the user for the
name of the remote and branch name. The remote must be known to git."
  (interactive)
  (let ((all-remotes (magit--remotes))
        remote branch)
    (unless all-remotes
      (error "No remote has been  configured"))
    (if branch-name-at-local
        (save-match-data
          (if (string-match "^remotes/\\([^/]+\\)/\\(.+\\)" branch-name-at-local)
              (setq remote (match-string 1 branch-name-at-local)
                    branch (match-string 2 branch-name-at-local))
            (error "Cannot parse remote and branch name from `%s'" branch-name-at-local)))
      (setq remote (magit-completing-read "Name of remote repository: " all-remotes nil t)
            branch (magit-completing-read "Name of branch in remote repository: "
                                          (magit--branches-for-remote-repo remote))))
    (unless (magit-get "remote" remote "url")
      (error "Unknown remote"))
    (magit-run-git "push"
                   remote
                   (concat ":refs/heads/" branch))))

(defvar magit-branches-buffer-name "*magit-branches*")

(defun magit--is-branch-at-point-remote()
  "Return t if the branch at point is a remote tracking branch"
  (get-text-property (point) 'remote))

(defun magit--branch-view-details (branch-line)
  "Extract details from branch -va output."
  (string-match (concat
                 "^\\(\\*? \\{1,2\\}\\)"       ; 1: current branch marker (maybe)
                 "\\(.+?\\) +"                 ; 2: branch name

                 "\\(?:"
                 "\\([0-9a-fA-F]\\{7,8\\}\\) " ; 3: sha1
                 "\\|\\(-> \\)"                ; 4: or the pointer to a ref
                 "\\)"

                 "\\(.+\\)"                    ; 5: message or ref
                 )
                branch-line)
  (let ((res (list (cons 'current (match-string 1 branch-line))
                   (cons 'branch  (match-string 2 branch-line))
                   (cons 'remote  (string-match-p "^remotes/" (match-string 2 branch-line))))))
    (unless (cdr (assoc 'remote res))
      (setq res (append (list (cons 'tracking
                                    (magit-remote-branch-for (cdr (assoc 'branch res))
                                                             t)))
                        res)))
    (if (match-string 4 branch-line)
        (cons (cons 'other-ref (match-string 5 branch-line)) res)
      (append
       (list
        (cons 'sha1 (match-string 3 branch-line))
        (cons 'msg (match-string 5 branch-line)))
       res))))

(defun magit-show-branches ()
  "Show all of the current branches in `other-window'."
  (interactive)
  (let ((buffer-existed (get-buffer magit-branches-buffer-name)))
    (unless (eq major-mode 'magit-show-branches-mode)
      (let ((topdir (magit-get-top-dir default-directory)))
        (switch-to-buffer-other-window magit-branches-buffer-name)
        (setq default-directory topdir)))
    (let ((inhibit-read-only t)
          (goto-branch-line (line-number-at-pos))
          (branches (mapcar 'magit--branch-view-details
                            (apply 'magit-git-lines "branch" "-va"
                                   magit-custom-options))))
      (erase-buffer)
      (insert
       (mapconcat
        (lambda (b)
          (propertize
           (concat
            (cdr (assoc 'current b))
            (propertize (or (cdr (assoc 'sha1 b))
                            "       ")
                        'face 'magit-log-sha1)
            " "
            (cdr (assoc 'branch b))
            (when (assoc 'other-ref b)
              (concat " (" (cdr (assoc 'other-ref b)) ")"))
            (when (cdr (assoc 'tracking b))
              (concat " [" (cdr (assoc 'tracking b)) "]")))
           'remote (cdr (assoc 'remote b))
           'branch-name (cdr (assoc 'branch b))
           'revision (cdr (assoc 'sha1 b))))
        branches
        "\n"))
      (magit-show-branches-mode)
      (goto-char (point-min))
      (if buffer-existed
          (forward-line (1- goto-branch-line))
        (while (and (< (point)
                       (point-max))
                    (not (string= (buffer-substring-no-properties (point) (1+ (point)))
                                  "*")))
          (forward-line 1)))))
  (setq buffer-read-only t))

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
      (if (string-match "^refs/remotes/\\([^/]+\\)/\\(.+\\)" ; 1: remote name; 2: branch name
                        new-tracked)
          (setq new-remote (match-string 1 new-tracked)
                new-branch (concat "refs/heads/" (match-string 2 new-tracked)))
        ;; Match refs that are unknown in the local repository. Can be
        ;; useful if you want to create a new branch in a remote
        ;; repository.
        (if (string-match "^\\([^ ]+\\) +(\\(.+\\))$" ; 1: branch name; 2: remote name
                          new-tracked)
            (setq new-remote (match-string 2 new-tracked)
                  new-branch (concat "refs/heads/" (match-string 1 new-tracked)))
            (error "Cannot parse the remote and branch name"))))
    (magit-set new-remote "branch" local-branch "remote")
    (magit-set new-branch "branch" local-branch "merge")
    (magit-show-branches)
    (if (string= (magit-get-current-branch) local-branch)
        (magit-refresh-buffer (magit-find-buffer 'status default-directory)))))

(defvar magit-ediff-file)
(defvar magit-ediff-windows)

(eval-when-compile (require 'ediff))

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
	  (insert (magit-git-output `("cat-file" "blob" ,(concat ":2:" file))))))
    (with-current-buffer their-buffer
      (if (string-match "^[0-9]+ [0-9a-f]+ 3" merge-status)
	  (insert (magit-git-output `("cat-file" "blob" ,(concat ":3:" file))))))
    ;; We have now created the 3 buffer with ours, theirs and the ancestor files
    (with-current-buffer (ediff-merge-buffers-with-ancestor our-buffer their-buffer base-buffer)
      (make-local-variable 'magit-ediff-file)
      (setq magit-ediff-file file)
      (make-local-variable 'magit-ediff-windows)
      (setq magit-ediff-windows windows)
      (make-local-variable 'ediff-quit-hook)
      (add-hook 'ediff-quit-hook
		(lambda ()
		  (let ((buffer-A ediff-buffer-A)
			(buffer-B ediff-buffer-B)
			(buffer-C ediff-buffer-C)
			(buffer-Ancestor ediff-ancestor-buffer)
			(file magit-ediff-file)
			(file-buffer)
			(windows magit-ediff-windows))
		    (ediff-cleanup-mess)
		    (find-file file)
		    (setq file-buffer (current-buffer))
		    (erase-buffer)
		    (insert-buffer-substring buffer-C)
		    (kill-buffer buffer-A)
		    (kill-buffer buffer-B)
		    (kill-buffer buffer-C)
		    (when (bufferp buffer-Ancestor) (kill-buffer buffer-Ancestor))
		    (set-window-configuration windows)
		    (magit-save-some-buffers
		     "Conflict resolution finished; you may save the buffer"
		     (lambda () (eq (current-buffer) file-buffer)))))))))

(defun magit-interactive-resolve-item ()
  (interactive)
  (magit-section-action (item info "resolv")
    ((diff)
     (magit-interactive-resolve (cadr info)))))

(defun magit-list-buffers ()
  "Return a list of magit buffers."
  (delq nil (mapcar (lambda (b)
                      (with-current-buffer b
                        (when (eq major-mode 'magit-mode)
                          b)))
                    (buffer-list))))

(defun remove-dupes (list)
  "Remove the duplicate items in a sorted list."
  (let (tmp-list head)
    (while list
      (setq head (pop list))
      (unless (equal head (car list))
        (push head tmp-list)))
    (reverse tmp-list)))

(defun magit-list-projects ()
  "Return a list of directories with a magit representation."
  (remove-dupes
   (sort
    (mapcar (lambda (b)
              (with-current-buffer b
                (directory-file-name default-directory)))
            (magit-list-buffers))
    'string=)))

(defun magit-submodule-update (&optional init)
  "Update the submodule of the current git repository

With a prefix arg, do a submodule update --init"
  (interactive "P")
  (let ((default-directory (magit-get-top-dir default-directory)))
    (apply #'magit-run-git-async "submodule" "update" (if init '("--init") ()))))

(defun magit-submodule-update-init ()
  "Update and init the submodule of the current git repository."
  (magit-submodule-update t))

(defun magit-submodule-init ()
  "Initialize the submodules"
  (interactive)
  (let ((default-directory (magit-get-top-dir default-directory)))
    (magit-run-git-async "submodule" "init")))

(defun magit-submodule-sync ()
  "Synchronizes submodules' remote URL configuration"
  (interactive)
  (let ((default-directory (magit-get-top-dir default-directory)))
    (magit-run-git-async "submodule" "sync")))

(defun magit-run-git-gui ()
  "Run `git gui' for the current git repository"
  (interactive)
  (let* ((default-directory (magit-get-top-dir default-directory)))
    (start-process "git gui" nil "git" "gui")))

(defun magit-run-gitk ()
  "Run `gitk --all' for the current git repository"
  (interactive)
  (let* ((default-directory (magit-get-top-dir default-directory)))
    (start-process "gitk" nil "gitk" "--all")))

;; for emacs 22 compatibility

(defun magit-string-match-p (regexp string &optional start)
  "Same as `string-match' except this function does not change
the match data."
  (let ((inhibit-changing-match-data t))
    (string-match regexp string start)))

(when (not (fboundp 'string-match-p))
  (fset 'string-match-p (symbol-function 'magit-string-match-p)))

(provide 'magit)
;;; magit.el ends here
