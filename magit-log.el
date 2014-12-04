;;; magit-log.el --- inspect Git history

;; Copyright (C) 2010-2014  The Magit Project Developers
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

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

;;; Code:

(require 'magit-core)
(require 'magit-diff)

(declare-function magit-read-file-from-rev 'magit)
(declare-function magit-blame-chunk-get 'magit-blame)
(declare-function magit-insert-status-headers 'magit)
(declare-function magit-show-commit 'magit)
(defvar magit-blame-mode)

(require 'ansi-color)
(require 'crm)

;;; Options
;;;; Log Mode

(defgroup magit-log nil
  "Inspect and manipulate Git history."
  :group 'magit-modes)

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

(defcustom magit-log-format-graph-function 'identity
  "Function used to format graphs in log buffers.
The function is called with one argument, the graph of a single
line as a propertized string.  It has to return the formatted
string.  Use `identity' to forgo changing the graph."
  :package-version '(magit . "2.1.0")
  :group 'magit-log
  :type '(choice (function-item identity)
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

;;;; Cherry Mode

(defcustom magit-cherry-buffer-name-format "*magit-cherry: %a*"
  "Name format for buffers used to display commits not merged upstream.

The following `format'-like specs are supported:
%a the absolute filename of the repository toplevel.
%b the basename of the repository toplevel."
  :group 'magit-log
  :type 'string)

(defcustom magit-cherry-sections-hook
  '(magit-insert-cherry-headers
    magit-insert-cherry-commits)
  "Hook run to insert sections into the cherry buffer."
  :package-version '(magit . "2.1.0")
  :group 'magit-log
  :type 'hook)

;;;; Reflog Mode

(defcustom magit-reflog-buffer-name-format "*magit-reflog: %a*"
  "Name format for buffers used to display reflog entries.

The following `format'-like specs are supported:
%a the absolute filename of the repository toplevel.
%b the basename of the repository toplevel."
  :package-version '(magit . "2.1.0")
  :group 'magit-log
  :type 'string)

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

;;;; Log Sections

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

;;; Commands

(magit-define-popup magit-log-popup
  "Popup console for log commands."
  'magit-popups
  :man-page "git-log"
  :switches '((?a "Show all refs"           "--all")
              (?b "Show all branches"       "--branches")
              (?g "Show graph"              "--graph")
              (?d "Show refnames"           "--decorate")
              (?S "Show signatures"         "--show-signature")
              (?u "Show diffs"              "--patch")
              (?s "Show diffstats"          "--stat"))
  :options  '((?f "Limit to files"          "-- "       magit-log-read-files)
              (?a "Limit to author"         "--author=" read-from-minibuffer)
              (?m "Search messages"         "--grep="   read-from-minibuffer)
              (?p "Search patches"          "-G"        read-from-minibuffer))
  :actions  '((?l "Log current"             magit-log-current)
              (?r "Reflog current"          magit-reflog-current)
              (?o "Log other"               magit-log)
              (?O "Reflog other"            magit-reflog)
              (?h "Log HEAD"                magit-log-head)
              (?H "Reflog HEAD"             magit-reflog-head))
  :default-arguments '("--graph" "--decorate")
  :default-action 'magit-log-current
  :max-action-columns 2)

(defvar magit-log-use-verbose-re
  (concat "^" (regexp-opt '("--patch" "--stat")))
  "Regexp matching arguments which trigger the use of verbose log.")

(defvar magit-log-remove-graph-re
  (concat "^" (regexp-opt '("-G" "--grep")))
  "Regexp matching arguments which are not compatible with `--graph'.")

(defun magit-log-read-args (use-current)
  (let* ((args  (magit-log-arguments))
         (files (--first (string-match-p "^-- " it) args)))
    (when files
      (setq args  (remove files args)
            files (split-string (substring files 3) ",")))
    (list (magit-log-read-revs use-current) args files)))

(defvar magit-log-read-revs-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map crm-local-completion-map)
    (define-key map "\s" 'self-insert-command)
    map))

(defun magit-log-read-revs (&optional use-current)
  (or (and use-current (--when-let (magit-get-current-branch) (list it)))
      (let* ((choose-completion-string-functions
              '(crm--choose-completion-string))
             (minibuffer-completion-table #'crm--collection-fn)
             (minibuffer-completion-confirm t)
             (crm-completion-table (magit-list-branch-names))
             (crm-separator "\\(\\.\\.\\.?\\|[, ]\\)")
             (default (or (magit-branch-or-commit-at-point)
                          (unless use-current
                            (magit-get-previous-branch))))
             (input (read-from-minibuffer "Log rev,s: "
                                          nil magit-log-read-revs-map
                                          nil 'magit-revision-history default)))
        (when (string-equal input "")
          (or (setq input default)
              (user-error "Nothing selected")))
        (split-string input "[, ]" t))))

(defun magit-log-read-files (_ignored initial-contents)
  (mapconcat 'identity
             (completing-read-multiple "File,s: " (magit-list-files)
                                       nil nil initial-contents) ","))

;;;###autoload
(defun magit-log-current (revs &optional args files)
  (interactive (magit-log-read-args t))
  (magit-log revs args files))

;;;###autoload
(defun magit-log (revs &optional args files)
  (interactive (magit-log-read-args nil))
  (magit-mode-setup magit-log-buffer-name-format nil
                    #'magit-log-mode
                    #'magit-log-refresh-buffer
                    (if (--any? (string-match-p magit-log-use-verbose-re it)
                                args)
                        'verbose
                      'oneline)
                    revs
                    (if (--any? (string-match-p magit-log-remove-graph-re it)
                                args)
                        (delete "--graph" args)
                      args)
                    files)
  (magit-log-goto-same-commit))

;;;###autoload
(defun magit-log-head (args)
  (interactive (list (magit-log-arguments)))
  (magit-log "HEAD" args))

;;;###autoload
(defun magit-log-buffer-file ()
  "Show log for the file visited in the current buffer."
  (interactive)
  (-if-let (file (or buffer-file-name magit-buffer-file-name))
      (magit-mode-setup magit-log-buffer-name-format nil
                        #'magit-log-mode
                        #'magit-log-refresh-buffer
                        'oneline
                        (list (or magit-buffer-refname
                                  (magit-get-current-branch) "HEAD"))
                        (magit-log-arguments)
                        (list (file-relative-name file (magit-toplevel))))
    (user-error "Buffer does not visit a file"))
  (magit-log-goto-same-commit))

;;;###autoload
(defun magit-reflog-current ()
  "Display the reflog of the current branch."
  (interactive)
  (magit-reflog (magit-get-current-branch)))

;;;###autoload
(defun magit-reflog (ref)
  "Display the reflog of a branch."
  (interactive (list (magit-read-local-branch "Show reflog for branch")))
  (magit-mode-setup magit-reflog-buffer-name-format nil
                    #'magit-reflog-mode
                    #'magit-reflog-refresh-buffer ref))

;;;###autoload
(defun magit-reflog-head ()
  "Display the HEAD reflog."
  (interactive)
  (magit-reflog "HEAD"))

(defun magit-log-toggle-margin ()
  "Show or hide the log margin."
  (interactive)
  (unless (derived-mode-p 'magit-log-mode 'magit-status-mode)
    (user-error "Buffer doesn't contain any logs"))
  (when (eq (car magit-refresh-args) 'verbose)
    (user-error "Log margin is redundant when showing verbose logs"))
  (magit-set-buffer-margin (not (cdr (window-margins)))))

(defun magit-log-show-more-entries (&optional arg)
  "Grow the number of log entries shown.

With no prefix optional ARG, show twice as many log entries.
With a numerical prefix ARG, add this number to the number of
shown log entries.  With a non-numeric prefix ARG, show all
entries"
  (interactive "P")
  (setq-local magit-log-cutoff-length
              (cond ((numberp arg) (+ magit-log-cutoff-length arg))
                    (arg magit-log-infinite-length)
                    (t (* magit-log-cutoff-length 2))))
  (let ((old-point (point)))
    (magit-refresh)
    (goto-char old-point)))

;;; Log Mode

(defvar magit-log-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "\C-c\C-b" 'magit-go-backward)
    (define-key map "\C-c\C-f" 'magit-go-forward)
    (define-key map "+" 'magit-log-show-more-entries)
    map)
  "Keymap for `magit-log-mode'.")

(define-derived-mode magit-log-mode magit-mode "Magit Log"
  "Mode for looking at Git log.
This mode is documented in info node `(magit)History'.

\\<magit-log-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-show-commit] or \\[magit-diff-show-or-scroll-up]\
 to visit the commit at point.
Type \\[magit-merge-popup] to merge the commit at point.
Type \\[magit-cherry-pick] to cherry-pick the commit at point.
Type \\[magit-reset-head] to reset HEAD to the commit at point.
\n\\{magit-log-mode-map}"
  :group 'magit-log
  (magit-set-buffer-margin magit-log-show-margin))

(defun magit-log-refresh-buffer (style revs args &optional files)
  (magit-insert-section (logbuf)
    (magit-insert-heading "Commits in " (mapconcat 'identity revs  " ")
      (and files (concat " touching "   (mapconcat 'identity files " "))))
    (if (eq style 'oneline)
        (magit-insert-log revs args files)
      (magit-insert-log-verbose revs args files)))
  (save-excursion
    (goto-char (point-min))
    (magit-format-log-margin)))

(defun magit-insert-log (revs &optional args files)
  (magit-git-wash (apply-partially 'magit-log-wash-log 'oneline)
    "log" (format "-%d" magit-log-cutoff-length) "--color"
    (format "--format=%%h%s %s[%%an][%%at]%%s"
            (if (member "--decorate" args) "%d" "")
            (if (member "--show-signature" args)
                (progn (setq args (remove "--show-signature" args)) "%G?")
              ""))
    (if (member "--decorate" args)
        (cons "--decorate=full" (remove "--decorate" args))
      args)
    revs "--" files))

(defun magit-insert-log-verbose (revs &optional args files)
  (magit-git-wash (apply-partially 'magit-log-wash-log 'verbose)
    "log" (format "-%d" magit-log-cutoff-length) "--color"
    (if (member "--decorate" args)
        (cons "--decorate=full" (remove "--decorate" args))
      args)
    revs "--" files))

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

(defconst magit-log-oneline-re
  (concat "^"
          "\\(?4:\\(?: *[-_/|\\*o.] *\\)+ *\\)?"   ; graph
          "\\(?1:[0-9a-fA-F]+\\) "                 ; sha1
          "\\(?:\\(?3:([^()]+)\\) \\)?"            ; refs
          "\\(?7:[BGUN]\\)?"                       ; gpg
          "\\[\\(?5:[^]]*\\)\\]"                   ; author
          "\\[\\(?6:[^]]*\\)\\]"                   ; date
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
          "\\(?2:.*\\)$"))                         ; msg

(defconst magit-log-bisect-log-re
  (concat "^# "
          "\\(?3:bad:\\|skip:\\|good:\\) "         ; "refs"
          "\\[\\(?1:[^]]+\\)\\] "                  ; sha1
          "\\(?2:.*\\)$"))                         ; msg

(defconst magit-log-reflog-re
  (concat "^"
          "\\(?1:[^ ]+\\) "                        ; sha1
          "\\[\\(?5:[^]]*\\)\\] "                  ; author
          "\\(?6:[^ ]*\\) "                        ; date
          "[^@]+@{\\(?9:[^}]+\\)} "                ; refsel
          "\\(?10:merge\\|[^:]+\\)?:? ?"           ; refsub
          "\\(?2:.*\\)?$"))                        ; msg

(defconst magit-reflog-subject-re
  (concat "\\(?1:[^ ]+\\) ?"                       ; command
          "\\(?2:\\(?: ?-[^ ]+\\)+\\)?"            ; option
          "\\(?: ?(\\(?3:[^)]+\\))\\)?"))          ; type

(defconst magit-log-stash-re
  (concat "^"
          "\\(?1:[^ ]+\\)"                         ; "sha1"
          "\\(?5: \\)"                             ; "author"
          "\\(?6:[^ ]+\\) "                        ; date
          "\\(?2:.*\\)$"))                         ; msg

(defconst magit-log-verbose-re
  (concat "^"
          "\\(\\(?:[-_/|\\*o.] *\\)+ *\\)?"
          "commit \\([0-9a-fA-F]+\\)"
          "\\(?: \\(([^()]+)\\)\\)?"))

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
  (let ((magit-log-count 0)
        (abbrev (magit-abbrev-length)))
    (magit-wash-sequence
     (if (eq style 'verbose)
         (apply-partially 'magit-log-wash-verbose abbrev)
       (apply-partially 'magit-log-wash-line style abbrev)))
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
  (when (derived-mode-p 'magit-log-mode)
    (cl-incf magit-log-count))
  (looking-at (pcase style
                (`oneline    magit-log-oneline-re)
                (`cherry     magit-log-cherry-re)
                (`module     magit-log-module-re)
                (`reflog     magit-log-reflog-re)
                (`stash      magit-log-stash-re)
                (`bisect-vis magit-log-bisect-vis-re)
                (`bisect-log magit-log-bisect-log-re)))
  (magit-bind-match-strings
      (hash msg refs graph author date gpg cherry refsel refsub side) nil
    (magit-delete-match)
    (magit-insert-section section (commit hash)
      (pcase style
        (`stash      (setf (magit-section-type section) 'stash))
        (`module     (setf (magit-section-type section) 'mcommit))
        (`bisect-log (setq hash (magit-rev-parse "--short" hash))))
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
      (insert (propertize hash 'face 'magit-hash) ?\s)
      (when graph
        (insert (funcall magit-log-format-graph-function graph)))
      (when refs
        (magit-insert (magit-format-ref-labels refs) nil ?\s))
      (when refsub
        (insert (format "%-2s " refsel))
        (magit-insert (magit-reflog-format-subject refsub)))
      (when msg
        (magit-insert msg (pcase (and gpg (aref gpg 0))
                            (?G 'magit-signature-good)
                            (?B 'magit-signature-bad)
                            (?U 'magit-signature-untrusted))))
      (when (memq style '(oneline reflog stash))
        (goto-char (line-beginning-position))
        (magit-format-log-margin author date))
      (forward-line)))
  (when (eq style 'oneline)
    (let ((align (make-string (1+ abbrev) ? )))
      (while (and (not (eobp)) (not (looking-at magit-log-oneline-re)))
        (insert align)
        (goto-char (line-beginning-position))
        (magit-format-log-margin nil nil)
        (forward-line))))
  t)

(defun magit-log-wash-verbose (abbrev)
  (cl-incf magit-log-count)
  (looking-at magit-log-verbose-re)
  (magit-bind-match-strings (graph hash refs) nil
    (magit-delete-match)
    (magit-insert-section (commit hash)
      (when graph (insert graph))
      (when refs
        (magit-insert (magit-format-ref-labels refs) nil ?\s))
      (magit-insert hash 'magit-hash ?\s)
      (forward-line)
      (while (and (not (eobp)) (not (looking-at magit-log-verbose-re)))
        (forward-line))))
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
                                (if (derived-mode-p 'magit-log-mode) 1 0))
                      nil ?\s (make-string 1 magit-ellipsis))
                     'face 'magit-log-author)
         " "
         (propertize (magit-format-duration
                      (abs (truncate (- (float-time)
                                        (string-to-number date))))
                      (symbol-value duration-spec)
                      unit-width)
                     'face 'magit-log-date)
         (and (derived-mode-p 'magit-log-mode)
              (propertize " " 'face 'fringe)))
      (magit-make-margin-overlay
       (propertize (make-string (1- width) ?\s) 'face 'default)
       (propertize " " 'face 'fringe)))))

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


(defun magit-log-maybe-show-more-entries (section)
  (when (and (eq (magit-section-type section) 'longer)
             magit-log-auto-more)
    (magit-log-show-more-entries)
    (forward-line -1)
    (magit-section-forward)))

(defun magit-log-maybe-show-commit (&optional section)
  (--when-let
      (or (and section
               (eq (magit-section-type section) 'commit)
               (or (and (magit-diff-auto-show-p 'log-follow)
                        (get-buffer-window magit-revision-buffer-name-format))
                   (and (magit-diff-auto-show-p 'log-oneline)
                        (derived-mode-p 'magit-log-mode)
                        (eq (car magit-refresh-args) 'oneline)))
               (magit-section-value section))
          (and magit-blame-mode
               (magit-diff-auto-show-p 'blame-follow)
               (get-buffer-window magit-revision-buffer-name-format)
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

;;; Select Mode

(defvar magit-log-select-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-log-mode-map)
    (define-key map "\C-c\C-b" 'undefined)
    (define-key map "\C-c\C-f" 'undefined)
    (define-key map "."        'magit-log-select-pick)
    (define-key map "e"        'magit-log-select-pick)
    (define-key map "\C-c\C-c" 'magit-log-select-pick)
    (define-key map "q"        'magit-log-select-quit)
    (define-key map "\C-c\C-k" 'magit-log-select-quit)
    map)
  "Keymap for `magit-log-select-mode'.")

(put 'magit-log-select-pick :advertised-binding [?\C-c ?\C-c])
(put 'magit-log-select-quit :advertised-binding [?\C-c ?\C-k])

(define-derived-mode magit-log-select-mode magit-log-mode "Magit Select"
  "Mode for selecting a commit from history."
  :group 'magit-log)

(defvar-local magit-log-select-pick-function nil)
(defvar-local magit-log-select-quit-function nil)

(defun magit-log-select (pick &optional quit desc branch args)
  (declare (indent defun))
  (magit-mode-setup magit-log-buffer-name-format nil
                    #'magit-log-select-mode
                    #'magit-log-refresh-buffer 'oneline
                    (list (or branch (magit-get-current-branch) "HEAD"))
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

;;; Cherry Mode

(defvar magit-cherry-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    map)
  "Keymap for `magit-cherry-mode'.")

(define-derived-mode magit-cherry-mode magit-mode "Magit Cherry"
  "Mode for looking at commits not merged upstream.

\\<magit-cherry-mode-map>\
Type \\[magit-show-commit] or \\[magit-diff-show-or-scroll-up]\
 to visit the commit at point.
Type \\[magit-cherry-pick] to cherry-pick the commit at point.
\n\\{magit-cherry-mode-map}"
  :group 'magit-modes)

;;;###autoload
(defun magit-cherry (head upstream)
  "Show commits in a branch that are not merged in the upstream branch."
  (interactive
   (let  ((head (magit-read-branch "Cherry head")))
     (list head (magit-read-other-branch "Cherry upstream" head
                                         (magit-get-tracked-branch head)))))
  (magit-mode-setup magit-cherry-buffer-name-format nil
                    #'magit-cherry-mode
                    #'magit-cherry-refresh-buffer upstream head))

(defun magit-cherry-refresh-buffer (upstream head)
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

;;; Reflog Mode

(defvar magit-reflog-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-log-mode-map)
    map)
  "Keymap for `magit-reflog-mode'.")

(define-derived-mode magit-reflog-mode magit-log-mode "Magit Reflog"
  "Mode for looking at Git reflog.
This mode is documented in info node `(magit)Reflogs'.

\\<magit-reflog-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-show-commit] or \\[magit-diff-show-or-scroll-up]\
 to visit the commit at point.
Type \\[magit-cherry-pick] to cherry-pick the commit at point.
Type \\[magit-reset-head] to reset HEAD to the commit at point.
\n\\{magit-reflog-mode-map}"
  :group 'magit-log)

(defun magit-reflog-refresh-buffer (ref)
  (magit-insert-section (reflogbuf)
    (magit-insert-heading "Local history of branch " ref)
    (magit-git-wash (apply-partially 'magit-log-wash-log 'reflog)
      "reflog" "show" "--format=%h [%an] %ct %gd %gs"
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

(defun magit-reflog-format-subject (subject)
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

;;; Log Sections

(defvar magit-unpulled-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-diff-unpulled)
    map)
  "Keymap for the `unpulled' section.")

(magit-define-section-jumper unpulled "Unpulled commits")

(defun magit-insert-unpulled-commits ()
  (-when-let (tracked (magit-get-tracked-branch nil t))
    (magit-insert-section (unpulled)
      (magit-insert-heading "Unpulled commits:")
      (magit-insert-log (concat "HEAD.." tracked) magit-log-section-args))))

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

(defun magit-insert-unpulled-cherries ()
  (-when-let (tracked (magit-get-tracked-branch nil t))
    (magit-insert-section (unpulled)
      (magit-insert-heading "Unpulled commits:")
      (magit-git-wash (apply-partially 'magit-log-wash-log 'cherry)
        "cherry" "-v" (magit-abbrev-arg) (magit-get-current-branch) tracked))))

(defvar magit-unpushed-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-diff-unpushed)
    map)
  "Keymap for the `unpushed' section.")

(magit-define-section-jumper unpushed "Unpushed commits")

(defun magit-insert-unpushed-commits ()
  (-when-let (tracked (magit-get-tracked-branch nil t))
    (magit-insert-section (unpushed)
      (magit-insert-heading "Unpushed commits:")
      (magit-insert-log (concat tracked "..HEAD") magit-log-section-args))))

(defun magit-insert-unpushed-cherries ()
  (-when-let (tracked (magit-get-tracked-branch nil t))
    (magit-insert-section (unpushed)
      (magit-insert-heading "Unpushed commits:")
      (magit-git-wash (apply-partially 'magit-log-wash-log 'cherry)
        "cherry" "-v" (magit-abbrev-arg) tracked))))

;;; Buffer Margins

(defun magit-set-buffer-margin (enable)
  (make-local-variable 'magit-log-show-margin)
  (let ((width (and enable
                    (if (and (derived-mode-p 'magit-log-mode)
                             (eq (car magit-refresh-args) 'verbose))
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

;;; magit-log.el ends soon
(provide 'magit-log)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; magit-log.el ends here
