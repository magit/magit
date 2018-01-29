;;; magit-refs.el --- listing references  -*- lexical-binding: t -*-

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

;; This library implements support for listing references in a buffer.

;;; Code:

(require 'magit)

(defvar bookmark-make-record-function)

;;; Options

(defgroup magit-refs nil
  "Inspect and manipulate Git branches and tags."
  :link '(info-link "(magit)References Buffer")
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

(defcustom magit-refs-show-remote-prefix nil
  "Whether to show the remote prefix in lists of remote branches.

This is redundant because the name of the remote is already shown
in the heading preceeding the list of its branches."
  :package-version '(magit . "2.12.0")
  :group 'magit-refs
  :type 'boolean)

(defcustom magit-refs-margin
  (list nil
        (nth 1 magit-log-margin)
        'magit-log-margin-width nil
        (nth 4 magit-log-margin))
  "Format of the margin in `magit-refs-mode' buffers.

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
  :group 'magit-refs
  :group 'magit-margin
  :safe (lambda (val) (memq val '(all branch nil)))
  :type magit-log-margin--custom-type
  :initialize 'magit-custom-initialize-reset
  :set-after '(magit-log-margin)
  :set (apply-partially #'magit-margin-set-variable 'magit-refs-mode))

(defcustom magit-refs-margin-for-tags nil
  "Whether to show information about tags in the margin.

This is disabled by default because it is slow if there are many
tags."
  :package-version '(magit . "2.9.0")
  :group 'magit-refs
  :group 'magit-margin
  :type 'boolean)

(defcustom magit-refs-filter-alist nil
  "Alist controlling which refs are omitted from `magit-refs-mode' buffers.

All keys are tried in order until one matches.  Then its value
is used and subsequent elements are ignored.  If the value is
non-nil, then the reference is displayed, otherwise it is not.
If no element matches, then the reference is displayed.

A key can either be a regular expression that the refname has
to match, or a function that takes the refname as only argument
and returns a boolean.  Contrary to how they are displayed in
the buffer, for comparison each tag begins with \"tags/\" and
each remote branch with \"<remote>/\"."
  :package-version '(magit . "2.12.0")
  :group 'magit-refs
  :type '(alist :key-type   (choice  :tag "Key" regexp function)
                :value-type (boolean :tag "Value"
                                     :on  "show (non-nil)"
                                     :off "omit (nil)")))

(defcustom magit-visit-ref-behavior nil
  "Control how `magit-visit-ref' behaves in `magit-refs-mode' buffers.

By default `magit-visit-ref' behaves like `magit-show-commit',
in all buffers, including `magit-refs-mode' buffers.  When the
type of the section at point is `commit' then \"RET\" is bound to
`magit-show-commit', and when the type is either `branch' or
`tag' then it is bound to `magit-visit-ref'.

\"RET\" is one of Magit's most essential keys and at least by
default it should behave consistently across all of Magit,
especially because users quickly learn that it does something
very harmless; it shows more information about the thing at point
in another buffer.

However \"RET\" used to behave differently in `magit-refs-mode'
buffers, doing surprising things, some of which cannot really be
described as \"visit this thing\".  If you have grown accustomed
to such inconsistent, but to you useful, behavior, then you can
restore that by adding one or more of the below symbols to the
value of this option.  But keep in mind that by doing so you
don't only introduce inconsistencies, you also lose some
functionality and might have to resort to `M-x magit-show-commit'
to get it back.

`magit-visit-ref' looks for these symbols in the order in which
they are described here.  If the presence of a symbol applies to
the current situation, then the symbols that follow do not affect
the outcome.

`focus-on-ref'

  With a prefix argument update the buffer to show commit counts
  and lists of cherry commits relative to the reference at point
  instead of relative to the current buffer or `HEAD'.

  Instead of adding this symbol, consider pressing \"C-u y o RET\".

`create-branch'

  If point is on a remote branch, then create a new local branch
  with the same name, use the remote branch as its upstream, and
  then check out the local branch.

  Instead of adding this symbol, consider pressing \"b c RET RET\",
  like you would do in other buffers.

`checkout-any'

  Check out the reference at point.  If that reference is a tag
  or a remote branch, then this results in a detached `HEAD'.

  Instead of adding this symbol, consider pressing \"b b RET\",
  like you would do in other buffers.

`checkout-branch'

  Check out the local branch at point.

  Instead of adding this symbol, consider pressing \"b b RET\",
  like you would do in other buffers."
  :package-version '(magit . "2.9.0")
  :group 'magit-refs
  :group 'magit-commands
  :options '(focus-on-ref create-branch checkout-any checkout-branch)
  :type '(list :convert-widget custom-hook-convert-widget))

;;; Mode

(defvar magit-refs-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "\C-y" 'magit-refs-set-show-commit-count)
    (define-key map "L"    'magit-margin-popup)
    map)
  "Keymap for `magit-refs-mode'.")

(define-derived-mode magit-refs-mode magit-mode "Magit Refs"
  "Mode which lists and compares references.

This mode is documented in info node `(magit)References Buffer'.

\\<magit-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-section-toggle] to expand or hide the section at point.
Type \\[magit-visit-thing] or \\[magit-diff-show-or-scroll-up] \
to visit the commit or branch at point.

Type \\[magit-branch-popup] to see available branch commands.
Type \\[magit-merge-popup] to merge the branch or commit at point.
Type \\[magit-cherry-pick-popup] to apply the commit at point.
Type \\[magit-reset] to reset `HEAD' to the commit at point.

\\{magit-refs-mode-map}"
  :group 'magit-refs
  (hack-dir-local-variables-non-file-buffer)
  (setq imenu-create-index-function
        #'magit-imenu--refs-create-index-function)
  (setq-local bookmark-make-record-function
              #'magit-bookmark--refs-make-record))

(defun magit-refs-refresh-buffer (ref &optional args)
  (setq magit-set-buffer-margin-refresh (not (magit-buffer-margin-p)))
  (unless ref
    (setq ref "HEAD"))
  (unless (magit-rev-verify ref)
    (setq magit-refs-show-commit-count nil))
  (magit-set-header-line-format
   (format "%s %s" ref (mapconcat #'identity args " ")))
  (magit-insert-section (branchbuf)
    (run-hooks 'magit-refs-sections-hook))
  (add-hook 'kill-buffer-hook 'magit-preserve-section-visibility-cache))

;;; Commands

(defcustom magit-show-refs-arguments nil
  "The arguments used in `magit-refs-mode' buffers."
  :group 'magit-git-arguments
  :group 'magit-refs
  :type '(repeat (string :tag "Argument")))

(defvar magit-show-refs-popup
  (list
   :variable 'magit-show-refs-arguments
   :man-page "git-branch"
   :switches '((?m "Merged to HEAD"            "--merged")
               (?M "Merged to master"          "--merged=master")
               (?n "Not merged to HEAD"        "--no-merged")
               (?N "Not merged to master"      "--no-merged=master"))
   :options  '((?c "Contains"   "--contains="  magit-read-branch-or-commit)
               (?m "Merged"     "--merged="    magit-read-branch-or-commit)
               (?n "Not merged" "--no-merged=" magit-read-branch-or-commit)
               (?s "Sort"       "--sort="      magit-read-ref-sort))
   :actions  '((?y "Show refs, comparing them with HEAD"
                   magit-show-refs-head)
               (?c "Show refs, comparing them with current branch"
                   magit-show-refs-current)
               (?o "Show refs, comparing them with other branch"
                   magit-show-refs))
   :default-action 'magit-show-refs-head
   :max-action-columns 1
   :use-prefix (lambda ()
                 (if (derived-mode-p 'magit-refs-mode)
                     (if current-prefix-arg 'popup 'default)
                   'popup))))

(magit-define-popup-keys-deferred 'magit-show-refs-popup)

(defun magit-read-ref-sort (prompt initial-input)
  (magit-completing-read prompt
                         '("-committerdate" "-authordate"
                           "committerdate" "authordate")
                         nil nil initial-input))

(defun magit-show-refs-get-buffer-args ()
  (cond ((and magit-use-sticky-arguments
              (derived-mode-p 'magit-refs-mode))
         (cadr magit-refresh-args))
        ((and (eq magit-use-sticky-arguments t)
              (--when-let (magit-mode-get-buffer 'magit-refs-mode)
                (with-current-buffer it
                  (cadr magit-refresh-args)))))
        (t
         (default-value 'magit-show-refs-arguments))))

(defun magit-show-refs-arguments ()
  (if (eq magit-current-popup 'magit-show-refs-popup)
      magit-current-popup-args
    (magit-show-refs-get-buffer-args)))

;;;###autoload
(defun magit-show-refs-popup (&optional arg)
  "Popup console for `magit-show-refs'."
  (interactive "P")
  (let ((magit-show-refs-arguments (magit-show-refs-get-buffer-args)))
    (magit-invoke-popup 'magit-show-refs-popup nil arg)))

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
Refs are compared with a branch read from the user."
  (interactive (list (magit-read-other-branch "Compare with")
                     (magit-show-refs-arguments)))
  (magit-mode-setup #'magit-refs-mode ref args))

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
  "Visit the reference or revision at point in another buffer.
If there is no revision at point or with a prefix argument prompt
for a revision.

This command behaves just like `magit-show-commit', except if
point is on a reference in a `magit-refs-mode' buffer (a buffer
listing branches and tags), in which case the behavior may be
different, but only if you have customized the option
`magit-visit-ref-behavior' (which see)."
  (interactive)
  (if (and (derived-mode-p 'magit-refs-mode)
           (magit-section-match '(branch tag)))
      (let ((ref (oref (magit-current-section) value)))
        (cond (current-prefix-arg
               (cond ((memq 'focus-on-ref magit-visit-ref-behavior)
                      (magit-show-refs ref))
                     (magit-visit-ref-behavior
                      ;; Don't prompt for commit to visit.
                      (let ((current-prefix-arg nil))
                        (call-interactively #'magit-show-commit)))))
              ((and (memq 'create-branch magit-visit-ref-behavior)
                    (magit-section-match [branch remote]))
               (let ((branch (cdr (magit-split-branch-name ref))))
                 (if (magit-branch-p branch)
                     (if (magit-rev-eq branch ref)
                         (magit-call-git "checkout" branch)
                       (setq branch (propertize branch 'face 'magit-branch-local))
                       (setq ref (propertize ref 'face 'magit-branch-remote))
                       (pcase (prog1 (read-char-choice (format (propertize "\
Branch %s already exists.
  [c]heckout %s as-is
  [r]reset %s to %s and checkout %s
  [a]bort " 'face 'minibuffer-prompt) branch branch branch ref branch)
                                                       '(?c ?r ?a))
                                (message "")) ; otherwise prompt sticks
                         (?c (magit-call-git "checkout" branch))
                         (?r (magit-call-git "checkout" "-B" branch ref))
                         (?a (user-error "Abort"))))
                   (magit-call-git "checkout" "-b" branch ref))
                 (setcar magit-refresh-args branch)
                 (magit-refresh)))
              ((or (memq 'checkout-any magit-visit-ref-behavior)
                   (and (memq 'checkout-branch magit-visit-ref-behavior)
                        (magit-section-match [branch local])))
               (magit-call-git "checkout" ref)
               (setcar magit-refresh-args ref)
               (magit-refresh))
              (t
               (call-interactively #'magit-show-commit))))
    (call-interactively #'magit-show-commit)))

;;; Sections
;;;; Section Keymaps

(defvar magit-branch-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing]  'magit-visit-ref)
    (define-key map [remap magit-delete-thing] 'magit-branch-delete)
    (define-key map "R"                        'magit-branch-rename)
    map)
  "Keymap for `branch' sections.")

(defvar magit-remote-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-delete-thing] 'magit-remote-remove)
    (define-key map "R"                        'magit-remote-rename)
    map)
  "Keymap for `remote' sections.")

(defvar magit-tag-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing]  'magit-visit-ref)
    (define-key map [remap magit-delete-thing] 'magit-tag-delete)
    map)
  "Keymap for `tag' sections.")

;;;; Section Variables

(defvar magit-refs-local-branch-format "%4c %-25n %U%m\n"
  "Format used for local branches in refs buffers.")
(defvar magit-refs-remote-branch-format "%4c %-25n %m\n"
  "Format used for remote branches in refs buffers.")
(defvar magit-refs-tags-format "%4c %-25n %m\n"
  "Format used for tags in refs buffers.")
(defvar magit-refs-indent-cherry-lines 3
  "Indentation of cherries in refs buffers.")

;;;; Branch Sections

(defun magit-insert-branch-description ()
  "Insert header containing the description of the current branch.
Insert a header line with the name and description of the
current branch.  The description is taken from the Git variable
`branch.<NAME>.description'; if that is undefined then no header
line is inserted at all."
  (-when-let* ((branch (magit-get-current-branch))
               (desc (magit-get "branch" branch "description"))
               (desc (split-string desc "\n")))
    (when (equal (car (last desc)) "")
      (setq desc (butlast desc)))
    (magit-insert-section (branchdesc branch t)
      (magit-insert-heading branch ": " (car desc))
      (when (cdr desc)
        (insert (mapconcat 'identity (cdr desc) "\n"))
        (insert "\n\n")))))

(defun magit-insert-local-branches ()
  "Insert sections showing all local branches."
  (magit-insert-section (local nil)
    (magit-insert-heading "Branches:")
    (dolist (line (magit-git-lines "for-each-ref" "--format=\
%(HEAD)%00%(refname:short)%00%(objectname:short)%00%(subject)%00\
%(upstream:short)%00%(upstream)%00%(upstream:track)"
                                   "refs/heads"
                                   (cadr magit-refresh-args)))
      (pcase-let* ((`(,head ,branch ,hash ,message
                            ,upstream ,uref ,utrack)
                    (-replace "" nil (split-string line "\0")))
                   (current (and (equal head "*") branch)))
        (magit-insert-branch
         branch magit-refs-local-branch-format current
         (if current
             'magit-branch-current
           'magit-branch-local)
         hash message upstream uref
         ;; Strip the brackets here because
         ;; %(upstream:track,nobracket) was added in Git v2.13.
         (and utrack (substring utrack 1 -1)))))
    (insert ?\n)
    (magit-make-margin-overlay nil t)))

(defun magit-insert-remote-branches ()
  "Insert sections showing all remote-tracking branches."
  (dolist (remote (magit-list-remotes))
    (magit-insert-section (remote remote)
      (magit-insert-heading
        (let ((pull (magit-get "remote" remote "url"))
              (push (magit-get "remote" remote "pushurl")))
          (format (propertize "Remote %s (%s):" 'face 'magit-section-heading)
                  (propertize remote 'face 'magit-branch-remote)
                  (concat pull (and pull push ", ") push))))
      (let (head)
        (dolist (line (magit-git-lines "for-each-ref" "--format=\
%(symref:short)%00%(refname:short)%00%(objectname:short)%00%(subject)"
                                       (concat "refs/remotes/" remote)
                                       (cadr magit-refresh-args)))
          (pcase-let ((`(,head-branch ,branch ,hash ,message)
                       (-replace "" nil (split-string line "\0"))))
            (if head-branch
                (progn (cl-assert (equal branch (concat remote "/HEAD")))
                       (setq head head-branch))
              (magit-insert-branch
               branch magit-refs-remote-branch-format nil
               (if (equal branch head)
                   'magit-branch-remote-head
                 'magit-branch-remote)
               hash message nil nil nil
               (and (not magit-refs-show-remote-prefix)
                    (1+ (length remote))))))))
      (insert ?\n)
      (magit-make-margin-overlay nil t))))

(defun magit-insert-branch (branch format &optional current face hash
                                   message upstream uref utrack substring)
  "For internal use, don't add to a hook."
  (unless magit-refs-show-commit-count
    (setq format (replace-regexp-in-string "%[0-9]\\([cC]\\)" "%1\\1" format t)))
  (if branch
      (when (magit-refs-insert-refname-p branch)
        (magit-insert-section it (branch branch t)
          (magit-insert-branch-1 it branch format
                                 current face hash message
                                 upstream uref utrack substring)))
    (magit-insert-section it (commit (magit-rev-parse "HEAD") t)
      (magit-insert-branch-1 it nil format
                             current face hash message
                             upstream uref utrack substring))))

(defun magit-insert-branch-1
    (section branch format current face
             &optional hash message upstream uref utrack substring)
  "For internal use, don't add to a hook."
  (let* ((focus (car magit-refresh-args))
         (head  (or focus "HEAD"))
         (count (and branch
                     (magit-refs-format-commit-count branch head format)))
         (mark  (cond (current
                       (propertize (if (member focus (list nil branch)) "@" ".")
                                   'face 'magit-head))
                      ((equal branch focus)
                       (propertize "#" 'face 'magit-tag)))))
    (when upstream
      (setq upstream (propertize upstream 'face
                                 (if (string-prefix-p "refs/heads/" uref)
                                     'magit-branch-local
                                   'magit-branch-remote))))
    (when (and substring branch)
      (setq substring (substring branch substring)))
    (magit-insert-heading
      (format-spec
       format
       `((?c . ,(or mark count ""))
         (?C . ,(or mark " "))
         (?h . ,(or (propertize hash 'face 'magit-hash) ""))
         (?m . ,(magit-log-propertize-keywords (or message "")))
         (?n . ,(let ((str (propertize (or substring branch "(detached)")
                                       'face face)))
                  (when (string-match "%-?\\([0-9]+\\)n" format)
                    (let ((width (string-to-number (match-string 1 format))))
                      (setq str (concat str (make-string
                                             (max 0 (- width (length str)))
                                             ?\s)))))
                  str))
         (?u . ,(or upstream ""))
         (?U . ,(if upstream
                    (format (propertize "[%s%s] " 'face 'magit-dimmed)
                            (if (equal utrack "gone")
                                (propertize upstream 'face 'error)
                              upstream)
                            (if (and utrack (not (equal utrack "gone")))
                                (concat " " utrack)
                              ""))
                  "")))))
    (when (magit-buffer-margin-p)
      (magit-refs-format-margin branch))
    (magit-refs-insert-cherry-commits head branch section)))

(defun magit-refs-insert-refname-p (refname)
  (--if-let (-first (-lambda ((key . _))
                      (if (functionp key)
                          (funcall key refname)
                        (string-match-p key refname)))
                    magit-refs-filter-alist)
      (cdr it)
    t))

(defun magit-refs-format-commit-count (ref head format &optional tag-p)
  (and (string-match-p "%-?[0-9]+c" format)
       (if tag-p
           (eq magit-refs-show-commit-count 'all)
         magit-refs-show-commit-count)
       (let ((count (cadr (magit-rev-diff-count head ref))))
         (and (> count 0)
              (propertize (number-to-string count) 'face 'magit-dimmed)))))

;;;; Tag Sections

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
            (when (magit-refs-insert-refname-p (concat "tags/" tag))
              (magit-insert-section section (tag tag t)
                (magit-insert-heading
                  (format-spec format
                               `((?n . ,(propertize tag 'face 'magit-tag))
                                 (?c . ,(or mark count ""))
                                 (?m . ,(or message "")))))
                (when (and (magit-buffer-margin-p)
                           magit-refs-margin-for-tags)
                  (magit-refs-format-margin (concat tag "^{commit}")))
                (magit-refs-insert-cherry-commits head tag section))))))
      (insert ?\n)
      (magit-make-margin-overlay nil t))))

;;;; Cherry Sections

(defun magit-refs-insert-cherry-commits (head ref section)
  (if (oref section hidden)
      (oset section washer
            (apply-partially #'magit-refs-insert-cherry-commits-1
                             head ref section))
    (magit-refs-insert-cherry-commits-1 head ref section)))

(defun magit-refs-insert-cherry-commits-1 (head ref _section)
  (let ((start (point))
        (magit-insert-section--current nil))
    (magit-git-wash (apply-partially 'magit-log-wash-log 'cherry)
      "cherry" "-v" (magit-abbrev-arg) head ref magit-refresh-args)
    (unless (= (point) start)
      (magit-make-margin-overlay nil t))))

(defun magit-refs-format-margin (commit)
  (save-excursion
    (goto-char (line-beginning-position 0))
    (let ((line (magit-rev-format "%ct%cN" commit)))
      (magit-log-format-margin (substring line 10)
                               (substring line 0 10)))))

(provide 'magit-refs)
;;; magit-refs.el ends here
