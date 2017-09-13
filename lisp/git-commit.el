;;; git-commit.el --- Edit Git commit messages  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2017  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Authors: Jonas Bernoulli <jonas@bernoul.li>
;;	Sebastian Wiesner <lunaryorn@gmail.com>
;;	Florian Ragwitz <rafl@debian.org>
;;	Marius Vollmer <marius.vollmer@gmail.com>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Package-Requires: ((emacs "24.4") (dash "2.13.0") (with-editor "2.6.0"))
;; Keywords: git tools vc
;; Homepage: https://github.com/magit/magit

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package assists the user in writing good Git commit messages.

;; While Git allows for the message to be provided on the command
;; line, it is preferable to tell Git to create the commit without
;; actually passing it a message.  Git then invokes the `$GIT_EDITOR'
;; (or if that is undefined `$EDITOR') asking the user to provide the
;; message by editing the file ".git/COMMIT_EDITMSG" (or another file
;; in that directory, e.g. ".git/MERGE_MSG" for merge commits).

;; When `global-git-commit-mode' is enabled, which it is by default,
;; then opening such a file causes the features described below, to
;; be enabled in that buffer.  Normally this would be done using a
;; major-mode but to allow the use of any major-mode, as the user sees
;; fit, it is done here by running a setup function, which among other
;; things turns on the preferred major-mode, by default `text-mode'.

;; Git waits for the `$EDITOR' to finish and then either creates the
;; commit using the contents of the file as commit message, or, if the
;; editor process exited with a non-zero exit status, aborts without
;; creating a commit.  Unfortunately Emacsclient (which is what Emacs
;; users should be using as `$EDITOR' or at least as `$GIT_EDITOR')
;; does not differentiate between "successfully" editing a file and
;; aborting; not out of the box that is.

;; By making use of the `with-editor' package this package provides
;; both ways of finish an editing session.  In either case the file
;; is saved, but Emacseditor's exit code differs.
;;
;;   C-c C-c  Finish the editing session successfully by returning
;;            with exit code 0.  Git then creates the commit using
;;            the message it finds in the file.
;;
;;   C-c C-k  Aborts the edit editing session by returning with exit
;;            code 1.  Git then aborts the commit.

;; Aborting the commit does not cause the message to be lost, but
;; relying solely on the file not being tampered with is risky.  This
;; package additionally stores all aborted messages for the duration
;; of the current session (i.e. until you close Emacs).  To get back
;; an aborted message use M-p and M-n while editing a message.
;;
;;   M-p      Replace the buffer contents with the previous message
;;            from the message ring.  Of course only after storing
;;            the current content there too.
;;
;;   M-n      Replace the buffer contents with the next message from
;;            the message ring, after storing the current content.

;; Some support for pseudo headers as used in some projects is
;; provided by these commands:
;;
;;   C-c C-s  Insert a Signed-off-by header.
;;   C-c C-a  Insert a Acked-by header.
;;   C-c C-m  Insert a Modified-by header.
;;   C-c C-t  Insert a Tested-by header.
;;   C-c C-r  Insert a Reviewed-by header.
;;   C-c C-o  Insert a Cc header.
;;   C-c C-p  Insert a Reported-by header.
;;   C-c M-s  Insert a Suggested-by header.

;; When Git requests a commit message from the user, it does so by
;; having her edit a file which initially contains some comments,
;; instructing her what to do, and providing useful information, such
;; as which files were modified.  These comments, even when left
;; intact by the user, do not become part of the commit message.  This
;; package ensures these comments are propertizes as such and further
;; prettifies them by using different faces for various parts, such as
;; files.

;; Finally this package highlights style errors, like lines that are
;; too long, or when the second line is not empty.  It may even nag
;; you when you attempt to finish the commit without having fixed
;; these issues.  The style checks and many other settings can easily
;; be configured:
;;
;;   M-x customize-group RET git-commit RET

;;; Code:
;;;; Dependencies

(require 'dash)
(require 'log-edit)
(require 'magit-utils nil t)
(require 'ring)
(require 'server)
(require 'with-editor)

(eval-when-compile (require 'recentf))

;;;; Declarations

(defvar flyspell-generic-check-word-predicate)
(defvar font-lock-beg)
(defvar font-lock-end)

(declare-function magit-expand-git-file-name 'magit-git)

;;; Options
;;;; Variables

(defgroup git-commit nil
  "Edit Git commit messages."
  :prefix "git-commit-"
  :link '(info-link "(magit)Editing Commit Messages")
  :group 'tools)

;;;###autoload
(define-minor-mode global-git-commit-mode
  "Edit Git commit messages.
This global mode arranges for `git-commit-setup' to be called
when a Git commit message file is opened.  That usually happens
when Git uses the Emacsclient as $GIT_EDITOR to have the user
provide such a commit message."
  :group 'git-commit
  :type 'boolean
  :global t
  :init-value t
  :initialize (lambda (symbol exp)
                (custom-initialize-default symbol exp)
                (when global-git-commit-mode
                  (add-hook 'find-file-hook 'git-commit-setup-check-buffer)))
  (if global-git-commit-mode
      (add-hook  'find-file-hook 'git-commit-setup-check-buffer)
    (remove-hook 'find-file-hook 'git-commit-setup-check-buffer)))

(defcustom git-commit-major-mode 'text-mode
  "Major mode used to edit Git commit messages.
The major mode configured here is turned on by the minor mode
`git-commit-mode'."
  :group 'git-commit
  :type '(choice (function-item text-mode)
                 (const :tag "No major mode")))

(defcustom git-commit-setup-hook
  '(git-commit-save-message
    git-commit-setup-changelog-support
    git-commit-turn-on-auto-fill
    git-commit-propertize-diff
    with-editor-usage-message)
  "Hook run at the end of `git-commit-setup'."
  :group 'git-commit
  :type 'hook
  :get (and (featurep 'magit-utils) 'magit-hook-custom-get)
  :options '(git-commit-save-message
             git-commit-setup-changelog-support
             git-commit-turn-on-auto-fill
             git-commit-turn-on-flyspell
             git-commit-propertize-diff
             bug-reference-mode
             with-editor-usage-message))

(defcustom git-commit-finish-query-functions
  '(git-commit-check-style-conventions)
  "List of functions called to query before performing commit.

The commit message buffer is current while the functions are
called.  If any of them returns nil, then the commit is not
performed and the buffer is not killed.  The user should then
fix the issue and try again.

The functions are called with one argument.  If it is non-nil,
then that indicates that the user used a prefix argument to
force finishing the session despite issues.  Functions should
usually honor this wish and return non-nil."
  :options '(git-commit-check-style-conventions)
  :type 'hook
  :group 'git-commit)

(defcustom git-commit-style-convention-checks '(non-empty-second-line)
  "List of checks performed by `git-commit-check-style-conventions'.
Valid members are `non-empty-second-line' and `overlong-summary-line'.
That function is a member of `git-commit-finish-query-functions'."
  :options '(non-empty-second-line overlong-summary-line)
  :type '(list :convert-widget custom-hook-convert-widget)
  :group 'git-commit)

(defcustom git-commit-summary-max-length 68
  "Column beyond which characters in the summary lines are highlighted.

The highlighting indicates that the summary is getting too long
by some standards.  It does in no way imply that going over the
limit a few characters or in some cases even many characters is
anything that deserves shaming.  It's just a friendly reminder
that if you can make the summary shorter, then you might want
to consider doing so."
  :group 'git-commit
  :safe 'numberp
  :type 'number)

(defcustom git-commit-fill-column nil
  "Override `fill-column' in commit message buffers.

If this is non-nil, then it should be an integer.  If that is the
case and the buffer-local value of `fill-column' is not already
set by the time `git-commit-turn-on-auto-fill' is called as a
member of `git-commit-setup-hook', then that function sets the
buffer-local value of `fill-column' to the value of this option.

This option exists mostly for historic reasons.  If you are not
already using it, then you probably shouldn't start doing so."
  :group 'git-commit
  :safe 'numberp
  :type '(choice (const :tag "use regular fill-column")
                 number))

(make-obsolete-variable 'git-commit-fill-column 'fill-column
                        "Magit 2.11.0" 'set)

(defcustom git-commit-known-pseudo-headers
  '("Signed-off-by" "Acked-by" "Modified-by" "Cc"
    "Suggested-by" "Reported-by" "Tested-by" "Reviewed-by")
  "A list of Git pseudo headers to be highlighted."
  :group 'git-commit
  :safe (lambda (val) (and (listp val) (-all-p 'stringp val)))
  :type '(repeat string))

;;;; Faces

(defgroup git-commit-faces nil
  "Faces used for highlighting Git commit messages."
  :prefix "git-commit-"
  :group 'git-commit
  :group 'faces)

(defface git-commit-summary
  '((t :inherit font-lock-type-face))
  "Face used for the summary in commit messages."
  :group 'git-commit-faces)

(defface git-commit-overlong-summary
  '((t :inherit font-lock-warning-face))
  "Face used for the tail of overlong commit message summaries."
  :group 'git-commit-faces)

(defface git-commit-nonempty-second-line
  '((t :inherit font-lock-warning-face))
  "Face used for non-whitespace on the second line of commit messages."
  :group 'git-commit-faces)

(defface git-commit-note
  '((t :inherit font-lock-string-face))
  "Face used for notes in commit messages."
  :group 'git-commit-faces)

(defface git-commit-pseudo-header
  '((t :inherit font-lock-string-face))
  "Font used for pseudo headers in commit messages."
  :group 'git-commit-faces)

(defface git-commit-known-pseudo-header
  '((t :inherit font-lock-keyword-face))
  "Face used for the keywords of known pseudo headers in commit messages."
  :group 'git-commit-faces)

(defface git-commit-comment-branch
  '((t :inherit font-lock-variable-name-face))
  "Face used for branch names in commit message comments."
  :group 'git-commit-faces)

(defface git-commit-comment-detached
  '((t :inherit git-commit-comment-branch))
  "Face used for detached `HEAD' in commit message comments."
  :group 'git-commit-faces)

(defface git-commit-comment-heading
  '((t :inherit git-commit-known-pseudo-header))
  "Face used for headings in commit message comments."
  :group 'git-commit-faces)

(defface git-commit-comment-file
  '((t :inherit git-commit-pseudo-header))
  "Face used for file names in commit message comments."
  :group 'git-commit-faces)

(defface git-commit-comment-action
  '((t :inherit git-commit-comment-branch))
  "Face used for actions in commit message comments."
  :group 'git-commit-faces)

;;; Keymap

(defvar git-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (cond ((featurep 'jkl)
           (define-key map (kbd "M-i") 'git-commit-prev-message)
           (define-key map (kbd "M-k") 'git-commit-next-message))
          (t
           (define-key map (kbd "M-p") 'git-commit-prev-message)
           (define-key map (kbd "M-n") 'git-commit-next-message)
           ;; Old bindings to avoid confusion
           (define-key map (kbd "C-c C-x a") 'git-commit-ack)
           (define-key map (kbd "C-c C-x i") 'git-commit-suggested)
           (define-key map (kbd "C-c C-x m") 'git-commit-modified)
           (define-key map (kbd "C-c C-x o") 'git-commit-cc)
           (define-key map (kbd "C-c C-x p") 'git-commit-reported)
           (define-key map (kbd "C-c C-x r") 'git-commit-review)
           (define-key map (kbd "C-c C-x s") 'git-commit-signoff)
           (define-key map (kbd "C-c C-x t") 'git-commit-test)))
    (define-key map (kbd "C-c C-a") 'git-commit-ack)
    (define-key map (kbd "C-c C-i") 'git-commit-suggested)
    (define-key map (kbd "C-c C-m") 'git-commit-modified)
    (define-key map (kbd "C-c C-o") 'git-commit-cc)
    (define-key map (kbd "C-c C-p") 'git-commit-reported)
    (define-key map (kbd "C-c C-r") 'git-commit-review)
    (define-key map (kbd "C-c C-s") 'git-commit-signoff)
    (define-key map (kbd "C-c C-t") 'git-commit-test)
    (define-key map (kbd "C-c M-s") 'git-commit-save-message)
    map)
  "Key map used by `git-commit-mode'.")

;;; Menu

(require 'easymenu)
(easy-menu-define git-commit-mode-menu git-commit-mode-map
  "Git Commit Mode Menu"
  '("Commit"
    ["Previous" git-commit-prev-message t]
    ["Next" git-commit-next-message t]
    "-"
    ["Ack" git-commit-ack :active t
     :help "Insert an 'Acked-by' header"]
    ["Sign-Off" git-commit-signoff :active t
     :help "Insert a 'Signed-off-by' header"]
    ["Modified-by" git-commit-modified :active t
     :help "Insert a 'Modified-by' header"]
    ["Tested-by" git-commit-test :active t
     :help "Insert a 'Tested-by' header"]
    ["Reviewed-by" git-commit-review :active t
     :help "Insert a 'Reviewed-by' header"]
    ["CC" git-commit-cc t
     :help "Insert a 'Cc' header"]
    ["Reported" git-commit-reported :active t
     :help "Insert a 'Reported-by' header"]
    ["Suggested" git-commit-suggested t
     :help "Insert a 'Suggested-by' header"]
    "-"
    ["Save" git-commit-save-message t]
    ["Cancel" with-editor-cancel t]
    ["Commit" with-editor-finish t]))

;;; Hooks

;;;###autoload
(defconst git-commit-filename-regexp "/\\(\
\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\
\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'")

(eval-after-load 'recentf
  '(add-to-list 'recentf-exclude git-commit-filename-regexp))

(add-to-list 'with-editor-file-name-history-exclude git-commit-filename-regexp)

(defun git-commit-setup-font-lock-in-buffer ()
  (and buffer-file-name
       (string-match-p git-commit-filename-regexp buffer-file-name)
       (git-commit-setup-font-lock)))

(add-hook 'after-change-major-mode-hook 'git-commit-setup-font-lock-in-buffer)

(defun git-commit-setup-check-buffer ()
  (and buffer-file-name
       (string-match-p git-commit-filename-regexp buffer-file-name)
       (git-commit-setup)))

;;;###autoload
(defun git-commit-setup ()
  ;; cygwin git will pass a cygwin path (/cygdrive/c/foo/.git/...),
  ;; try to handle this in window-nt Emacs.
  (--when-let
      (and (eq system-type 'windows-nt)
           (not (file-accessible-directory-p default-directory))
           (if (require 'magit-git nil t)
               ;; Emacs prepends a "c:".
               (magit-expand-git-file-name (substring buffer-file-name 2))
             ;; Fallback if we can't load `magit-git'.
             (and (string-match "\\`[a-z]:/\\(cygdrive/\\)?\\([a-z]\\)/\\(.*\\)"
                                buffer-file-name)
                  (concat (match-string 2 buffer-file-name) ":/"
                          (match-string 3 buffer-file-name)))))
    (when (file-accessible-directory-p (file-name-directory it))
      (find-alternate-file it)))
  (when git-commit-major-mode
    (let ((auto-mode-alist (list (cons (concat "\\`"
                                               (regexp-quote buffer-file-name)
                                               "\\'")
                                       git-commit-major-mode))))
      (normal-mode t)))
  (setq with-editor-show-usage nil)
  (with-editor-mode 1)
  (add-hook 'with-editor-finish-query-functions
            'git-commit-finish-query-functions nil t)
  (add-hook 'with-editor-pre-finish-hook
            'git-commit-save-message nil t)
  (add-hook 'with-editor-pre-cancel-hook
            'git-commit-save-message nil t)
  (setq with-editor-cancel-message
        'git-commit-cancel-message)
  (make-local-variable 'log-edit-comment-ring-index)
  (git-commit-mode 1)
  (git-commit-setup-font-lock)
  (when (boundp 'save-place)
    (setq save-place nil))
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "\\`\\(\\'\\|\n[^\n]\\)")
      (open-line 1)))
  (run-hooks 'git-commit-setup-hook)
  (set-buffer-modified-p nil))

(defun git-commit-setup-font-lock ()
  (let ((table (make-syntax-table (syntax-table))))
    (when comment-start
      (modify-syntax-entry (string-to-char comment-start) "." table))
    (modify-syntax-entry ?#  "." table)
    (modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?\' "." table)
    (modify-syntax-entry ?`  "." table)
    (set-syntax-table table))
  (setq-local comment-start
              (or (ignore-errors
                    (car (process-lines "git" "config" "core.commentchar")))
                  "#"))
  (setq-local comment-start-skip (format "^%s+[\s\t]*" comment-start))
  (setq-local comment-end-skip "\n")
  (setq-local comment-use-syntax nil)
  (setq-local font-lock-multiline t)
  (add-hook 'font-lock-extend-region-functions
            #'git-commit-extend-region-summary-line
            t t)
  (font-lock-add-keywords nil (git-commit-mode-font-lock-keywords) t))

(define-minor-mode git-commit-mode
  "Auxiliary minor mode used when editing Git commit messages.
This mode is only responsible for setting up some key bindings.
Don't use it directly, instead enable `global-git-commit-mode'."
  :lighter "")

(put 'git-commit-mode 'permanent-local t)

(defun git-commit-setup-changelog-support ()
  "Treat ChangeLog entries as paragraphs."
  (setq-local paragraph-start (concat paragraph-start "\\|\\*\\|(")))

(defun git-commit-turn-on-auto-fill ()
  "Unconditionally turn on Auto Fill mode.
If `git-commit-fill-column' is non-nil, and `fill-column'
doesn't already have a buffer-local value, then set that
to `git-commit-fill-column'."
  (when (and (numberp git-commit-fill-column)
             (not (local-variable-p 'fill-column)))
    (setq fill-column git-commit-fill-column))
  (setq-local comment-auto-fill-only-comments nil)
  (turn-on-auto-fill))

(defun git-commit-turn-on-flyspell ()
  "Unconditionally turn on Flyspell mode.
Also prevent comments from being checked and
finally check current non-comment text."
  (require 'flyspell)
  (turn-on-flyspell)
  (setq flyspell-generic-check-word-predicate
        'git-commit-flyspell-verify)
  (flyspell-buffer))

(defun git-commit-flyspell-verify ()
  (not (= (char-after (line-beginning-position)) ?#)))

(defun git-commit-finish-query-functions (force)
  (run-hook-with-args-until-failure
   'git-commit-finish-query-functions force))

(defun git-commit-check-style-conventions (force)
  "Check for violations of certain basic style conventions.

For each violation ask the user if she wants to proceed anyway.
Option `git-commit-check-style-conventions' controls which
conventions are checked."
  (or force
      (save-excursion
        (goto-char (point-min))
        (re-search-forward (git-commit-summary-regexp) nil t)
        (if (equal (match-string 1) "")
            t ; Just try; we don't know whether --allow-empty-message was used.
          (and (or (not (memq 'overlong-summary-line
                              git-commit-style-convention-checks))
                   (equal (match-string 2) "")
                   (y-or-n-p "Summary line is too long.  Commit anyway? "))
               (or (not (memq 'non-empty-second-line
                              git-commit-style-convention-checks))
                   (not (match-string 3))
                   (y-or-n-p "Second line is not empty.  Commit anyway? ")))))))

(defun git-commit-cancel-message ()
  (message
   (concat "Commit canceled"
           (and (memq 'git-commit-save-message with-editor-pre-cancel-hook)
                ".  Message saved to `log-edit-comment-ring'"))))

;;; History

(defun git-commit-prev-message (arg)
  "Cycle backward through message history, after saving current message.
With a numeric prefix ARG, go back ARG comments."
  (interactive "*p")
  (when (and (git-commit-save-message) (> arg 0))
    (setq log-edit-comment-ring-index
          (log-edit-new-comment-index
           arg (ring-length log-edit-comment-ring))))
  (save-restriction
    (goto-char (point-min))
    (narrow-to-region (point)
                      (if (re-search-forward (concat "^" comment-start))
                          (max 1 (- (point) 2))
                        (point-max)))
    (log-edit-previous-comment arg)))

(defun git-commit-next-message (arg)
  "Cycle forward through message history, after saving current message.
With a numeric prefix ARG, go forward ARG comments."
  (interactive "*p")
  (git-commit-prev-message (- arg)))

(defun git-commit-save-message ()
  "Save current message to `log-edit-comment-ring'."
  (interactive)
  (--when-let (git-commit-buffer-message)
    (unless (ring-member log-edit-comment-ring it)
      (ring-insert log-edit-comment-ring it))))

(defun git-commit-buffer-message ()
  (let ((flush (concat "^" comment-start))
        (str (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (when (re-search-forward (concat flush " -+ >8 -+$") nil t)
        (delete-region (point-at-bol) (point-max)))
      (goto-char (point-min))
      (flush-lines flush)
      (goto-char (point-max))
      (unless (eq (char-before) ?\n)
        (insert ?\n))
      (setq str (buffer-string)))
    (unless (string-match "\\`[ \t\n\r]*\\'" str)
      (when (string-match "\\`\n\\{2,\\}" str)
        (setq str (replace-match "\n" t t str)))
      (when (string-match "\n\\{2,\\}\\'" str)
        (setq str (replace-match "\n" t t str)))
      str)))

;;; Headers

(defun git-commit-ack (name mail)
  "Insert a header acknowledging that you have looked at the commit."
  (interactive (git-commit-self-ident))
  (git-commit-insert-header "Acked-by" name mail))

(defun git-commit-modified (name mail)
  "Insert a header to signal that you have modified the commit."
  (interactive (git-commit-self-ident))
  (git-commit-insert-header "Modified-by" name mail))

(defun git-commit-review (name mail)
  "Insert a header acknowledging that you have reviewed the commit."
  (interactive (git-commit-self-ident))
  (git-commit-insert-header "Reviewed-by" name mail))

(defun git-commit-signoff (name mail)
  "Insert a header to sign off the commit."
  (interactive (git-commit-self-ident))
  (git-commit-insert-header "Signed-off-by" name mail))

(defun git-commit-test (name mail)
  "Insert a header acknowledging that you have tested the commit."
  (interactive (git-commit-self-ident))
  (git-commit-insert-header "Tested-by" name mail))

(defun git-commit-cc (name mail)
  "Insert a header mentioning someone who might be interested."
  (interactive (git-commit-read-ident))
  (git-commit-insert-header "Cc" name mail))

(defun git-commit-reported (name mail)
  "Insert a header mentioning the person who reported the issue."
  (interactive (git-commit-read-ident))
  (git-commit-insert-header "Reported-by" name mail))

(defun git-commit-suggested (name mail)
  "Insert a header mentioning the person who suggested the change."
  (interactive (git-commit-read-ident))
  (git-commit-insert-header "Suggested-by" name mail))

(defun git-commit-self-ident ()
  (list (or (getenv "GIT_AUTHOR_NAME")
            (getenv "GIT_COMMITTER_NAME")
            (ignore-errors (car (process-lines "git" "config" "user.name")))
            user-full-name
            (read-string "Name: "))
        (or (getenv "GIT_AUTHOR_EMAIL")
            (getenv "GIT_COMMITTER_EMAIL")
            (getenv "EMAIL")
            (ignore-errors (car (process-lines "git" "config" "user.email")))
            (read-string "Email: "))))

(defun git-commit-read-ident ()
  (list (read-string "Name: ")
        (read-string "Email: ")))

(defun git-commit-insert-header (header name email)
  (setq header (format "%s: %s <%s>" header name email))
  (save-excursion
    (goto-char (point-max))
    (cond ((re-search-backward "^[-a-zA-Z]+: [^<]+? <[^>]+>" nil t)
           (end-of-line)
           (insert ?\n header)
           (unless (= (char-after) ?\n)
             (insert ?\n)))
          (t
           (while (re-search-backward (concat "^" comment-start) nil t))
           (unless (looking-back "\n\n" nil)
             (insert ?\n))
           (insert header ?\n)))
    (unless (or (eobp) (= (char-after) ?\n))
      (insert ?\n))))

;;; Font-Lock

(defconst git-commit-comment-headings
  '("Changes to be committed:"
    "Untracked files:"
    "Changed but not updated:"
    "Changes not staged for commit:"
    "Unmerged paths:"))

(defun git-commit-summary-regexp ()
  (concat
   ;; Leading empty lines and comments
   (format "\\`\\(?:^\\(?:\\s-*\\|%s.*\\)\n\\)*" comment-start)
   ;; Summary line
   (format "\\(.\\{0,%d\\}\\)\\(.*\\)" git-commit-summary-max-length)
   ;; Non-empty non-comment second line
   (format "\\(?:\n%s\\|\n\\(.+\\)\\)?" comment-start)))

(defun git-commit-extend-region-summary-line ()
  "Identify the multiline summary-regexp construct.
Added to `font-lock-extend-region-functions'."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (when (looking-at (git-commit-summary-regexp))
        (let ((summary-beg (match-beginning 0))
              (summary-end (match-end 0)))
          (when (or (< summary-beg font-lock-beg summary-end)
                    (< summary-beg font-lock-end summary-end))
            (setq font-lock-beg (min font-lock-beg summary-beg))
            (setq font-lock-end (max font-lock-end summary-end))))))))

(defun git-commit-mode-font-lock-keywords ()
  `(;; Comments
    (,(format "^%s.*" comment-start)
     (0 'font-lock-comment-face))
    (,(format "^%s On branch \\(.*\\)" comment-start)
     (1 'git-commit-comment-branch t))
    (,(format "^%s Not currently on any branch." comment-start)
     (1 'git-commit-comment-detached t))
    (,(format "^%s %s" comment-start
              (regexp-opt git-commit-comment-headings t))
     (1 'git-commit-comment-heading t))
    (,(format "^%s\t\\(?:\\([^:\n]+\\):\\s-+\\)?\\(.*\\)" comment-start)
     (1 'git-commit-comment-action t t)
     (2 'git-commit-comment-file t))
    ;; Pseudo headers
    (,(format "^\\(%s:\\)\\( .*\\)"
              (regexp-opt git-commit-known-pseudo-headers))
     (1 'git-commit-known-pseudo-header)
     (2 'git-commit-pseudo-header))
    ("^[-a-zA-Z]+: [^<]+? <[^>]+>"
     (0 'git-commit-pseudo-header))
    ;; Summary
    (,(git-commit-summary-regexp)
     (1 'git-commit-summary t))
    ;; - Note (overrides summary)
    ("\\[.+?\\]"
     (0 'git-commit-note t))
    ;; - Non-empty second line (overrides summary and note)
    (,(git-commit-summary-regexp)
     (2 'git-commit-overlong-summary t t)
     (3 'git-commit-nonempty-second-line t t))))

(defun git-commit-propertize-diff ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^diff --git" nil t)
      (beginning-of-line)
      (let ((buffer (current-buffer)))
        (insert
         (with-temp-buffer
           (insert
            (with-current-buffer buffer
              (prog1 (buffer-substring-no-properties (point) (point-max))
                (delete-region (point) (point-max)))))
           (diff-mode)
           (let (font-lock-verbose font-lock-support-mode)
             (if (fboundp 'font-lock-ensure)
                 (font-lock-ensure)
               (with-no-warnings
                 (font-lock-fontify-buffer))))
           (let (next (pos (point-min)))
             (while (setq next (next-single-property-change pos 'face))
               (put-text-property pos next 'font-lock-face
                                  (get-text-property pos 'face))
               (setq pos next))
             (put-text-property pos (point-max) 'font-lock-face
                                (get-text-property pos 'face)))
           (buffer-string)))))))

(provide 'git-commit)
;;; git-commit.el ends here
