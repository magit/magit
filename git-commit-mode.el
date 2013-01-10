;;; git-commit-mode.el --- Major mode for editing git commit messages

;; Copyright (c) 2012, 2013 Sebastian Wiesner <lunaryorn@gmail.com>
;; Copyright (c) 2010 Florian Ragwitz.
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;;      Florian Ragwitz <rafl@debian.org>
;; Maintainer: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/git-modes
;; Version: 0.10
;; Keywords: convenience vc git

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 51
;; Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; A major mode for editing Git commit messages.

;; * Formatting
;;
;; Highlight the formatting of git commit messages and indicate errors according
;; to the guidelines for commit messages (see
;; http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html).
;;
;; Highlight the first line (aka "summary") specially if it exceeds 54
;; characters.
;;
;; Enable `auto-fill-mode' and set the `fill-column' to 72 according to the
;; aforementioned guidelines.

;; * Headers
;;
;; Provide commands to insert standard headers into commit messages.
;;
;; - C-c C-x s or C-c C-s inserts Signed-off-by (`git-commit-signoff').
;; - C-C C-x a inserts Acked-by (`git-commit-ack').
;; - C-c C-x t inserts Tested-by (`git-commit-test').
;; - C-c C-x r inserts Reviewed-by (`git-commit-review').
;; - C-c C-x o inserts Cc (`git-commit-cc').
;; - C-c C-x p inserts Reported-by (`git-commit-reported').

;; * Committing
;;
;; C-c C-c finishes a commit.  By default this means to save and kill the
;; buffer.  Customize `git-commit-commit-function' to change this behaviour.
;;
;; Check a buffer for stylistic errors before committing, and ask for
;; confirmation before committing with style errors.

;; * Magit integration
;;
;; Overwrite `magit-log-edit-mode' to provide font locking and header insertion
;; for Magit.
;;
;; Change the keymap of `magit-log-edit-mode' to use the header insertion of
;; `git-commit-mode'.

;;; Code:

(require 'server)

(eval-when-compile
  (defvar magit-log-header-end))

(defgroup git-commit nil
  "Mode for editing git commit messages"
  :prefix "git-commit-"
  :group 'tools)

(defgroup git-commit-faces nil
  "Faces for highlighting git commit messages"
  :prefix "git-commit-"
  :group 'git-commit
  :group 'faces)

(defface git-commit-summary-face
  '((t :inherit font-lock-type-face))
  "Face used to highlight the summary in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-overlong-summary-face
  '((t :inherit font-lock-warning-face))
  "Face used to highlight overlong parts of git commit message summaries"
  :group 'git-commit-faces)

(defface git-commit-nonempty-second-line-face
  '((t :inherit font-lock-warning-face))
  "Face used to highlight text on the second line of git commit messages"
  :group 'git-commit-faces)

(defface git-commit-note-face
  '((t :inherit font-lock-string-face))
  "Face used to highlight notes in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-pseudo-header-face
  '((t :inherit font-lock-string-face))
  "Font used to hightlight pseudo headers in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-known-pseudo-header-face
  '((t :inherit font-lock-keyword-face))
  "Face used to hightlight common pseudo headers in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-branch-face
  '((t :inherit font-lock-variable-name-face))
  "Face used to highlight the branch name in comments in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-no-branch-face
  '((t :inherit git-commit-branch-face))
  "Face used when a commit is going to be made outside of any branches"
  :group 'git-commit-faces)

(defface git-commit-comment-heading-face
  '((t :inherit git-commit-known-pseudo-header-face))
  "Face used to highlight section headings in the default
comments in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-comment-file-face
  '((t :inherit git-commit-pseudo-header-face))
  "Face used to highlight file names in the default comments in
git commit messages"
  :group 'git-commit-faces)

(defface git-commit-comment-action-face
  '((t :inherit git-commit-branch-face))
  "Face used to highlight what has happened to files in the
default comments in git commit messages"
  :group 'git-commit-faces)

(defun git-commit-end-session ()
  "Save the buffer and end the session.

If the current buffer has clients from the Emacs server, call
`server-edit' to mark the buffer as done and let the clients
continue, otherwise kill the buffer via `kill-buffer'."
  (if server-buffer-clients
      (server-edit) ; The message buffer comes from emacsclient
    (kill-buffer)))

(defcustom git-commit-commit-function
  #'git-commit-end-session
  "Function called by `git-commit-commit' to actually perform a commit.

The function is called without argument, with the current buffer
being the commit message buffer.  It shall return t, if the
commit was successful, or nil otherwise."
  :group 'git-commit
  :type '(radio (function-item :doc "Save the buffer and end the session."
                               git-commit-end-session)
                (function)))

(defun git-commit-has-style-errors-p ()
  "Check whether the current buffer has style errors.

Return t, if the current buffer has style errors, or nil
otherwise."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (git-commit-find-summary-regexp) nil t)
      (or (string-match-p ".+" (or (match-string 2) ""))
          (string-match-p "^.+$" (or (match-string 3) ""))))))

(defun git-commit-may-do-commit (&optional force)
  "Check whether a commit may be performed.

Check for stylistic errors in the current message, unless FORCE
is non-nil.  If stylistic errors are found, ask the user to
confirm commit.

Return t if the commit may be performed, or nil otherwise."
  (if force t
    (if (git-commit-has-style-errors-p)
        (yes-or-no-p "Buffer has style errors. Commit anyway?")
      t)))

(defun git-commit-log-edit-commit (&optional force)
  "Finish edits and create a new commit.

Check for stylistic errors in the current message, unless FORCE
is non-nil.  If stylistic errors are found, ask the user to
confirm the commit."
  (interactive "P")
  (if (git-commit-may-do-commit force)
      (call-interactively 'magit-log-edit-commit)
    (message "Commit canceled due to stylistic errors.")))

(defun git-commit-commit (&optional force)
  "Finish editing the commit message and commit.

Saves the buffer and checks for style errors, unless prefix
argument FORCE is given.

Call `git-commit-commit-function` to actually perform the commit.
Customize this variable as needed.

Return t, if the commit was successful, or nil otherwise."
  (interactive "P")
  (save-buffer)
  (if (git-commit-may-do-commit force)
      (funcall git-commit-commit-function)
    (message "Commit canceled due to stylistic errors.")))

(defun git-commit-git-config-var (key)
  "Retrieve a git configuration value.
Invokes 'git config --get' to retrieve the value for the
configuration key KEY."
  (let* ((exit)
         (output
          (with-output-to-string
            (with-current-buffer
                standard-output
              (setq exit
                    (call-process "git" nil (list t nil) nil
                                  "config" "--get" key))))))
    (if (not (= 0 exit))
        nil
      (substring output 0 (- (length output) 1)))))

(defun git-commit-first-env-var (&rest vars)
  "Get the value of the first defined environment variable.
Walk VARS, call `getenv' on each element and return the first
non-nil return value of `getenv'."
  (let ((current vars)
        (val nil))
    (while (and (not val) current)
      (setq val (getenv (car current)))
      (setq current (cdr current)))
    val))

(defun git-commit-committer-name ()
  "Get the git committer name of the current user.
This uses the same mechanism git itself uses.  That is, using the
value of the 'GIT_AUTHOR_NAME' or 'GIT_COMMITTER_NAME'
environment variables, or the 'user.name' git configuration
variable.

If the above mechanism fails, the value of the variable
`user-full-name' is used."
  (or
   (git-commit-first-env-var "GIT_AUTHOR_NAME" "GIT_COMMITTER_NAME")
   (git-commit-git-config-var "user.name")
   user-full-name))

(defun git-commit-committer-email ()
  "Get the git committer email address of the current user.
This uses the same mechanism git itself uses.  That is, using the
value of the 'GIT_AUTHOR_EMAIL', 'GIT_COMMITTER_EMAIL', or
'EMAIL' environment variables, or the 'user.email' git
configuration variable.

If the above mechanism fails, the value of the variable
`user-email-address' is used."
  (or
   (git-commit-first-env-var "GIT_AUTHOR_EMAIL" "GIT_COMMITTER_EMAIL" "EMAIL")
   (git-commit-git-config-var "user.email")
   user-mail-address))

(defun git-commit-find-pseudo-header-position ()
  "Find the position at which commit pseudo headers should be inserted.

Those headers usually live at the end of a commit message, but
before any trailing comments git or the user might have
inserted."
  (save-excursion
    (let ((comment-start (point)))
      (goto-char (point-max))
      (if (not (re-search-backward "^\\S<[^\s:]+:.*$" nil t))
          ;; no headers yet, so we'll search backwards for a good place
          ;; to insert them
          (if (not (re-search-backward "^[^#].*?.*$" nil t))
              ;; no comment lines anywhere before end-of-buffer, so we
              ;; want to insert right there
              (point-max)
            ;; there's some comments at the end, so we want to insert
            ;; before those
            (beginning-of-line)
            (forward-line 1)
            (point))
        ;; we're at the last header, and we want the line right after
        ;; that to insert further headers
        (beginning-of-line)
        (forward-line 1)
        (point)))))

(defun git-commit-insert-header (type name email)
  "Insert a header into the commit message.
The inserted headers have the format 'TYPE: NAME <EMAIL>'.

The header is inserted at the position returned by
`git-commit-find-pseudo-header-position'.  When this position
isn't after an existing header or a newline, an extra newline is
inserted before the header."
  (let* ((header-at (git-commit-find-pseudo-header-position))
         (prev-line (or (save-excursion
                          (goto-char (- header-at 1))
                          (thing-at-point 'line)) ""))
         (pre       (if (or (string-match "^[^\s:]+:.+$" prev-line)
                            (string-match "\\`\\s-*$" prev-line))
                        "" "\n")))
    (save-excursion
      (goto-char header-at)
      (insert (format "%s%s: %s <%s>\n" pre type name email)))))

(defun git-commit-insert-header-as-self (type)
  "Insert a header with the name and email address of the current user.
Call `git-commit-insert-header' with the user name and email
address provided by `git-commit-committer-name' and
`git-commit-committer-email'.

TYPE is passed along unmodified."
  (let ((committer-name (git-commit-committer-name))
        (committer-email (git-commit-committer-email)))
    (git-commit-insert-header type committer-name committer-email)))

(defmacro git-define-git-commit-self (action header)
  "Create function git-commit-ACTION.
ACTION will be part of the function name.
HEADER is the actual header to be inserted into the comment."
  (let ((func-name (intern (concat "git-commit-" action))))
    `(defun ,func-name ()
       ,(format "Insert a '%s' header at the end of the commit message.

The author name and email address used for the header are
retrieved automatically with the same mechanism git uses."
                header)
       (interactive)
       (git-commit-insert-header-as-self ,header))))

(git-define-git-commit-self "ack"     "Acked-by")
(git-define-git-commit-self "review"  "Reviewed-by")
(git-define-git-commit-self "signoff" "Signed-off-by")
(git-define-git-commit-self "test"    "Tested-by")

(defmacro git-define-git-commit (action header)
  "Create interactive function git-commit-ACTION.
ACTION will be part of the function name.
HEADER is the actual header to be inserted into the comment."
  (let ((func-name (intern (concat "git-commit-" action))))
    `(defun ,func-name (name email)
       ,(format "Insert a '%s' header at the end of the commit message.
The value of the header is determined by NAME and EMAIL.

When called interactively, both NAME and EMAIL are read from the
minibuffer."
                header)
       (interactive
        (list (read-string "Name: ")
              (read-string "Email: ")))
       (git-commit-insert-header ,header name email))))

(git-define-git-commit "cc" "Cc")
(git-define-git-commit "reported" "Reported-by")

(defconst git-commit-known-pseudo-headers
  '("Signed-off-by"
    "Acked-by"
    "Cc"
    "Reported-by"
    "Tested-by"
    "Reviewed-by")
  "A list of git pseudo headers to be highlighted.")

(defconst git-commit-comment-headings-alist
  '(("Not currently on any branch." . git-commit-no-branch-face)
    ("Changes to be committed:" . git-commit-comment-heading-face)
    ("Untracked files:" . git-commit-comment-heading-face)
    ("Changed but not updated:" . git-commit-comment-heading-face)
    ("Changes not staged for commit:" . git-commit-comment-heading-face)
    ("Unmerged paths:" . git-commit-comment-heading-face))
  "Headings in message comments.

The `car' of each cell is the heading text, the `cdr' the face to
use for fontification.")

(defconst git-commit-skip-before-summary-regexp
  "\\(?:\\(?:\\s-*\\|\\s<.*\\)\n\\)*"
  "Regexp to skip empty lines and comments before the summary.

Do not use this expression directly, instead call
`git-commit-find-summary-regexp' to create a regular expression
to match the summary line.")

(defconst git-commit-summary-regexp
   "\\(?:^\\(.\\{,50\\}\\)\\(.*?\\)$\\)"
   "Regexp to match the summary line.

Do not use this expression directly, instead call
`git-commit-find-summary-regexp' to create a regular expression
to match the summary line.")

(defconst git-commit-nonempty-second-line-regexp
  "\\(?:\n\\(.*\\)\\)?$"
  "Regexp to match a nonempty line following the summary.

Do not use this expression directly, instead call
`git-commit-find-summary-regexp' to create a regular expression
to match the summary line.")

(defvar git-commit-skip-magit-header-regexp nil
  "Regexp to skip magit header.

This variable is nil until `magit' is loaded.

Do not use this expression directly, instead call
`git-commit-find-summary-regexp' to create a regular expression
to match the summary line.")

(eval-after-load 'magit
  ;; Configure regexp to skip Magit header
  '(setq git-commit-skip-magit-header-regexp
        (format
         "\\(?:\\(?:[A-Za-z0-9-_]+: *.*\n\\)*%s\\)?"
         (regexp-quote magit-log-header-end))))

(defun git-commit-find-summary-regexp ()
  "Create a regular expression to find the Git summary line.

Return a regular expression that starts at the beginning of the
buffer, skips over empty lines, comments and also over the magit
header, if the current buffer is a `magit-log-edit-mode' buffer,
and finds the summary line.

The regular expression matches three groups.  The first group is
the summary line, the second group contains any overlong part of
the summary, and the third group contains a nonempty line
following the summary line.  The latter two groups may be empty."
  (format "\\`%s%s%s%s"
          (if (eq major-mode 'magit-log-edit-mode)
              git-commit-skip-magit-header-regexp
            "")
          git-commit-skip-before-summary-regexp
          git-commit-summary-regexp
          git-commit-nonempty-second-line-regexp))

(defun git-commit-mode-summary-font-lock-keywords (&optional errors)
  "Create font lock keywords to fontify the Git summary.

If ERRORS is non-nil create keywords that highlight errors in the
summary line, not the summary line itself."
  (let ((regexp (git-commit-find-summary-regexp)))
    (if errors
        `(,regexp
          (2 'git-commit-overlong-summary-face t t)
          (3 'git-commit-nonempty-second-line-face t t))
      `(,regexp (1 'git-commit-summary-face t)))))

(defun git-commit-mode-heading-keywords ()
  "Create font lock keywords to fontify comment headings.

Known comment headings are provided by `git-commit-comment-headings'."
  (mapcar (lambda (cell) `(,(format "^\\s<\\s-+\\(%s\\)$"
                                    (regexp-quote (car cell)))
                           (1 ',(cdr cell) t)))
          git-commit-comment-headings-alist))

(defvar git-commit-mode-font-lock-keywords
  (append
   `(("^\\s<.*$" . 'font-lock-comment-face)
     ("^\\s<\\s-On branch \\(.*\\)$" (1 'git-commit-branch-face t))
     ("^\\s<\t\\(?:\\([^:]+\\):\\s-+\\)?\\(.*\\)$"
      (1 'git-commit-comment-action-face t t)
      (2 'git-commit-comment-file-face t))
     (,(concat "^\\("
               (regexp-opt git-commit-known-pseudo-headers)
               ":\\)\\(\s.*\\)$")
      (1 'git-commit-known-pseudo-header-face)
      (2 'git-commit-pseudo-header-face))
     ("^\\<\\S-+:\\s-.*$" . 'git-commit-pseudo-header-face)
     (eval . (git-commit-mode-summary-font-lock-keywords))
     ("\\[[^\n]+?\\]" (0 'git-commit-note-face t)) ; Notes override summary line
     ;; Warnings from overlong lines and nonempty second line override
     ;; everything
     (eval . (git-commit-mode-summary-font-lock-keywords t)))
   (git-commit-mode-heading-keywords)))

(defvar git-commit-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Short shortcut ;) for the frequently used signoff header
    (define-key map (kbd "C-c C-s") 'git-commit-signoff)
    ;; Verbose shortcuts for all headers to avoid conflicts with magit bindings
    (define-key map (kbd "C-c C-x s") 'git-commit-signoff)
    (define-key map (kbd "C-c C-x a") 'git-commit-ack)
    (define-key map (kbd "C-c C-x t") 'git-commit-test)
    (define-key map (kbd "C-c C-x r") 'git-commit-review)
    (define-key map (kbd "C-c C-x o") 'git-commit-cc)
    (define-key map (kbd "C-c C-x p") 'git-commit-reported)
    ;; Committing
    (define-key map (kbd "C-c C-c") 'git-commit-commit)
    map)
  "Key map used by `git-commit-mode'.")

(defvar git-commit-mode-syntax-table
  (let ((table (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\r ">" table)
    table)
  "Syntax table used by `git-commit-mode'.")

(defun git-commit-font-lock-diff ()
  "Add font lock on diff."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^diff --git" nil t)
      (let ((beg (match-beginning 0)))
        (let* ((buffer (current-buffer))
               (font-lock-verbose nil)
               (font-lock-support-mode nil)
               (text (with-temp-buffer
                       (insert
                        (with-current-buffer buffer
                          (buffer-substring-no-properties beg (point-max))))
                       (diff-mode)
                       (font-lock-fontify-buffer)
                       (let ((pos (point-min))
                             next)
                         (while (setq next (next-single-property-change pos 'face))
                           (put-text-property pos next 'font-lock-face
                                              (get-text-property pos 'face))
                           (setq pos next)))
                       (buffer-string))))
          (delete-region beg (point-max))
          (insert text))))))

;;;###autoload
(defun git-commit-mode-magit-setup ()
  (message "Magit integration is now always enabled!")
  "Compatibility function.

Obsolete function for compatibility with older releases.  Does
nothing.")

;;;###autoload
(define-derived-mode git-commit-mode text-mode "Git Commit"
  "Major mode for editing git commit messages.

This mode helps with editing git commit messages both by
providing commands to do common tasks, and by highlighting the
basic structure of and errors in git commit messages."
  ;; Font locking
  (setq font-lock-defaults '(git-commit-mode-font-lock-keywords t))
  (set (make-local-variable 'font-lock-multiline) t)
  ;; Filling according to the guidelines
  (setq fill-column 72)
  (turn-on-auto-fill)
  ;; Recognize changelog-style paragraphs
  (set (make-local-variable 'paragraph-start)
       (concat paragraph-start "\\|*\\|("))
  ;; Do not remember point location in commit messages
  (when (fboundp 'toggle-save-place)
    (toggle-save-place 0)))

;;;###autoload
(eval-after-load 'session
  #'(add-to-list 'session-mode-disable-list 'git-commit-mode))

;;;###autoload
;; Overwrite magit-log-edit-mode to derive it from git-commit-mode
(eval-after-load 'magit
  #'(define-derived-mode magit-log-edit-mode git-commit-mode "Magit Log Edit"))

;;;###autoload
;; Change the Magit log edit keymap to use our commit and header insertion
;; bindings
(eval-after-load 'magit
  #'(progn
      (substitute-key-definition 'magit-log-edit-toggle-signoff
                                 'git-commit-signoff
                                 magit-log-edit-mode-map)
      (substitute-key-definition 'magit-log-edit-commit
                                 'git-commit-log-edit-commit
                                 magit-log-edit-mode-map)))

;;;###autoload
(setq auto-mode-alist
      (append auto-mode-alist
              '(("/COMMIT_EDITMSG\\'" . git-commit-mode)
                ("/NOTES_EDITMSG\\'" . git-commit-mode)
                ("/MERGE_MSG\\'" . git-commit-mode)
                ("/TAG_EDITMSG\\'" . git-commit-mode)
                ("/PULLREQ_EDITMSG\\'" . git-commit-mode))))

(provide 'git-commit-mode)

;;; git-commit-mode.el ends here
