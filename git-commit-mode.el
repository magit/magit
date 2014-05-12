;;; git-commit-mode.el --- edit Git commit messages  -*- lexical-binding: t; -*-

;; Copyright (c) 2010-2012  Florian Ragwitz
;; Copyright (c) 2012-2013  Sebastian Wiesner
;; Copyright (C) 2010-2014  The Magit Project Developers

;; Authors: Jonas Bernoulli <jonas@bernoul.li>
;;	Sebastian Wiesner <lunaryorn@gmail.com>
;;	Florian Ragwitz <rafl@debian.org>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Version: 0.14.0
;; Homepage: https://github.com/magit/git-modes
;; Keywords: convenience vc git

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

;; Support for editing Git commit messages.

;;;; Formatting

;; Highlight the formatting of git commit messages and indicate errors according
;; to the guidelines for commit messages (see
;; http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html).
;;
;; Highlight the first line (aka "summary") specially if it exceeds 50
;; characters (configurable using `git-commit-summary-max-length').
;;
;; Enable `auto-fill-mode' and set the `fill-column' to 72 according to the
;; aforementioned guidelines (configurable using `git-commit-fill-column').

;;;; Headers

;; Provide commands to insert standard headers into commit messages.
;;
;; - C-c C-s inserts Signed-off-by (`git-commit-signoff').
;; - C-C C-a inserts Acked-by (`git-commit-ack').
;; - C-c C-t inserts Tested-by (`git-commit-test').
;; - C-c C-r inserts Reviewed-by (`git-commit-review').
;; - C-c C-o inserts Cc (`git-commit-cc').
;; - C-c C-p inserts Reported-by (`git-commit-reported').

;;;; Committing

;; C-c C-c finishes a commit.
;;
;; Check a buffer for stylistic errors before committing, and ask for
;; confirmation before committing with style errors.

;;; Code:
;;;; Dependencies

(require 'log-edit)
(require 'ring)
(require 'server)
(require 'with-editor)

;;;; Declarations

(defvar flyspell-generic-check-word-predicate)

;;; Options
;;;; Variables

(defgroup git-commit nil
  "Edit Git commit messages."
  :prefix "git-commit-"
  :group 'tools)

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

(defconst git-commit-setup-hook-options
  '(git-commit-save-message
    git-commit-setup-changelog-support
    git-commit-turn-on-auto-fill
    git-commit-turn-on-flyspell
    git-commit-propertize-diff
    with-editor-usage-message))

(defcustom git-commit-setup-hook git-commit-setup-hook-options
  "Hook run at the end of `git-commit-setup'."
  :group 'git-commit
  :type 'hook
  :options git-commit-setup-hook-options)

(defcustom git-commit-finish-query-functions
  '(git-commit-check-style-conventions)
  "List of functions called to query before performing commit.

The commit message buffer is current while the functions are
called.  If any of them returns nil, then the commit is not
performed and the buffer is not killed.  The user should then
fix the issue and try again.

The functions are called with one argument.  If it is non-nil
then that indicates that the user used a prefix argument to
force finishing the session despite issues.  Functions should
usually honor this wish and return non-nil."
  :options '(git-commit-check-style-conventions)
  :type 'hook
  :group 'git-commit)

(defcustom git-commit-summary-max-length 50
  "Fontify characters beyond this column in summary lines as errors."
  :group 'git-commit
  :type 'number)

(defcustom git-commit-fill-column 72
  "Automatically wrap commit message lines beyond this column."
  :group 'git-commit
  :type 'number)

(defcustom git-commit-known-pseudo-headers
  '("Signed-off-by" "Acked-by" "Cc"
    "Suggested-by" "Reported-by" "Tested-by" "Reviewed-by")
  "A list of git pseudo headers to be highlighted."
  :group 'git-commit
  :type '(repeat string))

;;;; Faces

(defgroup git-commit-faces nil
  "Faces for highlighting Git commit messages."
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

;;; Keymap

(defvar git-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") 'git-commit-signoff)
    (define-key map (kbd "C-c C-a") 'git-commit-ack)
    (define-key map (kbd "C-c C-t") 'git-commit-test)
    (define-key map (kbd "C-c C-r") 'git-commit-review)
    (define-key map (kbd "C-c C-o") 'git-commit-cc)
    (define-key map (kbd "C-c C-p") 'git-commit-reported)
    (define-key map (kbd "C-c C-i") 'git-commit-suggested)
    (define-key map (kbd "C-c M-s") 'git-commit-save-message)
    (define-key map (kbd "M-p")     'git-commit-prev-message)
    (define-key map (kbd "M-n")     'git-commit-next-message)
    ;; Old bindings to avoid confusion
    (define-key map (kbd "C-c C-x s") 'git-commit-signoff)
    (define-key map (kbd "C-c C-x a") 'git-commit-ack)
    (define-key map (kbd "C-c C-x t") 'git-commit-test)
    (define-key map (kbd "C-c C-x r") 'git-commit-review)
    (define-key map (kbd "C-c C-x o") 'git-commit-cc)
    (define-key map (kbd "C-c C-x p") 'git-commit-reported)
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

(defconst git-commit-filename-regexp "/\\(\
\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\
\\|BRANCH_DESCRIPTION\\)\\'")

(defun git-commit-setup-font-lock-in-buffer ()
  (and buffer-file-name
       (string-match-p git-commit-filename-regexp buffer-file-name)
       (git-commit-setup-font-lock)))

(add-hook 'after-change-major-mode-hook 'git-commit-setup-font-lock-in-buffer)

(defun git-commit-setup-check-buffer ()
  (and buffer-file-name
       (string-match-p git-commit-filename-regexp buffer-file-name)
       (git-commit-setup)))

(defun git-commit-setup ()
  (when git-commit-major-mode
    (funcall git-commit-major-mode))
  (setq with-editor-show-usage nil)
  (with-editor-mode 1)
  (add-hook 'with-editor-finish-query-functions
            'git-commit-finish-query-functions nil t)
  (add-hook 'with-editor-pre-cancel-hook
            'git-commit-save-message nil t)
  (setq with-editor-cancel-message
        'git-commit-cancel-message)
  (git-commit-mode 1)
  (git-commit-setup-font-lock)
  (when (boundp 'save-place)
    (setq save-place nil))
  (save-excursion
    (goto-char (point-min))
    (when (= (line-beginning-position)
             (line-end-position))
      (open-line 1)))
  (run-hooks 'git-commit-setup-hook))

(defun git-commit-setup-font-lock ()
  (set-syntax-table (let ((table (make-syntax-table (syntax-table))))
                      (modify-syntax-entry ?#  "<" table)
                      (modify-syntax-entry ?\n ">" table)
                      (modify-syntax-entry ?\r ">" table)
                      table))
  (setq-local comment-start "#")
  (setq-local comment-start-skip "^#+\\s-*")
  (setq-local comment-use-syntax nil)
  (setq-local font-lock-multiline t)
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
And set `fill-column' to `git-commit-fill-column'."
  (setq fill-column git-commit-fill-column)
  (turn-on-auto-fill))

(defun git-commit-turn-on-flyspell ()
  "Unconditionally turn on Auto Fill mode.
Also prevent comments from being checked and
finally check current non-comment text."
  (require 'flyspell)
  (turn-on-flyspell)
  (setq flyspell-generic-check-word-predicate
        'git-commit-mode-flyspell-verify)
  (flyspell-buffer))

(defun git-commit-mode-flyspell-verify ()
  (not (nth 4 (syntax-ppss)))) ; not inside a comment

(defun git-commit-finish-query-functions (force)
  (run-hook-with-args-until-failure
   'git-commit-finish-query-functions force))

(defun git-commit-check-style-conventions (force)
  (or (not (git-commit-has-style-errors-p)) force
      (or (y-or-n-p "Commit despite stylistic errors? ")
          (progn nil (message "Commit canceled due to stylistic errors")))))

(defun git-commit-cancel-message ()
  (message
   (concat "Commit canceled"
           (and (memq 'git-commit-save-message with-editor-pre-cancel-hook)
                ".  Message saved to `log-edit-comment-ring'"))))

;;; History

(defun git-commit-save-message ()
  "Save current message to `log-edit-comment-ring'."
  (interactive)
  (let ((message (buffer-substring
                  (point-min)
                  (git-commit-find-pseudo-header-position))))
    (when (and (string-match "^\\s-*\\sw" message)
               (or (ring-empty-p log-edit-comment-ring)
                   (not (ring-member log-edit-comment-ring message))))
      ;; if index is nil, we end up cycling back to message we just saved!
      (unless log-edit-comment-ring-index
        (setq log-edit-comment-ring-index 0))
      (ring-insert log-edit-comment-ring message))))

(defun git-commit-prev-message (arg)
  "Cycle backward through message history, after saving current message.
With a numeric prefix ARG, go back ARG comments."
  (interactive "*p")
  (git-commit-save-message)
  (save-restriction
    (narrow-to-region (point-min) (git-commit-find-pseudo-header-position))
    (log-edit-previous-comment arg)))

(defun git-commit-next-message (arg)
  "Cycle forward through message history, after saving current message.
With a numeric prefix ARG, go forward ARG comments."
  (interactive "*p")
  (git-commit-prev-message (- arg)))

;;; Headers

(defun git-commit-find-pseudo-header-position ()
  "Find the position at which commit pseudo headers should be inserted.

Those headers usually live at the end of a commit message, but
before any trailing comments git or the user might have
inserted."
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward "^[^#\n]" nil t)
        ;; we found last non-empty non-comment line, headers go after
        (forward-line 1)
      ;; there's only blanks & comments, headers go before comments
      (goto-char (point-min))
      (and (re-search-forward "^#" nil t) (forward-line 0)))
    (skip-chars-forward "\n")
    (point)))

(defun git-commit-determine-pre-for-pseudo-header ()
  "Find the characters to insert before the pseudo header.
Returns either zero, one or two newlines after computation.

`point' either points to an empty line (with a non-empty previous
line) or the end of a non-empty line."
  (let ((pre "")
        (prev-line nil))
    (if (not (eq (point) (point-at-bol)))
        (progn
          (setq pre (concat pre "\n"))
          (setq prev-line (thing-at-point 'line)))
      ;; else: (point) is at an empty line
      (when (not (eq (point) (point-min)))
        (setq prev-line
              (save-excursion
                (forward-line -1)
                (thing-at-point 'line)))))

    ;; we have prev-line now; if it doesn't match any known pseudo
    ;; header, add a newline
    (when prev-line
      (if (not (delq nil (mapcar (lambda (pseudo-header)
                                   (string-match pseudo-header prev-line))
                                 git-commit-known-pseudo-headers)))
          (setq pre (concat pre "\n"))))
    pre))

(defun git-commit-insert-header (type name email)
  "Insert a header into the commit message.
The inserted header has the format 'TYPE: NAME <EMAIL>'.

The header is inserted at the position returned by
`git-commit-find-pseudo-header-position'.  When this position
isn't after an existing header or a newline, an extra newline is
inserted before the header."
  (let ((header-at (git-commit-find-pseudo-header-position)))
    (save-excursion
      (goto-char header-at)
      (let ((pre (git-commit-determine-pre-for-pseudo-header)))
        (insert (format "%s%s: %s <%s>\n" pre type name email))))))

(defun git-commit-insert-header-as-self (type)
  "Insert a header with the name and email of the current user.
The inserted header has the format 'TYPE: NAME <EMAIL>'.
Also see `git-commit-insert-header'."
  (git-commit-insert-header
   type
   (or (getenv "GIT_AUTHOR_NAME")
       (getenv "GIT_COMMITTER_NAME")
       (ignore-errors (car (process-lines "git" "config" "user.name")))
       user-full-name)
   (or (getenv "GIT_AUTHOR_EMAIL")
       (getenv "GIT_COMMITTER_EMAIL")
       (getenv "EMAIL")
       (ignore-errors (car (process-lines "git" "config" "user.email")))
       user-mail-address)))

(defmacro git-commit-define-self-header-inserter (action header)
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

(git-commit-define-self-header-inserter "ack"     "Acked-by")
(git-commit-define-self-header-inserter "review"  "Reviewed-by")
(git-commit-define-self-header-inserter "signoff" "Signed-off-by")
(git-commit-define-self-header-inserter "test"    "Tested-by")

(defmacro git-commit-define-header-inserter (action header)
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

(git-commit-define-header-inserter "cc"        "Cc")
(git-commit-define-header-inserter "reported"  "Reported-by")
(git-commit-define-header-inserter "suggested" "Suggested-by")

(defconst git-commit-comment-headings-alist
  '(("Not currently on any branch."   . git-commit-no-branch-face)
    ("Changes to be committed:"       . git-commit-comment-heading-face)
    ("Untracked files:"               . git-commit-comment-heading-face)
    ("Changed but not updated:"       . git-commit-comment-heading-face)
    ("Changes not staged for commit:" . git-commit-comment-heading-face)
    ("Unmerged paths:"                . git-commit-comment-heading-face))
  "Headings in message comments.

The `car' of each cell is the heading text, the `cdr' the face to
use for fontification.")

(defun git-commit-summary-regexp ()
  (concat
   ;; Skip empty lines or comments before the summary
   "\\`\\(?:^\\(?:\\s-*\\|\\s<.*\\)\n\\)*"
   ;; The summary line
   (format "\\(.\\{0,%d\\}\\)\\(.*\\)" git-commit-summary-max-length)
   ;; Non-empty non-comment second line
   ;;
   ;; For instant highlighting of non-empty second lines in font-lock,
   ;; the last capturing group must capture the empty string ("") in
   ;; "summary line\n".
   ;; That's why the simpler regex "\\(?:\n\\([^\n#].*\\)\\)?",
   ;; which captures 'nil', can't be used.
   "\\(?:\n\\#\\|\n\\(.*\\)\\)?"))

(defun git-commit-has-style-errors-p ()
  "Check whether the current buffer has style errors.

Return t, if the current buffer has style errors, or nil
otherwise."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (git-commit-summary-regexp) nil t)
      (or (string-match-p ".+" (or (match-string 2) ""))
          (string-match-p "^.+$" (or (match-string 3) ""))))))

;;; Font-Lock

(defun git-commit-mode-summary-font-lock-keywords (&optional errors)
  "Create font lock keywords to fontify the Git summary.

If ERRORS is non-nil create keywords that highlight errors in the
summary line, not the summary line itself."
  (if errors
      `(,(git-commit-summary-regexp)
        (2 'git-commit-overlong-summary-face t t)
        (3 'git-commit-nonempty-second-line-face t t))
    `(,(git-commit-summary-regexp)
      (1 'git-commit-summary-face t))))

(defun git-commit-mode-heading-keywords ()
  "Create font lock keywords to fontify comment headings.

Known comment headings are provided by `git-commit-comment-headings'."
  (mapcar (lambda (cell) `(,(format "^\\s<\\s-+\\(%s\\)$"
                                    (regexp-quote (car cell)))
                           (1 ',(cdr cell) t)))
          git-commit-comment-headings-alist))

(defun git-commit-mode-font-lock-keywords ()
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

(defun git-commit-propertize-diff ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^diff --git" nil t)
      (let ((buffer (current-buffer)))
        (insert
         (with-temp-buffer
           (insert
            (with-current-buffer buffer
              (prog1 (buffer-substring-no-properties (point) (point-max))
                (delete-region (point) (point-max)))))
           (diff-mode)
           (let (font-lock-verbose font-lock-support-mode)
             (font-lock-fontify-buffer))
           (let (next (pos (point-min)))
             (while (setq next (next-single-property-change pos 'face))
               (put-text-property pos next 'font-lock-face
                                  (get-text-property pos 'face))
               (setq pos next)))
           (buffer-string)))))))

(provide 'git-commit-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; git-commit-mode.el ends here
