;;; git-commit.el --- edit Git commit messages  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2014  The Magit Project Developers
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

;; Authors: Jonas Bernoulli <jonas@bernoul.li>
;;	Sebastian Wiesner <lunaryorn@gmail.com>
;;	Florian Ragwitz <rafl@debian.org>
;;	Marius Vollmer <marius.vollmer@gmail.com>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Homepage: https://github.com/magit/magit
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

(require 'dash)
(require 'log-edit)
(require 'ring)
(require 'server)
(require 'with-editor)

(eval-when-compile (require 'recentf))

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

(defcustom git-commit-setup-hook
  '(git-commit-save-message
    git-commit-setup-changelog-support
    git-commit-turn-on-auto-fill
    git-commit-propertize-diff
    with-editor-usage-message)
  "Hook run at the end of `git-commit-setup'."
  :group 'git-commit
  :type 'hook
  :options '(magit-revert-buffers
             git-commit-save-message
             git-commit-setup-changelog-support
             git-commit-turn-on-auto-fill
             git-commit-turn-on-flyspell
             git-commit-propertize-diff
             with-editor-usage-message))

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
  "A list of Git pseudo headers to be highlighted."
  :group 'git-commit
  :type '(repeat string))

;;;; Faces

(defgroup git-commit-faces nil
  "Faces for highlighting Git commit messages."
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

(eval-after-load 'recentf
  '(add-to-list 'recentf-exclude git-commit-filename-regexp))

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
  (make-local-variable 'log-edit-comment-ring-index)
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
  "Unconditionally turn on Flyspell mode.
Also prevent comments from being checked and
finally check current non-comment text."
  (require 'flyspell)
  (turn-on-flyspell)
  (setq flyspell-generic-check-word-predicate
        'git-commit-mode-flyspell-verify)
  (flyspell-buffer))

(defun git-commit-mode-flyspell-verify ()
  (not (memq (get-text-property (point) 'face)
             '(font-lock-comment-face     font-lock-comment-delimiter-face
               git-commit-comment-branch  git-commit-comment-detached
               git-commit-comment-heading git-commit-comment-file
               git-commit-comment-action  git-commit-pseudo-header
               git-commit-known-pseudo-header))))

(defun git-commit-finish-query-functions (force)
  (run-hook-with-args-until-failure
   'git-commit-finish-query-functions force))

(defun git-commit-check-style-conventions (force)
  (or force
      (save-excursion
        (goto-char (point-min))
        (re-search-forward (git-commit-summary-regexp) nil t)
        (if (equal (match-string 1) "")
            t ; Just try; we don't know whether --allow-empty-message was used.
          (and (or (equal (match-string 2) "")
                   (y-or-n-p "Summary line is too long.  Commit anyway? "))
               (or (equal (match-string 3) "")
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
           (unless (looking-back "\n\n")
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
   (format "\\(?:\n%s\\|\n\\(.*\\)\\)?" comment-start)))

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

;;; git-commit.el ends soon

(define-obsolete-face-alias 'git-commit-summary-face 'git-commit-summary "1.1.0")
(define-obsolete-face-alias 'git-commit-overlong-summary-face 'git-commit-overlong-summary "1.1.0")
(define-obsolete-face-alias 'git-commit-nonempty-second-line-face 'git-commit-nonempty-second-line "1.1.0")
(define-obsolete-face-alias 'git-commit-note-face 'git-commit-note "1.1.0")
(define-obsolete-face-alias 'git-commit-pseudo-header-face 'git-commit-pseudo-header "1.1.0")
(define-obsolete-face-alias 'git-commit-known-pseudo-header-face 'git-commit-known-pseudo-header "1.1.0")
(define-obsolete-face-alias 'git-commit-branch-face 'git-commit-comment-branch "1.1.0")
(define-obsolete-face-alias 'git-commit-no-branch-face 'git-commit-comment-detached "1.1.0")
(define-obsolete-face-alias 'git-commit-comment-heading-face 'git-commit-comment-heading "1.1.0")
(define-obsolete-face-alias 'git-commit-comment-file-face 'git-commit-comment-file "1.1.0")
(define-obsolete-face-alias 'git-commit-comment-action-face 'git-commit-comment-action "1.1.0")

(provide 'git-commit)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; git-commit.el ends here
