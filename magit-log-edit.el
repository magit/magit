;;; magit-log-edit.el --- major mode for editing Git commit messages

;; Copyright (C) 2008  Marius Vollmer
;; Copyright (C) 2010  Phil Jackson

;; Author: Marius Vollmer <marius.vollmer@nokia.com>

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

;; A major mode for editing Git commit messages.

;;; Code:

(require 'log-edit)
(require 'magit)

;;; Options

(defcustom magit-log-edit-confirm-cancellation nil
  "Require acknowledgment before canceling the log edit buffer."
  :group 'magit
  :type 'boolean)

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

;;; Keymaps

(defvar magit-log-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x #")   'magit-log-edit-commit)
    (define-key map (kbd "C-c C-c") 'magit-log-edit-commit)
    (define-key map (kbd "C-c C-k") 'magit-log-edit-cancel-log-message)
    (define-key map (kbd "C-c C-]") 'magit-log-edit-cancel-log-message)
    (define-key map (kbd "C-x C-s") 'magit-log-edit-nop)
    (define-key map (kbd "C-c C-a") 'magit-log-edit-toggle-amending)
    (define-key map (kbd "C-c C-s") 'magit-log-edit-toggle-signoff)
    (define-key map (kbd "C-c C-v") 'magit-log-edit-toggle-gpgsign)
    (define-key map (kbd "C-c C-n") 'magit-log-edit-toggle-no-verify)
    (define-key map (kbd "C-c C-t") 'magit-log-edit-toggle-author)
    (define-key map (kbd "C-c C-e") 'magit-log-edit-toggle-allow-empty)
    (define-key map (kbd "M-p")     'log-edit-previous-comment)
    (define-key map (kbd "M-n")     'log-edit-next-comment)
    map))

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

;;; Mode

(define-derived-mode magit-log-edit-mode text-mode "Magit Log Edit"
  ;; Recognize changelog-style paragraphs
  (setq-local paragraph-start (concat paragraph-start "\\|*\\|(")))

;;;; Variables

(defvar magit-log-edit-buffer-name "*magit-edit-log*"
  "Buffer name for composing commit messages.")

(defvar magit-log-edit-prev-window-configuration nil)

(defvar magit-log-edit-status-buffer nil
  "Track associated *magit* buffers.
Do not customize this (used in the `magit-log-edit-mode' implementation
to switch back to the *magit* buffer associated with a given commit
operation after commit).")

;;;; Menu

;;; Commands
;;;; Invocation

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
        (unless (string= author-email "")
          (magit-log-edit-set-field
           'author (format "%s <%s>%s"
                           (if (string= "" author-name)
                               author-email
                             author-name)
                           author-email
                           (if (string= "" author-date)
                               ""
                             (format ", %s" author-date))))))
      (magit-pop-to-log-edit "commit"))))

(defun magit-pop-to-log-edit (operation)
  (let ((dir default-directory)
        (magit-buf (current-buffer))
        (buf (get-buffer-create magit-log-edit-buffer-name)))
    (setq magit-log-edit-prev-window-configuration
          (current-window-configuration))
    (pop-to-buffer buf)
    (setq default-directory dir)
    (when (file-exists-p (magit-git-dir "MERGE_MSG"))
      (insert-file-contents (magit-git-dir "MERGE_MSG")))
    (magit-log-edit-mode)
    (setq-local magit-log-edit-status-buffer magit-buf)
    (message "Type C-c C-c to %s (C-c C-k to cancel)." operation)))

(defun magit-log-edit-append (str)
  (with-current-buffer (get-buffer-create magit-log-edit-buffer-name)
    (goto-char (point-max))
    (insert str "\n")))

;;;; Mode Actions

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
                (file-exists-p (magit-git-dir "MERGE_HEAD"))
                (and commit-all
                     (not (magit-everything-clean-p))))
      (error (concat "Refusing to create empty commit. "
                     "Maybe you want to amend (%s) or allow-empty (%s)?")
             (key-description (car (where-is-internal
                                    'magit-log-edit-toggle-amending)))
             (key-description (car (where-is-internal
                                    'magit-log-edit-toggle-allow-empty)))))

    (magit-log-edit-push-to-comment-ring (buffer-string))
    (magit-log-edit-setup-author-env author)
    (magit-log-edit-set-fields nil)
    (magit-log-edit-cleanup)
    (when (= (buffer-size) 0)
      (insert "(Empty description)\n"))
    (let ((env process-environment)
          (commit-buf (current-buffer)))
      (with-current-buffer (magit-find-status-buffer default-directory)
        (let ((process-environment env))
          (if tag-name
              (apply #'magit-run-git-with-input commit-buf "tag"
                     `(,@tag-options ,tag-name "-a" "-F" "-" ,tag-rev))
            (apply #'magit-run-git-async-with-input commit-buf "commit"
                   `(,@magit-custom-options "-F" "-"
                     ,@(and commit-all (not allow-empty) (list "--all"))
                     ,@(and amend       (list "--amend"))
                     ,@(and allow-empty (list "--allow-empty"))
                     ,@(and sign-off    (list "--signoff"))
                     ,@(and gpg-sign    (list "-S"))
                     ,@(and no-verify   (list "--no-verify"))))))))
    ;; shouldn't we kill that buffer altogether?
    (erase-buffer)
    ;; potentially the local environment has been altered with settings that
    ;; were specific to this commit. Let's revert it
    (kill-local-variable 'process-environment)
    (let ((magit-buf magit-log-edit-status-buffer))
      (bury-buffer)
      (set-buffer magit-buf))
    (when (file-exists-p (magit-git-dir "MERGE_MSG"))
      (delete-file (magit-git-dir "MERGE_MSG")))
    (when magit-log-edit-prev-window-configuration
      (set-window-configuration magit-log-edit-prev-window-configuration)
      (setq magit-log-edit-prev-window-configuration nil))))

(defun magit-log-edit-cleanup ()
  (save-excursion
    (goto-char (point-min))
    (goto-char (point-min))
    (when (re-search-forward "[ \t\n]*\\'" nil t)
      (replace-match "\n" nil nil))))

(defun magit-log-edit-cancel-log-message ()
  "Abort edits and erase commit message being composed."
  (interactive)
  (when (or (not magit-log-edit-confirm-cancellation)
            (yes-or-no-p
             "Really cancel editing the log (any changes will be lost)?"))
    (erase-buffer)
    (bury-buffer)
    (when magit-log-edit-prev-window-configuration
      (set-window-configuration magit-log-edit-prev-window-configuration)
      (setq magit-log-edit-prev-window-configuration nil))))

(defun magit-log-edit-nop ()
  "Tell user nothing was saved and how to actually do it."
  (interactive)
  (message "Not saved. Use C-c C-c to finalize this commit message."))

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
  (magit-log-edit-toggle-input
   'author (format "%s <%s>"
                   (or (magit-get "user" "name") "Author Name")
                   (or (magit-get "user" "email") "author@email"))))

(defun magit-log-edit-toggle-allow-empty ()
  "Toggle whether this commit is allowed to be empty.
This means that the eventual commit does 'git commit --allow-empty'."
  (interactive)
  (magit-log-edit-toggle-field 'allow-empty t))

;;; Fields

(defconst magit-log-header-end "-- End of Magit header --\n")

(defun magit-log-edit-get-fields ()
  (let ((buf (get-buffer magit-log-edit-buffer-name))
        (result nil))
    (when buf
      (with-current-buffer buf
        (goto-char (point-min))
        (while (looking-at "^\\([A-Za-z0-9-_]+\\): *\\(.+\\)?$")
          (setq result (cons (cons (intern (downcase (match-string 1)))
                                   (read (or (match-string 2) "nil")))
                             result))
          (forward-line))
        (unless (looking-at (regexp-quote magit-log-header-end))
          (setq result nil))))
    (nreverse result)))

(defun magit-log-edit-set-fields (fields)
  (let ((buf (get-buffer-create magit-log-edit-buffer-name)))
    (with-current-buffer buf
      (goto-char (point-min))
      (when (search-forward-regexp
             (format "^\\([A-Za-z0-9-_]+:.*\n\\)*%s"
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
          (value
           (setq fields (append fields (list (cons name value))))))
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
      (setq yesp (and default 'first)))
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
        (setq fields (assq-delete-all name fields)
              result (cdr cell))
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
      (when date
        (setenv "GIT_AUTHOR_DATE" date)))))

(defun magit-log-edit-push-to-comment-ring (comment)
  (when (or (ring-empty-p log-edit-comment-ring)
            (not (equal comment (ring-ref log-edit-comment-ring 0))))
    (ring-insert log-edit-comment-ring comment)))

(provide 'magit-log-edit)
;;; magit-log-edit.el ends here
