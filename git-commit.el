;;; git-commit.el --- Major mode for editing git commit messages

;; This software is Copyright (c) 2010 by Florian Ragwitz.
;;
;; This is free software, licensed under:
;;   The GNU General Public License, Version 2, June 1991

;; Author: Florian Ragwitz <rafl@debian.org>
;; Version: 0.1
;; Keywords: git

;;; Commentary:
;;

;;; Code:

(defgroup git-commit '((jit-lock custom-group))
  "Mode for editing git commit messages"
  :group 'faces)

(defgroup git-commit-faces nil
  "Faces for highlighting git commit messages"
  :prefix "git-commit-"
  :group 'git-commit)

(defface git-commit-summary-face
  '((default (:weight bold))
    (((class grayscale) (background light))
     (:foreground "DimGray" :slant italic))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :slant italic))
    (((class color) (min-colors 88) (background light))
     (:foreground "VioletRed4"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "LightSalmon"))
    (((class color) (min-colors 16) (background light))
     (:foreground "RosyBrown"))
    (((class color) (min-colors 16) (background dark))
     (:foreground "LightSalmon"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:slant italic)))
  "Face used to highlight the summary in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-overlong-summary-face
  '((((class color) (min-colors 88) (background light))
     (:foreground "Red1" :weight bold))
    (((class color) (min-colors 88) (background dark))
     (:foreground "Pink" :weight bold))
    (((class color) (min-colors 16) (background light))
     (:foreground "Red1" :weight bold))
    (((class color) (min-colors 16) (background dark))
     (:foreground "Pink" :weight bold))
    (((class color) (min-colors 8)) (:foreground "red"))
    (t (:inverse-video t :weight bold)))
  "Face used to highlight overlong parts of git commit message summaries"
  :group 'git-commit-faces)

(defface git-commit-nonempty-second-line-face
  '((((class color) (min-colors 88) (background light))
     (:foreground "Red1" :weight bold))
    (((class color) (min-colors 88) (background dark))
     (:foreground "Pink" :weight bold))
    (((class color) (min-colors 16) (background light))
     (:foreground "Red1" :weight bold))
    (((class color) (min-colors 16) (background dark))
     (:foreground "Pink" :weight bold))
    (((class color) (min-colors 8)) (:foreground "red"))
    (t (:inverse-video t :weight bold)))
  "Face used to highlight text on the second line of git commit messages"
  :group 'git-commit-faces)

(defface git-commit-text-face
  '((t (:inherit default)))
  "Face used to highlight text in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-comment-face
  '((((class grayscale) (background light))
     (:foreground "DimGray" :weight bold :slant italic))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :weight bold :slant italic))
    (((class color) (min-colors 88) (background light))
     (:foreground "Firebrick"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "chocolate1"))
    (((class color) (min-colors 16) (background light))
     (:foreground "red"))
    (((class color) (min-colors 16) (background dark))
     (:foreground "red1"))
    (((class color) (min-colors 8) (background light))
     (:foreground "red"))
    (((class color) (min-colors 8) (background dark)))
    (t (:weight bold :slant italic)))
  "Face used to highlight comments in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-pseudo-header-face
  '((((class grayscale) (background light))
     (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Cyan1"))
    (((class color) (min-colors 16) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 16) (background dark)) (:foreground "Cyan"))
    (((class color) (min-colors 8)) (:foreground "cyan" :weight bold))
    (t (:weight bold)))
  "Font used to hightlight pseudo headers in git commit messages"
  :group 'git-commit-faces)

(defcustom git-commit-known-pseudo-headers
  '("Signed-off-by"
    "Acked-by"
    "Cc"
    "Reported-by"
    "Tested-by"
    "Reviewed-by")
  "A list of git pseudo headers to be highlighted."
  :group 'git-commit
  :type '(repeat string))

(defface git-commit-known-pseudo-header-face
  '((((class grayscale) (background light)) (:foreground "Gray90" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light))
     (:foreground "ForestGreen"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "PaleGreen"))
    (((class color) (min-colors 16) (background light))
     (:foreground "ForestGreen"))
    (((class color) (min-colors 16) (background dark))
     (:foreground "PaleGreen"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:weight bold :underline t)))
  "Face used to hightlight common pseudo headers in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-note-brace-face
  '((((class grayscale) (background light))
     (:foreground "LightGray" :weight bold :underline t))
    (((class grayscale) (background dark))
     (:foreground "Gray50" :weight bold :underline t))
    (((class color) (min-colors 88) (background light))
     (:foreground "dark cyan"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "Aquamarine"))
    (((class color) (min-colors 16) (background light))
     (:foreground "CadetBlue"))
    (((class color) (min-colors 16) (background dark))
     (:foreground "Aquamarine"))
    (((class color) (min-colors 8)) (:foreground "magenta"))
    (t (:weight bold :underline t)))
  "Face used to highlight braces within notes in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-note-address-face
  '((((class grayscale) (background light))
     (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Cyan1"))
    (((class color) (min-colors 16) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 16) (background dark)) (:foreground "Cyan"))
    (((class color) (min-colors 8)) (:foreground "cyan" :weight bold))
    (t (:weight bold)))
  "Face used to highlight email addresses within notes in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-note-face
  '((((class grayscale) (background light))
     (:foreground "DimGray" :slant italic))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :slant italic))
    (((class color) (min-colors 88) (background light))
     (:foreground "VioletRed4"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "LightSalmon"))
    (((class color) (min-colors 16) (background light))
     (:foreground "RosyBrown"))
    (((class color) (min-colors 16) (background dark))
     (:foreground "LightSalmon"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:slant italic)))
  "Face used to highlight notes within git commit messages"
  :group 'git-commit-faces)

(defface git-commit-branch-face
  '((((class grayscale) (background light))
     (:foreground "DimGray" :slant italic))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :slant italic))
    (((class color) (min-colors 88) (background light))
     (:foreground "VioletRed2"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "LightSalmon"))
    (((class color) (min-colors 16) (background light))
     (:foreground "RosyBrown"))
    (((class color) (min-colors 16) (background dark))
     (:foreground "LightSalmon"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:slant italic)))
  "Face used to highlight the branch name in comments in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-no-branch-face
  '((t :inherit git-commit-branch-face))
  "Face used when a commit is going to be made outside of any branches"
  :group 'git-commit-faces)

(defface git-commit-comment-heading-face
  '((t (:inherit git-commit-known-pseudo-header-face)))
  "Face used to highlight section headings in the default
comments in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-comment-file-face
  '((t (:inherit git-commit-pseudo-header-face)))
  "Face used to highlight file names in the default comments in
git commit messages"
  :group 'git-commit-faces)

(defface git-commit-comment-action-face
  '((t (:inherit git-commit-branch-face)))
  "Face used to highlight what has happened to files in the
default comments in git commit messages"
  :group 'git-commit-faces)

(defconst git-commit-font-lock-keywords-1
  (append
   '(("^\\(#\s+On branch \\)\\(.*\\)$"
      (1 'git-commit-comment-face)
      (2 'git-commit-branch-face)))
   (loop for exp in
         '(("Not currently on any branch." . git-commit-no-branch-face)
           ("Changes to be committed:"     . git-commit-comment-heading-face)
           ("Untracked files:"             . git-commit-comment-heading-face)
           ("Changed but not updated:"     . git-commit-comment-heading-face)
           ("Unmerged paths:"              . git-commit-comment-heading-face))
         collect `(,(concat "^\\(#\s+\\)\\(" (car exp) "\\)$")
                   (1 'git-commit-comment-face)
                   (2 ',(cdr exp))))
   `(("^\\(#\t\\)\\([^:]+\\)\\(:\s+\\)\\(.*\\)$"
      (1 'git-commit-comment-face)
      (2 'git-commit-comment-action-face)
      (3 'git-commit-comment-face)
      (4 'git-commit-comment-file-face))
     ("^\\(#\t\\)\\(.*\\)$"
      (1 'git-commit-comment-face)
      (2 'git-commit-comment-file-face))
     ("^#.*$"
      (0 'git-commit-comment-face))
     ("\\`\\(.\\{,50\\}\\)\\(.*?\\)\n\\(.*\\)$"
      (1 'git-commit-summary-face)
      (2 'git-commit-overlong-summary-face)
      (3 'git-commit-nonempty-second-line-face))
     (,(concat "^\\("
               (regexp-opt git-commit-known-pseudo-headers)
               ":\\)\\(\s.*\\)$")
      (1 'git-commit-known-pseudo-header-face)
      (2 'git-commit-pseudo-header-face))
     ("^\\w[^\s\n]+:\s.*$"
      (0 'git-commit-pseudo-header-face))
     ("^\\(\\[\\)\\([^\s@]+@[^\s@]+:\\)\\(.*\\)\\(\\]\\)$"
      (1 'git-commit-note-brace-face)
      (2 'git-commit-note-address-face)
      (3 'git-commit-note-face)
      (4 'git-commit-note-brace-face))
     (".*"
      (0 'git-commit-text-face)))))

(defvar git-commit-font-lock-keywords git-commit-font-lock-keywords-1)

(defvar git-commit-mode-hook nil
  "List of functions to be called when activating `git-commit-mode'.")

(defvar git-commit-commit-hook nil
  "List of functions to be called on `git-commit-commit'.")

(defun git-commit-commit ()
  "Finish editing the commit message and commit.
By default this only calls `save-buffer', as there is no general
way to actually trigger git to commit whatever the commit message
was intended for.

After calling `save-buffer', the hooks in
`git-commit-commit-hook' will be run.  If you have configured git
in a way that simply invokes Emacs for editing the commit
message, you might want to this:

  (add-hook 'git-commit-commit-hook
          (lambda () (save-buffers-kill-terminal)))"
  (interactive)
  (save-buffer)
  (run-hooks 'git-commit-commit-hook))

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
  (loop for var in vars
        do (let ((val (getenv var)))
             (when val (return val)))))

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
before any trailing comments git or the user might have inserted."
  (save-excursion
    (goto-char (point-max))
    (if (not (re-search-backward "^[^#][^\s:]+:.*$" nil t))
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
      (point))))

(defun git-commit-insert-header (type name email &optional note)
  "Insert a header into the commit message.
The inserted headers have the format 'TYPE: NAME <EMAIL>'.

If NOTE satisfies `stringp', an additional note of the format
'[EMAIL: NOTE]' is inserted after the header.

If NOTE is not nil and doesn't satisfy `stringp', the
surroundings of an additional note will be inserted, and the
point will be left where the content of the note needs to be
inserted.

The header is inserted at the position returned by
`git-commit-find-pseudo-header-position'.  When this position
isn't after an existing header or a newline, an extra newline is
inserted before the header."
  (let* ((header-at (git-commit-find-pseudo-header-position))
         (prev-line (save-excursion
                      (goto-char (- header-at 1))
                      (thing-at-point 'line)))
         (pre       (if (or (string-match "^[^\s:]+:.+$" prev-line)
                            (string-match "\\`\s*$" prev-line))
                        "" "\n"))
         (insert    (lambda ()
                      (goto-char header-at)
                      (insert (format "%s%s: %s <%s>\n" pre type name email))
                      (when note
                        (insert (format "[%s: %s]\n"
                                        email (if (stringp note) note "")))
                        (backward-char 2)))))
    (if (eq t note)
        (funcall insert)
      (save-excursion (funcall insert)))))

(defun git-commit-insert-header-as-self (type &optional note)
  "Insert a header with the name and email address of the current user.
Call `git-commit-insert-header' with the user name and email
address provided by `git-commit-committer-name' and
`git-commit-committer-email'.

TYPE and NOTE are passed along unmodified."
  (let ((committer-name (git-commit-committer-name))
        (committer-email (git-commit-committer-email)))
    (git-commit-insert-header type committer-name committer-email note)))

(defun git-commit-signoff (&optional note)
  "Insert a 'Signed-off-by' header at the end of the commit message.
If NOTE is given, an additional note will be inserted.

If NOTE satisfies `stringp', the value of NOTE will be inserted
as the content of the note.

If NOTE is not nil and doesn't satisfy `stringp', the
surroundings of an additional note will be inserted, and the
point will be left where the content of the note needs to be
inserted.

NOTE defaults to `current-prefix-arg'.

The author name and email address used for the header are
retrieved automatically with the same mechanism git uses."
  (interactive
   (list (when current-prefix-arg t)))
  (git-commit-insert-header-as-self "Signed-off-by" note))

(defun git-commit-ack (&optional note)
  "Insert an 'Acked-by' header at the end of the commit message.
If NOTE is given, an additional note will be inserted.

If NOTE satisfies `stringp', the value of NOTE will be inserted
as the content of the note.

If NOTE is not nil and doesn't satisfy `stringp', the
surroundings of an additional note will be inserted, and the
point will be left where the content of the note needs to be
inserted.

NOTE defaults to `current-prefix-arg'.

The author name and email address used for the header are
retrieved automatically with the same mechanism git uses."
  (interactive
   (list (when current-prefix-arg t)))
  (git-commit-insert-header-as-self "Acked-by" note))

(defun git-commit-test (&optional note)
  "Insert a 'Tested-by' header at the end of the commit message.
If NOTE is given, an additional note will be inserted.

If NOTE satisfies `stringp', the value of NOTE will be inserted
as the content of the note.

If NOTE is not nil and doesn't satisfy `stringp', the
surroundings of an additional note will be inserted, and the
point will be left where the content of the note needs to be
inserted.

NOTE defaults to `current-prefix-arg'.

The author name and email address used for the header are
retrieved automatically with the same mechanism git uses."
  (interactive
   (list (when current-prefix-arg t)))
  (git-commit-insert-header-as-self "Tested-by" note))

(defun git-commit-review (&optional note)
  "Insert a 'Reviewed-by' header at the end of the commit message.
If NOTE is given, an additional note will be inserted.

If NOTE satisfies `stringp', the value of NOTE will be inserted
as the content of the note.

If NOTE is not nil and doesn't satisfy `stringp', the
surroundings of an additional note will be inserted, and the
point will be left where the content of the note needs to be
inserted.

NOTE defaults to `current-prefix-arg'.

The author name and email address used for the header are
retrieved automatically with the same mechanism git uses."
  (interactive
   (list (when current-prefix-arg t)))
  (git-commit-insert-header-as-self "Reviewed-by" note))

(defun git-commit-cc (name email &optional note)
  "Insert a 'Cc' header at the end of the commit message.
The value of the header is determined by NAME and EMAIL.

When called interactively, both NAME and EMAIL are read from the
minibuffer.

If NOTE is given, an additional note will be inserted.

If NOTE satisfies `stringp', the value of NOTE will be inserted
as the content of the note.

If NOTE is not nil and doesn't satisfy `stringp', the
surroundings of an additional note will be inserted, and the
point will be left where the content of the note needs to be
inserted.

NOTE defaults to `current-prefix-arg'."
  (interactive
   (list (read-string "Name: ")
         (read-string "Email: ")
         (when current-prefix-arg t)))
  (git-commit-insert-header "Cc" name email note))

(defun git-commit-reported (name email &optional note)
  "Insert a 'Reported-by' header at the end of the commit message.
The value of the header is determined by NAME and EMAIL.

When called interactively, both NAME and EMAIL are read from the
minibuffer.

If NOTE is given, an additional note will be inserted.

If NOTE satisfies `stringp', the value of NOTE will be inserted
as the content of the note.

If NOTE is not nil and doesn't satisfy `stringp', the
surroundings of an additional note will be inserted, and the
point will be left where the content of the note needs to be
inserted.

NOTE defaults to `current-prefix-arg'."
  (interactive
   (list (read-string "Name: ")
         (read-string "Email: ")
         (when current-prefix-arg t)))
  (git-commit-insert-header "Reported-by" name email note))

(defvar git-commit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'git-commit-commit)
    (define-key map (kbd "C-c C-s") 'git-commit-signoff)
    (define-key map (kbd "C-c C-a") 'git-commit-ack)
    (define-key map (kbd "C-c C-t") 'git-commit-test)
    (define-key map (kbd "C-c C-r") 'git-commit-review)
    (define-key map (kbd "C-c C-o") 'git-commit-cc)
    (define-key map (kbd "C-c C-p") 'git-commit-reported)
    map))

;;;###autoload
(defun git-commit-mode ()
  "Major mode for editing git commit messages.
This mode helps with editing git commit messages both by
providing commands to do common tasks, and by highlighting the
basic structure of and errors in git commit messages.

Commands:\\<git-commit-map>
\\[git-commit-commit]   git-commit-commit  Finish editing and commit
\\[git-commit-signoff]   git-commit-signoff   Insert a Signed-off-by header
\\[git-commit-ack]   git-commit-ack   Insert an Acked-by header
\\[git-commit-test]   git-commit-test   Insert a Tested-by header
\\[git-commit-review]   git-commit-review   Insert a Reviewed-by header
\\[git-commit-cc]   git-commit-cc   Insert a Cc header
\\[git-commit-reported]   git-commit-reported   Insert a Reported-by header

Turning on git commit calls the hooks in `git-commit-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map git-commit-map)
  (setq font-lock-multiline t)
  (setq font-lock-defaults '(git-commit-font-lock-keywords t))
  (setq major-mode 'git-commit)
  (run-hooks 'git-commit-mode-hook)
  (setq mode-name "Git-Commit"))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("COMMIT_EDITMSG" . git-commit-mode))

(provide 'git-commit)

;;; git-commit.el ends here
