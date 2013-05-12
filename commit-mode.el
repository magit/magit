;;; commit-mode --- edit git commit messages

;; Copyright (C) 2010 Florian Ragwitz
;; Copyright (C) 2013 Ramkumar Ramachandra
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
;; Allows the editing of a git commit file (which you might get when
;; using 'git commit' or hitting 'c' in Magit). Assumes editing is
;; happening in a server.

;;; Code:

(require 'cl-lib)
(require 'magit)

(defgroup git-commit '((jit-lock custom-group))
  "Mode for editing git commit messages"
  :group 'faces)

(defgroup commit-mode-faces nil
  "Faces for highlighting git commit messages"
  :prefix "commit-mode-"
  :group 'git-commit)

(defface commit-mode-summary-face
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
  :group 'commit-mode-faces)

(defface commit-mode-overlong-summary-face
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
  :group 'commit-mode-faces)

(defface commit-mode-nonempty-second-line-face
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
  :group 'commit-mode-faces)

(defface commit-mode-text-face
  '((t (:inherit default)))
  "Face used to highlight text in git commit messages"
  :group 'commit-mode-faces)

(defface commit-mode-comment-face
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
  :group 'commit-mode-faces)

(defface commit-mode-magit-header-face
  '((t :inherit font-lock-preprocessor-face))
  "Face used to highlight the Magit header."
  :group 'commit-mode-faces)

(defface commit-mode-pseudo-header-face
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
  :group 'commit-mode-faces)

(defcustom commit-mode-known-pseudo-headers
  '("Signed-off-by"
    "Acked-by"
    "Cc"
    "Reported-by"
    "Tested-by"
    "Reviewed-by")
  "A list of git pseudo headers to be highlighted."
  :group 'git-commit
  :type '(repeat string))

(defface commit-mode-known-pseudo-header-face
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
  :group 'commit-mode-faces)

(defface commit-mode-note-brace-face
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
  :group 'commit-mode-faces)

(defface commit-mode-note-address-face
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
  :group 'commit-mode-faces)

(defface commit-mode-note-face
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
  :group 'commit-mode-faces)

(defface commit-mode-branch-face
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
  :group 'commit-mode-faces)

(defface commit-mode-no-branch-face
  '((t :inherit commit-mode-branch-face))
  "Face used when a commit is going to be made outside of any branches"
  :group 'commit-mode-faces)

(defface commit-mode-comment-heading-face
  '((t (:inherit commit-mode-known-pseudo-header-face)))
  "Face used to highlight section headings in the default
comments in git commit messages"
  :group 'commit-mode-faces)

(defface commit-mode-comment-file-face
  '((t (:inherit commit-mode-pseudo-header-face)))
  "Face used to highlight file names in the default comments in
git commit messages"
  :group 'commit-mode-faces)

(defface commit-mode-comment-action-face
  '((t (:inherit commit-mode-branch-face)))
  "Face used to highlight what has happened to files in the
default comments in git commit messages"
  :group 'commit-mode-faces)

(defconst commit-mode-font-lock-keywords
  (append
   '(("^\\(#\s+On branch \\)\\(.*\\)$"
      (1 'commit-mode-comment-face)
      (2 'commit-mode-branch-face)))
   (cl-loop for exp in
         '(("Not currently on any branch." . commit-mode-no-branch-face)
           ("Changes to be committed:"     . commit-mode-comment-heading-face)
           ("Untracked files:"             . commit-mode-comment-heading-face)
           ("Changed but not updated:"     . commit-mode-comment-heading-face)
           ("Unmerged paths:"              . commit-mode-comment-heading-face))
         collect `(,(concat "^\\(#\s+\\)\\(" (car exp) "\\)$")
                   (1 'commit-mode-comment-face)
                   (2 ',(cdr exp))))
   `(("^\\(#\t\\)\\([^:]+\\)\\(:\s+\\)\\(.*\\)$"
      (1 'commit-mode-comment-face)
      (2 'commit-mode-comment-action-face)
      (3 'commit-mode-comment-face)
      (4 'commit-mode-comment-file-face))
     ("^\\(#\t\\)\\(.*\\)$"
      (1 'commit-mode-comment-face)
      (2 'commit-mode-comment-file-face))
     ("^#.*$"
      (0 'commit-mode-comment-face))
     (,magit-log-header-re
      (0 'commit-mode-magit-header-face))
     (,(concat (regexp-quote magit-log-header-end) "\\(?:\\(?:[[:space:]]*\\|#.*\\)\n\\)*\\(.\\{,50\\}\\)\\(.*?\\)\\(?:\n\\(.*\\)\\)?$")
      (1 'commit-mode-summary-face)
      (2 'commit-mode-overlong-summary-face)
      (3 'commit-mode-nonempty-second-line-face))
     ("\\`\\(?:\\(?:[[:space:]]*\\|#.*\\)\n\\)*\\(.\\{,50\\}\\)\\(.*?\\)\\(?:\n\\(.*\\)\\)?$"
      (1 'commit-mode-summary-face)
      (2 'commit-mode-overlong-summary-face)
      (3 'commit-mode-nonempty-second-line-face))
     (,(concat "^\\("
               (regexp-opt commit-mode-known-pseudo-headers)
               ":\\)\\(\s.*\\)$")
      (1 'commit-mode-known-pseudo-header-face)
      (2 'commit-mode-pseudo-header-face))
     ("^\\w[^\s\n]+:\s.*$"
      (0 'commit-mode-pseudo-header-face))
     ("^\\(\\[\\)\\([^\s@]+@[^\s@]+:\\)\\(.*\\)\\(\\]\\)$"
      (1 'commit-mode-note-brace-face)
      (2 'commit-mode-note-address-face)
      (3 'commit-mode-note-face)
      (4 'commit-mode-note-brace-face))
     (".*"
      (0 'commit-mode-text-face)))))

(defun commit-mode-git-config-var (key)
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

(defun commit-mode-first-env-var (&rest vars)
  "Get the value of the first defined environment variable.
Walk VARS, call `getenv' on each element and return the first
non-nil return value of `getenv'."
  (cl-loop for var in vars
        do (let ((val (getenv var)))
             (when val (return val)))))

(defun commit-mode-committer-name ()
  "Get the git committer name of the current user.
This uses the same mechanism git itself uses.  That is, using the
value of the 'GIT_AUTHOR_NAME' or 'GIT_COMMITTER_NAME'
environment variables, or the 'user.name' git configuration
variable.

If the above mechanism fails, the value of the variable
`user-full-name' is used."
  (or
   (commit-mode-first-env-var "GIT_AUTHOR_NAME" "GIT_COMMITTER_NAME")
   (commit-mode-git-config-var "user.name")
   user-full-name))

(defun commit-mode-committer-email ()
  "Get the git committer email address of the current user.
This uses the same mechanism git itself uses.  That is, using the
value of the 'GIT_AUTHOR_EMAIL', 'GIT_COMMITTER_EMAIL', or
'EMAIL' environment variables, or the 'user.email' git
configuration variable.

If the above mechanism fails, the value of the variable
`user-email-address' is used."
  (or
   (commit-mode-first-env-var "GIT_AUTHOR_EMAIL" "GIT_COMMITTER_EMAIL" "EMAIL")
   (commit-mode-git-config-var "user.email")
   user-mail-address))

(defun commit-mode-find-pseudo-header-position ()
  "Find the position at which commit pseudo headers should be inserted.
Those headers usually live at the end of a commit message, but
before any trailing comments git or the user might have inserted."
  (save-excursion
    ;; skip the summary line, limit the search to comment region
    (goto-char (point-min))
    (forward-line 2)
    (let ((comment-start (point)))
      (goto-char (point-max))
      (if (not (re-search-backward "^[^#][^\s:]+:.*$" comment-start t))
          ;; no headers yet, so we'll search backwards for a good place
          ;; to insert them
          (if (not (re-search-backward "^[^#].*?.*$" comment-start t))
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

(defun commit-mode-insert-header (type name email &optional note)
  "Insert a header into the commit message.
The inserted headers have the format 'TYPE: NAME <EMAIL>'.

If NOTE satisfies `stringp', an additional note of the format
'[EMAIL: NOTE]' is inserted after the header.

If NOTE is not nil and doesn't satisfy `stringp', the
surroundings of an additional note will be inserted, and the
point will be left where the content of the note needs to be
inserted.

The header is inserted at the position returned by
`commit-mode-find-pseudo-header-position'.  When this position
isn't after an existing header or a newline, an extra newline is
inserted before the header."
  (let* ((header-at (commit-mode-find-pseudo-header-position))
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

(defun commit-mode-insert-header-as-self (type &optional note)
  "Insert a header with the name and email address of the current user.
Call `commit-mode-insert-header' with the user name and email
address provided by `commit-mode-committer-name' and
`commit-mode-committer-email'.

TYPE and NOTE are passed along unmodified."
  (let ((committer-name (commit-mode-committer-name))
        (committer-email (commit-mode-committer-email)))
    (commit-mode-insert-header type committer-name committer-email note)))

(defmacro commit-mode-define-header (action header)
  "Create function commit-mode-ACTION.
ACTION will be part of the function name.
HEADER is the actual header to be inserted into the comment."
  (let ((func-name (intern (concat "commit-mode-" action))))
    `(defun ,func-name (&optional note)
       ,(format "Insert a '%s' header at the end of the commit message.
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
                header)
       (interactive
        (list (when current-prefix-arg t)))
       (commit-mode-insert-header-as-self ,header note))))

(commit-mode-define-header "signoff"  "Signed-off-by")
(commit-mode-define-header "ack"      "Acked-by")
(commit-mode-define-header "review"   "Reviewed-by")
(commit-mode-define-header "test"     "Tested-by")
(commit-mode-define-header "cc"       "Cc")
(commit-mode-define-header "reported" "Reported-by")

(defvar commit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'server-edit)
    (define-key map (kbd "C-c C-s") 'commit-mode-signoff)
    (define-key map (kbd "C-c C-a") 'commit-mode-ack)
    (define-key map (kbd "C-c C-r") 'commit-mode-review)
    (define-key map (kbd "C-c C-t") 'commit-mode-test)
    (define-key map (kbd "C-c C-o") 'commit-mode-cc)
    (define-key map (kbd "C-c C-p") 'commit-mode-reported)
    map))

(defun commit-mode-font-lock-diff ()
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
(define-derived-mode commit-mode text-mode "Commit"
  "Major mode for editing git commit messages.
This mode helps with editing git commit messages both by
providing commands to do common tasks, and by highlighting the
basic structure of and errors in git commit messages.

Commands:\\<commit-mode-map>
\\[commit-mode-commit]   `commit-mode-commit'  Finish editing and commit
\\[commit-mode-signoff]   `commit-mode-signoff'   Insert a Signed-off-by header
\\[commit-mode-ack]   `commit-mode-ack'   Insert an Acked-by header
\\[commit-mode-test]   `commit-mode-test'   Insert a Tested-by header
\\[commit-mode-review]   `commit-mode-review'   Insert a Reviewed-by header
\\[commit-mode-cc]   `commit-mode-cc'   Insert a Cc header
\\[commit-mode-reported]   `commit-mode-reported'   Insert a Reported-by header
"
  (setq font-lock-multiline t)
  (setq font-lock-defaults '(commit-mode-font-lock-keywords t))
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (setq comment-start-skip "^#\s"
        comment-start "# "
        comment-end "")
  (commit-mode-font-lock-diff))

;;;###autoload
;; override the existing defition in magit.el where
;; magit-log-edit-mode is derived from text-mode
(define-derived-mode magit-log-edit-mode commit-mode "Magit Log Edit")

;;;###autoload
(setq auto-mode-alist
      (append auto-mode-alist
              '(("COMMIT_EDITMSG" . commit-mode)
                ("NOTES_EDITMSG" . commit-mode)
                ("MERGE_MSG" . commit-mode)
                ("TAG_EDITMSG" . commit-mode))))

(provide 'commit-mode)

;;; commit-mode.el ends here
