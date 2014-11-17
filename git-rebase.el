;;; git-rebase.el --- Major mode for editing git rebase files

;; Copyright (C) 2010-2014  The Magit Project Developers
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

;; Author: Phil Jackson <phil@shellarchive.co.uk>
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

;; Allows the editing of a git rebase file (which you might get when
;; using 'git rebase -i' or hitting 'E' in Magit). Assumes editing is
;; happening in a server.

;;; Code:

(require 'dash)
(require 'easymenu)
(require 'server)
(require 'with-editor)

(eval-when-compile (require 'recentf))

;;; Options
;;;; Variables

(defgroup git-rebase nil
  "Edit Git rebase sequences."
  :group 'tools)

(defcustom git-rebase-auto-advance t
  "Whether to move to next line after changing a line."
  :group 'git-rebase
  :type 'boolean)

(defcustom git-rebase-remove-instructions nil
  "Whether to remove the instructions from the rebase buffer.
Because you have seen them before and can still remember."
  :group 'git-rebase
  :type 'boolean)

(defcustom git-rebase-confirm-cancel t
  "Whether confirmation is required to cancel."
  :group 'git-rebase
  :type 'boolean)

;;;; Faces

(defgroup git-rebase-faces nil
  "Faces used by Git-Rebase mode."
  :group 'faces
  :group 'git-rebase)

(defface git-rebase-hash
  '((((class color) (background light)) :foreground "grey60")
    (((class color) (background  dark)) :foreground "grey40"))
  "Face for commit hashes."
  :group 'git-rebase-faces)

(defface git-rebase-description nil
  "Face for commit descriptions."
  :group 'git-rebase-faces)

(defface git-rebase-killed-action
  '((t (:inherit font-lock-comment-face :strike-through t)))
  "Face for commented action and exec lines."
  :group 'git-rebase-faces)

(define-obsolete-face-alias 'git-rebase-description-face
  'git-rebase-description "1.0.0")
(define-obsolete-face-alias 'git-rebase-killed-action-face
  'git-rebase-killed-action "1.0.0")

;;; Keymaps

(defvar git-rebase-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map [remap undo] 'git-rebase-undo)
    (define-key map (kbd "RET") 'git-rebase-show-commit)
    (define-key map (kbd "x")   'git-rebase-exec)
    (define-key map (kbd "c")   'git-rebase-pick)
    (define-key map (kbd "r")   'git-rebase-reword)
    (define-key map (kbd "e")   'git-rebase-edit)
    (define-key map (kbd "s")   'git-rebase-squash)
    (define-key map (kbd "f")   'git-rebase-fixup)
    (define-key map (kbd "y")   'git-rebase-insert)
    (define-key map (kbd "k")   'git-rebase-kill-line)
    (define-key map (kbd "C-k") 'git-rebase-kill-line)
    (define-key map (kbd "p")   'git-rebase-backward-line)
    (define-key map (kbd "n")   'forward-line)
    (define-key map (kbd "M-p") 'git-rebase-move-line-up)
    (define-key map (kbd "M-n") 'git-rebase-move-line-down)
    (define-key map (kbd "M-<up>") 'git-rebase-move-line-up)
    (define-key map (kbd "M-<down>") 'git-rebase-move-line-down)
    map)
  "Keymap for Git-Rebase mode.")

(easy-menu-define git-rebase-mode-menu git-rebase-mode-map
  "Git-Rebase mode menu"
  '("Rebase"
    ["Pick" git-rebase-pick t]
    ["Reword" git-rebase-reword t]
    ["Edit" git-rebase-edit t]
    ["Squash" git-rebase-squash t]
    ["Fixup" git-rebase-fixup t]
    ["Kill" git-rebase-kill-line t]
    ["Execute" git-rebase-exec t]
    ["Move Down" git-rebase-move-line-down t]
    ["Move Up" git-rebase-move-line-up t]
    "---"
    ["Cancel" with-editor-cancel t]
    ["Finish" with-editor-finish t]))

;;; Commands

(defun git-rebase-pick ()
  "Use commit on current line."
  (interactive)
  (git-rebase-set-action "pick"))

(defun git-rebase-reword ()
  "Edit message of commit on current line."
  (interactive)
  (git-rebase-set-action "reword"))

(defun git-rebase-edit ()
  "Stop at the commit on the current line."
  (interactive)
  (git-rebase-set-action "edit"))

(defun git-rebase-squash ()
  "Meld commit on current line into previous commit, edit message."
  (interactive)
  (git-rebase-set-action "squash"))

(defun git-rebase-fixup ()
  "Meld commit on current line into previous commit, discard its message."
  (interactive)
  (git-rebase-set-action "fixup"))

(defconst git-rebase-line
  "^\\(#?\\(?:[fprse]\\|pick\\|reword\\|edit\\|squash\\|fixup\\|exec\\)\\) \
\\(?:\\([^ \n]+\\) \\(.*\\)\\)?")

(defun git-rebase-set-action (action)
  (goto-char (line-beginning-position))
  (if (and (looking-at git-rebase-line)
           (not (string-match-p "\\(e\\|exec\\)$" (match-string 1))))
      (let ((inhibit-read-only t))
        (replace-match action t t nil 1)
        (when git-rebase-auto-advance
          (forward-line)))
    (ding)))

(defun git-rebase-move-line-up ()
  "Move the current commit (or command) up."
  (interactive)
  (goto-char (line-beginning-position))
  (if (bobp)
      (ding)
    (when (looking-at git-rebase-line)
      (let ((inhibit-read-only t))
        (transpose-lines 1))
      (forward-line -2))))

(defun git-rebase-move-line-down ()
  "Move the current commit (or command) down."
  (interactive)
  (goto-char (line-beginning-position))
  (when (and (looking-at git-rebase-line)
             (save-excursion
               (forward-line)
               (looking-at git-rebase-line)))
    (forward-line)
    (let ((inhibit-read-only t))
      (transpose-lines 1))
    (forward-line -1)))

(defun git-rebase-kill-line ()
  "Kill the current action line."
  (interactive)
  (goto-char (line-beginning-position))
  (when (and (looking-at git-rebase-line)
             (not (eq (char-after) ?#)))
    (let ((inhibit-read-only t))
      (insert ?#))
    (when git-rebase-auto-advance
      (forward-line))))

(defun git-rebase-insert (rev)
  "Read an arbitrary commit and insert it below current line."
  (interactive
   (list (if (fboundp 'magit-read-branch-or-commit)
             (magit-read-branch-or-commit "Insert revision")
           (read-string "Insert revision: "))))
  (forward-line)
  (--if-let (if (fboundp 'magit-rev-format)
                (magit-rev-format "%h %s" rev)
              (process-lines "git" "show" "-s" "--format=%h %s" rev))
      (let ((inhibit-read-only t))
        (insert "pick " it ?\n))
    (user-error "Unknown revision")))

(defun git-rebase-exec (arg)
  "Insert a shell command to be run after the proceeding commit.

If there already is such a command on the current line, then edit
that instead.  With a prefix argument insert a new command even
when there already is one on the current line.  With empty input
remove the command on the current line, if any."
  (interactive "P")
  (let ((inhibit-read-only t) initial command)
    (unless arg
      (goto-char (line-beginning-position))
      (when (looking-at "^#?\\(e\\|exec\\) \\(.*\\)")
        (setq initial (match-string-no-properties 2))))
    (setq command (read-shell-command "Execute: " initial))
    (pcase (list command initial)
      (`("" nil) (ding))
      (`(""  ,_)
       (delete-region (match-beginning 0) (1+ (match-end 0))))
      (`(,_ nil)
       (forward-line)
       (insert (concat "exec " command "\n"))
       (unless git-rebase-auto-advance
         (forward-line -1)))
      (_
       (replace-match (concat "exec " command) t t)
       (if git-rebase-auto-advance
           (forward-line)
         (goto-char (line-beginning-position)))))))

(defun git-rebase-undo (&optional arg)
  "Undo some previous changes.
Like `undo' but works in read-only buffers."
  (interactive "P")
  (let ((inhibit-read-only t))
    (undo arg)))

(defun git-rebase-show-commit (&optional arg)
  "Show the commit on the current line if any."
  (interactive "P")
  (save-excursion
    (goto-char (line-beginning-position))
    (--if-let (and (looking-at git-rebase-line)
                   (match-string 2))
        (if (fboundp 'magit-show-commit)
            (magit-show-commit it)
          (shell-command (concat "git show " it)))
      (ding))))

(defun git-rebase-backward-line (&optional n)
  "Move N lines backward (forward if N is negative).
Like `forward-line' but go into the opposite direction."
  (interactive "p")
  (forward-line (- n)))

;;; Mode

;;;###autoload
(define-derived-mode git-rebase-mode special-mode "Git Rebase"
  "Major mode for editing of a Git rebase file.

Rebase files are generated when you run 'git rebase -i' or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running 'man git-rebase' at the command line) for details."
  (setq font-lock-defaults '(git-rebase-mode-font-lock-keywords t t))
  (when git-rebase-remove-instructions
    (let ((inhibit-read-only t))
      (flush-lines "^\\($\\|#\\)")))
  (with-editor-mode 1)
  (when git-rebase-confirm-cancel
    (add-hook 'with-editor-cancel-query-functions
              'git-rebase-cancel-confirm nil t)))

(defun git-rebase-cancel-confirm (force)
  (or (not (buffer-modified-p)) force (y-or-n-p "Abort this rebase? ")))

(defconst git-rebase-mode-font-lock-keywords
  `(("^\\([efprs]\\|pick\\|reword\\|edit\\|squash\\|fixup\\) \\([^ \n]+\\) \\(.*\\)"
     (1 'font-lock-keyword-face)
     (2 'git-rebase-hash)
     (3 'git-rebase-description))
    ("^\\(exec\\) \\(.*\\)"
     (1 'font-lock-keyword-face)
     (2 'git-rebase-description))
    ("^# .*"      0 'font-lock-comment-face)
    ("^#[^ \n].*" 0 'git-rebase-killed-action t))
  "Font lock keywords for Git-Rebase mode.")

(defun git-rebase-mode-show-keybindings ()
  "Modify the \"Commands:\" section of the comment Git generates
at the bottom of the file so that in place of the one-letter
abbreviation for the command, it shows the command's keybinding.
By default, this is the same except for the \"pick\" command."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "^#  \\(.\\), \\([[:alpha:]]+\\) = " nil t)
      (let ((start (match-beginning 1))
            (end (match-end 1))
            (command (intern (concat "git-rebase-" (match-string 2)))))
        (when (fboundp command)
          (let ((overlay (make-overlay start end)))
            (overlay-put
             overlay 'display
             (key-description (where-is-internal command nil t)))))))))

(add-hook 'git-rebase-mode-hook 'git-rebase-mode-show-keybindings t)

(defun git-rebase-mode-disable-before-save-hook ()
  (set (make-local-variable 'before-save-hook) nil))

(add-hook 'git-rebase-mode-hook 'git-rebase-mode-disable-before-save-hook)

;;;###autoload
(defconst git-rebase-filename-regexp "/git-rebase-todo\\'")
;;;###autoload
(add-to-list 'auto-mode-alist
             (cons git-rebase-filename-regexp 'git-rebase-mode))

(eval-after-load 'recentf
  '(add-to-list 'recentf-exclude git-rebase-filename-regexp))

(provide 'git-rebase)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; git-rebase.el ends here
