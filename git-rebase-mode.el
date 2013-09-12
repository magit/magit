;;; git-rebase-mode.el --- Major mode for editing git rebase files

;; Copyright (C) 2010  Phil Jackson
;; Copyright (C) 2011  Peter J Weisberg

;; Author: Phil Jackson <phil@shellarchive.co.uk>
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

;; Allows the editing of a git rebase file (which you might get when
;; using 'git rebase -i' or hitting 'E' in Magit). Assumes editing is
;; happening in a server.

;;; Code:

(require 'easymenu)
(require 'rx)
(require 'server)
(require 'thingatpt)

;;; Options

(defgroup git-rebase nil
  "Customize Git-Rebase mode"
  :group 'tools)

(defcustom git-rebase-auto-advance nil
  "If non-nil, moves point forward a line after running an action."
  :group 'git-rebase
  :type 'boolean)

(defgroup git-rebase-faces nil
  "Customize Git-Rebase mode faces."
  :group 'faces
  :group 'git-rebase)

(defface git-rebase-killed-action-face
  '((((class color))
     :inherit font-lock-comment-face
     :strike-through t))
  "Action lines in the rebase TODO list that have been commented out."
  :group 'git-rebase-faces)

(defface git-rebase-description-face
  '((t :inherit font-lock-comment-face))
  "Face for one-line commit descriptions."
  :group 'git-rebase-faces)

;;; Regexps

(defconst git-rebase-action-line-re
  (rx
   line-start
   (? "#")
   (group
    (|
     (any "presf")
     "pick"
     "reword"
     "edit"
     "squash"
     "fixup"))
   (char space)
   (group
    (** 4 40 hex-digit)) ;sha1
   (char space)
   (group
    (* not-newline)))
  "Regexp that matches an action line in a rebase buffer.")

(defconst git-rebase-exec-line-re
  (rx
   line-start
   (? "#")
   (group
    (| "x"
       "exec"))
   (char space)
   (group
    (* not-newline)))
  "Regexp that matches an exec line in a rebase buffer.")

(defconst git-rebase-dead-line-re
  (rx-to-string `(and line-start
                      (char ?#)
                      (or (regexp ,(substring git-rebase-action-line-re 1))
                          (regexp ,(substring git-rebase-exec-line-re 1)))) t)
  "Regexp that matches a commented-out exec or action line in a rebase buffer.")

;;; Keymaps

(defvar git-rebase-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q")       'git-rebase-server-edit)
    (define-key map (kbd "C-c C-c") 'git-rebase-server-edit)
    (define-key map (kbd "a")       'git-rebase-abort)
    (define-key map (kbd "C-c C-k") 'git-rebase-abort)
    (define-key map [remap undo]    'git-rebase-undo)
    (define-key map (kbd "RET") 'git-rebase-show-commit)
    (define-key map (kbd "x")   'git-rebase-exec)
    (define-key map (kbd "c")   'git-rebase-pick)
    (define-key map (kbd "r")   'git-rebase-reword)
    (define-key map (kbd "e")   'git-rebase-edit)
    (define-key map (kbd "s")   'git-rebase-squash)
    (define-key map (kbd "f")   'git-rebase-fixup)
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
    ["Move Down" git-rebase-move-line-down t]
    ["Move Up" git-rebase-move-line-up t]
    ["Execute" git-rebase-exec t]
    "---"
    ["Abort" git-rebase-abort t]
    ["Done" git-rebase-server-edit t]))

;;; Utilities

(defun git-rebase-edit-line (change-to)
  "Change the keyword at the start of the current action line to
that of CHANGE-TO."
  (when (git-rebase-looking-at-action)
    (let ((buffer-read-only nil)
          (start (point)))
      (goto-char (point-at-bol))
      (delete-region (point) (progn (forward-word 1) (point)))
      (insert change-to)
      (goto-char start)
      (when git-rebase-auto-advance
        (forward-line)))))

(defmacro git-rebase-define-action (sym)
  (declare (indent defun))
  (let ((fn (intern (format "git-rebase-%s" sym))))
    `(progn
       (defun ,fn ()
	 (interactive)
	 (git-rebase-edit-line ,(symbol-name sym)))
       (put ',fn 'definition-name ',sym))))

(defun git-rebase-looking-at-action ()
  "Return non-nil if looking at an action line."
  (save-excursion
    (goto-char (point-at-bol))
    (looking-at git-rebase-action-line-re)))

(defun git-rebase-looking-at-action-or-exec ()
  "Return non-nil if looking at an action line or exec line."
  (save-excursion
    (goto-char (point-at-bol))
    (or (looking-at git-rebase-action-line-re)
        (looking-at git-rebase-exec-line-re))))

(defun git-rebase-looking-at-exec ()
  "Return non-nil if cursor is on an exec line."
  (string-match git-rebase-exec-line-re (thing-at-point 'line)))

(defun git-rebase-looking-at-killed-exec ()
  "Return non-nil if looking at an exec line that has been commented out."
  (let ((line (thing-at-point 'line)))
    (and (eq (aref line 0) ?#)
         (string-match git-rebase-exec-line-re line))))

;;; Commands

(git-rebase-define-action pick)
(git-rebase-define-action reword)
(git-rebase-define-action edit)
(git-rebase-define-action squash)
(git-rebase-define-action fixup)

(defun git-rebase-move-line-up ()
  "Move the current action line up."
  (interactive)
  (when (git-rebase-looking-at-action-or-exec)
    (let ((buffer-read-only nil)
          (col (current-column)))
      (goto-char (point-at-bol))
      (unless (bobp)
        (transpose-lines 1)
        (forward-line -2))
      (move-to-column col))))

(defun git-rebase-move-line-down ()
  "Assuming the next line is also an action line, move the current line down."
  (interactive)
  ;; if we're on an action and the next line is also an action
  (when (and (git-rebase-looking-at-action-or-exec)
             (save-excursion
               (forward-line)
               (git-rebase-looking-at-action-or-exec)))
    (let ((buffer-read-only nil)
          (col (current-column)))
      (forward-line 1)
      (transpose-lines 1)
      (forward-line -1)
      (move-to-column col))))

(defun git-rebase-server-edit ()
  "Save the action buffer and end the session."
  (interactive)
  (save-buffer)
  (server-edit))

(defun git-rebase-abort ()
  "Abort this rebase.
This is dune by emptying the buffer, saving and closing server
connection."
  (interactive)
  (when (or (not (buffer-modified-p))
            (y-or-n-p "Abort this rebase? "))
    (let ((buffer-read-only nil))
      (erase-buffer)
      (save-buffer)
      (server-edit))))

(defun git-rebase-kill-line ()
  "Kill the current action line."
  (interactive)
  (when (and (not (eq (char-after (point-at-bol)) ?#))
             (git-rebase-looking-at-action-or-exec))
    (beginning-of-line)
    (let ((buffer-read-only nil))
      (insert "#"))
    (forward-line)))

(defun git-rebase-exec (edit)
  "Prompt the user for a shell command to be executed, and
add it to the todo list.

If the cursor is on a commented-out exec line, uncomment the
current line instead of prompting.

When the prefix argument EDIT is non-nil and the cursor is on an
exec line, edit that line instead of inserting a new one.  If the
exec line was commented out, also uncomment it."
  (interactive "P")
  (cond
   ((and edit (git-rebase-looking-at-exec))
    (let ((new-line (git-rebase-read-exec-line
                     (match-string-no-properties 2 (thing-at-point 'line))))
          (inhibit-read-only t))
      (delete-region (point-at-bol) (point-at-eol))
      (if (not (equal "" new-line))
          (insert "exec " new-line)
        (delete-char -1)
        (forward-line))
      (move-beginning-of-line nil)))
   ((git-rebase-looking-at-killed-exec)
    (save-excursion
      (beginning-of-line)
      (let ((buffer-read-only nil))
        (delete-char 1))))
   (t
    (let ((inhibit-read-only t)
          (line (git-rebase-read-exec-line)))
      (unless (equal "" line)
        (move-end-of-line nil)
        (newline)
        (insert (concat "exec " line))))
    (move-beginning-of-line nil))))

(defun git-rebase-read-exec-line (&optional initial-line)
  (read-shell-command "Execute: " initial-line))

(defun git-rebase-undo (&optional arg)
  "A thin wrapper around `undo', which allows undoing in read-only buffers."
  (interactive "P")
  (let ((inhibit-read-only t))
    (undo arg)))

(defun git-rebase-show-commit (&optional arg)
  "Show the commit on the current line if any."
  (interactive "P")
  (save-excursion
    (goto-char (point-at-bol))
    (when (looking-at git-rebase-action-line-re)
      (let ((commit (match-string 2)))
        (if (fboundp 'magit-show-commit)
            (let ((default-directory (expand-file-name "../../")))
              (magit-show-commit commit nil nil 'select))
          (shell-command (concat "git show " commit)))))))

(defun git-rebase-backward-line (&optional n)
  "Move N lines backward (forward if N is negative).
Like `forward-line' but go into the opposite direction."
  (interactive "p")
  (forward-line (* n -1)))

;;; Mode

;;;###autoload
(define-derived-mode git-rebase-mode special-mode "Git Rebase"
  "Major mode for editing of a Git rebase file.

Rebase files are generated when you run 'git rebase -i' or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running 'man git-rebase' at the command line) for details."
  (setq font-lock-defaults '(git-rebase-mode-font-lock-keywords t t)))

(defvar git-rebase-mode-font-lock-keywords
  (list
   (list git-rebase-action-line-re
         '(1 font-lock-keyword-face)
         '(2 font-lock-builtin-face)
         '(3 'git-rebase-description-face))
   (list git-rebase-exec-line-re
         '(1 font-lock-keyword-face))
   (list (rx line-start (char "#") (* not-newline)) 0 font-lock-comment-face)
   (list git-rebase-dead-line-re 0 ''git-rebase-killed-action-face t))
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
(add-to-list 'auto-mode-alist
             '("git-rebase-todo" . git-rebase-mode))

(provide 'git-rebase-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; git-rebase-mode.el ends here
