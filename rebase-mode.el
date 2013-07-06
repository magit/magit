;;; rebase-mode -- edit git rebase files

;; Copyright (C) 2010  Phil Jackson
;; Copyright (C) 2011  Peter J Weisberg

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

;; Allows the editing of a git rebase file (which you might get when
;; using 'git rebase -i' or hitting 'E' in Magit). Assumes editing is
;; happening in a server.

;;; Code:

(require 'easymenu)
(require 'rx)
(require 'server)
(declare-function server-edit "server") 

(defgroup rebase-mode nil
  "Customize Rebase Mode"
  :group 'tools)

(defcustom rebase-mode-auto-advance nil
  "If non-nil, moves point forward a line after running an action."
  :group 'rebase-mode
  :type 'boolean)

(defgroup rebase-mode-faces nil
  "Customize Rebase Mode faces."
  :group 'faces
  :group 'rebase-mode)

(defface rebase-mode-killed-action-face
  '((((class color))
     :inherit font-lock-comment-face
     :strike-through t))
  "Action lines in the rebase TODO list that have been commented out."
  :group 'rebase-mode-faces)

(defface rebase-mode-description-face
  '((t :inherit font-lock-comment-face))
  "Face for one-line commit descriptions."
  :group 'rebase-mode-faces)

(defconst rebase-mode-action-line-re
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

(defconst rebase-mode-exec-line-re
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

(defconst rebase-mode-dead-line-re
  (rx-to-string `(and line-start
                      (char ?#)
                      (or (regexp ,(substring rebase-mode-action-line-re 1))
                          (regexp ,(substring rebase-mode-exec-line-re 1)))) t)
  "Regexp that matches a commented-out exec or action line in a rebase buffer.")

(defvar rebase-mode-font-lock-keywords
  (list
   (list rebase-mode-action-line-re
         '(1 font-lock-keyword-face)
         '(2 font-lock-builtin-face)
         '(3 'rebase-mode-description-face))
   (list rebase-mode-exec-line-re
         '(1 font-lock-keyword-face))
   (list (rx line-start (char "#") (* not-newline)) 0 font-lock-comment-face)
   (list rebase-mode-dead-line-re 0 ''rebase-mode-killed-action-face t))
  "Font lock keywords for `rebase-mode'.")

(defvar key-to-action-map
  '(("c" . "pick")
    ("r" . "reword")
    ("e" . "edit")
    ("s" . "squash")
    ("f" . "fixup"))
  "Mapping from key to action.")

(defvar rebase-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'server-edit)
    (define-key map (kbd "C-c C-c") 'server-edit)

    (define-key map (kbd "a") 'rebase-mode-abort)
    (define-key map (kbd "C-c C-k") 'rebase-mode-abort)

    (define-key map (kbd "RET") 'rebase-mode-show-commit)

    (define-key map (kbd "M-p") 'rebase-mode-move-line-up)
    (define-key map (kbd "M-n") 'rebase-mode-move-line-down)
    (define-key map (kbd "k") 'rebase-mode-kill-line)
    (define-key map (kbd "x") 'rebase-mode-exec)

    (define-key map (kbd "n") 'forward-line)
    (define-key map (kbd "p") 'rebase-mode-backward-line)
    (define-key map [remap undo] 'rebase-mode-undo)
    map)
  "Keymap for rebase-mode.
Note this will be added to by the top-level code which defines
the edit functions.")

(easy-menu-define rebase-mode-menu rebase-mode-map
  "Rebase-mode menu"
  '("Rebase"
    ["Pick" rebase-mode-pick t]
    ["Reword" rebase-mode-reword t]
    ["Edit" rebase-mode-edit t]
    ["Squash" rebase-mode-squash t]
    ["Fixup" rebase-mode-fixup t]
    ["Kill" rebase-mode-kill-line t]
    ["Move Down" rebase-mode-move-line-down t]
    ["Move Up" rebase-mode-move-line-up t]
    ["Execute" rebase-mode-exec t]
    "---"
    ["Abort" rebase-mode-abort t]
    ["Done" server-edit t]))

;; create the functions which edit the action lines themselves (based
;; on `key-to-action-map' above)
(mapc (lambda (key-action)
        (let ((fun-name (intern (concat "rebase-mode-" (cdr key-action)))))
          ;; define the function
          (eval `(defun ,fun-name ()
                   (interactive)
                   (rebase-mode-edit-line ,(cdr key-action))))

          ;; bind the function in `rebase-mode-map'
          (define-key rebase-mode-map (car key-action) fun-name)))
      key-to-action-map)

(defun rebase-mode-edit-line (change-to)
  "Change the keyword at the start of the current action line to
that of CHANGE-TO."
  (when (rebase-mode-looking-at-action)
    (let ((buffer-read-only nil)
          (start (point)))
      (goto-char (point-at-bol))
      (delete-region (point) (progn (forward-word 1) (point)))
      (insert change-to)
      (goto-char start)
      (when rebase-mode-auto-advance
        (forward-line)))))

(defun rebase-mode-looking-at-action ()
  "Return non-nil if looking at an action line."
  (save-excursion
    (goto-char (point-at-bol))
    (looking-at rebase-mode-action-line-re)))

(defun rebase-mode-looking-at-action-or-exec ()
  "Return non-nil if looking at an action line or exec line."
  (save-excursion
    (goto-char (point-at-bol))
    (or (looking-at rebase-mode-action-line-re)
        (looking-at rebase-mode-exec-line-re))))

(defun rebase-mode-looking-at-exec ()
  "Return non-nil if cursor is on an exec line."
  (string-match rebase-mode-exec-line-re (thing-at-point 'line)))

(defun rebase-mode-looking-at-killed-exec ()
  "Return non-nil if looking at an exec line that has been commented out."
  (let ((line (thing-at-point 'line)))
    (and (eq (aref line 0) ?#)
         (string-match rebase-mode-exec-line-re line))))

(defun rebase-mode-move-line-up ()
  "Move the current action line up."
  (interactive)
  (when (rebase-mode-looking-at-action-or-exec)
    (let ((buffer-read-only nil)
          (col (current-column)))
      (transpose-lines 1)
      (forward-line -2)
      (move-to-column col))))

(defun rebase-mode-move-line-down ()
  "Assuming the next line is also an action line, move the current line down."
  (interactive)
  ;; if we're on an action and the next line is also an action
  (when (and (rebase-mode-looking-at-action-or-exec)
             (save-excursion
               (forward-line)
               (rebase-mode-looking-at-action-or-exec)))
    (let ((buffer-read-only nil)
          (col (current-column)))
      (forward-line 1)
      (transpose-lines 1)
      (forward-line -1)
      (move-to-column col))))

(defun rebase-mode-abort ()
  "Abort this rebase.
This is dune by emptying the buffer, saving and closing server
connection."
  (interactive)
  (when (or (not (buffer-modified-p))
            (y-or-n-p "Abort this rebase? "))
    (let ((buffer-read-only nil))
      (delete-region (point-min) (point-max))
      (save-buffer)
      (server-edit))))

(defun rebase-mode-kill-line ()
  "Kill the current action line."
  (interactive)
  (when (and (not (eq (char-after (point-at-bol)) ?#))
             (rebase-mode-looking-at-action-or-exec))
    (beginning-of-line)
    (let ((buffer-read-only nil))
      (insert "#"))
    (forward-line)))

(defun rebase-mode-exec (edit)
  "Prompt the user for a shell command to be executed, and
add it to the todo list.

If the cursor is on a commented-out exec line, uncomment the
current line instead of prompting.

When the prefix argument EDIT is non-nil and the cursor is on an
exec line, edit that line instead of inserting a new one.  If the
exec line was commented out, also uncomment it."
  (interactive "P")
  (cond
   ((and edit (rebase-mode-looking-at-exec))
    (let ((new-line (rebase-mode-read-exec-line
                     (match-string-no-properties 2 (thing-at-point 'line))))
          (inhibit-read-only t))
      (delete-region (point-at-bol) (point-at-eol))
      (if (not (equal "" new-line))
          (insert "exec " new-line)
        (delete-char -1)
        (forward-line))
      (move-beginning-of-line nil)))
   ((rebase-mode-looking-at-killed-exec)
    (save-excursion
      (beginning-of-line)
      (let ((buffer-read-only nil))
        (delete-char 1))))
   (t
    (let ((inhibit-read-only t)
          (line (rebase-mode-read-exec-line)))
      (unless (equal "" line)
        (move-end-of-line nil)
        (newline)
        (insert (concat "exec " line))))
    (move-beginning-of-line nil))))

(defun rebase-mode-read-exec-line (&optional initial-line)
  (read-shell-command "Execute: " initial-line))

(defun rebase-mode-undo (&optional arg)
  "A thin wrapper around `undo', which allows undoing in read-only buffers."
  (interactive "P")
  (let ((inhibit-read-only t))
    (undo arg)))

(defun rebase-mode-show-commit (&optional arg)
  "Show the commit on the current line if any."
  (interactive "P")
  (save-excursion
    (goto-char (point-at-bol))
    (when (looking-at rebase-mode-action-line-re)
      (let ((commit (match-string 2)))
        (if (fboundp 'magit-show-commit)
            (magit-show-commit commit nil nil 'select)
          (shell-command (concat "git show " commit)))))))

(defun rebase-mode-backward-line (&optional n)
  "Move N lines backward (forward if N is negative).
Like `forward-line' but go into the opposite direction."
  (interactive "p")
  (forward-line (* n -1)))

;;;###autoload
(define-derived-mode rebase-mode special-mode "Rebase"
  "Major mode for editing of a Git rebase file.

Rebase files are generated when you run 'git rebase -i' or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running 'man git-rebase' at the command line) for details."
  (setq font-lock-defaults '(rebase-mode-font-lock-keywords t t)))

(defun rebase-mode-show-keybindings ()
  "Modify the \"Commands:\" section of the comment Git generates
at the bottom of the file so that in place of the one-letter
abbreviation for the command, it shows the command's keybinding.
By default, this is the same except for the \"pick\" command."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "^#  \\(.\\), \\([[:alpha:]]+\\) = " nil t)
      (let ((start (match-beginning 1))
            (end (match-end 1))
            (command (intern (concat "rebase-mode-" (match-string 2)))))
        (when (fboundp command)
          (let ((overlay (make-overlay start end)))
            (overlay-put overlay
                         'display
                         (key-description (where-is-internal command nil t)))))))))

(add-hook 'rebase-mode-hook 'rebase-mode-show-keybindings t)

(defun rebase-mode-disable-before-save-hook ()
  (set (make-local-variable 'before-save-hook) nil))

(add-hook 'rebase-mode-hook 'rebase-mode-disable-before-save-hook)

;;;###autoload
(add-to-list 'auto-mode-alist
             '("git-rebase-todo" . rebase-mode))

(provide 'rebase-mode)
;;; rebase-mode.el ends here
