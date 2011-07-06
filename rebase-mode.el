;;; rebase-mode -- edit git rebase files.

;; Copyright (C) 2010  Phil Jackson
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

;; Allows the editing of a git rebase file (which you might get when
;; using 'git rebase -i' or hitting 'E' in Magit). Assumes editing is
;; happening in a server.

;;; Code:

(defgroup rebase-mode nil
  "Customize Rebase Mode"
  :group 'faces)

(defface rebase-mode-killed-action-face
  '((((class color))
     :inherit font-lock-comment-face
     :strike-through t))
  "Action lines in the rebase TODO list that have been commented out."
  :group 'rebase-mode)

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
   (* not-newline))
  "Regexp that matches an action line in a rebase buffer.")

(defconst rebase-mode-exec-line-re
  (rx
   line-start
   (? "#")
   (group
    (| "x"
       "exec"))
   (char space)
   (* not-newline))
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
         '(2 font-lock-builtin-face))
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

    (define-key map (kbd "M-p") 'rebase-mode-move-line-up)
    (define-key map (kbd "M-n") 'rebase-mode-move-line-down)
    (define-key map (kbd "k") 'rebase-mode-kill-line)
    (define-key map (kbd "x") 'rebase-mode-exec-line)

    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)

    map)
  "Keymap for rebase-mode.  Note this will be added to by the
top-level code which defines the edit functions.")

(require 'easymenu)
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
    ["Execute" rebase-mode-exec-line t]
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
      (goto-char start))))

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

(defun rebase-mode-looking-at-killed-exec ()
  "Return non-nil if looking at an exec line that has been commented out"
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
  "Abort this rebase (by emptying the buffer, saving and closing
server connection)."
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
  (when (rebase-mode-looking-at-action-or-exec)
    (save-excursion
      (beginning-of-line)
      (if (not (looking-at "#"))
          (let ((buffer-read-only nil))
            (insert "#"))))))

(defvar rebase-mode-exec-hist nil
  "Contains history items for the prompt in `rebase-mode-exec-line`")

(defun rebase-mode-exec-line (&optional line)
  "If point is on a commented-out exec line, uncomment that line.
Otherwise, add a LINE that gets executed."
  (interactive)
  (cond
   ((rebase-mode-looking-at-killed-exec)
    (save-excursion
      (beginning-of-line)
      (let ((buffer-read-only nil))
        (delete-char 1))))
   (t
    (if (not line)
        (setq line (read-string "Execute: " nil rebase-mode-exec-hist)))
    (let ((buffer-read-only nil))
      (move-end-of-line nil)
      (newline)
      (insert (concat "exec " line))
      (move-beginning-of-line nil)))))

;;;###autoload
(define-derived-mode rebase-mode special-mode "Rebase"
  "Major mode for editing of a Git rebase file.

Rebase files are generated when you run 'git rebase -i' or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running 'man git-rebase' at the command line) for details."
  (setq font-lock-defaults '(rebase-mode-font-lock-keywords t t)))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("git-rebase-todo" . rebase-mode))

(provide 'rebase-mode)

;;; rebase-mode.el ends here
