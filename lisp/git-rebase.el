;;; git-rebase.el --- Edit Git rebase files  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2017  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Phil Jackson <phil@shellarchive.co.uk>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

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

;; This package assists the user in editing the list of commits to be
;; rewritten during an interactive rebase.

;; When the user initiates an interactive rebase, e.g. using "r e" in
;; a Magit buffer or on the command line using "git rebase -i REV",
;; Git invokes the `$GIT_SEQUENCE_EDITOR' (or if that is undefined
;; `$GIT_EDITOR' or even `$EDITOR') letting the user rearrange, drop,
;; reword, edit, and squash commits.

;; This package provides the major-mode `git-rebase-mode' which makes
;; doing so much more fun, by making the buffer more colorful and
;; providing the following commands:
;;
;;   C-c C-c  Tell Git to make it happen.
;;   C-c C-k  Tell Git that you changed your mind, i.e. abort.
;;
;;   p        Move point to previous line.
;;   n        Move point to next line.
;;
;;   M-p      Move the commit at point up.
;;   M-n      Move the commit at point down.
;;
;;   k        Drop the commit at point.
;;   c        Don't drop the commit at point.
;;   r        Change the message of the commit at point.
;;   e        Edit the commit at point.
;;   s        Squash the commit at point, into the one above.
;;   f        Like "s" but don't also edit the commit message.
;;   x        Add a script to be run with the commit at point
;;            being checked out.
;;
;;   SPC      Show the commit at point in another buffer.
;;   RET      Show the commit at point in another buffer and
;;            select its window.
;;   C-/      Undo last change.

;; You should probably also read the `git-rebase' manpage.

;;; Code:

(require 'dash)
(require 'easymenu)
(require 'server)
(require 'with-editor)
(require 'magit)

(and (require 'async-bytecomp nil t)
     (memq 'magit (bound-and-true-p async-bytecomp-allowed-packages))
     (fboundp 'async-bytecomp-package-mode)
     (async-bytecomp-package-mode 1))

(eval-when-compile (require 'recentf))

;;; Options
;;;; Variables

(defgroup git-rebase nil
  "Edit Git rebase sequences."
  :link '(info-link "(magit)Editing Rebase Sequences")
  :group 'tools)

(defcustom git-rebase-auto-advance t
  "Whether to move to next line after changing a line."
  :group 'git-rebase
  :type 'boolean)

(defcustom git-rebase-show-instructions t
  "Whether to show usage instructions inside the rebase buffer."
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

(defface git-rebase-hash '((t (:inherit magit-hash)))
  "Face for commit hashes."
  :group 'git-rebase-faces)

(defface git-rebase-description nil
  "Face for commit descriptions."
  :group 'git-rebase-faces)

(defface git-rebase-killed-action
  '((t (:inherit font-lock-comment-face :strike-through t)))
  "Face for commented action and exec lines."
  :group 'git-rebase-faces)

(defface git-rebase-comment-hash
  '((t (:inherit git-rebase-hash :weight bold)))
  "Face for commit hashes in commit message comments."
  :group 'git-rebase-faces)

(defface git-rebase-comment-heading
  '((t :inherit font-lock-keyword-face))
  "Face for headings in rebase message comments."
  :group 'git-commit-faces)

;;; Keymaps

(defvar git-rebase-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (cond ((featurep 'jkl)
           (define-key map [return]    'git-rebase-show-commit)
           (define-key map (kbd   "i") 'git-rebase-backward-line)
           (define-key map (kbd   "k") 'forward-line)
           (define-key map (kbd "M-i") 'git-rebase-move-line-up)
           (define-key map (kbd "M-k") 'git-rebase-move-line-down)
           (define-key map (kbd   "p") 'git-rebase-pick)
           (define-key map (kbd   ",") 'git-rebase-kill-line))
          (t
           (define-key map (kbd "C-m") 'git-rebase-show-commit)
           (define-key map (kbd   "p") 'git-rebase-backward-line)
           (define-key map (kbd   "n") 'forward-line)
           (define-key map (kbd "M-p") 'git-rebase-move-line-up)
           (define-key map (kbd "M-n") 'git-rebase-move-line-down)
           (define-key map (kbd   "c") 'git-rebase-pick)
           (define-key map (kbd   "k") 'git-rebase-kill-line)
           (define-key map (kbd "C-k") 'git-rebase-kill-line)))
    (define-key map (kbd "e") 'git-rebase-edit)
    (define-key map (kbd "m") 'git-rebase-edit)
    (define-key map (kbd "f") 'git-rebase-fixup)
    (define-key map (kbd "q") 'undefined)
    (define-key map (kbd "r") 'git-rebase-reword)
    (define-key map (kbd "w") 'git-rebase-reword)
    (define-key map (kbd "s") 'git-rebase-squash)
    (define-key map (kbd "x") 'git-rebase-exec)
    (define-key map (kbd "y") 'git-rebase-insert)
    (define-key map (kbd "SPC")     'git-rebase-show-or-scroll-up)
    (define-key map (kbd "DEL")     'git-rebase-show-or-scroll-down)
    (define-key map (kbd "C-x C-t") 'git-rebase-move-line-up)
    (define-key map [M-up]          'git-rebase-move-line-up)
    (define-key map [M-down]        'git-rebase-move-line-down)
    (define-key map [remap undo]    'git-rebase-undo)
    map)
  "Keymap for Git-Rebase mode.")

(cond ((featurep 'jkl)
       (put 'git-rebase-reword       :advertised-binding "r")
       (put 'git-rebase-move-line-up :advertised-binding (kbd "M-i"))
       (put 'git-rebase-kill-line    :advertised-binding ","))
      (t
       (put 'git-rebase-reword       :advertised-binding "r")
       (put 'git-rebase-move-line-up :advertised-binding (kbd "M-p"))
       (put 'git-rebase-kill-line    :advertised-binding "k")))

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

(defvar git-rebase-command-descriptions
  '((with-editor-finish           . "tell Git to make it happen")
    (with-editor-cancel           . "tell Git that you changed your mind, i.e. abort")
    (git-rebase-backward-line     . "move point to previous line")
    (forward-line                 . "move point to next line")
    (git-rebase-move-line-up      . "move the commit at point up")
    (git-rebase-move-line-down    . "move the commit at point down")
    (git-rebase-show-or-scroll-up . "show the commit at point in another buffer")
    (git-rebase-show-commit
     . "show the commit at point in another buffer and select its window")
    (undo                         . "undo last change")
    (git-rebase-kill-line         . "drop the commit at point")))

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

(defvar-local git-rebase-line nil)
(defvar-local git-rebase-comment-re nil)

(defun git-rebase-set-action (action)
  (goto-char (line-beginning-position))
  (if (and (looking-at git-rebase-line)
           (not (string-match-p "\\(e\\|exec\\)$" (match-string 1))))
      (let ((inhibit-read-only t))
        (replace-match action t t nil 1)
        (when git-rebase-auto-advance
          (forward-line)))
    (ding)))

(defun git-rebase-line-p (&optional pos)
  (save-excursion
    (when pos (goto-char pos))
    (goto-char (line-beginning-position))
    (looking-at-p git-rebase-line)))

(defun git-rebase-region-bounds ()
  (when (use-region-p)
    (let ((beg (save-excursion (goto-char (region-beginning))
                               (line-beginning-position)))
          (end (save-excursion (goto-char (region-end))
                               (line-end-position))))
      (when (and (git-rebase-line-p beg)
                 (git-rebase-line-p end))
        (list beg (1+ end))))))

(defun git-rebase-move-line-down (n)
  "Move the current commit (or command) N lines down.
If N is negative, move the commit up instead.  With an active
region, move all the lines that the region touches, not just the
current line."
  (interactive "p")
  (-let* (((beg end) (or (git-rebase-region-bounds)
                         (list (line-beginning-position)
                               (1+ (line-end-position)))))
          (pt-offset (- (point) beg))
          (mark-offset (and mark-active (- (mark) beg))))
    (save-restriction
      (narrow-to-region
       (point-min)
       (1+ (save-excursion
             (goto-char (point-min))
             (while (re-search-forward git-rebase-line nil t))
             (point))))
      (if (or (and (< n 0) (= beg (point-min)))
              (and (> n 0) (= end (point-max)))
              (> end (point-max)))
          (ding)
        (goto-char (if (< n 0) beg end))
        (forward-line n)
        (atomic-change-group
          (let ((inhibit-read-only t))
            (insert (delete-and-extract-region beg end)))
          (let ((new-beg (- (point) (- end beg))))
            (when (use-region-p)
              (setq deactivate-mark nil)
              (set-mark (+ new-beg mark-offset)))
            (goto-char (+ new-beg pt-offset))))))))

(defun git-rebase-move-line-up (n)
  "Move the current commit (or command) N lines up.
If N is negative, move the commit down instead.  With an active
region, move all the lines that the region touches, not just the
current line."
  (interactive "p")
  (git-rebase-move-line-down (- n)))

(defun git-rebase-highlight-region (start end window rol)
  (let ((inhibit-read-only t)
        (deactivate-mark nil)
        (bounds (git-rebase-region-bounds)))
    (mapc #'delete-overlay magit-section-highlight-overlays)
    (when bounds
      (magit-section-make-overlay (car bounds) (cadr bounds)
                                  'magit-section-heading-selection))
    (if (and bounds (not magit-keep-region-overlay))
        (funcall (default-value 'redisplay-unhighlight-region-function) rol)
      (funcall (default-value 'redisplay-highlight-region-function)
               start end window rol))))

(defun git-rebase-unhighlight-region (rol)
  (mapc #'delete-overlay magit-section-highlight-overlays)
  (funcall (default-value 'redisplay-unhighlight-region-function) rol))

(defun git-rebase-kill-line ()
  "Kill the current action line."
  (interactive)
  (goto-char (line-beginning-position))
  (when (and (looking-at git-rebase-line)
             (not (eq (char-after) (string-to-char comment-start))))
    (let ((inhibit-read-only t))
      (insert comment-start))
    (when git-rebase-auto-advance
      (forward-line))))

(defun git-rebase-insert (rev)
  "Read an arbitrary commit and insert it below current line."
  (interactive (list (magit-read-branch-or-commit "Insert revision")))
  (forward-line)
  (--if-let (magit-rev-format "%h %s" rev)
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
      (when (looking-at (concat git-rebase-comment-re "?"
                                "\\(e\\|exec\\) \\(.*\\)"))
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

(defun git-rebase--show-commit (&optional scroll)
  (let ((disable-magit-save-buffers t))
    (save-excursion
      (goto-char (line-beginning-position))
      (--if-let (and (looking-at git-rebase-line)
                     (match-string 2))
          (pcase scroll
            (`up   (magit-diff-show-or-scroll-up))
            (`down (magit-diff-show-or-scroll-down))
            (_     (apply #'magit-show-commit it (magit-diff-arguments))))
        (ding)))))

(defun git-rebase-show-commit ()
  "Show the commit on the current line if any."
  (interactive)
  (git-rebase--show-commit))

(defun git-rebase-show-or-scroll-up ()
  "Update the commit buffer for commit on current line.

Either show the commit at point in the appropriate buffer, or if
that buffer is already being displayed in the current frame and
contains information about that commit, then instead scroll the
buffer up."
  (interactive)
  (git-rebase--show-commit 'up))

(defun git-rebase-show-or-scroll-down ()
  "Update the commit buffer for commit on current line.

Either show the commit at point in the appropriate buffer, or if
that buffer is already being displayed in the current frame and
contains information about that commit, then instead scroll the
buffer down."
  (interactive)
  (git-rebase--show-commit 'down))

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
  :group 'git-rebase
  (setq comment-start (or (magit-get "core.commentChar") "#"))
  (setq git-rebase-comment-re (concat "^" (regexp-quote comment-start)))
  (setq git-rebase-line
        (concat "^\\(" (regexp-quote comment-start) "?"
                "\\(?:[fprse]\\|pick\\|reword\\|edit\\|squash\\|fixup\\|exec\\)\\) "
                "\\(?:\\([^ \n]+\\) \\(.*\\)\\)?"))
  (setq font-lock-defaults (list (git-rebase-mode-font-lock-keywords) t t))
  (unless git-rebase-show-instructions
    (let ((inhibit-read-only t))
      (flush-lines git-rebase-comment-re)))
  (with-editor-mode 1)
  (when git-rebase-confirm-cancel
    (add-hook 'with-editor-cancel-query-functions
              'git-rebase-cancel-confirm nil t))
  (setq-local redisplay-highlight-region-function 'git-rebase-highlight-region)
  (setq-local redisplay-unhighlight-region-function 'git-rebase-unhighlight-region)
  (add-hook 'with-editor-pre-cancel-hook  'git-rebase-autostash-save  nil t)
  (add-hook 'with-editor-post-cancel-hook 'git-rebase-autostash-apply nil t)
  (when (boundp 'save-place)
    (setq save-place nil)))

(defun git-rebase-cancel-confirm (force)
  (or (not (buffer-modified-p))
      force
      (magit-confirm 'abort-rebase "Abort this rebase")))

(defun git-rebase-autostash-save ()
  (--when-let (magit-file-line (magit-git-dir "rebase-merge/autostash"))
    (push (cons 'stash it) with-editor-cancel-alist)))

(defun git-rebase-autostash-apply ()
  (--when-let (cdr (assq 'stash with-editor-cancel-alist))
    (magit-stash-apply it)))

(defun git-rebase-match-comment-line (limit)
  (re-search-forward (concat git-rebase-comment-re ".*") limit t))

(defun git-rebase-match-killed-action (limit)
  (re-search-forward (concat git-rebase-comment-re "[^ \n].*") limit t))

(defun git-rebase-mode-font-lock-keywords ()
  "Font lock keywords for Git-Rebase mode."
  `(("^\\([efprs]\\|pick\\|reword\\|edit\\|squash\\|fixup\\) \\([^ \n]+\\) \\(.*\\)"
     (1 'font-lock-keyword-face)
     (2 'git-rebase-hash)
     (3 'git-rebase-description))
    ("^\\(exec\\) \\(.*\\)"
     (1 'font-lock-keyword-face)
     (2 'git-rebase-description))
    (git-rebase-match-comment-line 0 'font-lock-comment-face)
    (git-rebase-match-killed-action 0 'git-rebase-killed-action t)
    (,(format "^%s Rebase \\([^ ]*\\) onto \\([^ ]*\\)" comment-start)
     (1 'git-rebase-comment-hash t)
     (2 'git-rebase-comment-hash t))
    (,(format "^%s \\(Commands:\\)" comment-start)
     (1 'git-rebase-comment-heading t))))

(defun git-rebase-mode-show-keybindings ()
  "Modify the \"Commands:\" section of the comment Git generates
at the bottom of the file so that in place of the one-letter
abbreviation for the command, it shows the command's keybinding.
By default, this is the same except for the \"pick\" command."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (when (and git-rebase-show-instructions
                 (re-search-forward
                  (concat git-rebase-comment-re " Commands:\n")
                  nil t))
        (--each git-rebase-command-descriptions
          (insert (format "%s %-8s %s\n"
                          comment-start
                          (substitute-command-keys (format "\\[%s]" (car it)))
                          (cdr it))))
        (while (re-search-forward (concat git-rebase-comment-re
                                          "\\(  ?\\)\\([^,],\\) \\([^ ]+\\) = ")
                                  nil t)
          (let ((cmd (intern (concat "git-rebase-" (match-string 3)))))
            (if (not (fboundp cmd))
                (delete-region (line-beginning-position) (1+ (line-end-position)))
              (replace-match " " t t nil 1)
              (replace-match
               (format "%-8s"
                       (mapconcat #'key-description
                                  (--filter (not (eq (elt it 0) 'menu-bar))
                                            (reverse (where-is-internal cmd)))
                                  ", "))
               t t nil 2))))))))

(add-hook 'git-rebase-mode-hook 'git-rebase-mode-show-keybindings t)

(defun git-rebase-mode-disable-before-save-hook ()
  (set (make-local-variable 'before-save-hook) nil))

(add-hook 'git-rebase-mode-hook 'git-rebase-mode-disable-before-save-hook)

;;;###autoload
(defconst git-rebase-filename-regexp "/git-rebase-todo\\'")
;;;###autoload
(add-to-list 'auto-mode-alist
             (cons git-rebase-filename-regexp 'git-rebase-mode))

(add-to-list 'with-editor-server-window-alist
             (cons git-rebase-filename-regexp 'switch-to-buffer))

(eval-after-load 'recentf
  '(add-to-list 'recentf-exclude git-rebase-filename-regexp))

(add-to-list 'with-editor-file-name-history-exclude git-rebase-filename-regexp)

(provide 'git-rebase)
;;; git-rebase.el ends here
