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

(defvar rebase-mode-action-line-re
  (rx
   line-start
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
    (** 7 40 (char "0-9" "a-f" "A-F"))) ;sha1
   (char space)
   (* not-newline))
  "Regexp that matches an action line in a rebase buffer.")

(defvar rebase-mode-font-lock-keywords
  (list
   (list rebase-mode-action-line-re
         '(1 font-lock-keyword-face)
         '(2 font-lock-builtin-face))
   (list (rx line-start (char "#") (* not-newline)) 0 font-lock-comment-face))
  "Font lock keywords for rebase-mode.")

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

    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)

    map)
  "Keymap for rebase-mode. Note this will be added to by the
  top-level code which defines the edit functions.")

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
      (kill-region (point) (progn (forward-word 1) (point)))
      (insert change-to)
      (goto-char start))))

(defun rebase-mode-looking-at-action ()
  "Returns non-nil if looking at an action line."
  (save-excursion
    (goto-char (point-at-bol))
    (looking-at rebase-mode-action-line-re)))

(defun rebase-mode-move-line-up ()
  "Move the current action line up."
  (interactive)
  (when (rebase-mode-looking-at-action)
    (let ((buffer-read-only nil)
          (col (current-column)))
      (transpose-lines 1)
      (previous-line 2)
      (move-to-column col))))

(defun rebase-mode-move-line-down ()
  "Assuming the next line is also an action line, move the
current line down."
  (interactive)
  ;; if we're on an action and the next line is also an action
  (when (and (rebase-mode-looking-at-action)
             (save-excursion
               (forward-line)
               (rebase-mode-looking-at-action)))
    (let ((buffer-read-only nil)
          (col (current-column)))
      (next-line 1)
      (transpose-lines 1)
      (previous-line 1)
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
  (when (rebase-mode-looking-at-action)
    (let* ((buffer-read-only nil)
           (region (list (point-at-bol)
                         (progn (forward-line)
                                (point-at-bol))))
           ;; might be handy to let the user know what went
           ;; somehow... sometime
           (text (apply 'buffer-substring region)))
      (apply 'kill-region region))))

(defun rebase-mode ()
  (interactive)
  (kill-all-local-variables)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '(rebase-mode-font-lock-keywords t t nil nil))

  (use-local-map rebase-mode-map)
  (setq buffer-read-only t)
  (setq mode-name "rebase-mode" major-mode 'rebase-mode))

(add-to-list 'auto-mode-alist
             '("git-rebase-todo" . rebase-mode))

(provide 'rebase-mode)
