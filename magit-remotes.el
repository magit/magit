(defvar magit-show-remotes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "$") 'magit-display-process)
    (define-key map (kbd "RET") 'magit-show-remote)
    (define-key map (kbd "a") 'magit-add-remote)
    (define-key map (kbd "f") 'magit-fetch-remote)
    (define-key map (kbd "g") 'magit-show-remotes)
    (define-key map (kbd "k") 'magit-delete-remote)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "P") 'magit-prune-remote)
    (define-key map (kbd "q") 'magit-quit-window)
    (define-key map (kbd "r") 'magit-rename-remote)
    (define-key map (kbd "s") 'magit-show-remote)
    (define-key map (kbd "u") 'magit-remote-update)
    map))

(define-derived-mode magit-show-remotes-mode fundamental-mode
  "Magit Remotes")

(defvar magit-remotes-buffer-name "*magit-remotes*")
(defvar magit-remote-info-buffer-name "*magit-show-remote*")

(defun magit-show-remotes ()
  "Show all of the current remotes in `other-window'."
  (interactive)
  (let ((buffer-existed (get-buffer magit-remotes-buffer-name)))
    (unless (eq major-mode 'magit-show-remotes-mode)
      (let ((topdir (magit-get-top-dir default-directory)))
        (switch-to-buffer-other-window magit-remotes-buffer-name)
        (setq default-directory topdir)))
    (let ((inhibit-read-only t)
          (goto-remote-line (line-number-at-pos))
          (remotes (mapcar (lambda (remote)
                             (list (cons 'remote remote)
                                   (cons 'url (magit-get "remote" remote "url"))))
                           (magit--remotes))))
      (erase-buffer)
      (insert
       (mapconcat
        (lambda (r)
          (propertize
           (concat
            (cdr (assoc 'remote r))
            " ["
            (cdr (assoc 'url r))
            "]")
           'remote (cdr (assoc 'remote r))))
        remotes
        "\n"))
      (magit-show-remotes-mode)
      (goto-char (point-min))
      (if buffer-existed
          (forward-line (1- goto-remote-line)))))
  (setq buffer-read-only t))

(defun magit--remotes-go-to (remote)
  "Move point to the line containing the remote REMOTE."
  (goto-char (point-min))
  (when remote
    (while (and (< (point) (point-max))
                (not (string= (get-text-property (point) 'remote) remote)))
      (forward-line))
    (unless (string= (get-text-property (point) 'remote) remote)
      (goto-char (point-min)))))

(defun magit--remote-at-point ()
  "Get the name of the remote at point.
Error out if there's no remote at point."
  (let ((remote (get-text-property (point) 'remote)))
    (or remote
        (error "There is no remote to work on"))))

(defun magit-rename-remote ()
  "Rename the remote at point with `git remote rename'."
  (interactive)
  (let* ((current (magit--remote-at-point))
         (new-name (magit-completing-read (concat "Rename `" current "' to: ") nil)))
    (unless (string= (or new-name "") "")
      (magit-run-git "remote" "rename" current new-name)
      (magit-show-remotes)
      (magit--remotes-go-to new-name))))

(defun magit-add-remote ()
  "Add a new remote with `git remote add '."
  (interactive)
  (let ((name (magit-completing-read "Remote's name: " nil))
        (url (magit-completing-read "URL: " nil)))
    (if (string= (or name "") "")
        (error "You must provide a name."))
    (if (string= (or url "") "")
        (error "You must provide an URL."))
    (magit-run-git "remote" "add" name url)
    (magit-show-remotes)
    (magit--remotes-go-to name)))

(defun magit-delete-remote ()
  "Delete the remote at point with `git remote rm'."
  (interactive)
  (let ((remote (magit--remote-at-point))
        (next-remote (save-excursion
                       (forward-line)
                       (get-text-property (point) 'remote))))
    (when (yes-or-no-p (concat "Really remove remote `" remote "'? "))
      (magit-run-git "remote" "rm" remote)
      (magit-show-remotes)
      (magit--remotes-go-to next-remote))))

(defun magit-fetch-remote ()
  "Fetch the remote at point with `git fetch'."
  (interactive)
  (magit-run-git-async "fetch" (magit--remote-at-point)))

(defun magit-show-remote ()
  "Show information about the remote at point with `git remote show'."
  (interactive)
  (let ((remote (magit--remote-at-point))
	(buf (get-buffer-create magit-remote-info-buffer-name)))
    (display-buffer buf)
    (with-current-buffer buf
      (set-buffer buf)
      (let ((inhibit-read-only t)
            (map (make-sparse-keymap)))
        (erase-buffer)
        (insert (mapconcat 'identity
                           (magit-git-lines "remote" "show" remote)
                           "\n")
                "\n")
        (text-mode)
        (define-key map "q" 'magit-quit-window)
        (use-local-map map))
      (setq buffer-read-only t))))

(defun magit-prune-remote (&optional arg)
  "Delete stale tracking branches for the remote at point with `git remote prune'.
With prefix all remotes will be pruned."
  (interactive "P")
  (if (yes-or-no-p (if arg "Really prune all remotes (cannot be undone)? " "Really prune the remote (cannot be undone)? "))
      (if arg
          (dolist (remote (magit--remotes))
            (magit-run-git "remote" "prune" remote))
        (magit-run-git-async "remote" "prune" (magit--remote-at-point)))))

(provide 'magit-remotes)
