;;; magit-stgit.el --- StGit plug-in for Magit

;; Copyright (C) 2011  Lluis Vilanova
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

;; This plug-in provides StGit functionality as a separate component of Magit.

;; Available actions:
;; - visit: Shows the patch at point in the series (stg show)
;; - apply: Goes to the patch at point in the series (stg goto)
;; - discard: Deletes the marked/at point patch in the series (stg delete)

;; Available commands:
;; - `magit-stgit-refresh': Refresh the marked/at point patch in the series
;;   (stg refresh)
;; - `magit-stgit-repair': Repair the StGit metadata (stg repair)
;; - `magit-stgit-rebase': Rebase the whole series (stg rebase)

;; TODO:
;; - Let the user select which files must be included in a refresh.
;; - Missing actions for `magit-show-item-or-scroll-up' and
;;   `magit-show-item-or-scroll-down'.
;; - Marking a patch is slow and refreshes all buffers, which resets their
;;   position (i.e., the buffer is shown from its first line).

;;; Code:

(require 'magit)
(eval-when-compile
  (require 'cl))

;;; Customizables:

(defcustom magit-stgit-executable "stg"
  "The name of the StGit executable."
  :group 'magit
  :type 'string)

(defface magit-stgit-applied
  '((t :inherit magit-diff-add))
  "Face for an applied stgit patch."
  :group 'magit-faces)

(defface magit-stgit-current
  '((t :inherit magit-item-highlight))
  "Face for the current stgit patch."
  :group 'magit-faces)

(defface magit-stgit-other
  '((t :inherit magit-diff-del))
  "Face for a non-applied stgit patch."
  :group 'magit-faces)

(defface magit-stgit-marked
  '((t :inherit magit-item-mark))
  "Face for a marked stgit patch."
  :group 'magit-faces)

(defface magit-stgit-empty
  '((t :inherit magit-item-mark))
  "Face for an empty stgit patch."
  :group 'magit-faces)

;;; Common code:

(defvar magit-stgit--enabled nil
  "Whether this buffer has StGit support.")
(make-variable-buffer-local 'magit-stgit--enabled)

(defvar magit-stgit-mode)

(defun magit-stgit--enabled ()
  "Whether this buffer has StGit support enabled."
  (if (assoc 'magit-stgit--enabled (buffer-local-variables))
      magit-stgit--enabled
    (setq magit-stgit--enabled
          (and magit-stgit-mode
               (not (null
                     (member (concat (magit-get-current-branch) ".stgit")
                             (mapcar #'(lambda (line)
                                         (string-match "^\\*?\s*\\([^\s]*\\)"
                                                       line)
                                         (match-string 1 line))
                                     (magit-git-lines "branch")))))))))

(defun magit-stgit--enabled-reset ()
  "Reset the StGit enabled state."
  (kill-local-variable 'magit-stgit--enabled))

(defvar magit-stgit--marked-patch nil
  "The (per-buffer) currently marked patch in an StGit series.")
(make-variable-buffer-local 'magit-stgit--marked-patch)

;;; Menu:

(easy-menu-define magit-stgit-extension-menu
  nil
  "StGit extension menu"
  '("StGit"
    :active (magit-stgit--enabled)

    ["Refresh patch" magit-stgit-refresh
     :help "Refresh the contents of a patch in an StGit series"]
    ["Repair" magit-stgit-repair
     :help "Repair StGit metadata if branch was modified with git commands"]
    ["Rebase series" magit-stgit-rebase
     :help "Rebase an StGit patch series"]
    ))

(easy-menu-add-item 'magit-mode-menu
                    '("Extensions")
                    magit-stgit-extension-menu)

;;; Series section:

(defun magit-stgit--wash-patch ()
  (if (search-forward-regexp "^\\(.\\)\\(.\\) \\([^\s]*\\)\\(\s*# ?\\)\\(.*\\)"
                             (line-end-position) t)
      (let* ((empty-str "[empty] ")
             (indent-str (make-string (string-bytes empty-str) ?\ ))
             (empty (match-string 1))
             (state (match-string 2))
             (patch (match-string 3))
             (descr (match-string 5)))
        (delete-region (line-beginning-position) (line-end-position))
        (insert
         (cond ((string= empty "0")
                (propertize (concat empty-str " " state " " descr) 'face 'magit-stgit-empty))
               ((string= magit-stgit--marked-patch patch)
                (propertize (concat indent-str " " state " " descr) 'face 'magit-stgit-marked))
               ((string= state "+")
                (concat indent-str " " (propertize state 'face 'magit-stgit-applied) " " descr))
               ((string= state ">")
                (propertize (concat indent-str " " state " " descr) 'face 'magit-stgit-current))
               ((string= state "-")
                (concat indent-str " " (propertize state 'face 'magit-stgit-other) " " descr))))
        (goto-char (line-beginning-position))
        (magit-with-section patch 'series
          (magit-set-section-info patch)
          (goto-char (line-end-position)))
        (forward-line))
    (delete-region (line-beginning-position) (1+ (line-end-position))))
  t)

(defun magit-stgit--wash-series ()
    (let ((magit-old-top-section nil))
      (magit-wash-sequence #'magit-stgit--wash-patch)))

(magit-define-inserter series ()
  (when (executable-find magit-stgit-executable)
    (magit-insert-section 'series
                          "Series:" 'magit-stgit--wash-series
                          magit-stgit-executable "series" "-a" "-d" "-e")))

;;; Actions:

;; Copy of `magit-refresh-commit-buffer' (version 1.0.0)
(defun magit-stgit--refresh-patch-buffer (patch)
  (magit-create-buffer-sections
    (magit-insert-section nil nil
                       'magit-wash-commit
                       magit-stgit-executable
                       "show"
                       patch)))

;; Copy of `magit-show-commit' (version 1.0.0)
(defun magit-stgit--show-patch (patch &optional scroll)
  (when (magit-section-p patch)
    (setq patch (magit-section-info patch)))
  (let ((dir default-directory)
        (buf (get-buffer-create magit-commit-buffer-name)))
    (cond ((and (equal magit-currently-shown-commit patch)
                ;; if it's empty then the buffer was killed
                (with-current-buffer buf
                  (> (length (buffer-string)) 1)))
           (let ((win (get-buffer-window buf)))
             (cond ((not win)
                    (display-buffer buf))
                   (scroll
                    (with-selected-window win
                      (funcall scroll))))))
          (t
           (setq magit-currently-shown-commit patch)
           (display-buffer buf)
           (with-current-buffer buf
             (set-buffer buf)
             (goto-char (point-min))
             (magit-mode-init dir 'magit-commit-mode
                              #'magit-stgit--refresh-patch-buffer patch))))))

(magit-add-action (item info "visit")
  ((series)
   (magit-stgit--show-patch info)
   (pop-to-buffer magit-commit-buffer-name)))

(magit-add-action (item info "apply")
  ((series)
   (magit-run magit-stgit-executable "goto" info)))

(magit-add-action (item info "discard")
  ((series)
   (let ((patch (or magit-stgit--marked-patch info)))
     (if (yes-or-no-p (format "Delete patch '%s' in series? " patch))
         (progn
           (if (string= magit-stgit--marked-patch patch)
               (setq magit-stgit--marked-patch nil))
           (magit-run magit-stgit-executable "delete" patch))))))

(defun magit-stgit--set-marked-patch (patch)
  (setq magit-stgit--marked-patch
        (if (string= magit-stgit--marked-patch patch)
            nil
          patch)))

(magit-add-action (item info "mark")
  ((series)
   (magit-stgit--set-marked-patch info)
   (magit-refresh-all)))

;;; Commands:

(defun magit-stgit-refresh ()
  "Refresh the contents of a patch in an StGit series.
If there is no marked patch in the series, refreshes the current
patch.
Otherwise, refreshes the marked patch."
  (interactive)
  (if magit-stgit--marked-patch
      (magit-run magit-stgit-executable "refresh" "-p" magit-stgit--marked-patch)
    (magit-run magit-stgit-executable "refresh")))

(defun magit-stgit-repair ()
  "Repair StGit metadata if branch was modified with git commands.
In the case of Git commits these will be imported as new patches
into the series."
  (interactive)
  (message "Repairing series...")
  (magit-run magit-stgit-executable "repair")
  (message ""))

(defun magit-stgit-rebase ()
  "Rebase an StGit patch series."
  (interactive)
  (if (magit-get-current-remote)
      (progn
        (if (yes-or-no-p "Update remotes? ")
            (progn
              (message "Updating remotes...")
              (magit-run-git-async "remote" "update")))
        (magit-run magit-stgit-executable "rebase"
                   (format "remotes/%s/%s"
                           (magit-get-current-remote)
                           (magit-get-current-branch))))))

;;;###autoload
(define-minor-mode magit-stgit-mode "StGit support for Magit"
  :lighter " Stg" :require 'magit-stgit
  (or (derived-mode-p 'magit-mode)
      (error "This mode only makes sense with magit"))
  (if magit-stgit-mode
      (progn
        (add-hook 'magit-after-insert-stashes-hook 'magit-insert-series nil t))
    (progn
      (remove-hook 'magit-after-insert-stashes-hook 'magit-insert-series t)))
  (when (called-interactively-p 'any)
    (magit-refresh)))

;;;###autoload
(defun turn-on-magit-stgit ()
  "Unconditionally turn on `magit-stgit-mode'."
  (magit-stgit-mode 1))

(provide 'magit-stgit)
;;; magit-stgit.el ends here
