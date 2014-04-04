;;; magit-topgit.el --- topgit plug-in for Magit

;; Copyright (C) 2010-2014  The Magit Project Developers
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords: vc tools
;; Package: magit-topgit
;; Package-Requires: ((cl-lib "0.3") (magit "2.0.50"))

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

;; This plug-in provides topgit functionality as a separate component
;; of Magit.

;;; Code:

(require 'magit)
(eval-when-compile (require 'cl-lib))

;;; Options
;;;; Variables

(defgroup magit-topgit nil
  "Topgit support for Magit."
  :group 'magit-extensions)

(defcustom magit-topgit-executable "tg"
  "The name of the TopGit executable."
  :group 'magit-topgit
  :type 'string)

(defcustom magit-topgit-branch-prefix "t/"
  "Convention prefix for topic branch creation."
  :group 'magit-topgit
  :type 'string)

;;;; Faces

(defgroup magit-topgit-faces nil
  "Faces used by Magit-Topgit."
  :group 'magit-topgit
  :group 'magit-faces)

(defface magit-topgit-current
  '((t :weight bold :inherit magit-branch))
  "Face for section titles."
  :group 'magit-topgit-faces)

;;; Utilities

(defun magit-run-topgit (&rest args)
  (apply #'magit-call-topgit args)
  (magit-refresh))

(defun magit-call-topgit (&rest args)
  (apply #'magit-call-process magit-topgit-executable args))

(defun magit-run-topgit-async (&rest args)
  (message "Running %s %s" magit-topgit-executable
           (mapconcat 'identity args " "))
  (apply #'magit-start-topgit nil args))

(defun magit-start-topgit (&optional input &rest args)
  (apply #'magit-start-process magit-topgit-executable input args))

(defun magit-topgit-in-topic-p ()
  (and (file-exists-p ".topdeps")
       (executable-find magit-topgit-executable)))

;;; Commands

(defun magit-topgit-create-branch (branch parent)
  (when (zerop (or (string-match magit-topgit-branch-prefix branch) -1))
    (magit-run-topgit-async "create" branch parent)))

(defun magit-topgit-checkout (topic)
  (magit-checkout topic))

(defun magit-topgit-discard (topic)
  (when (yes-or-no-p "Discard topic? ")
    (magit-run-topgit-async "delete" "-f" topic)))

(defun magit-topgit-pull ()
  (when (magit-topgit-in-topic-p)
    (magit-run-topgit-async "update")))

(defun magit-topgit-push (arg)
  (when (magit-topgit-in-topic-p)
    (let* ((branch (or (magit-get-current-branch)
                       (user-error "Don't push a detached head.  That's gross")))
           (remote (magit-get "topgit" "remote"))
           (push-remote (if (or current-prefix-arg (not remote))
                            (magit-read-remote (format "Push %s to" branch))
                          remote)))
      (when (and (not remote)
                 (not current-prefix-arg))
        (magit-set push-remote "topgit" "remote"))
      (magit-run-topgit "push" "-r" push-remote))
    t))

(defun magit-topgit-remote-update (&optional remote)
  (when (magit-topgit-in-topic-p)
    (let* ((remote (magit-get "topgit" "remote"))
           (remote-update (if (or current-prefix-arg (not remote))
                              (magit-read-remote "Update remote")
                            remote)))
      (when (and (not remote)
                 (not current-prefix-arg))
        (magit-set remote-update "topgit" "remote")
        (magit-call-topgit "remote" "--populate" remote-update))
      (magit-run-topgit "remote" remote-update)))
  ;; We always return nil, as we also want
  ;; regular "git remote update" to happen.
  nil)

;;; Mode

;;;###autoload
(define-minor-mode magit-topgit-mode "Topgit support for Magit"
  :lighter " Topgit" :require 'magit-topgit
  (or (derived-mode-p 'magit-mode)
      (user-error "This mode only makes sense with magit"))
  (cond
   (magit-topgit-mode
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-topgit-topics
                            'magit-insert-stashes t t)
    (add-hook 'magit-branch-create-hook 'magit-topgit-create-branch nil t)
    (add-hook 'magit-remote-update-hook 'magit-topgit-remote-update nil t)
    (add-hook 'magit-pull-hook    'magit-topgit-pull nil t)
    (add-hook 'magit-push-hook    'magit-topgit-push nil t)
    (add-hook 'magit-visit-hook   'magit-topgit-checkout nil t)
    (add-hook 'magit-discard-hook 'magit-topgit-discard nil t))
   (t
    (remove-hook 'magit-status-sections-hook 'magit-insert-topgit-topics t)
    (remove-hook 'magit-branch-create-hook   'magit-topgit-create-branch t)
    (remove-hook 'magit-remote-update-hook   'magit-topgit-remote-update t)
    (remove-hook 'magit-pull-hook    'magit-topgit-pull t)
    (remove-hook 'magit-push-hook    'magit-topgit-push t)
    (remove-hook 'magit-visit-hook   'magit-topgit-checkout t)
    (remove-hook 'magit-discard-hook 'magit-topgit-discard t)))
  (when (called-interactively-p 'any)
    (magit-refresh)))

(put 'magit-topgit-checkout 'magit-section-action-context 'topgit-topic)
(put 'magit-topgit-discard  'magit-section-action-context 'topgit-topic)

;;;###autoload
(defun turn-on-magit-topgit ()
  "Unconditionally turn on `magit-topgit-mode'."
  (magit-topgit-mode 1))

;;; Topics Section

(defun magit-insert-topgit-topics ()
  (when magit-topgit-mode
    (magit-insert-section (topgit-topics)
      (magit-insert-heading "Topics:")
      (let ((beg (point)))
        (process-file magit-topgit-executable nil (list t nil) nil "summary")
        (if (= (point) beg)
            (magit-cancel-section)
          (save-restriction
            (narrow-to-region beg (point))
            (goto-char beg)
            (magit-wash-sequence #'magit-topgit-wash-topic)))))))

(defun magit-topgit-wash-topic ()
  (let ((fmt "^\\(.\\{7\\}\\)\\s-\\(\\S-+\\)\\s-+\\(.*\\)"))
    (if (re-search-forward fmt (line-end-position) t)
        (let ((flags (magit-topgit-parse-flags (match-string 1)))
              (topic (match-string 2)))
          (goto-char (line-beginning-position))
          (delete-char 8)
          (insert "\t")
          (goto-char (line-beginning-position))
          (magit-insert-section (topgit-topic topic)
            (let ((beg (1+ (line-beginning-position)))
                  (end (line-end-position)))
              (when (plist-get flags :current)
                (put-text-property beg end 'face 'magit-topgit-current))
              (when (plist-get flags :empty)
                (put-text-property
                 beg end 'face
                 `(:strike-through t :inherit ,(get-text-property beg 'face)))))
            (forward-line)))
      (delete-region (line-beginning-position) (1+ (line-end-position))))
    t))

(defun magit-topgit-parse-flags (flags-string)
  (let ((flags (string-to-list flags-string))
        (void-flag ?\ ))
    (list :current (not (eq (nth 0 flags) void-flag))
          :empty (not (eq (nth 1 flags) void-flag)))))

(provide 'magit-topgit)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-topgit.el ends here
