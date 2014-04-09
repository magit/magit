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

(defconst magit-topgit-topic-re "^\\(.\\{7\\}\\)\t\\([^ ]+\\) +\\(.*\\)")

(defun magit-topgit-read-topic (prompt)
  (magit-completing-read
   prompt (mapcar (lambda (line)
                    (string-match magit-topgit-topic-re line)
                    (match-string 2 line))
                  (process-lines magit-topgit-executable "summary"))
   nil t))

(defun magit-topgit-topic-args (prompt)
  (list (or (magit-section-case (value) (topgit-topic value))
            (magit-topgit-read-topic prompt))))

(defun magit-topgit-in-topic-p ()
  (and (file-exists-p ".topdeps")
       (executable-find magit-topgit-executable)))

;;; Commands

(magit-define-popup magit-topgit-popup
  "Popup console for Topgit commands."
  'magit-popups
  :actions '((?c "Create topic"  magit-topgit-create-branch)
             (?a "Update remote" magit-topgit-remote-update)
             (?f "Update topic"  magit-topgit-pull)
             (?p "Push topic"    magit-topgit-push)))

(defun magit-topgit-create-branch (branch parent)
  (interactive
   (list (read-string "Create topgit branch: " magit-topgit-branch-prefix)
         (magit-read-rev "Parent" (magit-get-current-branch))))
  (when (string-match-p magit-topgit-branch-prefix branch)
    (magit-run-topgit-async "create" branch parent)))

(defun magit-topgit-checkout (topic)
  (interactive (magit-topgit-topic-args "Checkout"))
  (magit-checkout topic))

(defun magit-topgit-discard (topic)
  (interactive (magit-topgit-topic-args "Discard"))
  (when (yes-or-no-p (format "Discard topic `%s'? " topic))
    (magit-run-topgit-async "delete" "-f" topic)))

(defun magit-topgit-pull ()
  (interactive)
  (if (magit-topgit-in-topic-p)
      (magit-run-topgit-async "update")
    (user-error "Not inside topgit topic")))

(defun magit-topgit-push (arg)
  (interactive "P")
  (if (magit-topgit-in-topic-p)
      (let* ((branch (or (magit-get-current-branch)
                         (user-error "Don't push a detached head.  That's gross")))
             (remote (magit-get "topgit" "remote"))
             (push-remote (if (or arg (not remote))
                              (magit-read-remote (format "Push %s to" branch))
                            remote)))
        (when (and (not remote) (not arg))
          (magit-set push-remote "topgit" "remote"))
        (magit-run-topgit "push" "-r" push-remote))
    (user-error "Not inside topgit topic")))

(defun magit-topgit-remote-update (&optional arg)
  (interactive "P")
  (if (magit-topgit-in-topic-p)
      (let* ((remote (magit-get "topgit" "remote"))
             (remote-update (if (or arg (not remote))
                                (magit-read-remote "Update remote")
                              remote)))
        (when (and (not remote) (not arg))
          (magit-set remote-update "topgit" "remote")
          (magit-call-topgit "remote" "--populate" remote-update))
        (magit-run-topgit "remote" remote-update)
        (magit-remote-update))
    (user-error "Not inside topgit topic")))

;;; Mode

(defvar magit-topgit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "T" 'magit-topgit-popup)
    map))

;;;###autoload
(define-minor-mode magit-topgit-mode "Topgit support for Magit"
  :lighter " Topgit" :require 'magit-topgit :keymap magit-topgit-mode-map
  (or (derived-mode-p 'magit-mode)
      (user-error "This mode only makes sense with magit"))
  (if magit-topgit-mode
      (magit-add-section-hook 'magit-status-sections-hook
                              'magit-insert-topgit-topics
                              'magit-insert-stashes t t)
    (remove-hook 'magit-status-sections-hook
                 'magit-insert-topgit-topics t))
  (when (called-interactively-p 'any)
    (magit-refresh)))

;;;###autoload
(defun turn-on-magit-topgit ()
  "Unconditionally turn on `magit-topgit-mode'."
  (magit-topgit-mode 1))

;;; Topics Section

(defvar magit-topgit-topic-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-topgit-checkout)
    (define-key map "k"  'magit-topgit-discard)
    map))

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
  (if (re-search-forward magit-topgit-topic-re (line-end-position) t)
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
               `(:strike-through t :inherit ,(get-text-property beg 'face))))
            (put-text-property beg end 'keymap 'magit-topgit-topic-map))
          (forward-line)))
    (delete-region (line-beginning-position) (1+ (line-end-position))))
  t)

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
