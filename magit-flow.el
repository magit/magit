;;; magit-flow.el --- git-flow plug-in for Magit

;; Copyright (C) 2012  Phil Jackson
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

;; This plug-in provides git-flow functionality as a separate
;; component of Magit. Only supports `feature` commands at the moment.

;;; Code:

(require 'magit)
(eval-when-compile
  (require 'cl))

(defun magit-run-git-flow (&rest args)
  (apply 'magit-run-git (cons "flow" args)))

(defun magit-run-git-lines-flow (&rest args)
  (apply 'magit-git-lines (cons "flow" args)))

(defun magit-flow-init ()
  (interactive)
  (magit-run-git-flow "init" "-d")
  (magit-display-process))

(defun magit-flow-feature-create ()
  (interactive)
  (let ((name (read-string "Create feature branch: ")))
    (magit-run-git-flow "feature" "start" name)))

(defun magit-flow-feature-list ()
  "List the feature branches managed by flow"
  (let ((current-feature nil)
        (all-features nil))
    (dolist (name (magit-run-git-lines-flow "feature" "list"))
      ;; is this the branch we're on
      (when (string-match "^\\* \\(.+\\)$" name 0)
        (setq current-feature (match-string 1 name)))
      ;; clean and append this line
      (let ((clean-name (replace-regexp-in-string "^\\*? +\\(.+\\)" "\\1" name)))
        (setq all-features (nconc all-features (list clean-name)))))
    (cons current-feature all-features)))

(defun magit-flow-feature-finish ()
  (interactive)
  (let* ((all (magit-flow-feature-list))
         (current (car all))
         (names (cdr all))
         (name (magit-completing-read "Branch to finish: " names nil t current)))
    (magit-run-git-flow "feature" "finish" name)
    (magit-display-process)))

(defun magit-flow-feature-diff ()
  (let ((dir default-directory)
        (buf (get-buffer-create "*magit-diff*")))
    (display-buffer buf)
    (with-current-buffer buf
      (magit-mode-init dir
                       'magit-diff-mode
                       '(lambda ()
                          (magit-create-buffer-sections
                            (magit-git-section 'diffbuf
                                               "Divergence from develop:"
                                               'magit-wash-diffs
                                               "flow" "feature" "diff")))))))

(defvar magit-flow-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") 'magit-key-mode-popup-flow)
    map))

;;;###autoload
(define-minor-mode magit-flow-mode "FLOW support for Magit"
  :lighter " FLOW"
  :require 'magit-flow
  :keymap 'magit-flow-mode-map)

;;;###autoload
(defun turn-on-magit-flow ()
  "Unconditionally turn on `magit-flow-mode'."
  (magit-flow-mode 1))

;; add the group and its keys
(progn
  ;; (re-)create the group
  (magit-key-mode-add-group 'flow)
  (magit-key-mode-insert-action
   'flow
   "n"
   "Create feature"
   'magit-flow-feature-create)

  (magit-key-mode-insert-action
   'flow
   "i"
   "Init"
   'magit-flow-init)

  (magit-key-mode-insert-action
   'flow
   "f"
   "Finish feature"
   'magit-flow-feature-finish)

  ;; generate and bind the menu popup function
  (magit-key-mode-generate 'flow))

(provide 'magit-flow)
;;; magit-flow.el ends here
