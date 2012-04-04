;;; magit-wip.el --- git-wip plug-in for Magit

;; Copyright (C) 2012  Jonas Bernoulli
;; Copyright (C) 2012  Ryan C. Thompson

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

;; When the global minor mode `magit-wip-mode' is turned on and a file is
;; saved inside a writable git repository then it is also committed to a
;; special Work In Progress ref.

;; This required the third-party git command `git-wip' which is available
;; from https://github.com/bartman/git-wip.

;;; TODO:

;; - Provide an interface for `git wip log'

;;; Code:

(require 'magit)

(defcustom magit-wip-commit-message "WIP %r"
  "Commit message for git-wip commits.

The following `format'-like specs are supported:
%f the full name of the file being saved, and
%r the name of the file being saved, relative to the repository root."
  :group 'magit
  :type 'string)

(defcustom magit-wip-echo-area-message "Wrote (wip) %f...done"
  "Message shown in the echo area after creating a for git-wip commit.

The following `format'-like specs are supported:
%f the full name of the file being saved, and
%r the name of the file being saved, relative to the repository root."
  :group 'magit
  :type '(choice (const :tag "No message" nil) string))

(defface magit-log-head-label-wip
  '((((class color) (background light))
     :box t
     :background "Grey95"
     :foreground "LightSkyBlue3")
    (((class color) (background dark))
     :box t
     :background "Grey07"
     :foreground "LightSkyBlue4"))
  "Face for git-wip labels shown in log buffer."
  :group 'magit-faces)

(defun magit-log-get-wip-color (suffix)
  (list (concat "(WIP) " suffix)
        'magit-log-head-label-wip))

(defconst magit-wip-refs-namespace
  '("wip" magit-log-get-wip-color))

(define-minor-mode magit-wip-mode
  "Magit support for git-wip.

When this global minor mode is turned on and a file is saved inside a
writable git repository then it is also committed to a special Work In
Progress ref."
  :group 'magit
  :global t
  (cond (magit-wip-mode
         (add-hook 'after-save-hook 'magit-wip-save)
         (add-to-list 'magit-refs-namespaces magit-wip-refs-namespace 'append))
        (t
         (remove-hook 'after-save-hook 'magit-wip-save)
         (setq magit-refs-namespaces
               (delete magit-wip-refs-namespace magit-refs-namespaces)))))

(defun magit-wip-save ()
  (let* ((top-dir (magit-get-top-dir default-directory))
         (name (buffer-file-name))
         (spec `((?r . ,(file-relative-name name top-dir))
                 (?f . ,(buffer-file-name)))))
    (when (and top-dir (file-writable-p top-dir))
      (magit-run-git "wip" "save"
                     (format-spec magit-wip-commit-message spec)
                     "--editor" "--" name)
      (when magit-wip-echo-area-message
        (message (format-spec magit-wip-echo-area-message spec))))))

(provide 'magit-wip)
;;; magit-wip.el ends here
