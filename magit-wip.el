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

;; This plug-in provides support for special work-in-progress refs.

;; This required the third-party git command `git wip' which is available
;; from https://github.com/bartman/git-wip.

;; The global mode `magit-wip-mode' provides highlighting of wip refs in
;; the Magit log buffers and `magit-wip-save-mode' commits to such a ref
;; when saving a file-visiting buffer.

;; Use `global-magit-wip-save-mode' to enable the latter globally.

;;; Code:

(require 'magit)

;;; Magit Wip Mode.

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

;;;###autoload
(define-minor-mode magit-wip-mode
  "In Magit log buffers; give wip refs a special appearance."
  :group 'magit
  :global t
  (if magit-wip-mode
      (add-to-list 'magit-refs-namespaces magit-wip-refs-namespace 'append)
    (setq magit-refs-namespaces
          (delete magit-wip-refs-namespace magit-refs-namespaces))))

;;; Magit Wip Record Mode.

(defcustom magit-wip-commit-message "WIP %r"
  "Commit message for git-wip commits.

The following `format'-like specs are supported:
%f the full name of the file being saved, and
%r the name of the file being saved, relative to the repository root
%g the root of the git repository."
  :group 'magit
  :type 'string)

(defcustom magit-wip-echo-area-message "Wrote %f (wip)"
  "Message shown in the echo area after creating a git-wip commit.

The following `format'-like specs are supported:
%f the full name of the file being saved, and
%r the name of the file being saved, relative to the repository root.
%g the root of the git repository."
  :group 'magit
  :type '(choice (const :tag "No message" nil) string))

(defvar magit-wip-save-mode-lighter " Wip")

;;;###autoload
(define-minor-mode magit-wip-save-mode
  "Magit support for committing to a work-in-progress ref.

When this minor mode is turned on and a file is saved inside a writable
git repository then it is also committed to a special work-in-progress
ref."
  :lighter magit-wip-save-mode-lighter
  (if magit-wip-save-mode
      (add-hook  'after-save-hook 'magit-wip-save t t)
    (remove-hook 'after-save-hook 'magit-wip-save t)))

;;;###autoload
(define-globalized-minor-mode global-magit-wip-save-mode
  magit-wip-save-mode turn-on-magit-wip-save
  :group 'magit)

(defun turn-on-magit-wip-save ()
  (when (and (buffer-file-name)
             (magit-get-top-dir default-directory))
    (magit-wip-save-mode 1)))

(defun magit-wip-save ()
  (let* ((top-dir (magit-get-top-dir default-directory))
         (name (buffer-file-name))
         (spec `((?r . ,(file-relative-name name top-dir))
                 (?f . ,(buffer-file-name))
                 (?g . ,top-dir))))
    (when (and top-dir (file-writable-p top-dir))
      (magit-run-git "wip" "save"
                     (format-spec magit-wip-commit-message spec)
                     "--editor" "--" name)
      (when magit-wip-echo-area-message
        (message (format-spec magit-wip-echo-area-message spec))))))

(provide 'magit-wip)
;;; magit-wip.el ends here
