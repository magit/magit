;;; magit-wip.el --- git-wip plug-in for Magit

;; Copyright (C) 2012  Jonas Bernoulli
;; Copyright (C) 2012  Ryan C. Thompson

;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

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

;; This requires the third-party git command "git wip" which is available
;; from https://github.com/bartman/git-wip.

;; The global mode `magit-wip-mode' provides highlighting of wip refs in
;; Magit buffers while the local mode `magit-wip-save-mode' commits to
;; such a ref when saving a file-visiting buffer.

;; To enable `magit-wip-save-mode' enable `global-magit-wip-save-mode'
;; and use the Magit extension mechanism to select the repositories in
;; which you want to use a work-in-progress ref.  Usually you also want
;; to enable `magit-wip-mode'.
;;
;;   (magit-wip-mode 1)
;;   (global-magit-wip-save-mode 1)
;;
;;   $ git config --add magit.extension wip-save           # or
;;   $ git config --global --add magit.extension wip-save

;; Note that `global-magit-wip-save-mode' is the only mode that uses the
;; extension mechanism for file-visiting buffers all other global modes
;; making use of it to turn on local modes in Magit buffers.

;;; Code:

(require 'magit)
(require 'format-spec)

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

;;; Magit Wip Save Mode.

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
      (add-hook  'after-save-hook 'magit-wip-save-safe t t)
    (remove-hook 'after-save-hook 'magit-wip-save-safe t)))

;;;###autoload
(define-globalized-minor-mode global-magit-wip-save-mode
  magit-wip-save-mode turn-on-magit-wip-save
  :group 'magit)

(defun turn-on-magit-wip-save ()
  (when (and (buffer-file-name)
             (magit-get-top-dir default-directory)
             (member "wip-save" (magit-get-all "magit.extension")))
    (if (= (magit-git-exit-code "wip" "-h") 0)
        (magit-wip-save-mode 1)
      (message "Git command 'git wip' cannot be found"))))

(defun magit-wip-save-safe ()
  (condition-case err
      (magit-wip-save)
    (error
     (message "Magit WIP got an error: %S" err))))

(defun magit-wip-save ()
  (let* ((top-dir (magit-get-top-dir default-directory))
         (name (file-truename (buffer-file-name)))
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
