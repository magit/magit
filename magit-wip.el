;;; magit-wip.el --- git-wip plug-in for Magit

;; Copyright (C) 2012-2014  The Magit Project Developers
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Keywords: vc tools
;; Package: magit

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

;; To enable `magit-wip-save-mode' enable `global-magit-wip-save-mode'
;; and use the Magit extension mechanism to select the repositories in
;; which you want to use a work-in-progress ref.
;;
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

(defun magit-wip-mode (&rest ignore)
  (message "magit-wip-mode is obsolete and doesn't do anything"))
(make-obsolete 'magit-wip-mode "This mode is a noop now" "2.0.0")

;;; Options

(defgroup magit-wip nil
  "Git-Wip support for Magit."
  :group 'magit-extensions)

(defcustom magit-wip-commit-message "WIP %r"
  "Commit message for git-wip commits.

The following `format'-like specs are supported:
%f the full name of the file being saved
%g the root of the git repository
%r the name of the file being saved,
   relative to the repository root."
  :group 'magit-wip
  :type 'string)

(defcustom magit-wip-echo-area-message "Wrote %f (wip)"
  "Message shown in the echo area after creating a git-wip commit.

The following `format'-like specs are supported:
%f the full name of the file being saved
%g the root of the git repository
%r the name of the file being saved,
   relative to the repository root."
  :group 'magit-wip
  :type '(choice (const :tag "No message" nil) string))

(defvar magit-wip-save-mode-lighter " Wip")

;;; Mode

;;;###autoload
(define-minor-mode magit-wip-save-mode
  "Magit support for committing to a work-in-progress ref.

When this minor mode is turned on and a file is saved inside a
writable git repository then it is also committed to a special
work-in-progress ref."
  :lighter magit-wip-save-mode-lighter
  (if magit-wip-save-mode
      (add-hook  'after-save-hook 'magit-wip-save t t)
    (remove-hook 'after-save-hook 'magit-wip-save t)))

;;;###autoload
(define-globalized-minor-mode global-magit-wip-save-mode
  magit-wip-save-mode turn-on-magit-wip-save
  :group 'magit-wip)

(defun turn-on-magit-wip-save ()
  "Conditionally turn on magit-wip-save-mode.

Turn on magit-wip-save-mode if the buffer is a file in a git
repository where wip-save is enabled in git config.

You can activate it with git config magit.extension wip-save."
  (when (and (buffer-file-name)
             (magit-get-top-dir)
             (member "wip-save" (magit-get-all "magit.extension")))
    (if (magit-git-success "wip" "-h")
        (magit-wip-save-mode 1)
      (message "Git command 'git wip' cannot be found"))))

(defun magit-wip-save ()
  (let* ((filename (expand-file-name (file-truename (buffer-file-name))))
         (filedir  (file-name-directory filename))
         (toplevel (magit-get-top-dir filedir))
         (blobname (file-relative-name filename toplevel))
         (spec `((?f . ,filename)
                 (?r . ,blobname)
                 (?g . ,toplevel))))
    (when (and toplevel (file-writable-p toplevel)
               (not (member blobname
                            (let ((default-directory filedir))
                              (magit-git-lines
                               "ls-files" "--other" "--ignored"
                               "--exclude-standard" "--full-name")))))
      (magit-run-git "wip" "save"
                     (format-spec magit-wip-commit-message spec)
                     "--editor" "--" filename)
      (when magit-wip-echo-area-message
        (message (format-spec magit-wip-echo-area-message spec))))))

(provide 'magit-wip)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-wip.el ends here
