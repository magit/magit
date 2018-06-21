;;; magit-browse.el ---                           -*- lexical-binding: t -*-

;; Copyright (C) 2010-2018  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
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
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Commentary:
;;; Code:

(require 'magit)
(require 'magit-collab)

;; TODO This is based or `orgit-export-alist',
;; which should probably be merged into this.
(defcustom magit-browse-alist
  `(("github.com[:/]\\(.+?\\)\\(?:\\.git\\)?$"
     "https://github.com/%n"
     "https://github.com/%n/commits/%r"
     "https://github.com/%n/commit/%r")
    ("gitlab.com[:/]\\(.+?\\)\\(?:\\.git\\)?$"
     "https://gitlab.com/%n"
     "https://gitlab.com/%n/commits/%r"
     "https://gitlab.com/%n/commit/%r")
    ("bitbucket.org[:/]\\(.+?\\)\\(?:\\.git\\)?$"
     "https://bitbucket.org/%n"
     "https://bitbucket.org/%n/commits/branch/%r"
     "https://bitbucket.org/%n/commits/%r")
    ("orgmode.org[:/]\\(.+\\)$"
     "http://orgmode.org/cgit.cgi/%n"
     "http://orgmode.org/cgit.cgi/%n/log/?h=%r"
     "http://orgmode.org/cgit.cgi/%n/commit/?id=%r")
    ("git.kernel.org/pub/scm[:/]\\(.+\\)$"
     "http://git.kernel.org/cgit/%n"
     "http://git.kernel.org/cgit/%n/log/?h=%r"
     "http://git.kernel.org/cgit/%n/commit/?id=%r"))
  ""
  :group 'magit-extensions
  :type '(repeat (list :tag "Remote template"
                       (regexp :tag "Remote regexp")
                       (string :tag "Status format")
                       (string :tag "Log format" :format "%{%t%}:    %v")
                       (string :tag "Revision format"))))

;; TODO Like `magit-upstream-repository', but not just for Github.
(defun magit-browse--upstream-repository ()
  (let ((remote (or (magit-get "magit.upstream") "origin")))
    (unless (magit-remote-p remote)
      (error "No remote named `%s' exists (consider setting `magit.upstream')"
             remote))
    remote))

;;;###autoload
(defun magit-browse ()
  "Visit the thing at point on the Git forge using `browse-url'."
  (interactive)
  (cond
   ;; ((and (eq major-mode 'magit-status-mode)
   ;;       (not (magit-region-sections 'commit)))
   ;;  (magit-browse--status))
   ((eq major-mode 'magit-revision-mode)
    (magit-browse--rev (car magit-refresh-args)))
   ((and (derived-mode-p 'magit-mode)
         ;; (magit-region-sections 'commit)
         (magit-commit-at-point))
    (magit-browse--rev (oref (magit-current-section) value)))
   ((and (eq major-mode 'magit-log-mode)
         (not (magit-region-sections 'commit)))
    (magit-browse--log))))

(defun magit-browse--status ()
  (error "Not implemented"))

(defun magit-browse--rev (rev)
  (let* ((url (magit-get "remote" (magit-browse--upstream-repository) "url"))
         (elt (--first (string-match (car it) url)
                       magit-browse-alist)))
    (browse-url
     (format-spec (nth 3 elt)
                  `((?n . ,(match-string 1 url))
                    (?r . ,(magit-rev-verify-commit rev)))))))

(defun magit-browse--log ()
  (error "Not implemented"))

(provide 'magit-browse)
;;; magit-browse.el ends here
