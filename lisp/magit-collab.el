;;; magit-collab.el --- collaboration tools       -*- lexical-binding: t -*-

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

;; This library implements various collaboration tools.  These tools
;; are only early incarnation -- implementing collaboration tools is
;; a top priority for future development.

;; Currently these tools (including `magit-branch-pull-request', which
;; is defined elsewhere) only support Github, but support for other
;; Git forges as well as mailing list based collaboration is in
;; planning.

;;; Code:

(require 'magit)
(require 'ghub)

;;; Variables

(defvar magit-github-token-scopes '(repo)
  "The Github API scopes needed by Magit.

`repo' is the only required scope.  Without this scope none of
Magit's features that use the API work.  Instead of this scope
you could use `public_repo' if you are only interested in public
repositories.

`repo' Grants read/write access to code, commit statuses,
  invitations, collaborators, adding team memberships, and
  deployment statuses for public and private repositories
  and organizations.

`public_repo' Grants read/write access to code, commit statuses,
  collaborators, and deployment statuses for public repositories
  and organizations. Also required for starring public
  repositories.")

;;; Commands

;;;###autoload
(defun magit-browse-pull-request (pr)
  "Visit pull-request PR using `browse-url'.

Currently this only supports Github, but that restriction will
be lifted eventually to support other Git forges."
  (interactive (list (magit-read-pull-request "Visit pull request")))
  (browse-url (format "https://github.com/%s/pull/%s"
                      (--> pr
                           (cdr (assq 'base it))
                           (cdr (assq 'repo it))
                           (cdr (assq 'full_name it)))
                      (cdr (assq 'number pr)))))

;;; Utilities

(defun magit-read-pull-request (prompt)
  "Read a pull request from the user, prompting with PROMPT.
Return the Git forge's API response.  Currently this function
only supports Github, but that will change eventually."
  (let* ((origin (magit-upstream-repository))
         (id     (magit--forge-id origin))
         (fmtfun (lambda (pull-request)
                   (format "%s  %s"
                           (cdr (assq 'number pull-request))
                           (cdr (assq 'title  pull-request)))))
         (prs    (ghub-get (format "/repos/%s/pulls" id) nil :auth 'magit))
         (choice (magit-completing-read
                  prompt (mapcar fmtfun prs) nil nil nil nil
                  (let ((default (thing-at-point 'github-pull-request)))
                    (and default (funcall fmtfun default)))))
         (number (and (string-match "\\([0-9]+\\)" choice)
                      (string-to-number (match-string 1 choice)))))
    (and number
         ;; Don't reuse the pr from the list, it lacks some information
         ;; that is only returned when requesting a single pr.  #3371
         (ghub-get (format "/repos/%s/pulls/%s" id number)
                   nil :auth 'magit))))

(defun magit-upstream-repository ()
  "Return the remote name of the upstream repository.

If the Git variable `magit.upstream' is set, then return its
value.  Otherwise return \"origin\".  If the remote does not
exist, then raise an error."
  (let ((remote (or (magit-get "magit.upstream") "origin")))
    (unless (magit-remote-p remote)
      (error "No remote named `%s' exists (consider setting `magit.upstream')"
             remote))
    (unless (magit--github-remote-p remote)
      (error "Currently only Github is supported"))
    remote))

(defun magit--forge-id (remote)
  (let ((url (magit-get "remote" remote "url")))
    (and (string-match "\\([^:/]+/[^/]+?\\)\\(?:\\.git\\)?\\'" url)
         (match-string 1 url))))

(defconst magit--github-url-regexp "\
\\`\\(?:git://\\|git@\\|ssh://git@\\|https://\\)\
\\(.*?\\)[/:]\
\\(\\([^:/]+\\)/\\([^/]+?\\)\\)\
\\(?:\\.git\\)?\\'")

(defun magit--github-url-p (url)
  (save-match-data
    (and url
         (string-match magit--github-url-regexp url)
         (let ((host (match-string 1 url)))
           ;; Match values like "github.com-as-someone", which are
           ;; translated to just "github.com" according to settings
           ;; in "~/.ssh/config".  Theoretically this could result
           ;; in false-positives, but that's rather unlikely.  #3392
           (and (or (string-match-p (regexp-quote "github.com") host)
                    (string-match-p
                     (regexp-quote (car (split-string (ghub--host) "/")))
                     host))
                host)))))

(defun magit--github-remote-p (remote)
  (or (--when-let (magit-git-string "remote" "get-url" "--push" remote)
        (magit--github-url-p it))
      (--when-let (magit-git-string "remote" "get-url" "--all" remote)
        (magit--github-url-p it))))

(defun magit--github-url-equal (r1 r2)
  (or (equal r1 r2)
      (save-match-data
        (let ((n1 (and (string-match magit--github-url-regexp r1)
                       (match-string 2 r1)))
              (n2 (and (string-match magit--github-url-regexp r2)
                       (match-string 2 r2))))
          (and n1 n2 (equal n1 n2))))))

(defun magit--pullreq-from-upstream-p (pr)
  (let-alist pr
    (equal .head.repo.full_name
           .base.repo.full_name)))

(defun magit--pullreq-branch (pr &optional assert-new)
  (let-alist pr
    (let ((branch .head.ref))
      (when (and (not (magit--pullreq-from-upstream-p pr))
                 (or (not .maintainer_can_modify)
                     (magit-branch-p branch)))
        (setq branch (format "pr-%s" .number)))
      (when (and assert-new (magit-branch-p branch))
        (user-error "Branch `%s' already exists" branch))
      branch)))

(provide 'magit-collab)
;;; magit-collab.el ends here
