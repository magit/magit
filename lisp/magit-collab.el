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

;;; Code:

(require 'magit)
(require 'ghub)

;;; Utilities

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
                    (string-match-p (regexp-quote (ghub--host)) host))
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

(provide 'magit-collab)
;;; magit-collab.el ends here
