;;; magit-compat.el --- compatibility code for Magit

;; Copyright (C) 2013  The Magit Project Developers.
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

;; Author: Jonas Bernoulli <jonas@bernoul.li>

;; Contains code from GNU Emacs <https://www.gnu.org/software/emacs/>,
;; released under the GNU General Public License version 3 or later.

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

;; This file contains code needed for compatibility
;; with older versions of GNU Emacs and Git.

;; Magit requires at least GNU Emacs 23.2.
;; The minimal Git version is unknown at this point.

;;; Code:

(declare-function magit-git-exit-code 'magit)

;;; Old Emacsen

(eval-and-compile

  ;; Added in Emacs 24.3.
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      (list 'set (list 'make-local-variable (list 'quote var)) val)))

  ;; Added in Emacs 24.3.
  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional docstring)
      "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
      (declare (debug defvar) (doc-string 3))
      (list 'progn (list 'defvar var val docstring)
            (list 'make-variable-buffer-local (list 'quote var)))))

  ;; Added in Emacs 23.3.
  (unless (fboundp 'string-prefix-p)
    (defun string-prefix-p (str1 str2 &optional ignore-case)
      "Return non-nil if STR1 is a prefix of STR2.
If IGNORE-CASE is non-nil, the comparison is done without paying attention
to case differences."
      (eq t (compare-strings str1 nil nil
                             str2 0 (length str1) ignore-case))))

  ;; Added in Emacs 23.3.
  (unless (fboundp 'string-match-p)
    (defun string-match-p (regexp string &optional start)
      "Same as `string-match' but don't change the match data."
      (let ((inhibit-changing-match-data t))
        (string-match regexp string start))))
  )

;;; Old Git
;;;; Common

(defvar-local magit-have-config-param 'unset)
(put 'magit-have-config-param 'permanent-local t)

(defun magit-configure-have-config-param ()
  (when (eq magit-have-config-param 'unset)
    (setq magit-have-config-param
          (= 0 (magit-git-exit-code "-c" "g.o=v" "config" "g.o")))))

;;;; Config

(defvar-local magit-have-graph 'unset)
(defvar-local magit-have-decorate 'unset)
(defvar-local magit-have-abbrev 'unset)
(defvar-local magit-have-revlist-count 'unset)

(put 'magit-have-graph 'permanent-local t)
(put 'magit-have-decorate 'permanent-local t)
(put 'magit-have-abbrev 'permanent-local t)
(put 'magit-have-revlist-count 'permanent-local t)

(defun magit-configure-have-graph ()
  (when (eq magit-have-graph 'unset)
    (setq magit-have-graph
          (= 0 (magit-git-exit-code "log" "--graph" "-n" "0")))))

(defun magit-configure-have-decorate ()
  (when (eq magit-have-decorate 'unset)
    (setq magit-have-decorate
          (= 0 (magit-git-exit-code "log" "--decorate=full" "-n" "0")))))

(defun magit-configure-have-abbrev ()
  (when (eq magit-have-abbrev 'unset)
    (setq magit-have-abbrev
          (= 0 (magit-git-exit-code "log" "--no-abbrev-commit" "-n" "0")))))

(defun magit-configure-have-revlist-count ()
  (when (eq magit-have-revlist-count 'unset)
    (setq magit-have-revlist-count
          (= 0 (magit-git-exit-code
                "rev-list" "--count" "--left-right" "HEAD")))))

(provide 'magit-compat)
;;; magit-compat.el ends here
