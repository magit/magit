;;; magit-compat.el --- compatibility code for Magit

;; Copyright (C) 2013  Jonas Bernoulli

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

;;; Code:

(eval-when-compile (require 'server))

(declare-function server-running-p 'server)
(declare-function magit-git-exit-code 'magit)

;;; Old Emacsen
;;;; Without Prefix

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

  ;; Added in Emacs 22.2.
  (unless (fboundp 'declare-function)
    (defmacro declare-function (&rest args)))
  )

;;;; With Prefix

(eval-and-compile

  ;; Added in Emacs 23.2.
  (if (fboundp 'with-silent-modifications)
      (defalias 'magit-with-silent-modifications 'with-silent-modifications)
    (defmacro magit-with-silent-modifications (&rest body)
      "Execute body without changing `buffer-modified-p'.
Also, do not record undo information."
      `(set-buffer-modified-p
        (prog1 (buffer-modified-p)
          (let ((buffer-undo-list t)
                before-change-functions
                after-change-functions)
            ,@body)))))

  ;; Added in Emacs 22.2.
  (if (fboundp 'start-file-process)
      (defalias 'magit-start-process 'start-file-process)
    (defalias 'magit-start-process 'start-process))
  )

;; Added in Emacs 22.2.
(defun magit-use-region-p ()
  (if (fboundp 'use-region-p)
      (use-region-p)
    (and transient-mark-mode mark-active)))

;; Added in Emacs 22.2.
(defun magit-server-running-p ()
  "Test whether server is running.

Return values:
  nil              the server is definitely not running.
  t                the server seems to be running.
  something else   we cannot determine whether it's running without using
                   commands which may have to wait for a long time."
  (require 'server)
  (if (functionp 'server-running-p)
      (server-running-p)
    (condition-case nil
        (if server-use-tcp
            (with-temp-buffer
              (insert-file-contents-literally
               (expand-file-name server-name server-auth-dir))
              (or (and (looking-at "127\\.0\\.0\\.1:[0-9]+ \\([0-9]+\\)")
                       (assq 'comm
                             (process-attributes
                              (string-to-number (match-string 1))))
                       t)
                  :other))
          (delete-process
           (make-network-process
            :name "server-client-test" :family 'local :server nil :noquery t
            :service (expand-file-name server-name server-socket-dir)))
          t)
      (file-error nil))))

;; RECURSIVE has been introduced in Emacs 23.2, XEmacs still lacks it.
;; This is copied and adapted from `tramp-compat-delete-directory'
(defun magit-delete-directory (directory &optional recursive)
  "Compatibility function for `delete-directory'."
  (if (null recursive)
      (delete-directory directory)
    (condition-case nil
        (funcall 'delete-directory directory recursive)
      ;; This Emacs version does not support the RECURSIVE flag.  We
      ;; use the implementation from Emacs 23.2.
      (wrong-number-of-arguments
       (setq directory (directory-file-name (expand-file-name directory)))
       (if (not (file-symlink-p directory))
           (mapc (lambda (file)
                   (if (eq t (car (file-attributes file)))
                       (magit-delete-directory file recursive)
                     (delete-file file)))
                 (directory-files
                  directory 'full "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*")))
       (delete-directory directory)))))

;;; Old Git
;;;; Common

(defvar magit-have-config-param 'unset)
(make-variable-buffer-local 'magit-have-config-param)
(put 'magit-have-config-param 'permanent-local t)

(defun magit-configure-have-config-param ()
  (when (eq magit-have-config-param 'unset)
    (setq magit-have-config-param
          (= 0 (magit-git-exit-code "-c" "g.o=v" "config" "g.o")))))

;;;; Config

(defvar magit-have-graph 'unset)
(defvar magit-have-decorate 'unset)
(defvar magit-have-abbrev 'unset)
(defvar magit-have-grep-reflog 'unset)

(make-variable-buffer-local 'magit-have-graph)
(make-variable-buffer-local 'magit-have-decorate)
(make-variable-buffer-local 'magit-have-abbrev)
(make-variable-buffer-local 'magit-have-grep-reflog)

(put 'magit-have-graph 'permanent-local t)
(put 'magit-have-decorate 'permanent-local t)
(put 'magit-have-abbrev 'permanent-local t)
(put 'magit-have-grep-reflog 'permanent-local t)

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

(defun magit-configure-have-grep-reflog ()
  (when (eq magit-have-grep-reflog 'unset)
    (setq magit-have-grep-reflog
          (= 0 (magit-git-exit-code
                "log" "--walk-reflogs" "--grep-reflog" "." "-n" "0")))))

(provide 'magit-compat)
;;; magit-compat.el ends here
