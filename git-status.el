;;; git-status -- control git from Emacs.

;; Copyright (C) 2008  Marius Vollmer
;;
;; Git-status is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; Git-status is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Introduction

;; Invoking the git-status function will show a buffer with the
;; current status of the current git repository and its checkout.
;; That buffer offers key bindings for manipulating the status in
;; simple ways.
;;
;; The status buffer mainly shows the difference between the working
;; tree and the index, and the difference between the index and the
;; current HEAD.  You can 'stage' individual hunks from the working
;; tree to the index, and you can commit the index.  The commit
;; message needs to be prepared in a special area of the status buffer
;; before committing.
;;
;; The status buffer also supports resolving conflicts.
;;
;; Maybe there will be some support for history browsing in the
;; future.

;;; Utilities

(defun gits-shell (cmd &rest args)
  (let ((str (shell-command-to-string (apply 'format cmd args))))
    (if (string= str "")
	nil
      (if (equal (elt str (- (length str) 1)) ?\n)
	  (substring str 0 (- (length str) 1))
	str))))

(defun gits-get-top-dir (cwd)
  (let* ((cwd (expand-file-name cwd))
	 (git-dir (gits-shell "cd '%s' && git-rev-parse --git-dir 2>/dev/null"
			      cwd)))
    (if git-dir
	(file-name-as-directory (or (file-name-directory git-dir) cwd))
      nil)))

(defun gits-get (key)
  (gits-shell "git-config %s" key))

;;; Main

(defun git-status (cwd)
  (interactive "DDirectory: ")
  (let ((dir (gits-get-top-dir cwd)))
    (or dir
	(error "Not a git repository."))
    (let ((buf (get-buffer-create "*git-status*")))
      (switch-to-buffer buf)
      (save-excursion
	(set-buffer buf)
	(setq default-directory dir)
	(erase-buffer)
	(insert (format "Repository:  %s\n"
			(abbreviate-file-name default-directory)))
	(insert (format "Branch:      %s\n"
			(gits-shell "git-symbolic-ref -q HEAD")))
	(let ((origin (gits-get "remote.origin.url")))
	  (if origin
	      (insert (format "Origin:      %s\n" origin))))
	(insert "\n")
	(insert "Local changes:\n")
	(call-process-shell-command "git diff" nil t)
	(insert "\n")
	(insert "Staged changes:\n")
	(call-process-shell-command "git diff --cached" nil t)))))

(defun git-status-here ()
  (interactive)
  (git-status default-directory))
