;;; magit-inotify.el --- Refresh status buffer if git tree changes -*- lexical-binding:t -*-

;; Copyright (C) 2013 Rüdiger Sonderfeld

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module comes with a minor mode `magit-inotify' which tracks changes in
;; the source tree using `inotify' and refreshes the magit status buffer.
;; Emacs 24.4 is required to make this work because of the inotify support.

;;; Code:

(require 'magit)
(require 'cl-lib)
(require 'inotify)

(defgroup magit-inotify nil
  "Refresh status buffer if source tree changes"
  :prefix "magit-inotify"
  :group 'magit)

(defun magit-inotify--directories ()
  "List all directories containing files watched by git."
  ;; TODO: add .git directory?
  (cons
   default-directory
   (cl-remove-duplicates
    (cl-loop for file in (magit-git-lines "ls-files")
             with tmp
             do (setq tmp (file-name-directory file))
             when tmp
             collect (expand-file-name tmp))
    :test #'string=)))

(defvar magit-inotify-data (make-hash-table)
  "A hash table to map watch-descriptors to a list (DIRECTORY STATUS-BUFFER).")

(defun magit-inotify--callback (ev)
  "Handle inotify callbacks.
Argument EV contains the watch data."
  (let* ((wd (car ev))
         (data (gethash wd magit-inotify-data))
         (buffer (cadr data)))
    (if (buffer-live-p buffer)
     (with-current-buffer buffer
       (magit-refresh))
     (inotify-rm-watch wd)
     (remhash wd magit-inotify-data))))

(defcustom magit-inotify-aspects '(modify create delete attrib onlydir)
  "Aspects to watch for.
See `inotify-add-watch'."
  :group 'magit-inotify
  :type '(repeat symbol))

(defun magit-inotify-start ()
  "Start watching for changes to the source tree using inotify.
This can only be called from a magit status buffer."
  (unless (derived-mode-p 'magit-status-mode)
    (error "Only works in magit status buffer"))
  (dolist (dir (magit-inotify--directories))
    (puthash (inotify-add-watch dir magit-inotify-aspects
                                #'magit-inotify--callback)
             (list dir (current-buffer))
             magit-inotify-data)))

(defun magit-inotify-stop ()
  "Stop watching for changes to the source tree using inotify.
This can only be called from a magit status buffer."
  (unless (derived-mode-p 'magit-status-mode)
    (error "Only works in magit status buffer"))
  (maphash
   (lambda (k v)
     (when (equal (cadr v) (current-buffer)) ; or use buffer?
       (inotify-rm-watch k)
       (remhash k magit-inotify-data)))
   magit-inotify-data))

(defun magit-inotify-watching-p ()
  "Return non-nil if current source tree is watched."
  (unless (derived-mode-p 'magit-status-mode)
    (error "Only works in magit status buffer"))
  (let (ret)
    (maphash (lambda (_k v)
               (when (and (not ret)
                          (equal (cadr v) (current-buffer)))
                 (setq ret t)))
             magit-inotify-data)
    ret))

(defcustom magit-inotify-lighter " MagitInotify"
  "String to display in mode line when `magit-inotify-mode' is active."
  :group 'magit-inotify
  :type 'string)

;;;###autoload
(define-minor-mode magit-inotify-mode
  "Refresh status buffer if source tree changes."
  :lighter magit-inotify-lighter
  :group 'magit-inotify
  (if magit-inotify-mode
      (magit-inotify-start)
    (magit-inotify-stop)))

(defun magit-inotify-stop-all ()
  "Stop watching for changes in all git trees."
  (interactive)
  (maphash
   (lambda (k _v) (inotify-rm-watch k))
   magit-inotify-data)
  (clrhash magit-inotify-data))

;;; Loading
(easy-menu-add-item magit-mode-menu nil
                    ["Auto Refresh" magit-inotify-mode
                     :style toggle
                     :selected (magit-inotify-watching-p)
                     :help "Use inotify to watch for changes in the source tree."]
                    "Refresh")

(defun magit-inotify-unload-function ()
  "Cleanup when module is unloaded."
  (easy-menu-remove-item magit-mode-menu nil "Auto Refresh"))

(provide 'magit-inotify)

;;; magit-inotify.el ends here
