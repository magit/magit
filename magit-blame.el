;;; magit-blame.el --- blame support for Magit

;; Copyright (C) 2012-2014  The Magit Project Developers
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

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

;; Annotates each line in file-visiting buffer with information from
;; the revision which last modified the line.

;;; Code:

(require 'magit)

;;; Options

(defgroup magit-blame nil
  "Blame support for Magit."
  :group 'magit-extensions)

(defcustom magit-blame-heading-format "%-20a %C %s"
  "Format used for blame headings."
  :group 'magit-blame
  :type 'regexp)

(defcustom magit-blame-time-format "%F %H:%M"
  "Format for time strings in blame headings."
  :group 'magit-blame
  :type 'string)

(defcustom magit-blame-show-headings t
  "Whether to initially show blame block headings.
The headings can also be toggled locally using command
`magit-blame-toggle-headings'."
  :group 'magit-blame
  :type 'boolean)

(defcustom magit-blame-disable-modes '(fci-mode yascroll-bar-mode)
  "List of modes not compatible with Magit-Blame mode.
This modes are turned off when Magit-Blame mode is turned on,
and then turned on again when turning on the latter."
  :group 'magit-blame
  :type '(repeat function))

(defcustom magit-blame-mode-lighter " Blame"
  "The mode-line lighter of the Magit-Blame mode."
  :group 'magit-blame
  :type '(choice (const :tag "No lighter" "") string))

(defvar magit-blame-log t)

(defface magit-blame-heading
  '((((class color) (background light))
     :background "grey80"
     :foreground "black")
    (((class color) (background dark))
     :background "grey25"
     :foreground "black"))
  "Face for blame headings."
  :group 'magit-faces)

(defface magit-blame-summary
  '((t :inherit magit-blame-heading))
  "Face used for commit summary in blame headings."
  :group 'magit-faces)

(defface magit-blame-hash
  '((t :inherit magit-blame-heading))
  "Face used for commit hash in blame headings."
  :group 'magit-faces)

(defface magit-blame-name
  '((t :inherit magit-blame-heading))
  "Face used for author and committer names in blame headings."
  :group 'magit-faces)

(defface magit-blame-date
  '((t :inherit magit-blame-heading))
  "Face used for dates in blame headings."
  :group 'magit-faces)

;;; Code

(defvar magit-blame-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-show-commit)
    (define-key map "\s" 'magit-diff-show-or-scroll-up)
    (define-key map "\d" 'magit-diff-show-or-scroll-down)
    (define-key map "b"  'magit-blame-popup)
    (define-key map "n"  'magit-blame-next-chunk)
    (define-key map "N"  'magit-blame-next-chunk-same-commit)
    (define-key map "p"  'magit-blame-previous-chunk)
    (define-key map "P"  'magit-blame-previous-chunk-same-commit)
    (define-key map "q"  'magit-blame-quit)
    (define-key map "t"  'magit-blame-toggle-headings)
    map))

(defvar-local magit-blame-buffer-read-only nil)
(defvar-local magit-blame-cache nil)
(defvar-local magit-blame-disabled-modes nil)
(defvar-local magit-blame-process nil)
(defvar-local magit-blame-recursive-p nil)
(defvar-local magit-blame-separator nil)

(define-minor-mode magit-blame-mode
  "Display blame information inline."
  :lighter 'magit-blame-mode-lighter
  (cond (magit-blame-mode
         (setq magit-blame-buffer-read-only buffer-read-only)
         (if (fboundp 'read-only-mode)
             (read-only-mode 1)
           (setq buffer-read-only t))
         (dolist (mode magit-blame-disable-modes)
           (when (and (boundp mode) (symbol-value mode))
             (funcall mode -1)
             (push mode magit-blame-disabled-modes)))
         (setq magit-blame-separator (magit-blame-format-separator))
         (unless (eq this-command 'magit-blame)
           (unless (magit-file-relative-name)
             (user-error "Current buffer has no associated file"))
           (let ((magit-blame-mode nil))
             (call-interactively 'magit-blame))))
        (t
         (unless magit-blame-buffer-read-only
           (if (fboundp 'read-only-mode)
               (read-only-mode -1)
             (setq buffer-read-only nil)))
         (dolist (mode magit-blame-disabled-modes)
           (funcall mode 1))
         (when (process-live-p magit-blame-process)
           (kill-process magit-blame-process))
         (save-excursion
           (save-restriction
             (widen)
             (dolist (ov (overlays-in (point-min) (point-max)))
               (when (overlay-get ov 'magit-blame)
                 (delete-overlay ov))))))))

(defadvice auto-revert-handler (around magit-blame activate)
  "If Magit-Blame mode is on, then turn it off, refresh the
buffer content and then also refresh the blame information,
by turning the mode on again."
  (if magit-blame-mode
      (progn (magit-blame-mode -1) ad-do-it
             (magit-blame-mode  1))
    ad-do-it))

(magit-define-popup magit-blame-popup
  "Popup console for blame commands."
  'magit-popups
  :man-page "git-blame"
  :switches '((?w "Ignore whitespace" "-w")
              (?r "Do not treat root commits as boundaries" "--root"))
  :options  '((?C "Detect lines moved or copied within a file" "-C" read-number)
              (?M "Detect lines moved or copied between files" "-M" read-number))
  :actions  '((?b "Blame" magit-blame))
  :default-arguments '("-w")
  :default-action 'magit-blame
  :use-prefix 'popup)

;;;###autoload
(defun magit-blame (revision file &optional args)
  "Display edit history of FILE up to REVISION.
Interactively blame the file being visited in the current buffer.
If the buffer visits a revision of that file, then blame up to
that revision, otherwise blame the file's full history, including
uncommitted changes.

If Magit-Blame mode is already turned on then blame recursively, by
visiting REVISION:FILE (using `magit-find-file'), where revision
is the revision before the revision that added the lines at
point.

ARGS is a list of additional arguments to pass to `git blame';
only arguments available from `magit-blame-popup' should be used."
  (interactive
   (let ((args (magit-blame-arguments)))
     (if magit-blame-mode
         (--if-let (magit-blame-chunk-get :previous-hash)
             (list it (magit-blame-chunk-get :previous-file)
                   args (magit-blame-chunk-get :previous-start))
           (user-error "Block has no further history"))
       (--if-let (magit-file-relative-name)
           (list (or magit-buffer-refname magit-buffer-revision) it args)
         (user-error "Buffer isn't visiting a file")))))
  (let ((show-headings magit-blame-show-headings)
        (default-directory (magit-get-top-dir)))
    (if revision
        (magit-find-file revision file)
      (find-file (expand-file-name file (magit-get-top-dir))))
    (unless magit-blame-mode
      (setq magit-blame-cache (make-hash-table :test 'equal))
      (setq this-command 'magit-blame)
      (magit-blame-mode 1)
      (setq-local magit-blame-show-headings show-headings)
      (message "Blaming...")
      (let ((magit-process-popup-time -1)
            (inhibit-magit-refresh t))
        (magit-run-git-async
         "blame" "--incremental" args
         "-L" (format "%s,%s"
                      (line-number-at-pos (window-start))
                      (line-number-at-pos (1- (window-end))))
         revision "--" file))
      (setq magit-blame-process magit-this-process)
      (set-process-filter magit-this-process 'magit-blame-process-filter)
      (set-process-sentinel
       magit-this-process
       `(lambda (process event)
          (when (memq (process-status process) '(exit signal))
            (magit-process-sentinel process event)
            (with-current-buffer (process-get process 'command-buf)
              (when magit-blame-mode
                (let ((magit-process-popup-time -1)
                      (inhibit-magit-refresh t)
                      (default-directory ,default-directory))
                  (magit-run-git-async "blame" "--incremental" ,@args
                                       ,revision "--" ,file))
                (setq magit-blame-process magit-this-process)
                (set-process-filter
                 magit-this-process 'magit-blame-process-filter)
                (set-process-sentinel
                 magit-this-process 'magit-blame-process-sentinel)))))))))

(defun magit-blame-process-sentinel (process event)
  (let ((status (process-status process)))
    (when (memq status '(exit signal))
      (magit-process-sentinel process event)
      (if (eq status 'exit)
          (message "Blaming...done")
        (with-current-buffer (process-get process 'command-buf)
          (magit-blame-mode -1))
        (message "Blaming...failed")))))

(defun magit-blame-process-filter (process string)
  (when magit-blame-log
    (magit-process-filter process string))
  (--when-let (process-get process 'partial-line)
    (setq string (concat it string))
    (setf (process-get process 'partial-line) nil))
  (with-current-buffer (process-get process 'command-buf)
    (when magit-blame-mode
      (let ((chunk (process-get process 'chunk))
            (lines (split-string string "\n" t)))
        (unless (string-match-p "\n\\'" string)
          (process-put process 'chunk chunk)
          (process-put process 'partial-line (car (last lines)))
          (setq lines (butlast lines)))
        (dolist (line lines)
          (cond
           ((equal line ""))
           ((not chunk)
            (string-match
             "^\\(.\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)" line)
            (setq chunk
                  (list :hash (let ((hash (match-string 1 line)))
                                (unless (equal hash (make-string 40 ?0))
                                  hash))
                        :previous-start (string-to-number (match-string 2 line))
                        :start (string-to-number (match-string 3 line))
                        :lines (string-to-number (match-string 4 line)))))
           ((string-match "^filename \\(.+\\)" line)
            (let* ((hash (plist-get chunk :hash))
                   (file (match-string 1 line)))
              (--if-let (gethash hash magit-blame-cache)
                  (setq chunk (nconc chunk it))
                (plist-put chunk :filename file)
                (puthash hash chunk magit-blame-cache)))
            (magit-blame-make-overlay chunk)
            (setq chunk nil))
           ((string-match "^previous \\(.\\{40\\}\\) \\(.+\\)" line)
            (plist-put chunk :previous-hash (match-string 1 line))
            (plist-put chunk :previous-file (match-string 2 line)))
           ((string-match "^\\([^ ]+?-mail\\) <\\([^>]+\\)>" line)
            (plist-put chunk (intern (concat ":" (match-string 1 line)))
                       (string-to-number (match-string 2 line))))
           ((string-match "^\\([^ ]+?-\\(?:time\\|tz\\)\\) \\(.+\\)" line)
            (plist-put chunk (intern (concat ":" (match-string 1 line)))
                       (string-to-number (match-string 2 line))))
           ((string-match "^\\([^ ]+\\) \\(.+\\)" line)
            (plist-put chunk (intern (concat ":" (match-string 1 line)))
                       (match-string 2 line))))
          (process-put process 'chunk chunk))))))

(defun magit-blame-make-overlay (chunk)
  (let ((ov (save-excursion
              (save-restriction
                (widen)
                (goto-char (point-min))
                (forward-line (1- (plist-get chunk :start)))
                (--when-let (--first (overlay-get it 'magit-blame)
                                     (overlays-at (point)))
                  (delete-overlay it))
                (make-overlay (point)
                              (progn (forward-line
                                      (plist-get chunk :lines))
                                     (point))))))
        (heading (magit-blame-format-heading chunk)))
    (overlay-put ov 'magit-blame chunk)
    (overlay-put ov 'magit-blame-heading heading)
    (overlay-put ov 'before-string
                 (if magit-blame-show-headings
                     heading
                   magit-blame-separator))))

(defun magit-blame-format-separator ()
  (propertize
   (concat (propertize " "  'display '(space :height (2)))
           (propertize "\n" 'line-height t))
   'face (list :background (face-attribute 'magit-blame-heading :background))))

(defun magit-blame-format-heading (chunk)
  (format-spec
   (propertize (concat magit-blame-heading-format "\n")
               'face 'magit-blame-heading)
   `((?H . ,(propertize (or (plist-get chunk :hash) "")
                        'face 'magit-blame-hash))
     (?s . ,(propertize (plist-get chunk :summary)
                        'face 'magit-blame-summary))
     (?a . ,(propertize (plist-get chunk :author)
                        'face 'magit-blame-name))
     (?A . ,(propertize (magit-blame-format-time-string
                         magit-blame-time-format
                         (plist-get chunk :author-time)
                         (plist-get chunk :author-tz))
                        'face 'magit-blame-time))
     (?c . ,(propertize (plist-get chunk :committer)
                        'face 'magit-blame-name))
     (?C . ,(propertize (magit-blame-format-time-string
                         magit-blame-time-format
                         (plist-get chunk :committer-time)
                         (plist-get chunk :committer-tz))
                        'face 'magit-blame-time)))))

(defun magit-blame-format-time-string (format time tz)
  (format-time-string
   format (seconds-to-time (+ time (* (/ tz 100) 60 60) (* (% tz 100) 60)))))

(defun magit-blame-quit ()
  "Turn of Magit-Blame mode.
If the buffer was created during a recursive blame,
then also kill the buffer."
  (interactive)
  (if magit-blame-recursive-p
      (kill-buffer)
    (magit-blame-mode -1)))

(defvar magit-blame-goto-chunk-hook
  '(magit-log-maybe-show-commit))

(defun magit-blame-next-chunk ()
  "Move to the next chunk."
  (interactive)
  (--if-let (next-single-char-property-change (point) 'magit-blame)
      (progn (goto-char it)
             (run-hooks 'magit-blame-goto-chunk-hook))
    (user-error "No more chunks")))

(defun magit-blame-previous-chunk ()
  "Move to the previous chunk."
  (interactive)
  (--if-let (previous-single-char-property-change (point) 'magit-blame)
      (progn (goto-char it)
             (run-hooks 'magit-blame-goto-chunk-hook))
    (user-error "No more chunks")))

(defun magit-blame-next-chunk-same-commit (&optional previous)
  "Move to the next chunk from the same commit.\n\n(fn)"
  (interactive)
  (-if-let (hash (magit-blame-chunk-get :hash))
      (let ((pos (point)) ov)
        (save-excursion
          (while (and (not ov)
                      (not (= pos (if previous (point-min) (point-max))))
                      (setq pos (funcall
                                 (if previous
                                     'previous-single-char-property-change
                                   'next-single-char-property-change)
                                 pos 'magit-blame)))
            (--when-let (magit-blame-overlay-at pos)
              (when (equal (magit-blame-chunk-get :hash pos) hash)
                (setq ov it)))))
        (if ov
            (goto-char (overlay-start ov))
          (user-error "No more chunks from same commit")))
    (user-error "This chunk hasn't been blamed yet")))

(defun magit-blame-previous-chunk-same-commit ()
  "Move to the previous chunk from the same commit."
  (interactive)
  (magit-blame-next-chunk-same-commit 'previous-single-char-property-change))

(defun magit-blame-toggle-headings ()
  "Show or hide blame chunk headings."
  (interactive)
  (setq magit-blame-show-headings (not magit-blame-show-headings))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((next (next-single-char-property-change (point) 'magit-blame)))
          (--when-let (magit-blame-overlay-at (point))
            (overlay-put it 'before-string
                         (if magit-blame-show-headings
                             (overlay-get it 'magit-blame-heading)
                           magit-blame-separator)))
          (goto-char (or next (point-max))))))))

(defun magit-blame-chunk-get (key &optional pos)
  (--when-let (magit-blame-overlay-at pos)
    (plist-get (overlay-get it 'magit-blame) key)))

(defun magit-blame-overlay-at (&optional pos)
  (--first (overlay-get it 'magit-blame)
           (overlays-at (or pos (point)))))

;;; magit-blame.el ends soon
(provide 'magit-blame)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-blame.el ends here
