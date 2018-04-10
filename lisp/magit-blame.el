;;; magit-blame.el --- blame support for Magit  -*- lexical-binding: t -*-

;; Copyright (C) 2012-2018  The Magit Project Contributors
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

;; Annotates each line in file-visiting buffer with information from
;; the revision which last modified the line.

;;; Code:

(require 'magit)

;;; Options

(defgroup magit-blame nil
  "Blame support for Magit."
  :link '(info-link "(magit)Blaming")
  :group 'magit-modes)

(defcustom magit-blame-heading-format "%-20a %C %s"
  "Format string used for blame headings.

The following placeholders are recognized:

  %H    hash
  %s    summary
  %a    author
  %A    author time
  %c    committer
  %C    committer time

The author and committer time formats can be specified with
`magit-blame-time-format'."
  :group 'magit-blame
  :type 'string)

(defcustom magit-blame-time-format "%F %H:%M"
  "Format for time strings in blame headings."
  :group 'magit-blame
  :type 'string)

(defcustom magit-blame-read-only t
  "Whether to initially make the blamed buffer read-only."
  :package-version '(magit . "2.13.0")
  :group 'magit-blame
  :type 'boolean)

(defcustom magit-blame-show-headings t
  "Whether to initially show blame block headings.
The headings can also be toggled locally using command
`magit-blame-toggle-headings'."
  :group 'magit-blame
  :type 'boolean)

(defcustom magit-blame-disable-modes '(fci-mode yascroll-bar-mode)
  "List of modes not compatible with Magit-Blame mode.
This modes are turned off when Magit-Blame mode is turned on,
and then turned on again when turning off the latter."
  :group 'magit-blame
  :type '(repeat (symbol :tag "Mode")))

(defcustom magit-blame-mode-lighter " Blame"
  "The mode-line lighter of the Magit-Blame mode."
  :group 'magit-blame
  :type '(choice (const :tag "No lighter" "") string))

(defcustom magit-blame-goto-chunk-hook
  '(magit-blame-maybe-update-revision-buffer
    magit-blame-maybe-show-message)
  "Hook run by `magit-blame-next-chunk' and `magit-blame-previous-chunk'."
  :package-version '(magit . "2.13.0")
  :group 'magit-blame
  :type 'hook
  :get 'magit-hook-custom-get
  :options '(magit-blame-maybe-update-revision-buffer
             magit-blame-maybe-show-message))

(defface magit-blame-heading
  '((((class color) (background light))
     :background "grey80"
     :foreground "black")
    (((class color) (background dark))
     :background "grey25"
     :foreground "white"))
  "Face for blame headings."
  :group 'magit-faces)

(defface magit-blame-summary
  '((t :inherit magit-blame-heading))
  "Face for commit summary in blame headings."
  :group 'magit-faces)

(defface magit-blame-hash
  '((t :inherit magit-blame-heading))
  "Face for commit hash in blame headings."
  :group 'magit-faces)

(defface magit-blame-name
  '((t :inherit magit-blame-heading))
  "Face for author and committer names in blame headings."
  :group 'magit-faces)

(defface magit-blame-date
  '((t :inherit magit-blame-heading))
  "Face for dates in blame headings."
  :group 'magit-faces)

;;; Mode

(defvar magit-blame-read-only-mode-map
  (let ((map (make-sparse-keymap)))
    (cond ((featurep 'jkl)
           (define-key map [return]    'magit-show-commit)
           (define-key map (kbd   "i") 'magit-blame-previous-chunk)
           (define-key map (kbd   "I") 'magit-blame-previous-chunk-same-commit)
           (define-key map (kbd   "k") 'magit-blame-next-chunk)
           (define-key map (kbd   "K") 'magit-blame-next-chunk-same-commit)
           (define-key map (kbd   "j") 'magit-blame)
           (define-key map (kbd   "l") 'magit-blame-reverse)
           (define-key map (kbd   "b") 'magit-blame-popup))
          (t
           (define-key map (kbd "C-m") 'magit-show-commit)
           (define-key map (kbd   "p") 'magit-blame-previous-chunk)
           (define-key map (kbd   "P") 'magit-blame-previous-chunk-same-commit)
           (define-key map (kbd   "n") 'magit-blame-next-chunk)
           (define-key map (kbd   "N") 'magit-blame-next-chunk-same-commit)
           (define-key map (kbd   "b") 'magit-blame)
           (define-key map (kbd   "f") 'magit-blame-reverse)
           (define-key map (kbd   "B") 'magit-blame-popup)))
    (define-key map (kbd   "t") 'magit-blame-toggle-headings)
    (define-key map (kbd   "q") 'magit-blame-quit)
    (define-key map (kbd "M-w") 'magit-blame-copy-hash)
    (define-key map (kbd "SPC") 'magit-diff-show-or-scroll-up)
    (define-key map (kbd "DEL") 'magit-diff-show-or-scroll-down)
    map)
  "Keymap for `magit-blame-read-only-mode'.")

(define-minor-mode magit-blame-read-only-mode
  "Provide keybindings for Magit-Blame mode.

This minor-mode provides the key bindings for Magit-Blame mode,
but only when Read-Only mode is also enabled because these key
bindings would otherwise conflict badly with regular bindings.

When both Magit-Blame mode and Read-Only mode are enabled, then
this mode gets automatically enabled too and when one of these
modes is toggled, then this mode also gets toggled automatically.

\\{magit-blame-read-only-mode-map}")

(defun magit-blame-put-keymap-before-view-mode ()
  "Put `magit-blame-read-only-mode' ahead of `view-mode' in `minor-mode-map-alist'."
  (--when-let (assq 'magit-blame-read-only--mode
                    (cl-member 'view-mode minor-mode-map-alist :key #'car))
    (setq minor-mode-map-alist
          (cons it (delq it minor-mode-map-alist))))
  (remove-hook 'view-mode-hook #'magit-blame-put-keymap-before-view-mode))

(add-hook 'view-mode-hook #'magit-blame-put-keymap-before-view-mode)

(defvar-local magit-blame-buffer-read-only nil)
(defvar-local magit-blame-cache nil)
(defvar-local magit-blame-disabled-modes nil)
(defvar-local magit-blame-process nil)
(defvar-local magit-blame-recursive-p nil)
(defvar-local magit-blame-type nil)
(defvar-local magit-blame-separator nil)

(define-minor-mode magit-blame-mode
  "Display blame information inline."
  :lighter magit-blame-mode-lighter
  (cond (magit-blame-mode
         (when (called-interactively-p 'any)
           (setq magit-blame-mode nil)
           (user-error
            (concat "Don't call `magit-blame-mode' directly; "
                    "instead use `magit-blame' or `magit-blame-popup'")))
         (add-hook 'after-save-hook     'magit-blame--run t t)
         (add-hook 'read-only-mode-hook 'magit-blame-toggle-read-only t t)
         (setq magit-blame-buffer-read-only buffer-read-only)
         (if (or magit-blame-read-only magit-buffer-file-name)
             (read-only-mode 1)
           (magit-blame-read-only-mode 1))
         (dolist (mode magit-blame-disable-modes)
           (when (and (boundp mode) (symbol-value mode))
             (funcall mode -1)
             (push mode magit-blame-disabled-modes)))
         (setq magit-blame-separator (magit-blame-format-separator)))
        (t
         (remove-hook 'after-save-hook     'magit-blame--run t)
         (remove-hook 'read-only-mode-hook 'magit-blame-toggle-read-only t)
         (unless magit-blame-buffer-read-only
           (read-only-mode -1))
         (magit-blame-read-only-mode -1)
         (dolist (mode magit-blame-disabled-modes)
           (funcall mode 1))
         (when (process-live-p magit-blame-process)
           (kill-process magit-blame-process))
         (magit-blame--clear-overlays))))

(defun magit-blame--clear-overlays ()
  (save-excursion
    (save-restriction
      (widen)
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (overlay-get ov 'magit-blame)
          (delete-overlay ov))))))

(defun magit-blame-toggle-read-only ()
  (magit-blame-read-only-mode (if buffer-read-only 1 -1)))

(defun auto-revert-handler--unless-magit-blame-mode ()
  "If Magit-Blame mode is on, then do nothing.  See #1731."
  magit-blame-mode)

(advice-add 'auto-revert-handler :before-until
            'auto-revert-handler--unless-magit-blame-mode)

;;; Popup

;;;###autoload (autoload 'magit-blame-popup "magit-blame" nil t)
(magit-define-popup magit-blame-popup
  "Popup console for blame commands."
  :man-page "git-blame"
  :switches '((?w "Ignore whitespace" "-w")
              (?r "Do not treat root commits as boundaries" "--root"))
  :options  '((?M "Detect lines moved or copied within a file" "-M")
              (?C "Detect lines moved or copied between files" "-C"))
  :actions  '((?b "Show blob touching these lines" magit-blame)
              (?r (lambda ()
                    (with-current-buffer magit-pre-popup-buffer
                      (and (not buffer-file-name)
                           (propertize "Show last blob with these lines"
                                       'face 'default))))
                  magit-blame-reverse)
              (?h "Toggle chunk headings" magit-blame-toggle-headings))
  :default-arguments '("-w")
  :max-action-columns 1
  :default-action 'magit-blame)

;;; Chunks

(defclass magit-blame-chunk ()
  (;; <orig-rev> <orig-line> <final-line> <num-lines>
   (orig-rev   :initarg :orig-rev)
   (orig-line  :initarg :orig-line)
   (final-line :initarg :final-line)
   (num-lines  :initarg :num-lines)
   ;; previous <prev-rev> <prev-file>
   (prev-rev   :initform nil)
   (prev-file  :initform nil)
   ;; filename <orig-file>
   (orig-file)))

(defun magit-current-blame-chunk ()
  (magit-blame-chunk-at (point)))

(defun magit-blame-chunk-at (pos)
  (--any (overlay-get it 'magit-blame)
         (overlays-at pos)))

(defun magit-blame-overlay-at (&optional pos)
  (--first (overlay-get it 'magit-blame)
           (overlays-at (or pos (point)))))

;;; Process

(defun magit-blame--run ()
  (magit-with-toplevel
    (unless magit-blame-mode
      (magit-blame-mode 1))
    (message "Blaming...")
    (magit-blame-run-process
     (or magit-buffer-refname magit-buffer-revision)
     (magit-file-relative-name nil (not magit-buffer-file-name))
     (if (memq magit-blame-type '(final removal))
         (cons "--reverse" (magit-blame-arguments))
       (magit-blame-arguments))
     (list (line-number-at-pos (window-start))
           (line-number-at-pos (1- (window-end nil t)))))
    (set-process-sentinel magit-this-process
                          'magit-blame-process-quickstart-sentinel)))

(defun magit-blame-run-process (revision file args &optional lines)
  (let ((process (magit-parse-git-async
                  "blame" "--incremental" args
                  (and lines (list "-L" (apply #'format "%s,%s" lines)))
                  revision "--" file)))
    (set-process-filter   process 'magit-blame-process-filter)
    (set-process-sentinel process 'magit-blame-process-sentinel)
    (process-put process 'arguments (list revision file args))
    (setq magit-blame-cache (make-hash-table :test 'equal))
    (setq magit-blame-process process)))

(defun magit-blame-process-quickstart-sentinel (process event)
  (when (memq (process-status process) '(exit signal))
    (magit-blame-process-sentinel process event t)
    (magit-blame-assert-buffer process)
    (with-current-buffer (process-get process 'command-buf)
      (when magit-blame-mode
        (let ((default-directory (magit-toplevel)))
          (apply #'magit-blame-run-process
                 (process-get process 'arguments)))))))

(defun magit-blame-process-sentinel (process _event &optional quiet)
  (let ((status (process-status process)))
    (when (memq status '(exit signal))
      (kill-buffer (process-buffer process))
      (if (and (eq status 'exit)
               (zerop (process-exit-status process)))
          (unless quiet
            (message "Blaming...done"))
        (magit-blame-assert-buffer process)
        (with-current-buffer (process-get process 'command-buf)
          (magit-blame-mode -1))
        (message "Blaming...failed")))))

(defun magit-blame-process-filter (process string)
  (internal-default-process-filter process string)
  (let ((buf  (process-get process 'command-buf))
        (pos  (process-get process 'parsed))
        (mark (process-mark process))
        cache)
    (with-current-buffer buf
      (setq cache magit-blame-cache))
    (with-current-buffer (process-buffer process)
      (goto-char pos)
      (let (end rev chunk alist)
        (while (and (< (point) mark)
                    (save-excursion
                      (setq end (re-search-forward "^filename .+\n" nil t))))
          (looking-at "^\\(.\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)")
          (setq chunk (magit-blame-chunk
                       :orig-rev   (setq rev         (match-string 1))
                       :orig-line  (string-to-number (match-string 2))
                       :final-line (string-to-number (match-string 3))
                       :num-lines  (string-to-number (match-string 4))))
          (forward-line)
          (while (< (point) end)
            (cond ((looking-at "^filename \\(.+\\)")
                   (oset chunk orig-file (match-string 1)))
                  ((looking-at "^previous \\(.\\{40\\}\\) \\(.+\\)")
                   (oset chunk prev-rev  (match-string 1))
                   (oset chunk prev-file (match-string 2)))
                  ((looking-at "^\\([^ ]+\\) \\(.+\\)")
                   (push (cons (match-string 1)
                               (match-string 2)) alist)))
            (forward-line))
          (if alist
              (puthash rev alist cache)
            (setq alist (gethash rev cache)))
          (magit-blame-make-overlay buf chunk alist)
          (setq alist nil)
          (process-put process 'parsed (point)))))))

(defun magit-blame-assert-buffer (process)
  (unless (buffer-live-p (process-get process 'command-buf))
    (kill-process process)
    (user-error "Buffer being blamed has been killed")))

;;; Display

(defun magit-blame-make-overlay (buf chunk alist)
  (with-current-buffer buf
    (with-slots (orig-rev final-line num-lines) chunk
      (let ((ov (save-excursion
                  (save-restriction
                    (widen)
                    (goto-char (point-min))
                    (forward-line (1- final-line))
                    (--when-let (magit-blame-overlay-at)
                      (delete-overlay it))
                    (make-overlay (point)
                                  (progn (forward-line num-lines)
                                         (point))))))
            (heading (cdr (assq 'heading alist))))
        (unless heading
          (setq heading (magit-blame-format-heading orig-rev alist))
          (nconc alist (list (cons 'heading heading))))
        (overlay-put ov 'magit-blame chunk)
        (overlay-put ov 'magit-blame-heading heading)
        (overlay-put ov 'before-string
                     (if magit-blame-show-headings
                         heading
                       magit-blame-separator))))))

(defun magit-blame-format-separator ()
  (propertize
   (concat (propertize " "  'display '(space :height (2)))
           (propertize "\n" 'line-height t))
   'face (list :background (face-attribute 'magit-blame-heading :background))))

(defun magit-blame-format-heading (rev alist)
  (if (equal rev "0000000000000000000000000000000000000000")
      (propertize "Not Yet Committed\n" 'face 'magit-blame-heading)
    (magit--format-spec
     (propertize (concat magit-blame-heading-format "\n")
                 'face 'magit-blame-heading)
     `((?H . ,(propertize rev 'face 'magit-blame-hash))
       (?s . ,(propertize (cdr (assoc "summary" alist))
                          'face 'magit-blame-summary))
       (?a . ,(propertize (cdr (assoc "author" alist))
                          'face 'magit-blame-name))
       (?c . ,(propertize (cdr (assoc "committer" alist))
                          'face 'magit-blame-name))
       (?A . ,(propertize (magit-blame-format-time-string
                           (cdr (assoc "author-time" alist))
                           (cdr (assoc "author-tz" alist)))
                          'face 'magit-blame-date))
       (?C . ,(propertize (magit-blame-format-time-string
                           (cdr (assoc "committer-time" alist))
                           (cdr (assoc "committer-tz" alist)))
                          'face 'magit-blame-date))))))

(defun magit-blame-format-time-string (time tz)
  (setq time (string-to-number time))
  (setq tz   (string-to-number tz))
  (format-time-string
   magit-blame-time-format
   (seconds-to-time (+ time (* (/ tz 100) 60 60) (* (% tz 100) 60)))))

(defun magit-blame-maybe-show-message ()
  (unless magit-blame-show-headings
    (let ((message-log-max 0))
      (--if-let (cdr (assq 'heading
                           (gethash (oref (magit-current-blame-chunk) orig-rev)
                                    magit-blame-cache)))
          (message "%s" (substring it 0 -1))
        (message "Commit data not available yet.  Still blaming.")))))

;;; Commands

;;;###autoload
(defun magit-blame ()
  "For each line show the revision that last touched it."
  (interactive)
  (magit-blame--pre-blame-assert 'addition)
  (magit-blame--pre-blame-setup  'addition)
  (magit-blame--run))

;;;###autoload
(defun magit-blame-reverse ()
  "For each line show the last revision in which a line still exists."
  (interactive)
  (unless magit-buffer-file-name
    (user-error "Only blob buffers can be blamed in reverse"))
  (magit-blame--pre-blame-assert 'final)
  (magit-blame--pre-blame-setup  'final)
  (magit-blame--run))

(defun magit-blame--pre-blame-assert (type)
  (unless (magit-toplevel)
    (magit--not-inside-repository-error))
  (if (and magit-blame-mode
           (eq type magit-blame-type))
      (-if-let (chunk (magit-current-blame-chunk))
          (unless (oref chunk prev-rev)
            (user-error "Chunk has no further history"))
        (user-error "Commit data not available yet.  Still blaming."))
    (unless (magit-file-relative-name nil (not magit-buffer-file-name))
      (if buffer-file-name
          (user-error "Buffer isn't visiting a tracked file")
        (user-error "Buffer isn't visiting a file")))))

(defun magit-blame--pre-blame-setup (type)
  (when magit-blame-mode
    (if (eq type magit-blame-type)
        (let ((show-headings magit-blame-show-headings))
          (magit-blame-visit-prev-file)
          (setq-local magit-blame-show-headings show-headings)
          (setq-local magit-blame-recursive-p t)
          ;; Set window-start for the benefit of quickstart.
          (redisplay))
      (magit-blame--clear-overlays)))
  (setq magit-blame-type type))

(defun magit-blame-visit-prev-file ()
  "Visit the blob before the one that added the current chunk."
  (interactive)
  (with-slots (prev-rev prev-file orig-line)
      (magit-current-blame-chunk)
    (unless prev-rev
      (user-error "Chunk has no further history"))
    (magit-with-toplevel
      (magit-find-file prev-rev prev-file))
    ;; TODO Adjust line like magit-diff-visit-file.
    (goto-char (point-min))
    (forward-line (1- orig-line))))

(defun magit-blame-visit-orig-file ()
  "Visit the blob that added the current chunk."
  (interactive)
  (with-slots (orig-rev orig-file orig-line)
      (magit-current-blame-chunk)
    (magit-with-toplevel
      (magit-find-file orig-rev orig-file))
    (goto-char (point-min))
    (forward-line (1- orig-line))))

(defun magit-blame-quit ()
  "Turn off Magit-Blame mode.
If the buffer was created during a recursive blame,
then also kill the buffer."
  (interactive)
  (kill-local-variable 'magit-blame-type)
  (if magit-blame-recursive-p
      (kill-buffer)
    (magit-blame-mode -1)))

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
  (-if-let (rev (oref (magit-current-blame-chunk) orig-rev))
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
              (when (equal (oref (magit-blame-chunk-at pos) orig-rev) rev)
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
  (setq-local magit-blame-show-headings (not magit-blame-show-headings))
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

(defun magit-blame-copy-hash ()
  "Save hash of the current chunk's commit to the kill ring.

When the region is active, then save the region's content
instead of the hash, like `kill-ring-save' would."
  (interactive)
  (if (use-region-p)
      (copy-region-as-kill nil nil 'region)
    (kill-new (message "%s" (oref (magit-current-blame-chunk) orig-rev)))))

;;; Utilities

(defun magit-blame-maybe-update-revision-buffer ()
  (unless magit--update-revision-buffer
    (setq magit--update-revision-buffer nil)
    (-when-let* ((chunk  (magit-current-blame-chunk))
                 (commit (oref chunk orig-rev))
                 (buffer (magit-mode-get-buffer 'magit-revision-mode nil t)))
      (setq magit--update-revision-buffer (list commit buffer))
      (run-with-idle-timer
       magit-update-other-window-delay nil
       (lambda ()
         (pcase-let ((`(,rev ,buf) magit--update-revision-buffer))
           (setq magit--update-revision-buffer nil)
           (when (buffer-live-p buf)
             (let ((magit-display-buffer-noselect t))
               (apply #'magit-show-commit rev (magit-diff-arguments))))))))))

(provide 'magit-blame)
;;; magit-blame.el ends here
