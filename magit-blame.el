;;; magit-blame.el --- blame support for magit

;; Copyright (C) 2012  Rüdiger Sonderfeld
;; Copyright (C) 2012  Yann Hodique
;; Copyright (C) 2011  byplayer
;; Copyright (C) 2010  Alexander Prusov
;; Copyright (C) 2009  Tim Moore
;; Copyright (C) 2008  Linh Dang
;; Copyright (C) 2008  Marius Vollmer

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords:

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

;; This code has been backported from Egg (Magit fork) to Magit

;;; Code:

(eval-when-compile (require 'cl))
(require 'magit)

(defface magit-blame-header
  '((t :inherit magit-header))
  "Face for blame header."
  :group 'magit-faces)

(defface magit-blame-sha1
  '((t :inherit (magit-log-sha1
                 magit-blame-header)))
  "Face for blame sha1."
  :group 'magit-faces)

(defface magit-blame-culprit
  '((t :inherit magit-blame-header))
  "Face for blame culprit."
  :group 'magit-faces)

(defface magit-blame-time
  '((t :inherit magit-blame-header))
  "Face for blame time."
  :group 'magit-faces)

(defface magit-blame-subject
  '((t :inherit (magit-log-message magit-blame-header)))
  "Face for blame tag line."
  :group 'magit-faces)

(defconst magit-blame-map
  (let ((map (make-sparse-keymap "Magit:Blame")))
    (define-key map (kbd "l") 'magit-blame-locate-commit)
    (define-key map (kbd "RET") 'magit-blame-locate-commit)
    (define-key map (kbd "q") 'magit-blame-mode)
    (define-key map (kbd "n") 'magit-blame-next-chunk)
    (define-key map (kbd "p") 'magit-blame-previous-chunk)
    map)
  "Keymap for an annotated section.\\{magit-blame-map}")

(defvar magit-blame-buffer-read-only)
(make-variable-buffer-local 'magit-blame-buffer-read-only)

;;;###autoload
(define-minor-mode magit-blame-mode
  "Display blame information inline."
  :keymap magit-blame-map
  :lighter " blame"
  (unless (buffer-file-name)
    (error "Current buffer has no associated file!"))
  (when (and (buffer-modified-p)
             (y-or-n-p (format "save %s first? " (buffer-file-name))))
    (save-buffer))

  (if magit-blame-mode
      (progn
        (setq magit-blame-buffer-read-only buffer-read-only)
        (magit-blame-file-on (current-buffer))
        (set-buffer-modified-p nil)
        (setq buffer-read-only t))
    (magit-blame-file-off (current-buffer))
    (set-buffer-modified-p nil)
    (setq buffer-read-only magit-blame-buffer-read-only)))

(defun magit-blame-file-off (buffer)
  (save-excursion
    (save-restriction
      (with-current-buffer buffer
        (widen)
        (mapc (lambda (ov)
                (if (overlay-get ov :blame)
                    (delete-overlay ov)))
              (overlays-in (point-min) (point-max)))))))

(defun magit-blame-file-on (buffer)
  (magit-blame-file-off buffer)
  (save-excursion
    (with-current-buffer buffer
      (save-restriction
        (with-temp-buffer
          (magit-git-insert (list "blame" "--porcelain" "--"
                                  (file-name-nondirectory
                                   (buffer-file-name buffer))))
          (magit-blame-parse buffer (current-buffer)))))))

(defun magit-blame-locate-commit (pos)
  "Jump to a commit in the branch history from an annotated blame section."
  (interactive "d")
  (let ((overlays (overlays-at pos))
        sha1)
    (dolist (ov overlays)
      (if (overlay-get ov :blame)
          (setq sha1 (plist-get (nth 3 (overlay-get ov :blame)) :sha1))))
    (if sha1
        (magit-show-commit sha1))))

(defun magit-find-next-overlay-change (BEG END PROP)
  "Return the next position after BEG where an overlay matching a
property PROP starts or ends. If there are no matching overlay
boundaries from BEG to END, the return value is nil."
  (save-excursion
    (goto-char BEG)
    (catch 'found
      (flet ((overlay-change (pos)
                             (if (< BEG END) (next-overlay-change pos)
                               (previous-overlay-change pos)))
             (within-bounds-p (pos)
                              (if (< BEG END) (< pos END)
                                (> pos END))))
        (let ((ov-pos BEG))
          ;; iterate through overlay changes from BEG to END
          (while (within-bounds-p ov-pos)
            (let* ((next-ov-pos (overlay-change ov-pos))
                   ;; search for an overlay with a PROP property
                   (next-ov
                    (let ((overlays (overlays-at next-ov-pos)))
                      (while (and overlays
                                  (not (overlay-get (car overlays) PROP)))
                        (setq overlays (cdr overlays)))
                      (car overlays))))
              (if next-ov
                  ;; found the next overlay with prop PROP at next-ov-pos
                  (throw 'found next-ov-pos)
                ;; no matching overlay found, keep looking
                (setq ov-pos next-ov-pos)))))))))

(defun magit-blame-next-chunk (pos)
  "Go to the next blame chunk."
  (interactive "d")
  (let ((next-chunk-pos (magit-find-next-overlay-change pos (point-max) :blame)))
    (when next-chunk-pos
      (goto-char next-chunk-pos))))

(defun magit-blame-previous-chunk (pos)
  "Go to the previous blame chunk."
  (interactive "d")
  (let ((prev-chunk-pos (magit-find-next-overlay-change pos (point-min) :blame)))
    (when prev-chunk-pos
      (goto-char prev-chunk-pos))))

(defcustom magit-time-format-string "%Y-%m-%dT%T%z"
  "How to format time in magit-blame header."
  :group 'magit
  :type 'string)

(defun magit-blame-decode-time (unixtime &optional tz)
  "Decode UNIXTIME into (HIGH LOW) format.

The second argument TZ can be used to add the timezone in (-)HHMM
format to UNIXTIME.  UNIXTIME should be either a number
containing seconds since epoch or Emacs's (HIGH LOW
. IGNORED) format."
  (when (numberp tz)
    (unless (numberp unixtime)
      (setq unixtime (float-time unixtime)))
    (let* ((ptz (abs tz))
           (min (+ (* (/ ptz 100) 60)
                   (mod ptz 100))))
      (setq unixtime (+ (* (if (< tz 0) (- min) min) 60) unixtime))))

  (when (numberp unixtime)
    (setq unixtime (seconds-to-time unixtime)))
  unixtime)

(defun magit-blame-format-time-string (format &optional unixtime tz)
  "Use FORMAT to format the time UNIXTIME, or now if omitted.

UNIXTIME is specified as a number containing seconds since epoch
or Emacs's (HIGH LOW . IGNORED) format.  The optional argument TZ
can be used to set the time zone.  If TZ is a number it is
treated as a (-)HHMM offset to Universal Time.  If TZ is not
a number and non-nil the time is printed in UTC.  If TZ is nil
the local zime zone is used.  The format of the function is
similar to `format-time-string' except for %Z which is not
officially supported at the moment."
  (unless unixtime
    (setq unixtime (current-time)))
  (when (numberp tz) ;; TODO add support for %Z
    (setq format (replace-regexp-in-string "%z" (format "%+05d" tz) format)))
  (format-time-string format (magit-blame-decode-time unixtime tz) tz))

(defun magit-blame-parse (target-buf blame-buf)
  "Parse blame-info in buffer BLAME-BUF and decorate TARGET-BUF buffer."
  (save-match-data
    (let ((blank (propertize " " 'face 'magit-blame-header))
          (nl (propertize "\n" 'face 'magit-blame-header))
          (commit-hash (make-hash-table :test 'equal :size 577))
          commit commit-info old-line new-line num old-file subject author
          author-time author-timezone info ov beg end blame)
      (with-current-buffer blame-buf
        (goto-char (point-min))
        ;; search for a ful commit info
        (while (re-search-forward "^\\([0-9a-f]\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)$" nil t)
          (setq commit (match-string-no-properties 1)
                old-line (string-to-number
                          (match-string-no-properties 2))
                new-line (string-to-number
                          (match-string-no-properties 3))
                num (string-to-number
                     (match-string-no-properties 4)))
          ;; was this commit already seen (and stored in the hash)?
          (setq commit-info (gethash commit commit-hash))
          ;; Nope, this is the 1st time, the full commit-info follow.
          (unless commit-info
            (re-search-forward "^author \\(.+\\)$")
            (setq author (match-string-no-properties 1))
            (re-search-forward "^author-time \\(.+\\)$")
            (setq author-time (string-to-number
                               (match-string-no-properties 1)))
            (re-search-forward "^author-tz \\(.+\\)$")
            (setq author-timezone (string-to-number
                                   (match-string-no-properties 1)))
            (re-search-forward "^summary \\(.+\\)$")
            (setq subject (match-string-no-properties 1))
            (re-search-forward "^filename \\(.+\\)$")
            (setq old-file (match-string-no-properties 1))
            (setq commit-info (list :sha1 commit :author author
                                    :author-time author-time
                                    :author-timezone author-timezone
                                    :subject subject :file old-file))
            ;; save it in the hash
            (puthash commit commit-info commit-hash))
          ;; add the current blame-block into the list INFO.
          (setq info (cons (list old-line new-line num commit-info)
                           info))))
      ;; now do from beginning
      (setq info (nreverse info))
      (with-current-buffer target-buf
        ;; for every blame chunk
        (dolist (chunk info)
          (setq commit-info (nth 3 chunk)
                old-line (nth 0 chunk)
                new-line (nth 1 chunk)
                num (nth 2 chunk)
                commit (plist-get commit-info :sha1)
                author (plist-get commit-info :author)
                author-time (plist-get commit-info :author-time)
                author-timezone (plist-get commit-info :author-timezone)
                subject (plist-get commit-info :subject))

          (goto-char (point-min))
          (forward-line (1- new-line))

          (setq beg (line-beginning-position)
                end (save-excursion
                      (forward-line num)
                      (line-beginning-position)))
          ;; mark the blame chunk
          (put-text-property beg end :blame chunk)

          ;; make an overlay with blame info as 'before-string
          ;; on the current chunk.
          (setq ov (make-overlay beg end))
          (overlay-put ov :blame chunk)
          (setq blame (concat
                       (propertize (substring-no-properties commit 0 8)
                                   'face 'magit-blame-sha1)
                       blank
                       (propertize (format "%-20s" author)
                                   'face 'magit-blame-culprit)
                       blank
                       (propertize (magit-blame-format-time-string
                                    magit-time-format-string
                                    author-time author-timezone)
                                   'face 'magit-blame-time)
                       blank
                       (propertize subject 'face 'magit-blame-subject)
                       blank nl))
          (overlay-put ov 'before-string blame))))))

(provide 'magit-blame)
;;; magit-blame.el ends here
