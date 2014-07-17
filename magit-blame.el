;;; magit-blame.el --- blame support for Magit

;; Copyright (C) 2012-2014  The Magit Project Developers
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Package: magit

;; Contains code from Egg (Emacs Got Git) <https://github.com/byplayer/egg>,
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

;; Control git-blame from Magit.
;; This code has been backported from Egg (Magit fork) to Magit.

;;; Code:

(require 'cl-lib)
(require 'magit)

;;; Options

(defgroup magit-blame nil
  "Git-blame support for Magit."
  :group 'magit-extensions)

(defcustom magit-blame-ignore-whitespace t
  "Ignore whitespace when determining blame information."
  :group 'magit-blame
  :type 'boolean)

(defcustom magit-blame-time-format "%F %T %z"
  "How to format time in magit-blame header."
  :group 'magit-blame
  :type 'string)

(defface magit-blame-heading
  '((((class color) (background light)) :background "grey80" :foreground "black")
    (((class color) (background  dark)) :background "grey25" :foreground "black"))
  "Face for blame header."
  :group 'magit-faces)

(defface magit-blame-hash
  '((t :inherit (magit-hash magit-blame-heading)))
  "Face for blame sha1."
  :group 'magit-faces)

(defface magit-blame-culprit
  '((t :inherit magit-blame-heading))
  "Face for blame culprit."
  :group 'magit-faces)

(defface magit-blame-time
  '((t :inherit magit-blame-heading))
  "Face for blame time."
  :group 'magit-faces)

(defface magit-blame-subject
  '((t :inherit (magit-log-message magit-blame-heading)))
  "Face for blame tag line."
  :group 'magit-faces)

;;; Keymaps

(defvar magit-blame-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") 'magit-blame-locate-commit)
    (define-key map (kbd "RET") 'magit-blame-locate-commit)
    (define-key map (kbd "q") 'magit-blame-mode)
    (define-key map (kbd "n") 'magit-blame-next-chunk)
    (define-key map (kbd "p") 'magit-blame-previous-chunk)
    map)
  "Keymap for an annotated section.\\{magit-blame-map}")

(easy-menu-define magit-blame-mode-menu magit-blame-map
  "Magit blame menu"
  '("Blame"
    ["Locate Commit" magit-blame-locate-commit t]
    ["Next" magit-blame-next-chunk t]
    ["Previous" magit-blame-previous-chunk t]
    "---"
    ["Quit" magit-blame-mode t]))

;;; Mode

(defvar-local magit-blame-buffer-read-only nil)

;;;###autoload
(define-minor-mode magit-blame-mode
  "Display blame information inline."
  :keymap magit-blame-map
  :lighter " blame"
  (unless (buffer-file-name)
    (user-error "Current buffer has no associated file!"))
  (when (and (buffer-modified-p)
             (y-or-n-p (format "save %s first? " (buffer-file-name))))
    (save-buffer))

  (cond (magit-blame-mode
         (setq magit-blame-buffer-read-only buffer-read-only)
         (magit-blame-file-on (current-buffer))
         (set-buffer-modified-p nil)
         (setq buffer-read-only t))
        (t
         (magit-blame-file-off (current-buffer))
         (set-buffer-modified-p nil)
         (setq buffer-read-only magit-blame-buffer-read-only))))

(defun magit-blame-file-off (buffer)
  (save-excursion
    (save-restriction
      (with-current-buffer buffer
        (widen)
        (mapc (lambda (ov)
                (when (overlay-get ov :blame)
                  (delete-overlay ov)))
              (overlays-in (point-min) (point-max)))))))

(defun magit-blame-file-on (buffer)
  (magit-blame-file-off buffer)
  (save-excursion
    (with-current-buffer buffer
      (when (fboundp 'fci-mode)
        (fci-mode -1))
      (save-restriction
        (with-temp-buffer
          (apply 'magit-git-insert "blame" "--porcelain"
                 `(,@(and magit-blame-ignore-whitespace (list "-w")) "--"
                   ,(file-name-nondirectory (buffer-file-name buffer))))
          (magit-blame-parse buffer (current-buffer)))))))

;;; Commands

(defun magit-blame-locate-commit (pos)
  "Jump to a commit in the branch history from an annotated blame section."
  (interactive "d")
  (let ((overlays (overlays-at pos))
        sha1)
    (dolist (ov overlays)
      (when (overlay-get ov :blame)
        (setq sha1 (plist-get (nth 3 (overlay-get ov :blame)) :sha1))))
    (when sha1
      (magit-show-commit sha1))))

(defun magit-blame-next-chunk ()
  "Go to the next blame chunk."
  (interactive)
  (let ((next (next-single-property-change (point) :blame)))
    (when next
      (goto-char next))))

(defun magit-blame-previous-chunk ()
  "Go to the previous blame chunk."
  (interactive)
  (let ((prev (previous-single-property-change (point) :blame)))
    (when prev
      (goto-char prev))))

;;; Parse

(defun magit-blame-format-time-string (format time tz)
  (format-time-string
   format (seconds-to-time (+ time (* (/ tz 100) 60 60) (* (% tz 100) 60)))))

(defun magit-blame-parse (target-buf blame-buf)
  "Parse blame-info in buffer BLAME-BUF and decorate TARGET-BUF buffer."
  (save-match-data
    (let ((blank (propertize " " 'face 'magit-blame-heading))
          (nl (propertize "\n" 'face 'magit-blame-heading))
          (commit-hash (make-hash-table :test 'equal :size 577))
          commit commit-info old-line new-line num old-file subject author
          author-time author-timezone info ov beg end blame)
      (with-current-buffer blame-buf
        (goto-char (point-min))
        ;; search for a ful commit info
        (while (re-search-forward
                "^\\([0-9a-f]\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)$"
                nil t)
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
                                   'face 'magit-blame-hash)
                       blank
                       (propertize (format "%-20s" author)
                                   'face 'magit-blame-culprit)
                       blank
                       (propertize (magit-blame-format-time-string
                                    magit-blame-time-format
                                    author-time author-timezone)
                                   'face 'magit-blame-time)
                       blank
                       (propertize subject 'face 'magit-blame-subject)
                       blank nl))
          (overlay-put ov 'before-string blame))))))

;;; magit-blame.el ends soon
(provide 'magit-blame)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-blame.el ends here
