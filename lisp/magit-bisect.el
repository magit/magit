;;; magit-bisect.el --- bisect support for Magit

;; Copyright (C) 2011-2015  The Magit Project Contributors
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

;; Use a binary search to find the commit that introduced a bug.

;;; Code:

(require 'magit)

(defface magit-bisect-good
  '((t :foreground "DarkOliveGreen"))
  "Face for good bisect revisions."
  :group 'magit-faces)

(defface magit-bisect-skip
  '((t :foreground "DarkGoldenrod"))
  "Face for skipped bisect revisions."
  :group 'magit-faces)

(defface magit-bisect-bad
  '((t :foreground "IndianRed4"))
  "Face for bad bisect revisions."
  :group 'magit-faces)

;;;###autoload (autoload 'magit-bisect-popup "magit-bisect" nil t)
(magit-define-popup magit-bisect-popup
  "Popup console for bisect commands."
  'magit-commands 'magit-popup-sequence-mode
  :man-page "git-bisect"
  :actions            '((?B "Start" magit-bisect-start)
                        (?R "Start & Run" magit-bisect-start-and-run))
  :sequence-actions   '((?r "Reset" magit-bisect-reset)
                        (?b "Bad"   magit-bisect-bad)
                        (?g "Good"  magit-bisect-good)
                        (?k "Skip"  magit-bisect-skip)
                        (?a "Run"   magit-bisect-run))
  :sequence-predicate 'magit-bisect-in-progress-p)

;;;###autoload
(defun magit-bisect-start (bad good)
  "Start a bisect session.

Bisecting a bug means to find the commit that introduced it.
This command starts such a bisect session by asking for a know
good and a bad commit.  To move the session forward use the
other actions from the bisect popup (\
\\<magit-status-mode-map>\\[magit-bisect-popup])."
  (interactive
   (if (magit-bisect-in-progress-p)
       (user-error "Already bisecting")
     (let ((b (magit-read-branch-or-commit "Start bisect with bad revision")))
       (list b (magit-read-other-branch-or-commit "Good revision" b)))))
  (magit-bisect-async "start" (list bad good) t))

;;;###autoload
(defun magit-bisect-start-and-run ()
  "Calls `magit-bisect-start' followed by `magit-bisect-run'."
  (interactive)
  (call-interactively 'magit-bisect-start)
  (call-interactively 'magit-bisect-run))

;;;###autoload
(defun magit-bisect-reset ()
  "After bisecting, cleanup bisection state and return to original `HEAD'."
  (interactive)
  (when (magit-confirm 'reset-bisect)
    (magit-run-git "bisect" "reset")
    (ignore-errors (delete-file (magit-git-dir "BISECT_CMD_OUTPUT")))))

;;;###autoload
(defun magit-bisect-good ()
  "While bisecting, mark the current commit as good.
Use this after you have asserted that the commit does not contain
the bug in question."
  (interactive)
  (magit-bisect-async "good"))

;;;###autoload
(defun magit-bisect-bad ()
  "While bisecting, mark the current commit as bad.
Use this after you have asserted that the commit does contain the
bug in question."
  (interactive)
  (magit-bisect-async "bad"))

;;;###autoload
(defun magit-bisect-skip ()
  "While bisecting, skip the current commit.
Use this if for some reason the current commit is not a good one
to test.  This command lets Git choose a different one."
  (interactive)
  (magit-bisect-async "skip"))

;;;###autoload
(defun magit-bisect-run (cmdline)
  "Bisect automatically by running commands after each step."
  (interactive (list (read-shell-command "Bisect shell command: ")))
  (magit-bisect-async "run" (list cmdline)))

(defun magit-bisect-async (subcommand &optional args no-assert)
  (unless (or no-assert (magit-bisect-in-progress-p))
    (user-error "Not bisecting"))
  (magit-with-toplevel
    (let ((file (magit-git-dir "BISECT_CMD_OUTPUT")))
      (ignore-errors (delete-file file))
      (magit-run-git-with-logfile file "bisect" subcommand args))
    (magit-process-wait)
    (magit-refresh)))

(defun magit-bisect-in-progress-p ()
  (file-exists-p (magit-git-dir "BISECT_LOG")))

(defun magit-insert-bisect-output ()
  "While bisecting, insert section with output from `git bisect'."
  (when (magit-bisect-in-progress-p)
    (let ((lines
           (or (magit-file-lines (magit-git-dir "BISECT_CMD_OUTPUT"))
               (list "Bisecting: (no saved bisect output)"
                     "It appears you have invoked `git bisect' from a shell."
                     "There is nothing wrong with that, we just cannot display"
                     "anything useful here.  Consult the shell output instead.")))
          (done-re "^[a-z0-9]\\{40\\} is the first bad commit$"))
      (magit-insert-section (bisect-output t)
        (magit-insert-heading
          (propertize (or (and (string-match done-re (car lines)) (pop lines))
                          (--first (string-match done-re it) lines)
                          (pop lines))
                      'face 'magit-section-heading))
        (dolist (line lines)
          (insert line "\n"))))
    (insert "\n")))

(defun magit-insert-bisect-rest ()
  "While bisecting, insert section visualizing the bisect state."
  (when (magit-bisect-in-progress-p)
    (magit-insert-section (bisect-view)
      (magit-insert-heading "Bisect Rest:")
      (magit-git-wash (apply-partially 'magit-log-wash-log 'bisect-vis)
        "bisect" "visualize" "git" "log"
        "--format=%h%d %s" "--decorate=full"))))

(defun magit-insert-bisect-log ()
  "While bisecting, insert section logging bisect progress."
  (when (magit-bisect-in-progress-p)
    (magit-insert-section (bisect-log)
      (magit-insert-heading "Bisect Log:")
      (magit-git-wash #'magit-wash-bisect-log "bisect" "log")
      (insert ?\n))))

(defun magit-wash-bisect-log (args)
  (let (beg)
    (while (progn (setq beg (point-marker))
                  (re-search-forward "^\\(git bisect [^\n]+\n\\)" nil t))
      (magit-bind-match-strings (heading) nil
        (magit-delete-match)
        (save-restriction
          (narrow-to-region beg (point))
          (goto-char (point-min))
          (magit-insert-section (bisect-log heading t)
            (magit-insert (propertize heading 'face
                                      'magit-section-secondary-heading))
            (magit-insert-heading)
            (magit-wash-sequence
             (apply-partially 'magit-log-wash-line 'bisect-log
                              (magit-abbrev-length)))
            (insert ?\n)))))
    (when (re-search-forward
           "# first bad commit: \\[\\([a-z0-9]\\{40\\}\\)\\] [^\n]+\n" nil t)
      (magit-bind-match-strings (hash) nil
        (magit-delete-match)
        (magit-insert-section (bisect-log)
          (magit-insert (concat hash " is the first bad commit\n")))))))

;;; magit-bisect.el ends soon
(provide 'magit-bisect)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-bisect.el ends here
