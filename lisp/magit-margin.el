;;; magit-margin.el --- Margins in Magit buffers  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2025 The Magit Project Contributors

;; Author: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>
;; Maintainer: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Magit is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements support for showing additional information
;; in the margins of Magit buffers.  Currently this is only used for
;; commits, for which the committer date or age, and optionally the
;; author name are shown.

;;; Code:

(require 'magit-base)
(require 'magit-transient)
(require 'magit-mode)

;;; Options

(defgroup magit-margin nil
  "Information Magit displays in the margin.

You can change the STYLE and AUTHOR-WIDTH of all `magit-*-margin'
options to the same values by customizing `magit-log-margin'
*before* `magit' is loaded.  If you do that, then the respective
values for the other options will default to what you have set
for that variable.  Likewise if you set `magit-log-margin's INIT
to nil, then that is used in the default of all other options.  But
setting it to t, i.e., re-enforcing the default for that option,
does not carry to other options."
  :link '(info-link "(magit)Log Margin")
  :group 'magit-log)

;;; Settings

(defvar-local magit--right-margin-delayed nil)

(defvar-local magit--right-margin-config nil)
(put 'magit--right-margin-config 'permanent-local t)

(defun magit--right-margin-active ()
  (car magit--right-margin-config))

(defun magit--right-margin-option ()
  (pcase major-mode
    ('magit-cherry-mode        'magit-cherry-margin)
    ('magit-log-mode           'magit-log-margin)
    ('magit-log-select-mode    'magit-log-select-margin)
    ('magit-reflog-mode        'magit-reflog-margin)
    ('magit-refs-mode          'magit-refs-margin)
    ('magit-stashes-mode       'magit-stashes-margin)
    ('magit-status-mode        'magit-status-margin)
    ('forge-notifications-mode 'magit-status-margin)
    ('forge-topics-mode        'magit-status-margin)))

;;; Commands

(transient-define-prefix magit-margin-settings ()
  "Change what information is displayed in the right margin."
  :info-manual "(magit) Log Margin"
  ["Margin"
   (magit-toggle-margin)
   (magit-cycle-margin-style)
   (magit-toggle-margin-details)
   (magit-refs-set-show-commit-count)])

(transient-define-suffix magit-toggle-margin ()
  "Show or hide the right margin."
  :description "Toggle visibility"
  :key "L"
  :transient t
  (interactive)
  (unless (magit--right-margin-option)
    (user-error "Magit margin isn't supported in this buffer"))
  (setcar magit--right-margin-config (not (magit--right-margin-active)))
  (magit-set-buffer-margins))

(defvar magit-margin-default-time-format nil
  "See https://github.com/magit/magit/pull/4605.")

(transient-define-suffix magit-cycle-margin-style ()
  "Cycle style used for the right margin."
  :description "Cycle style"
  :key "l"
  :transient t
  (interactive)
  (unless (magit--right-margin-option)
    (user-error "Magit margin isn't supported in this buffer"))
  ;; This is only suitable for commit margins (there are not others).
  (setf (cadr magit--right-margin-config)
        (pcase (cadr magit--right-margin-config)
          ('age 'age-abbreviated)
          ('age-abbreviated
           (let ((default (or magit-margin-default-time-format
                              (cadr (symbol-value (magit--right-margin-option))))))
             (if (stringp default) default "%Y-%m-%d %H:%M ")))
          (_ 'age)))
  (magit-set-buffer-margins nil t))

(transient-define-suffix magit-toggle-margin-details ()
  "Show or hide details in the right margin."
  :description "Toggle details"
  :key "d"
  :transient t
  (interactive)
  (unless (magit--right-margin-option)
    (user-error "Magit margin isn't supported in this buffer"))
  (setf (nth 3 magit--right-margin-config)
        (not (nth 3 magit--right-margin-config)))
  (magit-set-buffer-margins nil t))

;;; Core

(defun magit-set-buffer-margins (&optional reset-right refresh-right)
  (let ((lmargin nil)
        (rmargin nil)
        (roption (magit--right-margin-option)))
    (when (or lmargin roption)
      (when roption
        (let* ((default (symbol-value roption))
               (default-width (nth 2 default)))
          (when (or reset-right (not magit--right-margin-config))
            (setq magit--right-margin-config (copy-sequence default)))
          (pcase-let ((`(,enable ,style ,_width ,details ,details-width)
                       magit--right-margin-config))
            (setq rmargin enable)
            (when (functionp default-width)
              (setf (nth 2 magit--right-margin-config)
                    (funcall default-width style details details-width))))))
      (dolist (window (get-buffer-window-list nil nil 0))
        (with-selected-window window
          (magit-set-window-margins window)
          (if (or lmargin rmargin)
              (add-hook  'window-configuration-change-hook
                         #'magit-set-window-margins nil t)
            (remove-hook 'window-configuration-change-hook
                         #'magit-set-window-margins t))))
      (when (and rmargin (or refresh-right magit--right-margin-delayed))
        (magit-refresh-buffer)))))

(defun magit-set-window-margins (&optional window)
  (when (or window (setq window (get-buffer-window)))
    (with-selected-window window
      (set-window-margins
       nil
       (if (characterp (car (magit-section-visibility-indicator)))
           1
         (car (window-margins)))
       (and (magit--right-margin-active)
            (nth 2 magit--right-margin-config))))))

(cl-defun magit-make-margin-overlay (&optional string (previous-line nil sline))
  "Display STRING in the margin of the previous (or current) line.
If point is at the beginning of a line, set the margin string for
the previous line, otherwise for the current line.  Semi-obsolete
optional PREVIOUS-LINE can be used to explicitly specify which
line is affected."
  (save-excursion
    (forward-line (if (if sline previous-line (bolp)) -1 0))
    ;; Don't put the overlay on the complete line to work around #1880.
    (let ((o (make-overlay (1+ (point)) (line-end-position) nil t)))
      (overlay-put o 'evaporate t)
      (overlay-put o 'before-string
                   (propertize "o" 'display
                               (list (list 'margin 'right-margin)
                                     (or string " ")))))))

(defvar magit-margin-overlay-conditions
  '( unpulled unpushed recent stashes local cherries
     [remote branchbuf]
     [shelved branchbuf]
     [tags branchbuf]
     topics discussions issues pullreqs))

(defun magit-maybe-make-margin-overlay ()
  (when (magit-section-match magit-margin-overlay-conditions
                             magit-insert-section--current)
    (magit-make-margin-overlay)))

;;; Custom Support

(defun magit-margin-set-variable (mode symbol value)
  (set-default symbol value)
  (message "Updating margins in %s buffers..." mode)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode mode)
        (magit-set-buffer-margins t)
        (magit-refresh))))
  (message "Updating margins in %s buffers...done" mode))

(defconst magit-log-margin--custom-type
  '(list (boolean :tag "Show margin initially")
         (choice  :tag "Show committer"
                  (string :tag "date using time-format" "%Y-%m-%d %H:%M ")
                  (const  :tag "date's age" age)
                  (const  :tag "date's age (abbreviated)" age-abbreviated))
         (const   :tag "Calculate width using magit-log-margin-width"
                  magit-log-margin-width)
         (boolean :tag "Show author name by default")
         (integer :tag "Show author name using width")))

;;; Time Utilities

(defvar magit--age-spec
  `((?Y "year"   "years"   ,(round (* 60 60 24 365.2425)))
    (?M "month"  "months"  ,(round (* 60 60 24 30.436875)))
    (?w "week"   "weeks"   ,(* 60 60 24 7))
    (?d "day"    "days"    ,(* 60 60 24))
    (?h "hour"   "hours"   ,(* 60 60))
    (?m "minute" "minutes" 60)
    (?s "second" "seconds" 1))
  "Time units used when formatting relative commit ages.

The value is a list of time units, beginning with the longest.
Each element has the form (CHAR UNIT UNITS SECONDS).  UNIT is the
time unit, UNITS is the plural of that unit.  CHAR is a character
abbreviation.  And SECONDS is the number of seconds in one UNIT.

This is defined as a variable to make it possible to use time
units for a language other than English.  It is not defined
as an option, because most other parts of Magit are always in
English.")

(defun magit--age (date &optional abbreviate)
  (named-let calc ((age (abs (- (float-time)
                                (if (stringp date)
                                    (string-to-number date)
                                  date))))
                   (spec magit--age-spec))
    (pcase-let* ((`((,char ,unit ,units ,weight) . ,spec) spec)
                 (cnt (round (/ age weight 1.0))))
      (if (or (not spec)
              (>= (/ age weight) 1))
          (list cnt (cond (abbreviate char)
                          ((= cnt 1) unit)
                          (units)))
        (calc age spec)))))

;;; _
(provide 'magit-margin)
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("and$"         . "cond-let--and$")
;;   ("and>"         . "cond-let--and>")
;;   ("and-let"      . "cond-let--and-let")
;;   ("if-let"       . "cond-let--if-let")
;;   ("when-let"     . "cond-let--when-let")
;;   ("while-let"    . "cond-let--while-let")
;;   ("match-string" . "match-string")
;;   ("match-str"    . "match-string-no-properties"))
;; End:
;;; magit-margin.el ends here
