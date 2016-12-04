;;; magit-margin.el --- margins in Magit buffers  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2016  The Magit Project Contributors
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

;; This library implements support for showing additional information
;; in the margins of Magit buffers.  Currently this is only used for
;; commits, for which the author name and optionally committer date or
;; age are shown.

;;; Code:

(require 'dash)

(require 'magit-section)
(require 'magit-mode)

(defgroup magit-margin nil
  "Information Magit displays in the margin.

If you want to change the DATE-STYLE of all `magit-*-margin'
options to the same value, you can do so by only customizing
`magit-log-margin' *before* `magit' is loaded.  If you do so,
then the respective value for the other options will default
to what you have set for `magit-log-margin'."
  :group 'magit-log)

(defvar-local magit-set-buffer-margin-refresh nil)

(defvar-local magit-show-margin nil)
(put 'magit-show-margin 'permanent-local t)

(defvar-local magit-margin-age-width nil)
(put 'magit-margin-age-width 'permanent-local t)

(defvar magit--age-spec)

;;; Commands

(defun magit-toggle-margin ()
  "Show or hide the Magit margin."
  (interactive)
  (unless (derived-mode-p 'magit-log-mode 'magit-status-mode
                          'magit-refs-mode 'magit-cherry-mode)
    (user-error "Magit margin isn't supported in this buffer"))
  (magit-set-buffer-margin (not (cdr (window-margins)))))

;;; Core

(defun magit-margin-get (prop)
  (pcase prop
    (:age-width magit-margin-age-width)
    (:option (pcase major-mode
               (`magit-cherry-mode     'magit-cherry-margin)
               (`magit-log-mode        'magit-log-margin)
               (`magit-log-select-mode 'magit-log-select-margin)
               (`magit-reflog-mode     'magit-reflog-margin)
               (`magit-refs-mode       'magit-refs-margin)
               (`magit-stashes-mode    'magit-stashes-margin)
               (`magit-status-mode     'magit-status-margin)))
    (_ (nth (pcase prop
              (:initially 0)
              (:person    1)
              (:style     2))
            (symbol-value (magit-margin-get :option))))))

(defun magit-maybe-show-margin ()
  "Maybe show the margin, depending on the major-mode and an option."
  (cond ((local-variable-p 'magit-show-margin)
         (magit-set-buffer-margin magit-show-margin))
        ((magit-margin-get :option)
         (magit-set-buffer-margin (magit-margin-get :initially)))))

(defun magit-set-buffer-margin (enable)
  (let ((style (magit-margin-get :style)))
    (setq magit-margin-age-width
          (+ 1 ; gap between committer and time
             ;;; width of unit
             (if (eq style 'age-abbreviated)
                 1  ; single character
               (+ 1 ; gap between count and unit
                  (apply #'max (--map (max (length (nth 1 it))
                                           (length (nth 2 it)))
                                      magit--age-spec))))))
    (let ((width (and enable
                      (+ (-if-let (width (magit-margin-get :person))
                             (1+ width)
                           0)
                         (if (stringp style)
                             (length (format-time-string style))
                           (+ 2 ; count width
                              (magit-margin-get :age-width)))))))
      (setq magit-show-margin width)
      (when (and enable magit-set-buffer-margin-refresh)
        (magit-refresh-buffer))
      (dolist (window (get-buffer-window-list nil nil 0))
        (with-selected-window window
          (set-window-margins nil (car (window-margins)) width)
          (if enable
              (add-hook  'window-configuration-change-hook
                         'magit-set-buffer-margin-1 nil t)
            (remove-hook 'window-configuration-change-hook
                         'magit-set-buffer-margin-1 t)))))))

(defun magit-set-buffer-margin-1 ()
  (-when-let (window (get-buffer-window))
    (with-selected-window window
      (set-window-margins nil (car (window-margins)) magit-show-margin))))

(defun magit-make-margin-overlay (&optional string previous-line)
  (if previous-line
      (save-excursion
        (forward-line -1)
        (magit-make-margin-overlay string))
    ;; Don't put the overlay on the complete line to work around #1880.
    (let ((o (make-overlay (1+ (line-beginning-position))
                           (line-end-position)
                           nil t)))
      (overlay-put o 'evaporate t)
      (overlay-put o 'before-string
                   (propertize "o" 'display
                               (list (list 'margin 'right-margin)
                                     (or string " ")))))))

(defun magit-maybe-make-margin-overlay ()
  (when (or (magit-section-match
             '(unpulled unpushed recent stashes local cherries)
             magit-insert-section--current)
            (and (eq major-mode 'magit-refs-mode)
                 (magit-section-match
                  '(remote commit)
                  magit-insert-section--current)))
    (magit-make-margin-overlay nil t)))

;;; Custom Support

(defun magit-margin-set-variable (mode symbol value)
  (set-default symbol value)
  (message "Updating margins in %s buffers..." mode)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode mode)
        (magit-set-buffer-margin magit-show-margin)
        (magit-refresh))))
  (message "Updating margins in %s buffers...done" mode))

(defconst magit-log-margin--custom-type
  '(list (boolean :tag "Show initially")
         (integer :tag "Show author name using width")
         (choice  :tag "Show committer"
                  (string :tag "date using format" "%Y-%m-%d %H:%M ")
                  (const  :tag "date's age" age)
                  (const  :tag "date's age (abbreviated)" age-abbreviated))))

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
  (cl-labels ((fn (age spec)
                  (-let [(char unit units weight) (car spec)]
                    (let ((cnt (round (/ age weight 1.0))))
                      (if (or (not (cdr spec))
                              (>= (/ age weight) 1))
                          (list cnt (cond (abbreviate char)
                                          ((= cnt 1) unit)
                                          (t units)))
                        (fn age (cdr spec)))))))
    (fn (abs (- (float-time) (string-to-number date)))
        magit--age-spec)))

;;; magit-margin.el ends soon
(provide 'magit-margin)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; magit-margin.el ends here
