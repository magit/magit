;;; magit-key-mode.el --- interactively tune git invocation

;; Copyright (C) 2010-2014  The Magit Project Developers
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

;; Author: Phil Jackson <phil@shellarchive.co.uk>
;; Package: magit

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

;; Interactively tune git invocation.

;;; Code:

(require 'button)
(require 'format-spec)

(eval-when-compile (require 'cl-lib))

(defvar magit-popup-previous-winconf)

;;; Options

(defcustom magit-popup-show-usage t
  "Whether to show usage information when entering a popup."
  :group 'magit
  :type 'boolean)

;;; Faces

(defface magit-popup-header
  '((t :inherit font-lock-keyword-face))
  "Face for key mode header lines."
  :group 'magit-faces)

(defface magit-popup-key
  '((t :inherit font-lock-builtin-face))
  "Face for key mode buttons."
  :group 'magit-faces)

(defface magit-popup-argument
  '((t :inherit font-lock-warning-face))
  "Face used to display enabled arguments in popups."
  :group 'magit-faces)

(defface magit-popup-disabled-argument
  '((t :inherit shadow))
  "Face used to display disabled arguments in popups."
  :group 'magit-faces)

(defface magit-popup-option-value
  '((t :inherit font-lock-string-face))
  "Face used to display option values in popups."
  :group 'magit-faces)

(define-obsolete-face-alias 'magit-key-mode-header-face 'magit-popup-header "2.0.0")
(define-obsolete-face-alias 'magit-key-mode-button-face 'magit-popup-key "2.0.0")
(define-obsolete-face-alias 'magit-key-mode-switch-face 'magit-popup-argument "2.0.0")

;;; (being refactored)

(defvar magit-popup-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map 'nodigits)
    (define-key map [?q]    'magit-popup-quit)
    (define-key map [?\C-g] 'magit-popup-quit)
    (define-key map (kbd "C-p") 'backward-button)
    (define-key map (kbd "DEL") 'backward-button)
    (define-key map (kbd "C-n") 'forward-button)
    (define-key map (kbd "TAB") 'forward-button)
    map))

(defmacro magit-define-popup (name doc &rest plist)
  (declare (indent defun) (doc-string 2))
  (let ((msym (intern (format "%s-map" name))))
    `(progn
       (defun ,name () ,doc
         (interactive)
         (magit-popup-mode-setup ',name))
       (defvar ,name
         (list ,@plist))
       (defvar ,msym
         (magit-define-popup-keymap ',name ,name))
       (put ',msym 'definition-name ',name))))

(defun magit-define-popup-switch (popup map key switch)
  (define-key map key
    `(lambda () (interactive)
       (magit-invoke-popup-switch ',popup ,switch))))

(defun magit-define-popup-option (popup map key option reader)
  (define-key map key
    `(lambda () (interactive)
       (magit-invoke-popup-option ',popup ,option ',reader))))

(defun magit-define-popup-action (popup map key command)
  (define-key map key
    `(lambda () (interactive)
       (magit-invoke-popup-action ',popup ',command))))

(defun magit-define-popup-keymap (popup spec)
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-popup-mode-map)
    (dolist (e (plist-get spec :switches))
      (magit-define-popup-switch popup map (car e) (nth 2 e)))
    (dolist (e (plist-get spec :options))
      (magit-define-popup-option popup map (car e) (nth 2 e) (nth 3 e)))
    (dolist (e (plist-get spec :actions))
      (magit-define-popup-action popup map (car e) (nth 2 e)))
    (define-key map "?" `(lambda ()
                           (interactive)
                           (magit-popup-help ',popup)))
    map))

(defvar-local magit-popup-current-options nil)
(defvar-local magit-popup-current-switches nil)

(defvar magit-current-popup-args nil)

(defun magit-invoke-popup-switch (popup switch)
  (if (member switch magit-popup-current-switches)
      (setq magit-popup-current-switches
            (delete switch magit-popup-current-switches))
    (add-to-list 'magit-popup-current-switches switch))
  (magit-refresh-popup-buffer popup))

(defun magit-invoke-popup-option (popup arg-name input-func)
  (let ((elt (assoc arg-name magit-popup-current-options))
        (val (funcall input-func (concat arg-name ": "))))
    (cond ((or (not val) (equal val "")) (setq val nil))
          ((string-match-p "^\s+$" val)  (setq val "")))
    (if elt
        (setcdr elt val)
      (push (cons arg-name val) magit-popup-current-options))
    (magit-refresh-popup-buffer popup)))

(defun magit-invoke-popup-action (popup func)
  (let ((magit-current-popup-args
         (nconc magit-popup-current-switches
                (mapcar (lambda (elt)
                          (concat (car elt) (cdr elt)))
                        magit-popup-current-options))))
    (magit-popup-quit)
    (call-interactively func)))

(defun magit-popup-help (popup)
  (let* ((spec (symbol-value popup))
         (man-page (plist-get spec :man-page))
         (seq (read-key-sequence
               (format "Enter command prefix%s: "
                       (if man-page
                           (format ", `?' for man `%s'" man-page)
                         ""))))
         (actions (plist-get spec :actions)))
    (cond
      ((assoc seq actions) (describe-function (nth 2 (assoc seq actions))))
      ((equal seq "?")
       (if man-page
           (man man-page)
         (error "No man page associated with `%s'" popup)))
      (t (error "No help associated with `%s'" seq)))))

(defun magit-popup-quit ()
  (interactive)
  (let ((buf (current-buffer)))
    (set-window-configuration magit-popup-previous-winconf)
    (kill-buffer buf)))

;;; Mode

(defvar-local magit-popup-previous-winconf nil)

(define-derived-mode magit-popup-mode fundamental-mode "MagitPopup"
  ""
  (setq buffer-read-only t)
  (set (make-local-variable 'scroll-margin) 0))

(put 'magit-popup-mode 'mode-class 'special)

(defun magit-popup-mode-setup (popup)
  (magit-popup-mode-display-buffer
   (get-buffer-create (format "*%s*" popup)))
  (use-local-map
   (symbol-value (intern (format "%s-map" popup))))
  (magit-refresh-popup-buffer popup)
  (fit-window-to-buffer)
  (when magit-popup-show-usage
    (message (concat "Type a prefix key to toggle it. "
                     "Run actions with their prefixes. "
                     "'?' for more help."))))

(defun magit-popup-mode-display-buffer (buffer)
  (let ((winconf (current-window-configuration)))
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer buffer)
    (magit-popup-mode)
    (setq magit-popup-previous-winconf winconf)))

(defun magit-refresh-popup-buffer (popup)
  (let ((inhibit-read-only t)
        (key (ignore-errors
               (car (button-get (button-at (point)) 'args))))
        (spec (symbol-value popup)))
    (erase-buffer)
    (save-excursion
      (magit-popup-insert-buttons popup "Switches" " %-4k %d %s"
                                  'magit-invoke-popup-switch
                                  (plist-get spec :switches))
      (magit-popup-insert-buttons popup "Options"  " %-4k %d %o"
                                  'magit-invoke-popup-option
                                  (plist-get spec :options)
                                  (not magit-popup-options-in-cols))
      (magit-popup-insert-buttons popup "Actions"  " %-3k %d"
                                  'magit-invoke-popup-action
                                  (plist-get spec :actions)))
    (if key
        (while (and (forward-button 1)
                    (not (equal (car (button-get (button-at (point)) 'args))
                                key))))
      (re-search-forward "^Actions" nil t)
      (forward-button 1))))

;;; Draw

(defun magit-popup-insert-buttons (popup heading format invoke items
                                         &optional one-col-each)
  (when items
    (insert (propertize heading 'face 'magit-popup-header) "\n")
    (setq items
          (mapcar (lambda (item)
                    (cons (magit-popup-format-button format item) item))
                  items))
    (let ((maxlen (apply 'max (mapcar (lambda (e) (length (car e))) items)))
          item)
      (while (setq item (pop items))
        (let ((beg (point)))
          (insert (car item))
          (make-button beg (point) 'face nil
                       'args (cons invoke (cons popup (cdr item)))
                       'action
                       (lambda (button)
                         (let ((args (button-get button 'args)))
                           (apply (car args) (cddr args)))))
          (let ((padding (- (+ maxlen 3) (length (car item)))))
            (if (or one-col-each
                    (not items)
                    (> (+ (current-column) padding maxlen)
                       (window-width)))
                (insert "\n")
              (insert (make-string padding ?\s)))))))
    (insert "\n")))

(defun magit-popup-format-button (format arg)
  (let* ((k (propertize (car arg) 'face 'magit-popup-key))
         (d (nth 1 arg))
         (a (unless (symbolp (nth 2 arg)) (nth 2 arg)))
         (v (and a (cdr (assoc a magit-popup-current-options)))))
    (when a
      (setq a (propertize
               a 'face (if (or (member a magit-popup-current-switches)
                               (assoc  a magit-popup-current-options))
                           'magit-popup-argument
                         'magit-popup-disabled-argument))))
    (when v
      (setq v (propertize (format "\"%s\"" v)
                          'face 'magit-popup-option-value)))
    (format-spec format
                 `((?k . ,(concat k ":"))
                   (?d . ,d)
                   (?s . ,(concat "(" a ")"))
                   (?o . ,(concat "(" a v ")"))))))

(provide 'magit-key-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-key-mode.el ends here
