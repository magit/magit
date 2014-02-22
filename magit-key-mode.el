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
(require 'cl-lib)
(require 'format-spec)

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

;;; Keymap

(defvar magit-popup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'magit-invoke-popup-action)
    (define-key map [?- t]  'magit-invoke-popup-switch)
    (define-key map [?= t]  'magit-invoke-popup-option)
    (define-key map [?\C-g] 'magit-popup-quit)
    (define-key map [??]    'magit-popup-help)
    (define-key map [?\C-h] 'magit-popup-help)
    (define-key map [?\d]   'backward-button)
    (define-key map [?\C-p] 'backward-button)
    (define-key map [?\t]   'forward-button)
    (define-key map [?\C-n] 'forward-button)
    map))

;;; Buttons

(define-button-type 'magit-popup-button
  'face nil
  'action (lambda (button)
            (funcall (button-get button 'function)
                     (button-get button 'event))))

(define-button-type 'magit-popup-switch-button
  'supertype 'magit-popup-button
  'function  'magit-invoke-popup-switch
  'property  :switches
  'heading   "Switches\n"
  'format    " %k: %d %s"
  'prefix    ?-
  'onecol    nil)

(define-button-type 'magit-popup-option-button
  'supertype 'magit-popup-button
  'function  'magit-invoke-popup-option
  'property  :options
  'heading   "Options\n"
  'format    " %k: %d %o"
  'prefix    ?=
  'onecol    t)

(define-button-type 'magit-popup-action-button
  'supertype 'magit-popup-button
  'function  'magit-invoke-popup-action
  'property  :actions
  'heading   "Actions\n"
  'format    " %k: %d"
  'prefix    nil
  'onecol    nil)

;;; Events

(defvar-local magit-this-popup nil)
(defvar-local magit-this-popup-events nil)

(defun magit-popup-get (key)
  (if (memq key '(:switches :options :actions))
      (plist-get magit-this-popup-events key)
    (plist-get (symbol-value magit-this-popup) key)))

(defvar magit-current-popup nil)
(defvar magit-current-popup-args nil)

(defun magit-popup-get-args ()
  (cl-mapcan (lambda (elt)
               (when (nth 4 elt)
                 (list (concat (nth 2 elt) (nth 5 elt)))))
             (append (magit-popup-get :switches)
                     (magit-popup-get :options))))

(defun magit-popup-setup-events ()
  (let ((def (symbol-value magit-this-popup)))
    (setq magit-this-popup-events
          (list :switches
                (mapcar (lambda (elt)
                          (append elt (list nil nil)))
                        (plist-get def :switches))
                :options
                (mapcar (lambda (elt)
                          (append elt (list nil nil)))
                        (plist-get def :options))
                :actions (plist-get def :actions)))))

;;; Define

(defmacro magit-define-popup (name doc &rest plist)
  (declare (indent defun) (doc-string 2))
  (let ((custom (intern (format "%s-defaults" name))))
    `(progn
       (defun ,name (&optional arg) ,doc
         (interactive)
         (magit-invoke-popup ',name))
       (defvar ,name
         (list ,@plist)))))

(defun magit-define-popup-switch (popup key desc switch
                                        &optional enable at prepend)
  (declare (indent defun))
  (magit-define-popup-key popup :switches key
    (list desc switch enable) at prepend))

(defun magit-define-popup-option (popup key desc option reader
                                        &optional value at prepend)
  (declare (indent defun))
  (magit-define-popup-key popup :options key
    (list desc option reader value) at prepend))

(defun magit-define-popup-action (popup key desc command
                                        &optional at prepend)
  (declare (indent defun))
  (magit-define-popup-key popup :actions key
    (list desc command) at prepend))

(defun magit-define-popup-key (popup type key def
                                     &optional at prepend)
  (declare (indent defun))
  (if (memq type '(:switches :options :actions))
      (let* ((plist (symbol-value popup))
             (value (plist-get plist type))
             (elt   (assoc key value)))
        (if elt
            (setcdr elt def)
          (setq elt (cons key def)))
        (if at
            (when (setq at (cl-member at value :key 'car :test 'equal))
              (setq value (cl-delete key value :key 'car :test 'equal))
              (if prepend
                  (progn (push (car at) (cdr at))
                         (setcar at elt))
                (push elt (cdr at))))
          (setq value (cl-delete key value :key 'car :test 'equal)))
        (unless (assoc key value)
          (setq value (if prepend
                          (cons elt value)
                        (append value (list elt)))))
        (set popup (plist-put plist type value)))
    (error "Unknown popup event type: %s" type)))

(defun magit-change-popup-key (popup type from to)
  (setcar (assoc from (plist-get (symbol-value popup) type)) to))

(defun magit-remove-popup-key (popup type key)
  (let* ((plist (symbol-value popup))
         (alist (plist-get plist type))
         (value (assoc key alist)))
    (set popup (plist-put plist type (delete value alist)))))

;;; Invoke

(defun magit-invoke-popup (popup)
  (magit-popup-mode-setup popup))

(defun magit-invoke-popup-switch (event)
  (interactive (list last-command-event))
  (let ((elt (assoc event (magit-popup-get :switches))))
    (if  elt
        (progn (setf (nth 4 elt) (not (nth 4 elt)))
               (magit-refresh-popup-buffer))
      (error "%c isn't bound to any switch" event))))

(defun magit-invoke-popup-option (event)
  (interactive (list last-command-event))
  (let ((elt (assoc event (magit-popup-get :options))))
    (if  elt
        (let ((val (funcall (nth 3 elt) (concat (nth 2 elt) ": "))))
          (cond ((or (not val) (equal val "")) (setq val nil))
                ((string-match-p "^\s+$" val)  (setq val "")))
          (setf (nth 4 elt) (and val t))
          (setf (nth 5 elt) val)
          (magit-refresh-popup-buffer))
      (error "%c isn't bound to any option" event))))

(defun magit-invoke-popup-action (event)
  (interactive (list last-command-event))
  (let ((def (nth 2 (assoc event (magit-popup-get :actions)))))
    (if  def
        (let ((magit-current-popup magit-this-popup)
              (magit-current-popup-args (magit-popup-get-args)))
          (magit-popup-quit)
          (call-interactively def))
      (if (eq event ?q)
          (magit-popup-quit)
        (error "%c isn't bound to any action" event)))))

(defun magit-popup-help ()
  (interactive)
  (let* ((man-page (magit-popup-get :man-page))
         (char (aref (read-key-sequence
                      (format "Enter command prefix%s: "
                              (if man-page
                                  (format ", `?' for man `%s'" man-page)
                                "")))
                     0))
         (actions (magit-popup-get :actions)))
    (cond
      ((assoc char actions)
       (describe-function (nth 2 (assoc char actions))))
      ((equal char ??)
       (if man-page
           (man man-page)
         (error "No man page associated with `%s'" magit-this-popup)))
      (t (error "No help associated with `%c'" char)))))

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
  (magit-popup-mode-display-buffer (get-buffer-create
                                    (format "*%s*" popup)))
  (setq magit-this-popup popup)
  (magit-popup-setup-events)
  (magit-refresh-popup-buffer)
  (fit-window-to-buffer))

(defun magit-popup-mode-display-buffer (buffer)
  (let ((winconf (current-window-configuration)))
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer buffer)
    (magit-popup-mode)
    (setq magit-popup-previous-winconf winconf)))

(defun magit-refresh-popup-buffer ()
  (let* ((inhibit-read-only t)
         (button (button-at (point)))
         (prefix (and button (button-get button 'prefix)))
         (event  (and button (button-get button 'event))))
    (erase-buffer)
    (save-excursion
      (magit-popup-insert-buttons 'magit-popup-switch-button)
      (magit-popup-insert-buttons 'magit-popup-option-button)
      (magit-popup-insert-buttons 'magit-popup-action-button))
    (if event
        (while (and (forward-button 1)
                    (let ((b (button-at (point))))
                      (or (not (equal (button-get b 'prefix) prefix))
                          (not (equal (button-get b 'event)  event))))))
      (re-search-forward "^Actions" nil t)
      (forward-button 1))))

;;; Draw

(defun magit-popup-insert-buttons (type)
  (let ((items (mapcar (lambda (item)
                         (cons (magit-popup-format-button type item) item))
                       (magit-popup-get (button-type-get type 'property)))))
    (when items
      (insert (propertize (button-type-get type 'heading)
                          'face 'magit-popup-header))
      (let ((maxlen (apply 'max (mapcar (lambda (e) (length (car e))) items)))
            (onecol (button-type-get type 'onecol))
            item)
        (while (setq item (pop items))
          (let ((beg (point)))
            (insert (car item))
            (make-button beg (point) 'type type 'event (cadr item))
            (let ((padding (- (+ maxlen 3) (length (car item)))))
              (if (or onecol
                      (not items)
                      (> (+ (current-column) padding maxlen)
                         (window-width)))
                  (insert "\n")
                (insert (make-string padding ?\s)))))))
      (insert "\n"))))

(defun magit-popup-format-button (type arg)
  (let* ((c (button-type-get type 'prefix))
         (k (propertize (concat (and c (char-to-string c))
                                (char-to-string (car arg)))
                        'face 'magit-popup-key))
         (d (nth 1 arg))
         (a (unless (symbolp (nth 2 arg)) (nth 2 arg)))
         (v (or (nth 5 arg) (nth 4 arg))))
    (when a
      (setq a (propertize
               a 'face (if v
                           'magit-popup-argument
                         'magit-popup-disabled-argument))))
    (setq v (if (or (booleanp v)
                    (string-equal v ""))
                nil
              (propertize (format "\"%s\"" v)
                          'face 'magit-popup-option-value)))
    (format-spec (button-type-get type 'format)
                 `((?k . ,k)
                   (?d . ,d)
                   (?s . ,(concat "(" a ")"))
                   (?o . ,(concat "(" a v ")"))))))

(provide 'magit-key-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-key-mode.el ends here
