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

(declare-function info 'info)
(declare-function Man-find-section 'man)
(declare-function Man-next-section 'man)

(defvar magit-popup-previous-winconf)

;;; Options

(defcustom magit-popup-show-help-echo t
  ""
  :group 'magit
  :type 'boolean)

(defcustom magit-popup-show-help-section t
  ""
  :group 'magit
  :type 'boolean)

(defcustom magit-popup-use-prefix-argument 'disabled
  ""
  :group 'magit
  :type '(choice
          (const :tag "Use default action, else show popup" default)
          (const :tag "Show popup, else use default action" popup)
          (const :tag "Ignore prefix argument" nil)
          (const :tag "Abort and show usage information" disabled)))

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
    (define-key map [?\C-h ?i] 'magit-popup-info)
    (define-key map [?\C-t] 'magit-popup-toggle-show-popup-commands)
    (define-key map [?\d]   'backward-button)
    (define-key map [?\C-p] 'backward-button)
    (define-key map [?\t]   'forward-button)
    (define-key map [?\C-n] 'forward-button)
    (define-key map [?\r]   'push-button)
    map))

(defvar magit-popup-internal-commands
  '(("Push current button"  push-button)
    ("Goto previous button" backward-button)
    ("Goto next button"     forward-button)
    ("View popup manual"    magit-popup-info)
    ("Popup help prefix"    magit-popup-help)
    ("Toggle help section"  magit-popup-toggle-show-popup-commands)
    ("Abort"                magit-popup-quit)))

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
  'formatter 'magit-popup-format-argument-button
  'format    " %k: %d (%a)"
  'prefix    ?-
  'maxcols   :max-switch-columns)

(define-button-type 'magit-popup-option-button
  'supertype 'magit-popup-button
  'function  'magit-invoke-popup-option
  'property  :options
  'heading   "Options\n"
  'formatter 'magit-popup-format-argument-button
  'format    " %k: %d (%a%v)"
  'prefix    ?=
  'maxcols   1)

(define-button-type 'magit-popup-action-button
  'supertype 'magit-popup-button
  'function  'magit-invoke-popup-action
  'property  :actions
  'heading   "Actions\n"
  'formatter 'magit-popup-format-action-button
  'format    " %k: %d"
  'prefix    nil
  'maxcols   :max-action-columns)

(define-button-type 'magit-popup-command-button
  'supertype 'magit-popup-action-button
  'formatter 'magit-popup-format-command-button
  'action    (lambda (button)
               (call-interactively
                (button-get button 'function))))

(define-button-type 'magit-popup-internal-command-button
  'supertype 'magit-popup-command-button
  'heading   "Popup Commands\n"
  'maxcols   3)

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
         (interactive "P")
         (magit-invoke-popup ',name arg))
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

(defun magit-invoke-popup (popup arg)
  (let* ((value   (symbol-value popup))
         (default (plist-get value :default-action))
         (local   (plist-get value :use-prefix))
         (use-prefix (or local magit-popup-use-prefix-argument)))
    (cond
     ((and arg (eq magit-popup-use-prefix-argument 'disabled))
      (customize-option-other-window 'magit-popup-use-prefix-argument)
      (error (concat "The meaning of prefix arguments has changed.  "
                     "Please explicitly enable their use again.")))
     ((or (and (eq use-prefix 'default) arg)
          (and (eq use-prefix 'popup) (not arg)))
      (if default
          (progn (when (and arg (listp arg))
                   (setq current-prefix-arg (and (not (= (car arg) 4))
                                                 (list (/ (car arg) 4)))))
                 (call-interactively default))
        (message "%s has no default action; showing popup instead." popup)
        (magit-popup-mode-setup popup)))
     ((memq use-prefix '(disabled default popup nil))
      (magit-popup-mode-setup popup)
      (when magit-popup-show-usage
        (message (concat "Type C-h i to view popup manual, "
                         "? to describe an argument or action."))))
     (local
      (error "Invalid :use-prefix popup property value: %s" use-prefix))
     (t
      (error "Invalid magit-popup-use-prefix-argument value: %s" use-prefix)
      ))))

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

(defun magit-popup-quit ()
  (interactive)
  (let ((buf (current-buffer))
        (winconf magit-popup-previous-winconf))
    (if (derived-mode-p 'magit-popup-mode)
        (kill-buffer)
      (magit-popup-help-mode -1)
      (kill-local-variable 'magit-popup-previous-winconf))
    (when winconf
      (set-window-configuration winconf))))

;;; Help

(defun magit-popup-toggle-show-popup-commands ()
  (interactive)
  (setq magit-popup-show-help-section
        (not magit-popup-show-help-section))
  (magit-refresh-popup-buffer)
  (fit-window-to-buffer))

(defun magit-popup-help ()
  (interactive)
  (let* ((man (magit-popup-get :man-page))
         (key (read-key-sequence
               (concat "Describe key" (and man " (? for manpage)") ": ")))
         (int (aref key (1- (length key))))
         (def (or (lookup-key (current-local-map)  key t)
                  (lookup-key (current-global-map) key))))
    (cl-case def
      (magit-invoke-popup-switch
       (magit-popup-woman
        man (nth 2 (assoc int (magit-popup-get :switches)))))
      (magit-invoke-popup-option
       (magit-popup-woman
        man (nth 2 (assoc int (magit-popup-get :options)))))
      (magit-popup-help
       (magit-popup-woman man nil))
      (self-insert-command
       (setq def (nth 2 (assoc int (magit-popup-get :actions))))
       (if def
           (magit-popup-describe-function def)
         (ding)
         (message nil)))
      (nil (ding)
           (message nil))
      (t   (magit-popup-describe-function def)))))

(defun magit-popup-woman (topic argument)
  (unless topic
    (error "No man page associated with %s"
           (magit-popup-get :man-page)))
  (let ((winconf (current-window-configuration)))
    (delete-other-windows)
    (split-window-below)
    (woman topic)
    (if (and argument
             (Man-find-section "OPTIONS")
             (re-search-forward (format "^\t\\(-., \\)?%s[[=\n]" argument)
                                (save-excursion
                                  (Man-next-section 1)
                                  (point))
                                t))
        (goto-char (1+ (match-beginning 0)))
      (goto-char (point-min)))
    (setq magit-popup-previous-winconf winconf))
  (magit-popup-help-mode)
  (fit-window-to-buffer (next-window)))

(defun magit-popup-describe-function (function)
  (let ((winconf (current-window-configuration)))
    (delete-other-windows)
    (split-window-below)
    (other-window 1)
    (describe-function function)
    (fit-window-to-buffer)
    (other-window 1)
    (setq magit-popup-previous-winconf winconf)
    (magit-popup-help-mode)))

(defun magit-popup-info ()
  (interactive)
  (let ((winconf (current-window-configuration)))
    (delete-other-windows)
    (split-window-below)
    (info "(magit.info)Popups")
    (magit-popup-help-mode)
    (setq magit-popup-previous-winconf winconf))
  (magit-popup-help-mode)
  (fit-window-to-buffer (next-window)))

(define-minor-mode magit-popup-help-mode
  ""
  :keymap '(([remap Man-quit]    . magit-popup-quit)
            ([remap Info-exit]   . magit-popup-quit)
            ([remap quit-window] . magit-popup-quit)))

;;; Mode

(defvar-local magit-popup-previous-winconf nil)

(define-derived-mode magit-popup-mode fundamental-mode "MagitPopup"
  ""
  (setq buffer-read-only t)
  (set (make-local-variable 'scroll-margin) 0)
  (set (make-local-variable 'magit-popup-show-help-section)
       magit-popup-show-help-section))

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
      (magit-popup-insert-section 'magit-popup-switch-button)
      (magit-popup-insert-section 'magit-popup-option-button)
      (magit-popup-insert-section 'magit-popup-action-button)
      (run-hooks 'magit-refresh-popup-buffer-hook)
      (when magit-popup-show-help-section
        (magit-popup-insert-command-section
         'magit-popup-internal-command-button
         magit-popup-internal-commands)))
    (if event
        (while (and (forward-button 1)
                    (let ((b (button-at (point))))
                      (or (not (equal (button-get b 'prefix) prefix))
                          (not (equal (button-get b 'event)  event))))))
      (re-search-forward "^Actions" nil t)
      (forward-button 1))))

;;; Draw

(defvar magit-popup-min-padding 3)

(defun magit-popup-insert-section (type &optional spec)
  (let* ((heading   (button-type-get type 'heading))
         (formatter (button-type-get type 'formatter))
         (buttons (mapcar (lambda (elt)
                            (funcall formatter type elt))
                          (or spec (magit-popup-get
                                    (button-type-get type 'property)))))
         (maxcols (button-type-get type 'maxcols)))
    (cl-typecase maxcols
      (keyword (setq maxcols (magit-popup-get maxcols)))
      (symbol  (setq maxcols (symbol-value maxcols))))
    (when buttons
      (insert (propertize heading 'face 'magit-popup-header))
      (unless (string-match "\n$" heading)
        (insert "\n"))
      (let ((colwidth
             (+ (apply 'max (mapcar (lambda (e) (length (car e))) buttons))
                magit-popup-min-padding)))
        (dolist (button buttons)
          (unless (bolp)
            (let ((padding (- colwidth (% (current-column) colwidth))))
              (if (and (< (+ (current-column) padding colwidth)
                          (window-width))
                       (< (ceiling (/ (current-column) (* colwidth 1.0)))
                          (or maxcols 1000)))
                  (insert (make-string padding ?\s))
                (insert "\n"))))
          (apply 'insert-button button)))
      (insert (if (= (char-before) ?\n) "\n" "\n\n")))))

(defun magit-popup-format-argument-button (type item)
  (list (format-spec
         (button-type-get type 'format)
         `((?k . ,(propertize (concat (char-to-string
                                       (button-type-get type 'prefix))
                                      (key-description
                                       (if (vectorp (car item))
                                           (car item)
                                         (vector (car item)))))
                              'face 'magit-popup-key))
           (?d . ,(nth 1 item))
           (?a . ,(propertize (nth 2 item)
                              'face (if (nth 4 item)
                                        'magit-popup-argument
                                      'magit-popup-disabled-argument)))
           (?v . ,(if (nth 4 item)
                      (propertize (format "\"%s\"" (nth 5 item))
                                  'face 'magit-popup-option-value)
                    ""))))
        'type type 'event (car item)))

(defun magit-popup-format-action-button (type item)
  (list (format-spec
         (button-type-get type 'format)
         `((?k . ,(propertize (key-description (if (vectorp (car item))
                                                   (car item)
                                                 (vector (car item))))
                              'face 'magit-popup-key))
           (?d . ,(nth 1 item))))
        'type type 'event (car item)))

(defun magit-popup-insert-command-section (type spec)
  (magit-popup-insert-section
   type (mapcar (lambda (elt)
                  (list (car (where-is-internal (cadr elt)
                                                (current-local-map)))
                        (car elt)))
                spec)))

(defun magit-popup-format-command-button (type item)
  (nconc (magit-popup-format-action-button type item)
         (list 'function (cadr item))))

(provide 'magit-key-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-key-mode.el ends here
