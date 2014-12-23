;;; magit-popup.el --- infix arguments with feedback

;; Copyright (C) 2010-2014  The Magit Project Developers
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

;; This library was inspired by and replaces library `magit-key-mode',
;; which was written by Phil Jackson <phil@shellarchive.co.uk> and is
;; distributed under the GNU General Public License version 3 or later.

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

;; This library implements a generic interface for toggling switches
;; and setting options and then invoking an Emacs command which does
;; something with these arguments.  The prototypical use is for the
;; command to call an external process, passing on the arguments as
;; command line arguments.  But this is only one of many possible
;; uses (though the one this library is optimized for).

;; With the Emacs concept of "prefix arguments" in mind this could be
;; described as "infix arguments with feedback in a buffer".

;; Commands that set the prefix argument for the subsequent command do
;; not limit what that next command could be.  But entering a command
;; console popup does limit the selection to the commands defined for
;; that popup, and so we use the term "infix" instead of "prefix".

;;; Code:

(require 'cl-lib)
(require 'dash)

(require 'magit-mode)

(require 'button)
(require 'format-spec)

(declare-function info 'info)
(declare-function Man-find-section 'man)
(declare-function Man-next-section 'man)

;;; Settings
;;;; Custom Groups

(defgroup magit-popup nil
  "Infix arguments with a popup as feedback."
  :group 'convenience)

(defgroup magit-popup-faces nil
  "Faces used by Magit-Popup."
  :group 'magit-popup)

;;;; Custom Options

(defcustom magit-popup-show-help-echo t
  "Show usage information in the echo area."
  :group 'magit-popup
  :type 'boolean)

(defcustom magit-popup-show-help-section t
  "Initially show section with commands common to all popups.
This section can also be toggled temporarily using \
\\<magit-popup-mode-map>\\[magit-popup-toggle-show-popup-commands]."
  :group 'magit-popup
  :type 'boolean)

(defcustom magit-popup-use-prefix-argument 'disabled
  "Control how prefix arguments affect infix argument popups.

This option controls the effect that the use of a prefix argument
before entering a popup has.  The *intended* default is `default',
but the *actual* default is `disabled'.  This is necessary because
the old popup implementation did simply forward such a pre-popup
prefix argument to the action invoked from the popup, and changing
that without users being aware of it could lead to tears.

`disabled' Bring up a Custom option buffer so that the user reads
           the above and then makes an informed choice.

`default'  With a prefix argument directly invoke the popup's
           default action (an Emacs command), instead of bringing
           up the popup.

           When the default action is invoked like this, then the
           prefix and infix arguments might be passed on verbatim
           or modified.  How exactly this happens is still subject
           to change.  If it seems to dangerous that the behavior
           might change at any time, then use `nil' for now.

`popup'    With a prefix argument bring up the popup, otherwise
           directly invoke the popup's default action.

`nil'      Ignore prefix arguments."
  :group 'magit-popup
  :type '(choice
          (const :tag "Use default action, else show popup" default)
          (const :tag "Show popup, else use default action" popup)
          (const :tag "Ignore prefix argument" nil)
          (const :tag "Abort and show usage information" disabled)))

;;;; Custom Faces

(defface magit-popup-header
  '((t :inherit font-lock-keyword-face))
  "Face for key mode header lines."
  :group 'magit-popup-faces)

(defface magit-popup-key
  '((t :inherit font-lock-builtin-face))
  "Face for key mode buttons."
  :group 'magit-popup-faces)

(defface magit-popup-argument
  '((t :inherit font-lock-warning-face))
  "Face used to display enabled arguments in popups."
  :group 'magit-popup-faces)

(defface magit-popup-disabled-argument
  '((t :inherit shadow))
  "Face used to display disabled arguments in popups."
  :group 'magit-popup-faces)

(defface magit-popup-option-value
  '((t :inherit font-lock-string-face))
  "Face used to display option values in popups."
  :group 'magit-popup-faces)

(define-obsolete-face-alias 'magit-key-mode-header-face 'magit-popup-header "2.1.0")
(define-obsolete-face-alias 'magit-key-mode-button-face 'magit-popup-key "2.1.0")
(define-obsolete-face-alias 'magit-key-mode-switch-face 'magit-popup-argument "2.1.0")

;;;; Keymap

(defvar magit-popup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'magit-invoke-popup-action)
    (define-key map [?- t]        'magit-invoke-popup-switch)
    (define-key map [?= t]        'magit-invoke-popup-option)
    (define-key map [?\C-c ?\C-c] 'magit-popup-set-default-arguments)
    (define-key map [?\C-x ?\C-s] 'magit-popup-save-default-arguments)
    (define-key map [?\C-g]       'magit-popup-quit)
    (define-key map [??]          'magit-popup-help)
    (define-key map [?\C-h ?i]    'magit-popup-info)
    (define-key map [?\C-t]       'magit-popup-toggle-show-popup-commands)
    (define-key map [?\d]         'backward-button)
    (define-key map [?\C-p]       'backward-button)
    (define-key map [?\t]         'forward-button)
    (define-key map [?\C-n]       'forward-button)
    (define-key map [?\r]         'push-button)
    map))

(defvar magit-popup-internal-commands
  '(("Set defaults"          magit-popup-set-default-arguments)
    ("Goto previous button"  backward-button)
    ("View popup manual"     magit-popup-info)
    ("Save defaults"         magit-popup-save-default-arguments)
    ("Goto next button"      forward-button)
    ("  Toggle help section" magit-popup-toggle-show-popup-commands)
    ("    Abort"             magit-popup-quit)
    ("Push button"           push-button)
    ("    Popup help prefix" magit-popup-help)))

;;;; Buttons

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
  'format    " %k %d (%a)"
  'prefix    ?-
  'maxcols   1)

(define-button-type 'magit-popup-option-button
  'supertype 'magit-popup-button
  'function  'magit-invoke-popup-option
  'property  :options
  'heading   "Options\n"
  'formatter 'magit-popup-format-argument-button
  'format    " %k %d (%a%v)"
  'prefix    ?=
  'maxcols   1)

(define-button-type 'magit-popup-action-button
  'supertype 'magit-popup-button
  'function  'magit-invoke-popup-action
  'property  :actions
  'heading   "Actions\n"
  'formatter 'magit-popup-format-action-button
  'format    " %k %d"
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

(defun magit-popup-get (prop)
  (if (memq prop '(:switches :options :actions))
      (plist-get magit-this-popup-events prop)
    (plist-get (symbol-value magit-this-popup) prop)))

(defun magit-popup-put (prop val)
  (if (memq prop '(:switches :options :actions))
      (setq magit-this-popup-events
            (plist-put magit-this-popup-events prop val))
    (error "Property %s isn't supported" prop)))

(defvar magit-current-popup nil)
(defvar magit-current-popup-args nil)

(defun magit-current-popup-args (&rest filter)
  (let ((-compare-fn (lambda (a b) (magit-popup-arg-match b a))))
    (-filter (if (eq (car filter) :not)
                 (lambda (arg) (not (-contains? (cdr filter) arg)))
               (when (eq (car filter) :only)
                 (pop filter))
               (lambda (arg) (-contains? filter arg)))
             magit-current-popup-args)))

(defun magit-popup-arg-match (pattern string)
  (if (or (string-match-p "=$" pattern)
          (string-match-p "^-[A-Z]$" pattern))
      (string-match (format "^%s\\(.*\\)$" pattern) string)
    (string-equal string pattern)))

(cl-defstruct magit-popup-event key dsc arg fun use val)

(defun magit-popup-event-keydsc (ev)
  (let ((key (magit-popup-event-key ev)))
    (key-description (if (vectorp key) key (vector key)))))

(defun magit-popup-lookup (event type)
  (--first (equal (magit-popup-event-key it) event)
           (magit-popup-get type)))

(defun magit-popup-get-args ()
  (cl-mapcan (lambda (elt)
               (when (magit-popup-event-use elt)
                 (list (format "%s%s"
                               (magit-popup-event-arg elt)
                               (or (magit-popup-event-val elt) "")))))
             (append (magit-popup-get :switches)
                     (magit-popup-get :options))))

(defun magit-popup-convert-switches (val def)
  (mapcar (lambda (ev)
            (let ((a (nth 2 ev)))
              (make-magit-popup-event
               :key (car ev) :dsc (cadr ev) :arg a
               :use (and (member a val) t))))
          def))

(defun magit-popup-convert-options (val def)
  (mapcar (lambda (ev)
            (let* ((a (nth 2 ev))
                   (r (format "^%s\\(.*\\)" a))
                   (v (--first (string-match r it) val)))
              (make-magit-popup-event
               :key (car ev)  :dsc (cadr ev) :arg a
               :use (and v t) :val (and v (match-string 1 v))
               :fun (nth 3 ev))))
          def))

(defun magit-popup-convert-actions (val def)
  (mapcar (lambda (ev)
            (make-magit-popup-event
             :key (car ev) :dsc (cadr ev) :fun (nth 2 ev)))
          def))

;;; Define

(defmacro magit-define-popup (name doc &rest args)
  "\n\n(fn NAME DOC [GROUP [MODE [OPTION]]] :KEYWORD VALUE...)"
  (declare (indent defun) (doc-string 2))
  (let* ((grp  (unless (keywordp (car args)) (pop args)))
         (mode (unless (keywordp (car args)) (pop args)))
         (opt  (symbol-name name))
         (opt  (if (keywordp (car args))
                   (intern (concat (if (string-match-p "-popup$" opt)
                                       (substring opt 0 -6)
                                     opt)
                                   "-arguments"))
                 (pop args))))
    `(progn
       (defun ,name (&optional arg) ,doc
         (interactive "P")
         (magit-invoke-popup ',name ,mode arg))
       (defvar ,name
         (list :variable ',opt ,@args))
       (defcustom ,opt (plist-get ,name :default-arguments)
         ""
         ,@(and grp (list :group grp))
         :type '(repeat (string :tag "Argument")))
       (defun ,opt ()
         (if (eq magit-current-popup ',name)
             magit-current-popup-args
           ,opt))
       (put ',opt 'definition-name ',name))))

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

(defvar-local magit-popup-previous-winconf nil)

(defun magit-invoke-popup (popup mode arg)
  (let* ((def     (symbol-value popup))
         (val     (symbol-value (plist-get def :variable)))
         (default (plist-get def :default-action))
         (local   (plist-get def :use-prefix))
         (use-prefix (or local magit-popup-use-prefix-argument)))
    (cond
     ((and arg (eq magit-popup-use-prefix-argument 'disabled))
      (customize-option-other-window 'magit-popup-use-prefix-argument)
      (error (concat "The meaning of prefix arguments has changed.  "
                     "Please explicitly enable their use again.")))
     ((or (and (eq use-prefix 'default) arg)
          (and (eq use-prefix 'popup) (not arg)))
      (if default
          (let ((magit-current-popup (list popup 'default))
                (magit-current-popup-args
                 (let ((magit-this-popup popup)
                       (magit-this-popup-events nil))
                   (magit-popup-default-setup val def)
                   (magit-popup-get-args))))
            (when (and arg (listp arg))
              (setq current-prefix-arg (and (not (= (car arg) 4))
                                            (list (/ (car arg) 4)))))
            (call-interactively default))
        (message "%s has no default action; showing popup instead." popup)
        (magit-popup-mode-setup popup mode)))
     ((memq use-prefix '(disabled default popup nil))
      (magit-popup-mode-setup popup mode)
      (when magit-popup-show-help-echo
        (message (concat "Type C-h i to view popup manual, "
                         "? to describe an argument or action."))))
     (local
      (error "Invalid :use-prefix popup property value: %s" use-prefix))
     (t
      (error "Invalid magit-popup-use-prefix-argument value: %s" use-prefix)
      ))))

(defun magit-invoke-popup-switch (event)
  (interactive (list last-command-event))
  (--if-let (magit-popup-lookup event :switches)
      (progn
        (setf (magit-popup-event-use it)
              (not (magit-popup-event-use it)))
        (magit-refresh-popup-buffer))
    (error "%c isn't bound to any switch" event)))

(defun magit-invoke-popup-option (event)
  (interactive (list last-command-event))
  (--if-let (magit-popup-lookup event :options)
      (progn
        (if (magit-popup-event-use it)
            (setf (magit-popup-event-use it) nil)
          (let* ((arg (magit-popup-event-arg it))
                 (val (funcall
                       (magit-popup-event-fun it)
                       (concat arg (unless (string-match-p "=$" arg) ": "))
                       (magit-popup-event-val it))))
            (setf (magit-popup-event-use it) t)
            (setf (magit-popup-event-val it) val)))
        (magit-refresh-popup-buffer))
    (error "%c isn't bound to any option" event)))

(defun magit-invoke-popup-action (event)
  (interactive (list last-command-event))
  (--if-let (magit-popup-lookup event :actions)
      (let ((magit-current-popup magit-this-popup)
            (magit-current-popup-args (magit-popup-get-args))
            (command (magit-popup-event-fun it)))
        (magit-popup-quit)
        (call-interactively command)
        (setq this-command command))
    (if (eq event ?q)
        (magit-popup-quit)
      (error "%c isn't bound to any action" event))))

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

;;; Save

(defun magit-popup-set-default-arguments (arg)
  (interactive "P")
  (customize-set-variable (magit-popup-get :variable)
                          (magit-popup-get-args))
  (unless arg (magit-popup-quit)))

(defun magit-popup-save-default-arguments (arg)
  (interactive "P")
  (customize-save-variable (magit-popup-get :variable)
                           (magit-popup-get-args))
  (unless arg (magit-popup-quit)))

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
       (magit-popup-woman man (magit-popup-lookup int :switches)))
      (magit-invoke-popup-option
       (magit-popup-woman man (magit-popup-lookup int :options)))
      (magit-popup-help
       (magit-popup-woman man nil))
      (self-insert-command
       (setq def (magit-popup-lookup int :actions))
       (if def
           (magit-popup-describe-function (magit-popup-event-fun def))
         (ding)
         (message nil)))
      (nil (ding)
           (message nil))
      (t   (magit-popup-describe-function def)))))

(defun magit-popup-woman (topic arg)
  (unless topic
    (error "No man page associated with %s"
           (magit-popup-get :man-page)))
  (when arg
    (setq arg (magit-popup-event-arg arg)))
  (let ((winconf (current-window-configuration)))
    (delete-other-windows)
    (split-window-below)
    (with-no-warnings ; display-buffer-function is obsolete
      (let ((display-buffer-alist nil)
            (display-buffer-function nil))
        (woman topic)))
    (if (and arg
             (Man-find-section "OPTIONS")
             (re-search-forward (format "^\t\\(-., \\)?%s[[=\n]" arg)
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
    (with-no-warnings ; display-buffer-function is obsolete
      (let ((display-buffer-alist nil)
            (display-buffer-function nil))
        (describe-function function)))
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
  "Auxiliary minor mode used to restore previous window configuration.
When some sort of help buffer is created from within a popup,
then this minor mode is turned on in that buffer, so that when
the user quits it, the previous window configuration is also
restored."
  :keymap '(([remap Man-quit]    . magit-popup-quit)
            ([remap Info-exit]   . magit-popup-quit)
            ([remap quit-window] . magit-popup-quit)))

;;; Modes

(define-derived-mode magit-popup-mode fundamental-mode "MagitPopup"
  "Major mode for infix argument popups."
  (setq buffer-read-only t)
  (setq-local scroll-margin 0)
  (setq-local magit-popup-show-help-section magit-popup-show-help-section)
  (add-hook 'magit-popup-setup-hook 'magit-popup-default-setup nil t))

(put 'magit-popup-mode 'mode-class 'special)

(defvar magit-popup-setup-hook nil)

(defun magit-popup-default-setup (val def)
  (magit-popup-put :switches (magit-popup-convert-switches
                              val (plist-get def :switches)))
  (magit-popup-put :options  (magit-popup-convert-options
                              val (plist-get def :options)))
  (magit-popup-put :actions  (magit-popup-convert-actions
                              val (plist-get def :actions))))

(define-derived-mode magit-popup-sequence-mode magit-popup-mode "MagitPopup"
  "Major mode for infix argument popups, which are affected by state.
Used for popups that display different actions depending on some
external state.  Within Magit this is used for sequence commands
such as rebase.  The function `:sequence-predicate', which takes
no arguments, is used to determine whether to use the actions
defined with regular `:actions' or those in `:sequence-actions'.
When a sequence is in progress the arguments are not available
in the popup."
  (remove-hook 'magit-popup-setup-hook 'magit-popup-default-setup t)
  (add-hook    'magit-popup-setup-hook
               (lambda (val def)
                 (if (funcall (magit-popup-get :sequence-predicate))
                     (magit-popup-put
                      :actions (magit-popup-convert-actions
                                val (magit-popup-get :sequence-actions)))
                   (magit-popup-default-setup val def)))
               t t))

(defun magit-popup-mode-setup (popup mode)
  (let ((val (symbol-value (plist-get (symbol-value popup) :variable)))
        (def (symbol-value popup)))
    (magit-popup-mode-display-buffer (get-buffer-create
                                      (format "*%s*" popup))
                                     (or mode 'magit-popup-mode))
    (setq magit-this-popup popup)
    (run-hook-with-args 'magit-popup-setup-hook val def))
  (magit-refresh-popup-buffer)
  (fit-window-to-buffer))

(defun magit-popup-mode-display-buffer (buffer mode)
  (let ((winconf (current-window-configuration)))
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer buffer)
    (funcall mode)
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
         (buttons (mapcar (lambda (ev)
                            (funcall formatter type ev))
                          (or spec (magit-popup-get
                                    (button-type-get type 'property)))))
         (maxcols (button-type-get type 'maxcols))
         (pred (magit-popup-get :sequence-predicate)))
    (if (and pred (funcall pred))
        (setq maxcols nil)
      (cl-typecase maxcols
        (keyword (setq maxcols (magit-popup-get maxcols)))
        (symbol  (setq maxcols (symbol-value maxcols)))))
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

(defun magit-popup-format-argument-button (type ev)
  (list (format-spec
         (button-type-get type 'format)
         `((?k . ,(propertize (concat
                               (--when-let (button-type-get type 'prefix)
                                 (char-to-string it))
                               (magit-popup-event-keydsc ev))
                              'face 'magit-popup-key))
           (?d . ,(magit-popup-event-dsc ev))
           (?a . ,(propertize (magit-popup-event-arg ev)
                              'face (if (magit-popup-event-use ev)
                                        'magit-popup-argument
                                      'magit-popup-disabled-argument)))
           (?v . ,(let ((val (magit-popup-event-val ev)))
                    (if (and (magit-popup-event-use ev)
                             (not (equal val "")))
                        (propertize (format "\"%s\"" val)
                                    'face 'magit-popup-option-value)
                      "")))))
        'type type 'event (magit-popup-event-key ev)))

(defun magit-popup-format-action-button (type ev)
  (list (format-spec
         (button-type-get type 'format)
         `((?k . ,(propertize (magit-popup-event-keydsc ev)
                              'face 'magit-popup-key))
           (?d . ,(magit-popup-event-dsc ev))))
        'type type 'event (magit-popup-event-key ev)))

(defun magit-popup-insert-command-section (type spec)
  (magit-popup-insert-section
   type (mapcar (lambda (elt)
                  (list (car (where-is-internal (cadr elt)
                                                (current-local-map)))
                        (car elt)))
                spec)))

(defun magit-popup-format-command-button (type elt)
  (nconc (magit-popup-format-action-button
          type (make-magit-popup-event :key (car  elt)
                                       :dsc (cadr elt)))
         (list 'function (cadr elt))))

;;; magit-popup.el ends soon

(defconst magit-popup-font-lock-keywords
  (eval-when-compile
    `((,(concat "(\\(magit-define-popup\\)\\_>"
                "[ \t'\(]*"
                "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
       (1 'font-lock-keyword-face)
       (2 'font-lock-function-name-face nil t)))))

(font-lock-add-keywords 'emacs-lisp-mode magit-popup-font-lock-keywords)

(provide 'magit-popup)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-popup.el ends here
