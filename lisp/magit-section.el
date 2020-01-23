;;; magit-section.el --- Sections for read-only buffers  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2020  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Package-Requires: ((emacs "25.1") (dash "20180910"))
;; Keywords: tools
;; Homepage: https://github.com/magit/magit

;; Magit-Section is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit-Section is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This package implements the main user interface of Magit — the
;; collapsible sections that make up its buffers.  This package used
;; to be distributed as part of Magit but now it can also be used by
;; other packages that have nothing to do with Magit or Git.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'eieio)

(eval-when-compile
  (require 'benchmark)
  (require 'subr-x))

;;; Hooks

(defvar magit-section-movement-hook nil
  "Hook run by `magit-section-goto'.
That function in turn is used by all section movement commands.")

(defvar magit-section-highlight-hook
  '(magit-section-highlight
    magit-section-highlight-selection)
  "Functions used to highlight the current section.
Each function is run with the current section as only argument
until one of them returns non-nil.")

(defvar magit-section-unhighlight-hook nil
  "Functions used to unhighlight the previously current section.
Each function is run with the current section as only argument
until one of them returns non-nil.  Most sections are properly
unhighlighted without requiring a specialized unhighlighter,
diff-related sections being the only exception.")

(defvar magit-section-set-visibility-hook
  '(magit-section-cached-visibility)
  "Hook used to set the initial visibility of a section.
Stop at the first function that returns non-nil.  The returned
value should be `show', `hide' or nil.  If no function returns
non-nil, determine the visibility as usual, i.e. use the
hardcoded section specific default (see `magit-insert-section').")

(defvar magit-section-goto-successor-hook nil
  "Hook used to go to the same section as was current before a refresh.
This is only used if the standard mechanism for doing so did not
succeed.")

;;; Options

(defgroup magit-section nil
  "Expandable sections."
  :link '(info-link "(magit)Sections")
  :group 'extensions)

(defcustom magit-section-show-child-count t
  "Whether to append the number of children to section headings.
This only applies to sections for which doing so makes sense."
  :package-version '(magit . "2.1.0")
  :group 'magit-section
  :type 'boolean)

(defcustom magit-section-cache-visibility t
  "Whether to cache visibility of sections.

Sections always retain their visibility state when they are being
recreated during a refresh.  But if a section disappears and then
later reappears again, then this option controls whether this is
the case.

If t, then cache the visibility of all sections.  If a list of
section types, then only do so for matching sections.  If nil,
then don't do so for any sections."
  :package-version '(magit . "2.12.0")
  :group 'magit-section
  :type '(choice (const  :tag "Don't cache visibility" nil)
                 (const  :tag "Cache visibility of all sections" t)
                 (repeat :tag "Cache visibility for section types" symbol)))

(defcustom magit-section-initial-visibility-alist
  '((stashes . hide))
  "Alist controlling the initial visibility of sections.

Each element maps a section type or lineage to the initial
visibility state for such sections.  The state has to be one of
`show' or `hide', or a function that returns one of these symbols.
A function is called with the section as the only argument.

Use the command `magit-describe-section' to determine a section's
lineage or type.  The vector in the output is the section lineage
and the type is the first element of that vector.  Wildcards can
be used, see `magit-section-match'.

Currently this option is only used to override hardcoded defaults,
but in the future it will also be used set the defaults.

An entry whose key is `magit-status-initial-section' specifies
the visibility of the section `magit-status-goto-initial-section'
jumps to.  This does not only override defaults, but also other
entries of this alist."
  :package-version '(magit . "2.12.0")
  :group 'magit-section
  :type '(alist :key-type (sexp :tag "Section type/lineage")
                :value-type (choice (const hide)
                                    (const show)
                                    function)))

(defcustom magit-section-visibility-indicator
  (if (window-system)
      '(magit-fringe-bitmap> . magit-fringe-bitmapv)
    '("…" . t))
  "Whether and how to indicate that a section can be expanded/collapsed.

If nil, then don't show any indicators.
Otherwise the value has to have one of these two forms:

\(EXPANDABLE-BITMAP . COLLAPSIBLE-BITMAP)

  Both values have to be variables whose values are fringe
  bitmaps.  In this case every section that can be expanded or
  collapsed gets an indicator in the left fringe.

  To provide extra padding around the indicator, set
  `left-fringe-width' in `magit-mode-hook'.

\(STRING . BOOLEAN)

  In this case STRING (usually an ellipsis) is shown at the end
  of the heading of every collapsed section.  Expanded sections
  get no indicator.  The cdr controls whether the appearance of
  these ellipsis take section highlighting into account.  Doing
  so might potentially have an impact on performance, while not
  doing so is kinda ugly."
  :package-version '(magit . "3.0.0")
  :group 'magit-section
  :type '(choice (const :tag "No indicators" nil)
                 (cons  :tag "Use +- fringe indicators"
                        (const magit-fringe-bitmap+)
                        (const magit-fringe-bitmap-))
                 (cons  :tag "Use >v fringe indicators"
                        (const magit-fringe-bitmap>)
                        (const magit-fringe-bitmapv))
                 (cons  :tag "Use bold >v fringe indicators)"
                        (const magit-fringe-bitmap-bold>)
                        (const magit-fringe-bitmap-boldv))
                 (cons  :tag "Use custom fringe indicators"
                        (variable :tag "Expandable bitmap variable")
                        (variable :tag "Collapsible bitmap variable"))
                 (cons  :tag "Use ellipses at end of headings"
                        (string :tag "Ellipsis" "…")
                        (choice :tag "Use face kludge"
                                (const :tag "Yes (potentially slow)" t)
                                (const :tag "No (kinda ugly)" nil)))))

(defcustom magit-keep-region-overlay nil
  "Whether to keep the region overlay when there is a valid selection.

By default Magit removes the regular region overlay if, and only
if, that region constitutes a valid selection as understood by
Magit commands.  Otherwise it does not remove that overlay, and
the region looks like it would in other buffers.

There are two types of such valid selections: hunk-internal
regions and regions that select two or more sibling sections.
In such cases Magit removes the region overlay and instead
highlights a slightly larger range.  All text (for hunk-internal
regions) or the headings of all sections (for sibling selections)
that are inside that range (not just inside the region) are acted
on by commands such as the staging command.  This buffer range
begins at the beginning of the line on which the region begins
and ends at the end of the line on which the region ends.

Because Magit acts on this larger range and not the region, it is
actually quite important to visualize that larger range.  If we
don't do that, then one might think that these commands act on
the region instead.  If you want to *also* visualize the region,
then set this option to t.  But please note that when the region
does *not* constitute a valid selection, then the region is
*always* visualized as usual, and that it is usually under such
circumstances that you want to use a non-magit command to act on
the region.

Besides keeping the region overlay, setting this option to t also
causes all face properties, except for `:foreground', to be
ignored for the faces used to highlight headings of selected
sections.  This avoids the worst conflicts that result from
displaying the region and the selection overlays at the same
time.  We are not interested in dealing with other conflicts.
In fact we *already* provide a way to avoid all of these
conflicts: *not* changing the value of this option.

It should be clear by now that we consider it a mistake to set
this to display the region when the Magit selection is also
visualized, but since it has been requested a few times and
because it doesn't cost much to offer this option we do so.
However that might change.  If the existence of this option
starts complicating other things, then it will be removed."
  :package-version '(magit . "2.3.0")
  :group 'magit-section
  :type 'boolean)

(defcustom magit-section-disable-line-numbers t
  "In Magit buffers, whether to disable modes that display line numbers.

Some users who turn on `global-display-line-numbers-mode' (or
`global-nlinum-mode' or `global-linum-mode') expect line numbers
to be displayed everywhere except in Magit buffers.  Other users
do not expect Magit buffers to be treated differently.  At least
in theory users in the first group should not use the global mode,
but that ship has sailed, thus this option."
  :package-version '(magit . "3.0.0")
  :group 'magit-section
  :type 'boolean)

;;; Faces

(defgroup magit-section-faces nil
  "Faces used by Magit-Section."
  :group 'magit-section
  :group 'faces)

(defface magit-section-highlight
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey95")
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey20"))
  "Face for highlighting the current section."
  :group 'magit-section-faces)

(defface magit-section-heading
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "DarkGoldenrod4"
     :weight bold)
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "LightGoldenrod2"
     :weight bold))
  "Face for section headings."
  :group 'magit-section-faces)

(defface magit-section-secondary-heading
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :weight bold))
  "Face for section headings of some secondary headings."
  :group 'magit-section-faces)

(defface magit-section-heading-selection
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "salmon4")
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "LightSalmon3"))
  "Face for selected section headings."
  :group 'magit-section-faces)

;;; Classes

(defvar magit--current-section-hook nil
  "Internal variable used for `magit-describe-section'.")

(defvar magit--section-type-alist nil)

(defclass magit-section ()
  ((keymap   :initform nil :allocation :class)
   (type     :initform nil :initarg :type)
   (value    :initform nil :initarg :value)
   (start    :initform nil :initarg :start)
   (content  :initform nil)
   (end      :initform nil)
   (hidden   :initform nil)
   (washer   :initform nil)
   (process  :initform nil)
   (heading-highlight-face :initform nil)
   (inserter :initform (symbol-value 'magit--current-section-hook))
   (parent   :initform nil :initarg :parent)
   (children :initform nil)))

;;; Mode

(defvar symbol-overlay-inhibit-map)

(defvar magit-section-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "C-i") 'magit-section-toggle)
    (define-key map [C-tab]     'magit-section-cycle)
    (define-key map [M-tab]     'magit-section-cycle)
    ;; [backtab] is the most portable binding for Shift+Tab.
    (define-key map [backtab]   'magit-section-cycle-global)
    (define-key map (kbd   "^") 'magit-section-up)
    (define-key map (kbd   "p") 'magit-section-backward)
    (define-key map (kbd   "n") 'magit-section-forward)
    (define-key map (kbd "M-p") 'magit-section-backward-sibling)
    (define-key map (kbd "M-n") 'magit-section-forward-sibling)
    (define-key map "1"         'magit-section-show-level-1)
    (define-key map "2"         'magit-section-show-level-2)
    (define-key map "3"         'magit-section-show-level-3)
    (define-key map "4"         'magit-section-show-level-4)
    (define-key map (kbd "M-1") 'magit-section-show-level-1-all)
    (define-key map (kbd "M-2") 'magit-section-show-level-2-all)
    (define-key map (kbd "M-3") 'magit-section-show-level-3-all)
    (define-key map (kbd "M-4") 'magit-section-show-level-4-all)
    map))

(define-derived-mode magit-section-mode special-mode "Magit-Sections"
  "Parent major mode from which major modes with Magit-like sections inherit.

Magit-Section is documented in info node `(magit-section)'."
  :group 'magit-section
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local line-move-visual t) ; see #1771
  ;; Turn off syntactic font locking, but not by setting
  ;; `font-lock-defaults' because that would enable font locking, and
  ;; not all magit plugins may be ready for that (see #3950).
  (setq-local font-lock-syntactic-face-function #'ignore)
  (setq show-trailing-whitespace nil)
  (setq-local symbol-overlay-inhibit-map t)
  (setq list-buffers-directory (abbreviate-file-name default-directory))
  ;; (hack-dir-local-variables-non-file-buffer)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (add-hook 'post-command-hook #'magit-section-update-highlight t t)
  (add-hook 'deactivate-mark-hook #'magit-section-update-highlight t t)
  (setq-local redisplay-highlight-region-function
              'magit-section--highlight-region)
  (setq-local redisplay-unhighlight-region-function
              'magit-section--unhighlight-region)
  (when magit-section-disable-line-numbers
    (when (bound-and-true-p global-linum-mode)
      (linum-mode -1))
    (when (and (fboundp 'nlinum-mode)
               (bound-and-true-p global-nlinum-mode))
      (nlinum-mode -1))
    (when (and (fboundp 'display-line-numbers-mode)
               (bound-and-true-p global-display-line-numbers-mode))
      (display-line-numbers-mode -1)))
  (add-hook 'kill-buffer-hook 'magit-preserve-section-visibility-cache))

;;; Core

(defvar-local magit-root-section nil
  "The root section in the current buffer.
All other sections are descendants of this section.  The value
of this variable is set by `magit-insert-section' and you should
never modify it.")
(put 'magit-root-section 'permanent-local t)

(defun magit-current-section ()
  "Return the section at point."
  (or (get-text-property (point) 'magit-section) magit-root-section))

(defun magit-section-ident (section)
  "Return an unique identifier for SECTION.
The return value has the form ((TYPE . VALUE)...)."
  (with-slots (type value parent) section
    (cons (cons type
                (cond ((eieio-object-p value)
                       (magit-section-ident-value value))
                      ((not (memq type '(unpulled unpushed))) value)
                      ((string-match-p "@{upstream}" value) value)
                      ;; Unfortunately Git chokes on "@{push}" when
                      ;; the value of `push.default' does not allow a
                      ;; 1:1 mapping.  Arbitrary commands may consult
                      ;; the section value so we cannot use "@{push}".
                      ;; But `unpushed' and `unpulled' sections should
                      ;; keep their identity when switching branches
                      ;; so we have to use another value here.
                      ((string-match-p "\\`\\.\\." value) "..@{push}")
                      (t "@{push}..")))
          (and parent
               (magit-section-ident parent)))))

(cl-defgeneric magit-section-ident-value (value)
  "Return a constant representation of VALUE.
VALUE is the value of a `magit-section' object.  If that is an
object itself, then that is not suitable to be used to identify
the section because two objects may represent the same thing but
not be equal.  If possible a method should be added for such
objects, which returns a value that is equal.  Otherwise the
catch-all method is used, which just returns the argument
itself.")

(cl-defmethod magit-section-ident-value (arg) arg)

(defun magit-get-section (ident &optional root)
  "Return the section identified by IDENT.
IDENT has to be a list as returned by `magit-section-ident'.
If optional ROOT is non-nil, then search in that section tree
instead of in the one whose root `magit-root-section' is."
  (setq ident (reverse ident))
  (let ((section (or root magit-root-section)))
    (when (eq (car (pop ident))
              (oref section type))
      (while (and ident
                  (pcase-let* ((`(,type . ,value) (car ident))
                               (value (magit-section-ident-value value)))
                    (setq section
                          (cl-find-if (lambda (section)
                                        (and (eq (oref section type) type)
                                             (equal (magit-section-ident-value
                                                     (oref section value))
                                                    value)))
                                      (oref section children)))))
        (pop ident))
      section)))

(defun magit-section-lineage (section)
  "Return the lineage of SECTION.
The return value has the form (TYPE...)."
  (cons (oref section type)
        (when-let ((parent (oref section parent)))
          (magit-section-lineage parent))))

(defvar magit-insert-section--current nil "For internal use only.")
(defvar magit-insert-section--parent  nil "For internal use only.")
(defvar magit-insert-section--oldroot nil "For internal use only.")

;;; Commands
;;;; Movement

(defun magit-section-forward ()
  "Move to the beginning of the next visible section."
  (interactive)
  (if (eobp)
      (user-error "No next section")
    (let ((section (magit-current-section)))
      (if (oref section parent)
          (let ((next (and (not (oref section hidden))
                           (not (= (oref section end)
                                   (1+ (point))))
                           (car (oref section children)))))
            (while (and section (not next))
              (unless (setq next (car (magit-section-siblings section 'next)))
                (setq section (oref section parent))))
            (if next
                (magit-section-goto next)
              (user-error "No next section")))
        (magit-section-goto 1)))))

(defun magit-section-backward ()
  "Move to the beginning of the current or the previous visible section.
When point is at the beginning of a section then move to the
beginning of the previous visible section.  Otherwise move to
the beginning of the current section."
  (interactive)
  (if (bobp)
      (user-error "No previous section")
    (let ((section (magit-current-section)) children)
      (cond
       ((and (= (point)
                (1- (oref section end)))
             (setq children (oref section children)))
        (magit-section-goto (car (last children))))
       ((and (oref section parent)
             (not (= (point)
                     (oref section start))))
        (magit-section-goto section))
       (t
        (let ((prev (car (magit-section-siblings section 'prev))))
          (if prev
              (while (and (not (oref prev hidden))
                          (setq children (oref prev children)))
                (setq prev (car (last children))))
            (setq prev (oref section parent)))
          (cond (prev
                 (magit-section-goto prev))
                ((oref section parent)
                 (user-error "No previous section"))
                ;; Eob special cases.
                ((not (get-text-property (1- (point)) 'invisible))
                 (magit-section-goto -1))
                (t
                 (goto-char (previous-single-property-change
                             (1- (point)) 'invisible))
                 (forward-line -1)
                 (magit-section-goto (magit-current-section))))))))))

(defun magit-section-up ()
  "Move to the beginning of the parent section."
  (interactive)
  (--if-let (oref (magit-current-section) parent)
      (magit-section-goto it)
    (user-error "No parent section")))

(defun magit-section-forward-sibling ()
  "Move to the beginning of the next sibling section.
If there is no next sibling section, then move to the parent."
  (interactive)
  (let ((current (magit-current-section)))
    (if (oref current parent)
        (--if-let (car (magit-section-siblings current 'next))
            (magit-section-goto it)
          (magit-section-forward))
      (magit-section-goto 1))))

(defun magit-section-backward-sibling ()
  "Move to the beginning of the previous sibling section.
If there is no previous sibling section, then move to the parent."
  (interactive)
  (let ((current (magit-current-section)))
    (if (oref current parent)
        (--if-let (car (magit-section-siblings current 'prev))
            (magit-section-goto it)
          (magit-section-backward))
      (magit-section-goto -1))))

(defun magit-section-goto (arg)
  (if (integerp arg)
      (progn (forward-line arg)
             (setq arg (magit-current-section)))
    (goto-char (oref arg start)))
  (run-hook-with-args 'magit-section-movement-hook arg))

(defun magit-section-set-window-start (section)
  "Ensure the beginning of SECTION is visible."
  (unless (pos-visible-in-window-p (oref section end))
    (set-window-start (selected-window) (oref section start))))

(defmacro magit-define-section-jumper (name heading type &optional value)
  "Define an interactive function to go some section.
Together TYPE and VALUE identify the section.
HEADING is the displayed heading of the section."
  (declare (indent defun))
  `(defun ,name (&optional expand) ,(format "\
Jump to the section \"%s\".
With a prefix argument also expand it." heading)
     (interactive "P")
     (--if-let (magit-get-section
                (cons (cons ',type ,value)
                      (magit-section-ident magit-root-section)))
         (progn (goto-char (oref it start))
                (when expand
                  (with-local-quit (magit-section-show it))
                  (recenter 0)))
       (message ,(format "Section \"%s\" wasn't found" heading)))))

;;;; Visibility

(defun magit-section-show (section)
  "Show the body of the current section."
  (interactive (list (magit-current-section)))
  (oset section hidden nil)
  (magit-section--maybe-wash section)
  (when-let ((beg (oref section content)))
    (remove-overlays beg (oref section end) 'invisible t))
  (magit-section-maybe-update-visibility-indicator section)
  (magit-section-maybe-cache-visibility section)
  (dolist (child (oref section children))
    (if (oref child hidden)
        (magit-section-hide child)
      (magit-section-show child))))

(defun magit-section--maybe-wash (section)
  (when-let ((washer (oref section washer)))
    (oset section washer nil)
    (let ((inhibit-read-only t)
          (magit-insert-section--parent section)
          (content (oref section content)))
      (save-excursion
        (if (and content (< content (oref section end)))
            (funcall washer section) ; already partially washed (hunk)
          (goto-char (oref section end))
          (oset section content (point-marker))
          (funcall washer)
          (oset section end (point-marker)))))
    (magit-section-update-highlight)))

(defun magit-section-hide (section)
  "Hide the body of the current section."
  (interactive (list (magit-current-section)))
  (if (eq section magit-root-section)
      (user-error "Cannot hide root section")
    (oset section hidden t)
    (when-let ((beg (oref section content)))
      (let ((end (oref section end)))
        (remove-overlays beg end 'invisible t)
        (let ((o (make-overlay beg end)))
          (overlay-put o 'evaporate t)
          (overlay-put o 'invisible t))))
    (magit-section-maybe-update-visibility-indicator section)
    (magit-section-maybe-cache-visibility section)))

(defun magit-section-toggle (section)
  "Toggle visibility of the body of the current section."
  (interactive (list (magit-current-section)))
  (if (eq section magit-root-section)
      (user-error "Cannot hide root section")
    (goto-char (oref section start))
    (if (oref section hidden)
        (magit-section-show section)
      (magit-section-hide section))))

(defun magit-section-toggle-children (section)
  "Toggle visibility of bodies of children of the current section."
  (interactive (list (magit-current-section)))
  (goto-char (oref section start))
  (let* ((children (oref section children))
         (show (--any-p (oref it hidden) children)))
    (dolist (c children)
      (oset c hidden show)))
  (magit-section-show section))

(defun magit-section-show-children (section &optional depth)
  "Recursively show the bodies of children of the current section.
With a prefix argument show children that deep and hide deeper
children."
  (interactive (list (magit-current-section)))
  (magit-section-show-children-1 section depth)
  (magit-section-show section))

(defun magit-section-show-children-1 (section &optional depth)
  (dolist (child (oref section children))
    (oset child hidden nil)
    (if depth
        (if (> depth 0)
            (magit-section-show-children-1 child (1- depth))
          (magit-section-hide child))
      (magit-section-show-children-1 child))))

(defun magit-section-hide-children (section)
  "Recursively hide the bodies of children of the current section."
  (interactive (list (magit-current-section)))
  (mapc 'magit-section-hide (oref section children)))

(defun magit-section-show-headings (section)
  "Recursively show headings of children of the current section.
Only show the headings, previously shown text-only bodies are
hidden."
  (interactive (list (magit-current-section)))
  (magit-section-show-headings-1 section)
  (magit-section-show section))

(defun magit-section-show-headings-1 (section)
  (dolist (child (oref section children))
    (oset child hidden nil)
    (when (or (oref child children)
              (not (oref child content)))
      (magit-section-show-headings-1 child))))

(defun magit-section-cycle (section)
  "Cycle visibility of current section and its children."
  (interactive (list (magit-current-section)))
  (goto-char (oref section start))
  (if (oref section hidden)
      (progn (magit-section-show section)
             (magit-section-hide-children section))
    (let ((children (oref section children)))
      (cond ((and (--any-p (oref it hidden)   children)
                  (--any-p (oref it children) children))
             (magit-section-show-headings section))
            ((-any-p 'magit-section-hidden-body children)
             (magit-section-show-children section))
            (t
             (magit-section-hide section))))))

(defun magit-section-cycle-global ()
  "Cycle visibility of all sections in the current buffer."
  (interactive)
  (let ((children (oref magit-root-section children)))
    (cond ((and (--any-p (oref it hidden)   children)
                (--any-p (oref it children) children))
           (magit-section-show-headings magit-root-section))
          ((-any-p 'magit-section-hidden-body children)
           (magit-section-show-children magit-root-section))
          (t
           (mapc 'magit-section-hide children)))))

(defun magit-section-hidden-body (section &optional pred)
  (--if-let (oref section children)
      (funcall (or pred '-any-p) 'magit-section-hidden-body it)
    (and (oref section content)
         (oref section hidden))))

(defun magit-section-invisible-p (section)
  "Return t if the SECTION's body is invisible.
When the body of an ancestor of SECTION is collapsed then
SECTION's body (and heading) obviously cannot be visible."
  (or (oref section hidden)
      (--when-let (oref section parent)
        (magit-section-invisible-p it))))

(defun magit-section-show-level (level)
  "Show surrounding sections up to LEVEL.
If LEVEL is negative, show up to the absolute value.
Sections at higher levels are hidden."
  (if (< level 0)
      (let ((s (magit-current-section)))
        (setq level (- level))
        (while (> (1- (length (magit-section-ident s))) level)
          (setq s (oref s parent))
          (goto-char (oref s start)))
        (magit-section-show-children magit-root-section (1- level)))
    (cl-do* ((s (magit-current-section)
                (oref s parent))
             (i (1- (length (magit-section-ident s)))
                (cl-decf i)))
        ((cond ((< i level) (magit-section-show-children s (- level i 1)) t)
               ((= i level) (magit-section-hide s) t))
         (magit-section-goto s)))))

(defun magit-section-show-level-1 ()
  "Show surrounding sections on first level."
  (interactive)
  (magit-section-show-level 1))

(defun magit-section-show-level-1-all ()
  "Show all sections on first level."
  (interactive)
  (magit-section-show-level -1))

(defun magit-section-show-level-2 ()
  "Show surrounding sections up to second level."
  (interactive)
  (magit-section-show-level 2))

(defun magit-section-show-level-2-all ()
  "Show all sections up to second level."
  (interactive)
  (magit-section-show-level -2))

(defun magit-section-show-level-3 ()
  "Show surrounding sections up to third level."
  (interactive)
  (magit-section-show-level 3))

(defun magit-section-show-level-3-all ()
  "Show all sections up to third level."
  (interactive)
  (magit-section-show-level -3))

(defun magit-section-show-level-4 ()
  "Show surrounding sections up to fourth level."
  (interactive)
  (magit-section-show-level 4))

(defun magit-section-show-level-4-all ()
  "Show all sections up to fourth level."
  (interactive)
  (magit-section-show-level -4))

;;;; Auxiliary

(defun magit-describe-section-briefly (section &optional ident)
  "Show information about the section at point.
With a prefix argument show the section identity instead of the
section lineage.  This command is intended for debugging purposes."
  (interactive (list (magit-current-section) current-prefix-arg))
  (let ((str (format "#<%s %S %S %s-%s>"
                     (eieio-object-class section)
                     (let ((val (oref section value)))
                       (cond ((stringp val)
                              (substring-no-properties val))
                             ((and (eieio-object-p val)
                                   (fboundp 'cl-prin1-to-string))
                              (cl-prin1-to-string val))
                             (t
                              val)))
                     (if ident
                         (magit-section-ident section)
                       (apply #'vector (magit-section-lineage section)))
                     (when-let ((m (oref section start)))
                       (marker-position m))
                     (when-let ((m (oref section end)))
                       (marker-position m)))))
    (if (called-interactively-p 'any)
        (message "%s" str)
      str)))

(cl-defmethod cl-print-object ((section magit-section) stream)
  "Print `magit-describe-section' result of SECTION."
  ;; Used by debug and edebug as of Emacs 26.
  (princ (magit-describe-section-briefly section) stream))

(defun magit-describe-section (section &optional interactive-p)
  "Show information about the section at point."
  (interactive (list (magit-current-section) t))
  (let ((inserter-section section))
    (while (and inserter-section (not (oref inserter-section inserter)))
      (setq inserter-section (oref inserter-section parent)))
    (when (and inserter-section (oref inserter-section inserter))
      (setq section inserter-section)))
  (pcase (oref section inserter)
    (`((,hook ,fun) . ,src-src)
     (help-setup-xref `(magit-describe-section ,section) interactive-p)
     (with-help-window (help-buffer)
       (with-current-buffer standard-output
         (insert (format-message
                  "%s\n  is inserted by `%s'\n  from `%s'"
                  (magit-describe-section-briefly section)
                  (make-text-button (symbol-name fun) nil
                                    :type 'help-function
                                    'help-args (list fun))
                  (make-text-button (symbol-name hook) nil
                                    :type 'help-variable
                                    'help-args (list hook))))
         (pcase-dolist (`(,hook ,fun) src-src)
           (insert (format-message
                    ",\n  called by `%s'\n  from `%s'"
                    (make-text-button (symbol-name fun) nil
                                      :type 'help-function
                                      'help-args (list fun))
                    (make-text-button (symbol-name hook) nil
                                      :type 'help-variable
                                      'help-args (list hook)))))
         (insert ".\n\n")
         (insert
          (format-message
           "`%s' is "
           (make-text-button (symbol-name fun) nil
                             :type 'help-function 'help-args (list fun))))
         (describe-function-1 fun))))
    (_ (message "%s, inserter unknown"
                (magit-describe-section-briefly section)))))

;;; Match

(cl-defun magit-section-match
    (condition &optional (section (magit-current-section)))
  "Return t if SECTION matches CONDITION.

SECTION defaults to the section at point.  If SECTION is not
specified and there also is no section at point, then return
nil.

CONDITION can take the following forms:
  (CONDITION...)  matches if any of the CONDITIONs matches.
  [CLASS...]      matches if the section's class is the same
                  as the first CLASS or a subclass of that;
                  the section's parent class matches the
                  second CLASS; and so on.
  [* CLASS...]    matches sections that match [CLASS...] and
                  also recursively all their child sections.
  CLASS           matches if the section's class is the same
                  as CLASS or a subclass of that; regardless
                  of the classes of the parent sections.

Each CLASS should be a class symbol, identifying a class that
derives from `magit-section'.  For backward compatibility CLASS
can also be a \"type symbol\".  A section matches such a symbol
if the value of its `type' slot is `eq'.  If a type symbol has
an entry in `magit--section-type-alist', then a section also
matches that type if its class is a subclass of the class that
corresponds to the type as per that alist.

Note that it is not necessary to specify the complete section
lineage as printed by `magit-describe-section-briefly', unless
of course you want to be that precise."
  (and section (magit-section-match-1 condition section)))

(defun magit-section-match-1 (condition section)
  (cl-assert condition)
  (and section
       (if (listp condition)
           (--first (magit-section-match-1 it section) condition)
         (magit-section-match-2 (if (symbolp condition)
                                    (list condition)
                                  (cl-coerce condition 'list))
                                section))))

(defun magit-section-match-2 (condition section)
  (if (eq (car condition) '*)
      (or (magit-section-match-2 (cdr condition) section)
          (when-let ((parent (oref section parent)))
            (magit-section-match-2 condition parent)))
    (and (let ((c (car condition)))
           (if (class-p c)
               (cl-typep section c)
             (if-let ((class (cdr (assq c magit--section-type-alist))))
                 (cl-typep section class)
               (eq (oref section type) c))))
         (or (not (setq condition (cdr condition)))
             (when-let ((parent (oref section parent)))
               (magit-section-match-2 condition parent))))))

(defun magit-section-value-if (condition &optional section)
  "If the section at point matches CONDITION, then return its value.

If optional SECTION is non-nil then test whether that matches
instead.  If there is no section at point and SECTION is nil,
then return nil.  If the section does not match, then return
nil.

See `magit-section-match' for the forms CONDITION can take."
  (when-let ((section (or section (magit-current-section))))
    (and (magit-section-match condition section)
         (oref section value))))

(defmacro magit-section-when (condition &rest body)
  "If the section at point matches CONDITION, evaluate BODY.

If the section matches, then evaluate BODY forms sequentially
with `it' bound to the section and return the value of the last
form.  If there are no BODY forms, then return the value of the
section.  If the section does not match or if there is no section
at point, then return nil.

See `magit-section-match' for the forms CONDITION can take."
  (declare (obsolete
            "instead use `magit-section-match' or `magit-section-value-if'."
            "Magit 2.90.0")
           (indent 1)
           (debug (sexp body)))
  `(--when-let (magit-current-section)
     ;; Quoting CONDITION here often leads to double-quotes, which
     ;; isn't an issue because `magit-section-match-1' implicitly
     ;; deals with that.  We shouldn't force users of this function
     ;; to not quote CONDITION because that would needlessly break
     ;; backward compatibility.
     (when (magit-section-match ',condition it)
       ,@(or body '((oref it value))))))

(defmacro magit-section-case (&rest clauses)
  "Choose among clauses on the type of the section at point.

Each clause looks like (CONDITION BODY...).  The type of the
section is compared against each CONDITION; the BODY forms of the
first match are evaluated sequentially and the value of the last
form is returned.  Inside BODY the symbol `it' is bound to the
section at point.  If no clause succeeds or if there is no
section at point, return nil.

See `magit-section-match' for the forms CONDITION can take.
Additionally a CONDITION of t is allowed in the final clause, and
matches if no other CONDITION match, even if there is no section
at point."
  (declare (indent 0)
           (debug (&rest (sexp body))))
  `(let* ((it (magit-current-section)))
     (cond ,@(mapcar (lambda (clause)
                       `(,(or (eq (car clause) t)
                              `(and it
                                    (magit-section-match-1 ',(car clause) it)))
                         ,@(cdr clause)))
                     clauses))))

(defun magit-section-match-assoc (section alist)
  "Return the value associated with SECTION's type or lineage in ALIST."
  (-some (pcase-lambda (`(,key . ,val))
           (and (magit-section-match-1 key section) val))
         alist))

;;; Create

(defvar magit-insert-section-hook nil
  "Hook run after `magit-insert-section's BODY.
Avoid using this hook and only ever do so if you know
what you are doing and are sure there is no other way.")

(defmacro magit-insert-section (&rest args)
  "Insert a section at point.

TYPE is the section type, a symbol which is prefixed with the
name of the package.  (For historic reasons the types used by
Magit and Forge do not use a package prefix.)  Many commands
that act on the current section behave differently depending
on its type.

Optional VALUE is the value of the section, usually a string
that is required when acting on the section.

When optional HIDE is non-nil collapse the section body by
default, i.e. when first creating the section, but not when
refreshing the buffer.  Else expand it by default.  This can be
overwritten using `magit-section-set-visibility-hook'.  When a
section is recreated during a refresh, then the visibility of
predecessor is inherited and HIDE is ignored (but the hook is
still honored).

BODY is any number of forms that actually insert the section's
heading and body.  Optional NAME, if specified, has to be a
symbol, which is then bound to the object of the section being
inserted.

Before BODY is evaluated the `start' of the section object is set
to the value of `point' and after BODY was evaluated its `end' is
set to the new value of `point'; BODY is responsible for moving
`point' forward.

If it turns out inside BODY that the section is empty, then
`magit-cancel-section' can be used to abort and remove all traces
of the partially inserted section.  This can happen when creating
a section by washing Git's output and Git didn't actually output
anything this time around.

For historic reasons, if a variable `magit-TYPE-section-map'
or `forge-TYPE-section-map' exists, then use that as the
text-property `keymap' of all text belonging to the section (but
this may be overwritten in subsections).  TYPE can also have the
form `(eval FORM)' in which case FORM is evaluated at runtime.

\(fn [NAME] (TYPE &optional VALUE HIDE) &rest BODY)"
  (declare (indent defun)
           (debug ([&optional symbolp]
                   (&or [("eval" symbolp) &optional form form]
                        [symbolp &optional form form])
                   body)))
  (let ((tp (cl-gensym "type"))
        (s* (and (symbolp (car args))
                 (pop args)))
        (s  (cl-gensym "section")))
    `(let* ((,tp ,(let ((type (nth 0 (car args))))
                    (if (eq (car-safe type) 'eval)
                        (cadr type)
                      `',type)))
            (,s (funcall (if (class-p ,tp)
                             ,tp
                           (or (cdr (assq ,tp magit--section-type-alist))
                               'magit-section))
                         :type
                         (if (class-p ,tp)
                             (or (car (rassq ,tp magit--section-type-alist))
                                 (error "BUG: No entry for %s in %s" ,tp
                                        'magit--section-type-alist))
                           ,tp)
                         :value ,(nth 1 (car args))
                         :start (point-marker)
                         :parent magit-insert-section--parent)))
       (oset ,s hidden
             (if-let ((value (run-hook-with-args-until-success
                              'magit-section-set-visibility-hook ,s)))
                 (eq value 'hide)
               (if-let ((incarnation (and magit-insert-section--oldroot
                                          (magit-get-section
                                           (magit-section-ident ,s)
                                           magit-insert-section--oldroot))))
                   (oref incarnation hidden)
                 (if-let ((value (magit-section-match-assoc
                                  ,s magit-section-initial-visibility-alist)))
                     (progn
                       (when (functionp value)
                         (setq value (funcall value ,s)))
                       (eq value 'hide))
                   ,(nth 2 (car args))))))
       (let ((magit-insert-section--current ,s)
             (magit-insert-section--parent  ,s)
             (magit-insert-section--oldroot
              (or magit-insert-section--oldroot
                  (unless magit-insert-section--parent
                    (prog1 magit-root-section
                      (setq magit-root-section ,s))))))
         (catch 'cancel-section
           ,@(if s*
                 `((let ((,s* ,s))
                     ,@(cdr args)))
               (cdr args))
           ;; `magit-insert-section-hook' should *not* be run with
           ;; `magit-run-section-hook' because it's a hook that runs
           ;; on section insertion, not a section inserting hook.
           (run-hooks 'magit-insert-section-hook)
           (magit-insert-child-count ,s)
           (set-marker-insertion-type (oref ,s start) t)
           (let* ((end (oset ,s end (point-marker)))
                  (class-map (oref-default ,s keymap))
                  (magit-map (intern (format "magit-%s-section-map"
                                             (oref ,s type))))
                  (forge-map (intern (format "forge-%s-section-map"
                                             (oref ,s type))))
                  (map (or (and         class-map  (symbol-value class-map))
                           (and (boundp magit-map) (symbol-value magit-map))
                           (and (boundp forge-map) (symbol-value forge-map)))))
             (save-excursion
               (goto-char (oref ,s start))
               (while (< (point) end)
                 (let ((next (or (next-single-property-change
                                  (point) 'magit-section)
                                 end)))
                   (unless (get-text-property (point) 'magit-section)
                     (put-text-property (point) next 'magit-section ,s)
                     (when map
                       (put-text-property (point) next 'keymap map)))
                   (goto-char next)))))
           (if (eq ,s magit-root-section)
               (let ((magit-section-cache-visibility nil))
                 (magit-section-show ,s))
             (oset (oref ,s parent) children
                   (nconc (oref (oref ,s parent) children)
                          (list ,s)))))
         ,s))))

(defun magit-cancel-section ()
  "Cancel inserting the section that is currently being inserted.
Remove all traces of that section."
  (when magit-insert-section--current
    (if (not (oref magit-insert-section--current parent))
        (insert "(empty)\n")
      (delete-region (oref magit-insert-section--current start)
                     (point))
      (setq magit-insert-section--current nil)
      (throw 'cancel-section nil))))

(defun magit-insert-heading (&rest args)
  "Insert the heading for the section currently being inserted.

This function should only be used inside `magit-insert-section'.

When called without any arguments, then just set the `content'
slot of the object representing the section being inserted to
a marker at `point'.  The section should only contain a single
line when this function is used like this.

When called with arguments ARGS, which have to be strings, or
nil, then insert those strings at point.  The section should not
contain any text before this happens and afterwards it should
again only contain a single line.  If the `face' property is set
anywhere inside any of these strings, then insert all of them
unchanged.  Otherwise use the `magit-section-heading' face for
all inserted text.

The `content' property of the section object is the end of the
heading (which lasts from `start' to `content') and the beginning
of the the body (which lasts from `content' to `end').  If the
value of `content' is nil, then the section has no heading and
its body cannot be collapsed.  If a section does have a heading,
then its height must be exactly one line, including a trailing
newline character.  This isn't enforced, you are responsible for
getting it right.  The only exception is that this function does
insert a newline character if necessary."
  (declare (indent defun))
  (when args
    (let ((heading (apply #'concat args)))
      (insert (if (or (text-property-not-all 0 (length heading)
                                             'font-lock-face nil heading)
                      (text-property-not-all 0 (length heading)
                                             'face nil heading))
                  heading
                (propertize heading 'font-lock-face 'magit-section-heading)))))
  (unless (bolp)
    (insert ?\n))
  (when (fboundp 'magit-maybe-make-margin-overlay)
    (magit-maybe-make-margin-overlay))
  (oset magit-insert-section--current content (point-marker)))

(defmacro magit-insert-section-body (&rest body)
  "Use BODY to insert the section body, once the section is expanded.
If the section is expanded when it is created, then this is
like `progn'.  Otherwise BODY isn't evaluated until the section
is explicitly expanded."
  (declare (indent 0))
  (let ((f (cl-gensym))
        (s (cl-gensym)))
    `(let ((,f (lambda () ,@body))
           (,s magit-insert-section--current))
       (if (oref ,s hidden)
           (oset ,s washer
                 (lambda ()
                   (funcall ,f)
                   (magit-section-maybe-remove-visibility-indicator ,s)))
         (funcall ,f)))))

(defun magit-insert-headers (hook)
  (let* ((header-sections nil)
         (magit-insert-section-hook
          (cons (lambda ()
                  (push magit-insert-section--current
                        header-sections))
                (if (listp magit-insert-section-hook)
                    magit-insert-section-hook
                  (list magit-insert-section-hook)))))
    (magit-run-section-hook hook)
    (when header-sections
      (insert "\n")
      ;; Make the first header into the parent of the rest.
      (when (cdr header-sections)
        (cl-callf nreverse header-sections)
        (let* ((1st-header (pop header-sections))
               (header-parent (oref 1st-header parent)))
          (oset header-parent children (list 1st-header))
          (oset 1st-header children header-sections)
          (oset 1st-header content (oref (car header-sections) start))
          (oset 1st-header end (oref (car (last header-sections)) end))
          (dolist (sub-header header-sections)
            (oset sub-header parent 1st-header)))))))

(defun magit-insert-child-count (section)
  "Modify SECTION's heading to contain number of child sections.

If `magit-section-show-child-count' is non-nil and the SECTION
has children and its heading ends with \":\", then replace that
with \" (N)\", where N is the number of child sections.

This function is called by `magit-insert-section' after that has
evaluated its BODY.  Admittedly that's a bit of a hack."
  ;; This has to be fast, not pretty!
  (let (content count)
    (when (and magit-section-show-child-count
               (setq count (length (oref section children)))
               (> count 0)
               (setq content (oref section content))
               (eq (char-before (1- content)) ?:))
      (save-excursion
        (goto-char (- content 2))
        (insert (format " (%s)" count))
        (delete-char 1)))))

;;; Highlight

(defvar-local magit-section-highlight-overlays nil)
(defvar-local magit-section-highlighted-section nil)
(defvar-local magit-section-highlighted-sections nil)
(defvar-local magit-section-unhighlight-sections nil)
(defun magit-section-update-highlight ()
  (let ((section (magit-current-section)))
    (unless (eq section magit-section-highlighted-section)
      (let ((inhibit-read-only t)
            (deactivate-mark nil)
            (selection (magit-region-sections)))
        (mapc #'delete-overlay magit-section-highlight-overlays)
        (setq magit-section-highlight-overlays nil)
        (setq magit-section-unhighlight-sections
              magit-section-highlighted-sections)
        (setq magit-section-highlighted-sections nil)
        (unless (eq section magit-root-section)
          (run-hook-with-args-until-success
           'magit-section-highlight-hook section selection))
        (dolist (s magit-section-unhighlight-sections)
          (run-hook-with-args-until-success
           'magit-section-unhighlight-hook s selection))
        (restore-buffer-modified-p nil)
        (unless (eq magit-section-highlighted-section section)
          (setq magit-section-highlighted-section
                (and (not (oref section hidden))
                     section)))))
    (magit-section-maybe-paint-visibility-ellipses)))

(defun magit-section-highlight (section selection)
  "Highlight SECTION and if non-nil all sections in SELECTION.
This function works for any section but produces undesirable
effects for diff related sections, which by default are
highlighted using `magit-diff-highlight'.  Return t."
  (when-let ((face (oref section heading-highlight-face)))
    (dolist (section (or selection (list section)))
      (magit-section-make-overlay
       (oref section start)
       (or (oref section content)
           (oref section end))
       face)))
  (cond (selection
         (magit-section-make-overlay (oref (car selection) start)
                                     (oref (car (last selection)) end)
                                     'magit-section-highlight)
         (magit-section-highlight-selection nil selection))
        (t
         (magit-section-make-overlay (oref section start)
                                     (oref section end)
                                     'magit-section-highlight)))
  t)

(defun magit-section-highlight-selection (_ selection)
  "Highlight the section-selection region.
If SELECTION is non-nil, then it is a list of sections selected by
the region.  The headings of these sections are then highlighted.

This is a fallback for people who don't want to highlight the
current section and therefore removed `magit-section-highlight'
from `magit-section-highlight-hook'.

This function is necessary to ensure that a representation of
such a region is visible.  If neither of these functions were
part of the hook variable, then such a region would be
invisible."
  (when (and selection
             (not (and (eq this-command 'mouse-drag-region))))
    (dolist (section selection)
      (magit-section-make-overlay (oref section start)
                                  (or (oref section content)
                                      (oref section end))
                                  'magit-section-heading-selection))
    t))

(defun magit-section-make-overlay (start end face)
  ;; Yes, this doesn't belong here.  But the alternative of
  ;; spreading this hack across the code base is even worse.
  (when (and magit-keep-region-overlay
             (memq face '(magit-section-heading-selection
                          magit-diff-file-heading-selection
                          magit-diff-hunk-heading-selection)))
    (setq face (list :foreground (face-foreground face))))
  (let ((ov (make-overlay start end nil t)))
    (overlay-put ov 'font-lock-face face)
    (overlay-put ov 'evaporate t)
    (push ov magit-section-highlight-overlays)
    ov))

(defun magit-section-goto-successor (section line char arg)
  (let ((ident (magit-section-ident section)))
    (--if-let (magit-get-section ident)
        (let ((start (oref it start)))
          (goto-char start)
          (unless (eq it magit-root-section)
            (ignore-errors
              (forward-line line)
              (forward-char char))
            (unless (eq (magit-current-section) it)
              (goto-char start))))
      (or (run-hook-with-args-until-success
           'magit-section-goto-successor-hook section arg)
          (goto-char (--if-let (magit-section-goto-successor-1 section)
                         (if (eq (oref it type) 'button)
                             (point-min)
                           (oref it start))
                       (point-min)))))))

(defun magit-section-goto-successor-1 (section)
  (or (--when-let (pcase (oref section type)
                    (`staged 'unstaged)
                    (`unstaged 'staged)
                    (`unpushed 'unpulled)
                    (`unpulled 'unpushed))
        (magit-get-section `((,it) (status))))
      (--when-let (car (magit-section-siblings section 'next))
        (magit-get-section (magit-section-ident it)))
      (--when-let (car (magit-section-siblings section 'prev))
        (magit-get-section (magit-section-ident it)))
      (--when-let (oref section parent)
        (or (magit-get-section (magit-section-ident it))
            (magit-section-goto-successor-1 it)))))

;;; Region

(defvar-local magit-section--region-overlays nil)

(defun magit-section--delete-region-overlays ()
  (mapc #'delete-overlay magit-section--region-overlays)
  (setq magit-section--region-overlays nil))

(defun magit-section--highlight-region (start end window rol)
  (magit-section--delete-region-overlays)
  (if (and (not magit-keep-region-overlay)
           (or (magit-region-sections)
               (run-hook-with-args-until-success 'magit-region-highlight-hook
                                                 (magit-current-section)))
           (not (= (line-number-at-pos start)
                   (line-number-at-pos end)))
           ;; (not (eq (car-safe last-command-event) 'mouse-movement))
           )
      (funcall (default-value 'redisplay-unhighlight-region-function) rol)
    (funcall (default-value 'redisplay-highlight-region-function)
             start end window rol)))

(defun magit-section--unhighlight-region (rol)
  (setq magit-section-highlighted-section nil)
  (magit-section--delete-region-overlays)
  (funcall (default-value 'redisplay-unhighlight-region-function) rol))

;;; Visibility

(defvar-local magit-section-visibility-cache nil)
(put 'magit-section-visibility-cache 'permanent-local t)

(defun magit-section-cached-visibility (section)
  "Set SECTION's visibility to the cached value."
  (cdr (assoc (magit-section-ident section)
              magit-section-visibility-cache)))

(cl-defun magit-section-cache-visibility
    (&optional (section magit-insert-section--current))
  ;; Emacs 25's `alist-get' lacks TESTFN.
  (let* ((id  (magit-section-ident section))
         (elt (assoc id magit-section-visibility-cache))
         (val (if (oref section hidden) 'hide 'show)))
    (if elt
        (setcdr elt val)
      (push (cons id val) magit-section-visibility-cache))))

(cl-defun magit-section-maybe-cache-visibility
    (&optional (section magit-insert-section--current))
  (when (or (eq magit-section-cache-visibility t)
            (memq (oref section type)
                  magit-section-cache-visibility))
    (magit-section-cache-visibility section)))

(defun magit-section-maybe-update-visibility-indicator (section)
  (when magit-section-visibility-indicator
    (let ((beg (oref section start))
          (cnt (oref section content))
          (end (oref section end)))
      (when (and cnt (or (not (= cnt end)) (oref section washer)))
        (let ((eoh (save-excursion
                     (goto-char beg)
                     (line-end-position))))
          (cond
           ((symbolp (car-safe magit-section-visibility-indicator))
            ;; It would make more sense to put the overlay only on the
            ;; location we actually don't put it on, but then inserting
            ;; before that location (while taking care not to mess with
            ;; the overlay) would cause the fringe bitmap to disappear
            ;; (but not other effects of the overlay).
            (let ((ov (magit--overlay-at (1+ beg) 'magit-vis-indicator 'fringe)))
              (unless ov
                (setq ov (make-overlay (1+ beg) eoh))
                (overlay-put ov 'evaporate t)
                (overlay-put ov 'magit-vis-indicator 'fringe))
              (overlay-put
               ov 'before-string
               (propertize "fringe" 'display
                           (list 'left-fringe
                                 (if (oref section hidden)
                                     (car magit-section-visibility-indicator)
                                   (cdr magit-section-visibility-indicator))
                                 (face-foreground 'fringe))))))
           ((stringp (car-safe magit-section-visibility-indicator))
            (let ((ov (magit--overlay-at (1- eoh) 'magit-vis-indicator 'eoh)))
              (cond ((oref section hidden)
                     (unless ov
                       (setq ov (make-overlay (1- eoh) eoh))
                       (overlay-put ov 'evaporate t)
                       (overlay-put ov 'magit-vis-indicator 'eoh))
                     (overlay-put ov 'after-string
                                  (car magit-section-visibility-indicator)))
                    (ov
                     (delete-overlay ov)))))))))))

(defvar-local magit--ellipses-sections nil)

(defun magit-section-maybe-paint-visibility-ellipses ()
  ;; This is needed because we hide the body instead of "the body
  ;; except the final newline and additionally the newline before
  ;; the body"; otherwise we could use `buffer-invisibility-spec'.
  (when (stringp (car-safe magit-section-visibility-indicator))
    (let* ((sections (append magit--ellipses-sections
                             (setq magit--ellipses-sections
                                   (or (magit-region-sections)
                                       (list (magit-current-section))))))
           (beg (--map (oref it start) sections))
           (end (--map (oref it end)   sections)))
      (when (region-active-p)
        ;; This ensures that the region face is removed from ellipses
        ;; when the region becomes inactive, but fails to ensure that
        ;; all ellipses within the active region use the region face,
        ;; because the respective overlay has not yet been updated at
        ;; this time.  The magit-selection face is always applied.
        (push (region-beginning) beg)
        (push (region-end)       end))
      (setq beg (apply #'min beg))
      (setq end (apply #'max end))
      (dolist (ov (overlays-in beg end))
        (when (eq (overlay-get ov 'magit-vis-indicator) 'eoh)
          (overlay-put
           ov 'after-string
           (propertize
            (car magit-section-visibility-indicator) 'font-lock-face
            (let ((pos (overlay-start ov)))
              (delq nil (nconc (--map (overlay-get it 'font-lock-face)
                                      (overlays-at pos))
                               (list (get-char-property
                                      pos 'font-lock-face))))))))))))

(defun magit-section-maybe-remove-visibility-indicator (section)
  (when (and magit-section-visibility-indicator
             (= (oref section content)
                (oref section end)))
    (dolist (o (overlays-in (oref section start)
                            (save-excursion
                              (goto-char (oref section start))
                              (1+ (line-end-position)))))
      (when (overlay-get o 'magit-vis-indicator)
        (delete-overlay o)))))

;;; Utilities

(cl-defun magit-section-selected-p (section &optional (selection nil sselection))
  (and (not (eq section magit-root-section))
       (or  (eq section (magit-current-section))
            (memq section (if sselection
                              selection
                            (setq selection (magit-region-sections))))
            (--when-let (oref section parent)
              (magit-section-selected-p it selection)))))

(defun magit-section-parent-value (section)
  (when-let ((parent (oref section parent)))
    (oref parent value)))

(defun magit-section-siblings (section &optional direction)
  "Return a list of the sibling sections of SECTION.

If optional DIRECTION is `prev', then return siblings that come
before SECTION.  If it is `next', then return siblings that come
after SECTION.  For all other values, return all siblings
excluding SECTION itself."
  (when-let ((parent (oref section parent)))
    (let ((siblings (oref parent children)))
      (pcase direction
        (`prev  (cdr (member section (reverse siblings))))
        (`next  (cdr (member section siblings)))
        (_      (remq section siblings))))))

(defun magit-region-values (&optional condition multiple)
  "Return a list of the values of the selected sections.

Return the values that themselves would be returned by
`magit-region-sections' (which see)."
  (--map (oref it value)
         (magit-region-sections condition multiple)))

(defun magit-region-sections (&optional condition multiple)
  "Return a list of the selected sections.

When the region is active and constitutes a valid section
selection, then return a list of all selected sections.  This is
the case when the region begins in the heading of a section and
ends in the heading of the same section or in that of a sibling
section.  If optional MULTIPLE is non-nil, then the region cannot
begin and end in the same section.

When the selection is not valid, then return nil.  In this case,
most commands that can act on the selected sections will instead
act on the section at point.

When the region looks like it would in any other buffer then
the selection is invalid.  When the selection is valid then the
region uses the `magit-section-highlight' face.  This does not
apply to diffs where things get a bit more complicated, but even
here if the region looks like it usually does, then that's not
a valid selection as far as this function is concerned.

If optional CONDITION is non-nil, then the selection not only
has to be valid; all selected sections additionally have to match
CONDITION, or nil is returned.  See `magit-section-match' for the
forms CONDITION can take."
  (when (region-active-p)
    (let* ((rbeg (region-beginning))
           (rend (region-end))
           (sbeg (get-text-property rbeg 'magit-section))
           (send (get-text-property rend 'magit-section)))
      (when (and send
                 (not (eq send magit-root-section))
                 (not (and multiple (eq send sbeg))))
        (let ((siblings (cons sbeg (magit-section-siblings sbeg 'next)))
              sections)
          (when (and (memq send siblings)
                     (magit-section-position-in-heading-p sbeg rbeg)
                     (magit-section-position-in-heading-p send rend))
            (while siblings
              (push (car siblings) sections)
              (when (eq (pop siblings) send)
                (setq siblings nil)))
            (setq sections (nreverse sections))
            (when (or (not condition)
                      (--all-p (magit-section-match condition it) sections))
              sections)))))))

(defun magit-section-position-in-heading-p (&optional section pos)
  "Return t if POSITION is inside the heading of SECTION.
POSITION defaults to point and SECTION defaults to the
current section."
  (unless section
    (setq section (magit-current-section)))
  (unless pos
    (setq pos (point)))
  (and section
       (>= pos (oref section start))
       (<  pos (or (oref section content)
                   (oref section end)))
       t))

(defun magit-section-internal-region-p (&optional section)
  "Return t if the region is active and inside SECTION's body.
If optional SECTION is nil, use the current section."
  (and (region-active-p)
       (or section (setq section (magit-current-section)))
       (let ((beg (get-text-property (region-beginning) 'magit-section)))
         (and (eq beg (get-text-property   (region-end) 'magit-section))
              (eq beg section)))
       (not (or (magit-section-position-in-heading-p section (region-beginning))
                (magit-section-position-in-heading-p section (region-end))))
       t))

(defun magit-section--backward-protected ()
  "Move to the beginning of the current or the previous visible section.
Same as `magit-section-backward' but for non-interactive use.
Suppress `magit-section-movement-hook', and return a boolean to
indicate whether a section was found, instead of raising an error
if not."
  (condition-case nil
      (let ((magit-section-movement-hook nil))
        (magit-section-backward)
        t)
    (user-error nil)))

(defun magit-section--backward-find (predicate)
  "Move to the first previous section satisfying PREDICATE.
PREDICATE does not take any parameter and should not move
point."
  (let (found)
    (while (and (setq found (magit-section--backward-protected))
                (not (funcall predicate))))
    found))

(defun magit-wash-sequence (function)
  "Repeatedly call FUNCTION until it returns nil or eob is reached.
FUNCTION has to move point forward or return nil."
  (while (and (not (eobp)) (funcall function))))

(defun magit-add-section-hook (hook function &optional at append local)
  "Add to the value of section hook HOOK the function FUNCTION.

Add FUNCTION at the beginning of the hook list unless optional
APPEND is non-nil, in which case FUNCTION is added at the end.
If FUNCTION already is a member, then move it to the new location.

If optional AT is non-nil and a member of the hook list, then
add FUNCTION next to that instead.  Add before or after AT, or
replace AT with FUNCTION depending on APPEND.  If APPEND is the
symbol `replace', then replace AT with FUNCTION.  For any other
non-nil value place FUNCTION right after AT.  If nil, then place
FUNCTION right before AT.  If FUNCTION already is a member of the
list but AT is not, then leave FUNCTION where ever it already is.

If optional LOCAL is non-nil, then modify the hook's buffer-local
value rather than its global value.  This makes the hook local by
copying the default value.  That copy is then modified.

HOOK should be a symbol.  If HOOK is void, it is first set to nil.
HOOK's value must not be a single hook function.  FUNCTION should
be a function that takes no arguments and inserts one or multiple
sections at point, moving point forward.  FUNCTION may choose not
to insert its section(s), when doing so would not make sense.  It
should not be abused for other side-effects.  To remove FUNCTION
again use `remove-hook'."
  (unless (boundp hook)
    (error "Cannot add function to undefined hook variable %s" hook))
  (unless (default-boundp hook)
    (set-default hook nil))
  (let ((value (if local
                   (if (local-variable-p hook)
                       (symbol-value hook)
                     (unless (local-variable-if-set-p hook)
                       (make-local-variable hook))
                     (copy-sequence (default-value hook)))
                 (default-value hook))))
    (if at
        (when (setq at (member at value))
          (setq value (delq function value))
          (cond ((eq append 'replace)
                 (setcar at function))
                (append
                 (push function (cdr at)))
                (t
                 (push (car at) (cdr at))
                 (setcar at function))))
      (setq value (delq function value)))
    (unless (member function value)
      (setq value (if append
                      (append value (list function))
                    (cons function value))))
    (when (eq append 'replace)
      (setq value (delq at value)))
    (if local
        (set hook value)
      (set-default hook value))))

(defvar-local magit-disabled-section-inserters nil)

(defun magit-disable-section-inserter (fn)
  "Disable the section inserter FN in the current repository.
It is only intended for use in \".dir-locals.el\" and
\".dir-locals-2.el\".  Also see info node `(magit)Per-Repository
Configuration'."
  (cl-pushnew fn magit-disabled-section-inserters))

(put 'magit-disable-section-inserter 'safe-local-eval-function t)

(defun magit-run-section-hook (hook &rest args)
  "Run HOOK with ARGS, warning about invalid entries."
  (let ((entries (symbol-value hook)))
    (unless (listp entries)
      (setq entries (list entries)))
    (--when-let (-remove #'functionp entries)
      (message "`%s' contains entries that are no longer valid.
%s\nUsing standard value instead.  Please re-configure hook variable."
               hook
               (mapconcat (lambda (sym) (format "  `%s'" sym)) it "\n"))
      (sit-for 5)
      (setq entries (eval (car (get hook 'standard-value)))))
    (dolist (entry entries)
      (let ((magit--current-section-hook (cons (list hook entry)
                                               magit--current-section-hook)))
        (unless (memq entry magit-disabled-section-inserters)
          (if (bound-and-true-p magit-refresh-verbose)
              (message "  %-50s %s" entry
                       (benchmark-elapse (apply entry args)))
            (apply entry args)))))))

(cl-defun magit--overlay-at (pos prop &optional (val nil sval) testfn)
  (cl-find-if (lambda (o)
                (let ((p (overlay-properties o)))
                  (and (plist-member p prop)
                       (or (not sval)
                           (funcall (or testfn #'eql)
                                    (plist-get p prop)
                                    val)))))
              (overlays-at pos t)))

;;; _
(provide 'magit-section)
;;; magit-section.el ends here
