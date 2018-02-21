;;; magit-section.el --- section functionality  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2018  The Magit Project Contributors
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

;; This library implements "sections" as used in all Magit buffers.
;; If you have used Magit before, then you probably know what that
;; means, otherwise think "read-only Org-Mode for Git", kinda.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'eieio)

(require 'magit-utils)

(declare-function magit-maybe-make-margin-overlay "magit-margin" ())
(declare-function magit-repository-local-get "magit-mode"
                  (key &optional default repository))
(declare-function magit-repository-local-set "magit-mode"
                  (key value &optional repository))
(defvar magit-keep-region-overlay)

;;; Options

(defgroup magit-section nil
  "Expandable sections."
  :link '(info-link "(magit)Sections")
  :group 'magit)

(defcustom magit-section-show-child-count t
  "Whether to append the number of children to section headings.
This only applies to sections for which doing so makes sense."
  :package-version '(magit . "2.1.0")
  :group 'magit-section
  :type 'boolean)

(defcustom magit-section-movement-hook
  '(magit-hunk-set-window-start
    magit-log-maybe-update-revision-buffer
    magit-log-maybe-show-more-commits)
  "Hook run by `magit-section-goto'.
That function in turn is used by all section movement commands."
  :package-version '(magit . "2.3.0")
  :group 'magit-section
  :type 'hook
  :options '(magit-hunk-set-window-start
             magit-status-maybe-update-revision-buffer
             magit-status-maybe-update-blob-buffer
             magit-log-maybe-update-revision-buffer
             magit-log-maybe-update-blob-buffer
             magit-log-maybe-show-more-commits))

(defcustom magit-section-highlight-hook
  '(magit-diff-highlight
    magit-section-highlight
    magit-section-highlight-selection)
  "Functions used to highlight the current section.
Each function is run with the current section as only argument
until one of them returns non-nil."
  :package-version '(magit . "2.1.0")
  :group 'magit-section
  :type 'hook
  :options '(magit-diff-highlight
             magit-section-highlight
             magit-section-highlight-selection))

(defcustom magit-section-unhighlight-hook
  '(magit-diff-unhighlight)
  "Functions used to unhighlight the previously current section.
Each function is run with the current section as only argument
until one of them returns non-nil.  Most sections are properly
unhighlighted without requiring a specialized unhighlighter,
diff-related sections being the only exception."
  :package-version '(magit . "2.1.0")
  :group 'magit-section
  :type 'hook
  :options '(magit-diff-unhighlight))

(defcustom magit-section-set-visibility-hook
  '(magit-diff-expansion-threshold
    magit-section-cached-visibility)
  "Hook used to set the initial visibility of a section.
Stop at the first function that returns non-nil.  The returned
value should be `show', `hide' or nil.  If no function returns
non-nil, determine the visibility as usual, i.e. use the
hardcoded section specific default (see `magit-insert-section')."
  :package-version '(magit . "2.4.0")
  :group 'magit-section
  :type 'hook
  :options '(magit-diff-expansion-threshold
             magit-section-cached-visibility))

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

(defcustom magit-section-initial-visibility-alist nil
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
but in the future it will also be used set the defaults."
  :package-version '(magit . "2.12.0")
  :group 'magit-section
  :type '(alist :key-type (sexp :tag "Section type/lineage")
                :value-type (choice (const hide)
                                    (const show)
                                    function)))

(defface magit-section-highlight
  '((((class color) (background light)) :background "grey95")
    (((class color) (background  dark)) :background "grey20"))
  "Face for highlighting the current section."
  :group 'magit-faces)

(defface magit-section-heading
  '((((class color) (background light)) :foreground "DarkGoldenrod4" :weight bold)
    (((class color) (background  dark)) :foreground "LightGoldenrod2" :weight bold))
  "Face for section headings."
  :group 'magit-faces)

(defface magit-section-secondary-heading '((t :weight bold))
  "Face for section headings of some secondary headings."
  :group 'magit-faces)

(defface magit-section-heading-selection
  '((((class color) (background light)) :foreground "salmon4")
    (((class color) (background  dark)) :foreground "LightSalmon3"))
  "Face for selected section headings."
  :group 'magit-faces)

;;; Core

(defclass magit-section ()
  ((type     :initform nil :accessor magit-section-type     :initarg :type)
   (value    :initform nil :accessor magit-section-value    :initarg :value)
   (start    :initform nil :accessor magit-section-start    :initarg :start)
   (content  :initform nil :accessor magit-section-content)
   (end      :initform nil :accessor magit-section-end)
   (hidden   :initform nil :accessor magit-section-hidden)
   (washer   :initform nil :accessor magit-section-washer)
   (process  :initform nil)
   (parent   :initform nil :accessor magit-section-parent   :initarg :parent)
   (children :initform nil :accessor magit-section-children)))

(defclass magit-file-section (magit-section)
  ((source   :initform nil)
   (header   :initform nil)))

(defclass magit-hunk-section (magit-section)
  ((refined  :initform nil)))

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
                (cond ((not (memq type '(unpulled unpushed))) value)
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

(defun magit-get-section (ident &optional root)
  "Return the section identified by IDENT.
IDENT has to be a list as returned by `magit-section-ident'."
  (setq ident (reverse ident))
  (let ((section (or root magit-root-section)))
    (when (eq (car (pop ident))
              (oref section type))
      (while (and ident
                  (setq section
                        (--first
                         (and (eq    (caar ident) (oref it type))
                              (equal (cdar ident) (oref it value)))
                         (oref section children))))
        (pop ident))
      section)))

(defun magit-section-lineage (section)
  "Return the lineage of SECTION.
The return value has the form [TYPE...]."
  (apply #'vector (mapcar #'car (magit-section-ident section))))

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

(defun magit-hunk-set-window-start (section)
  "When SECTION is a `hunk', ensure that its beginning is visible.
It the SECTION has a different type, then do nothing."
  (when (magit-hunk-section-p section)
    (magit-section-set-window-start section)))

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
  (-when-let (washer (oref section washer))
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
    (magit-section-update-highlight))
  (-when-let (beg (oref section content))
    (remove-overlays beg (oref section end) 'invisible t))
  (magit-section-maybe-cache-visibility section)
  (dolist (child (oref section children))
    (if (oref child hidden)
        (magit-section-hide child)
      (magit-section-show child))))

(defun magit-section-hide (section)
  "Hide the body of the current section."
  (interactive (list (magit-current-section)))
  (if (eq section magit-root-section)
      (user-error "Cannot hide root section")
    (oset section hidden t)
    (-when-let (beg (oref section content))
      (let ((end (oref section end)))
        (remove-overlays beg end 'invisible t)
        (let ((o (make-overlay beg end)))
          (overlay-put o 'evaporate t)
          (overlay-put o 'invisible t))))
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
  (--each (oref section children)
    (oset it hidden nil)
    (if depth
        (if (> depth 0)
            (magit-section-show-children-1 it (1- depth))
          (magit-section-hide it))
      (magit-section-show-children-1 it))))

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
  (--each (oref section children)
    (oset it hidden nil)
    (when (or (oref it children)
              (not (oref it content)))
      (magit-section-show-headings-1 it))))

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

(defun magit-section-cycle-diffs ()
  "Cycle visibility of diff-related sections in the current buffer."
  (interactive)
  (-when-let (sections
              (cond ((derived-mode-p 'magit-status-mode)
                     (--mapcat
                      (when it
                        (when (oref it hidden)
                          (magit-section-show it))
                        (oref it children))
                      (list (magit-get-section '((staged)   (status)))
                            (magit-get-section '((unstaged) (status))))))
                    ((derived-mode-p 'magit-diff-mode)
                     (-filter #'magit-file-section-p
                              (oref magit-root-section children)))))
    (if (--any-p (oref it hidden) sections)
        (dolist (s sections)
          (magit-section-show s)
          (magit-section-hide-children s))
      (let ((children (--mapcat (oref it children) sections)))
        (cond ((and (--any-p (oref it hidden)   children)
                    (--any-p (oref it children) children))
               (mapc 'magit-section-show-headings sections))
              ((-any-p 'magit-section-hidden-body children)
               (mapc 'magit-section-show-children sections))
              (t
               (mapc 'magit-section-hide sections)))))))

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

(defun magit-describe-section ()
  "Show information about the section at point.
This command is intended for debugging purposes."
  (interactive)
  (let ((section (magit-current-section)))
    (message "%S %S %s-%s"
             (oref section value)
             (magit-section-lineage section)
             (marker-position (oref section start))
             (marker-position (oref section end)))))

;;; Match

(cl-defun magit-section-match
    (condition &optional (section (magit-current-section)))
  "Return t if SECTION matches CONDITION.

SECTION defaults to the section at point.  If SECTION is not
specified and there also is no section at point, then return
nil.

CONDITION can take the following forms:
  (CONDITION...)  matches if any of the CONDITIONs matches.
  [TYPE...]       matches if the first TYPE matches the type
                  of the section, the second matches that of
                  its parent, and so on.
  [* TYPE...]     matches sections that match [TYPE...] and
                  also recursively all their child sections.
  TYPE            matches sections of TYPE regardless of the
                  types of the parent sections.

Each TYPE is a symbol.  Note that it is not necessary to specify
all TYPEs up to the root section as printed by
`magit-describe-type', unless of course you want to be that
precise."
  (and section
       (magit-section-match-1 condition
                              (mapcar #'car (magit-section-ident section)))))

(defun magit-section-match-1 (condition type-list)
  (if (listp condition)
      (--first (magit-section-match-1 it type-list) condition)
    (magit-section-match-2 (if (symbolp condition)
                               (list condition)
                             (append condition nil))
                           type-list)))

(defun magit-section-match-2 (l1 l2)
  (or (null l1)
      (if (eq (car l1) '*)
          (or (magit-section-match-2 (cdr l1) l2)
              (and l2
                   (magit-section-match-2 l1 (cdr l2))))
        (and l2
             (equal (car l1) (car l2))
             (magit-section-match-2 (cdr l1) (cdr l2))))))

(defmacro magit-section-when (condition &rest body)
  "If the section at point matches CONDITION, evaluate BODY.

If the section matches, then evaluate BODY forms sequentially
with `it' bound to the section and return the value of the last
form.  If there are no BODY forms, then return the value of the
section.  If the section does not match or if there is no section
at point, then return nil.

See `magit-section-match' for the forms CONDITION can take."
  (declare (indent 1)
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
  (let ((lineage (cl-gensym "lineage")))
    `(let* ((it (magit-current-section))
            (,lineage (and it (append (magit-section-lineage it) nil))))
       (cond ,@(mapcar (lambda (clause)
                         `(,(or (eq (car clause) t)
                                `(and it (magit-section-match-1
                                          ',(car clause) ,lineage)))
                           ,@(cdr clause)))
                       clauses)))))

(defun magit-section-match-assoc (section alist)
  "Return the value associated with SECTION's type or lineage in ALIST."
  (let ((ident (mapcar #'car (magit-section-ident section))))
    (--some (pcase-let ((`(,key . ,val) it))
              (and (magit-section-match-1 key ident) val))
            alist)))

;;; Create

(defvar magit-insert-section-hook nil
  "Hook run after `magit-insert-section's BODY.
Avoid using this hook and only ever do so if you know
what you are doing and are sure there is no other way.")

(defmacro magit-insert-section (&rest args)
  "Insert a section at point.

TYPE is the section type, a symbol.  Many commands that act on
the current section behave differently depending on that type.
Also if a variable `magit-TYPE-section-map' exists, then use
that as the text-property `keymap' of all text belonging to the
section (but this may be overwritten in subsections).  TYPE can
also have the form `(eval FORM)' in which case FORM is evaluated
at runtime.

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
symbol, which is then bound to the struct of the section being
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
            (,s (funcall (pcase ,tp
                           (`file 'magit-file-section)
                           (`hunk 'magit-hunk-section)
                           (_     'magit-section))
                         ""
                         :type ,tp
                         :value ,(nth 1 (car args))
                         :start (point-marker)
                         :parent magit-insert-section--parent)))
       (oset ,s hidden
             (-if-let (value (run-hook-with-args-until-success
                              'magit-section-set-visibility-hook ,s))
                 (eq value 'hide)
               (-if-let (incarnation (and magit-insert-section--oldroot
                                          (magit-get-section
                                           (magit-section-ident ,s)
                                           magit-insert-section--oldroot)))
                   (oref incarnation hidden)
                 (-if-let (value (magit-section-match-assoc
                                  ,s magit-section-initial-visibility-alist))
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
           (run-hooks 'magit-insert-section-hook)
           (magit-insert-child-count ,s)
           (set-marker-insertion-type (oref ,s start) t)
           (let* ((end (oset ,s end (point-marker)))
                  (map (intern (format "magit-%s-section-map" (oref ,s type))))
                  (map (and (boundp map) (symbol-value map))))
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

The `content' property of the section struct is the end of the
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
      (insert (if (text-property-not-all 0 (length heading) 'face nil heading)
                  heading
                (propertize heading 'face 'magit-section-heading)))))
  (unless (bolp)
    (insert ?\n))
  (magit-maybe-make-margin-overlay)
  (oset magit-insert-section--current content (point-marker)))

(defvar magit-insert-headers--hook nil "For internal use only.")
(defvar magit-insert-headers--beginning nil "For internal use only.")

(defun magit-insert-headers (hooks)
  (let ((magit-insert-section-hook
         (cons 'magit-insert-remaining-headers
               (if (listp magit-insert-section-hook)
                   magit-insert-section-hook
                 (list magit-insert-section-hook))))
        (magit-insert-headers--hook hooks)
        wrapper)
    (setq magit-insert-headers--beginning (point))
    (while (and (setq wrapper (pop magit-insert-headers--hook))
                (= (point) magit-insert-headers--beginning))
      (funcall wrapper))))

(defun magit-insert-remaining-headers ()
  (if (= (point) magit-insert-headers--beginning)
      (magit-cancel-section)
    (magit-insert-heading)
    (remove-hook 'magit-insert-section-hook 'magit-insert-remaining-headers)
    (mapc #'funcall magit-insert-headers--hook)
    (insert "\n")))

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

;;; Update

(defvar-local magit-section-highlight-overlays nil)
(defvar-local magit-section-highlighted-section nil)
(defvar-local magit-section-highlighted-sections nil)
(defvar-local magit-section-unhighlight-sections nil)

(defun magit-section-update-region (_)
  "When the region is a valid section-selection, highlight them all."
  ;; At least that's what it does conceptually.  In actuality it just
  ;; returns a list of those sections, and it doesn't even matter if
  ;; this is a member of `magit-region-highlight-hook'.  It probably
  ;; should be removed, but I want to make sure before removing it.
  (magit-region-sections))

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
        (--each magit-section-unhighlight-sections
          (run-hook-with-args-until-success
           'magit-section-unhighlight-hook it selection))
        (restore-buffer-modified-p nil)
        (unless (eq magit-section-highlighted-section section)
          (setq magit-section-highlighted-section
                (and (not (oref section hidden))
                     section))))
      (when (version< emacs-version "25.1")
        (setq deactivate-mark nil)))))

(defun magit-section-highlight (section selection)
  "Highlight SECTION and if non-nil all sections in SELECTION.
This function works for any section but produces undesirable
effects for diff related sections, which by default are
highlighted using `magit-diff-highlight'.  Return t."
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
    (--each selection
      (magit-section-make-overlay (oref it start)
                                  (or (oref it content)
                                      (oref it end))
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
    (overlay-put ov 'face face)
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
      (or (and (magit-hunk-section-p section)
               (-when-let (parent (magit-get-section
                                   (magit-section-ident
                                    (oref section parent))))
                 (let* ((children (oref parent children))
                        (siblings (magit-section-siblings section 'prev))
                        (previous (nth (length siblings) children)))
                   (if (not arg)
                       (--when-let (or previous (car (last children)))
                         (magit-section-goto it)
                         t)
                     (when previous
                       (magit-section-goto previous))
                     (if (and (stringp arg)
                              (re-search-forward arg (oref parent end) t))
                         (goto-char (match-beginning 0))
                       (goto-char (oref (car (last children)) end))
                       (forward-line -1)
                       (while (looking-at "^ ")    (forward-line -1))
                       (while (looking-at "^[-+]") (forward-line -1))
                       (forward-line))))))
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

;;; Visibility

(defvar-local magit-section-visibility-cache nil)
(put 'magit-section-visibility-cache 'permanent-local t)

(defun magit-section-cached-visibility (section)
  "Set SECTION's visibility to the cached value."
  (cdr (assoc (magit-section-ident section)
              magit-section-visibility-cache)))

(cl-defun magit-section-cache-visibility
    (&optional (section magit-insert-section--current))
  ;; Emacs 24 doesn't have `alist-get'.
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

(defun magit-preserve-section-visibility-cache ()
  (when (derived-mode-p 'magit-status-mode 'magit-refs-mode)
    (magit-repository-local-set
     (cons major-mode 'magit-section-visibility-cache)
     magit-section-visibility-cache)))

(defun magit-restore-section-visibility-cache (mode)
  (setq magit-section-visibility-cache
        (magit-repository-local-get
         (cons mode 'magit-section-visibility-cache))))

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
  (-when-let (parent (oref section parent))
    (oref parent value)))

(defun magit-section-siblings (section &optional direction)
  "Return a list of the sibling sections of SECTION.

If optional DIRECTION is `prev', then return siblings that come
before SECTION.  If it is `next', then return siblings that come
after SECTION.  For all other values, return all siblings
excluding SECTION itself."
  (-when-let (parent (oref section parent))
    (let ((siblings (oref parent children)))
      (pcase direction
        (`prev  (cdr (member section (reverse siblings))))
        (`next  (cdr (member section siblings)))
        (_      (remq section siblings))))))

(defun magit-region-values (&optional types multiple)
  "Return a list of the values of the selected sections.

Also see `magit-region-sections' whose doc-string explains when a
region is a valid section selection.  If the region is not active
or is not a valid section selection, then return nil.  If optional
TYPES is non-nil then the selection not only has to be valid; the
types of all selected sections additionally have to match one of
TYPES, or nil is returned."
  (--map (oref it value)
         (magit-region-sections types multiple)))

(defun magit-region-sections (&optional types multiple)
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

If optional TYPES is non-nil, then the selection not only has to
be valid; the types of all selected sections additionally have
to match one of TYPES, or nil is returned.  TYPES can also be a
single type, instead of a list of types."
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
            (when (and types (symbolp types))
              (setq types (list types)))
            (when (or (not types)
                      (--all-p (memq (oref it type) types) sections))
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
  (or (default-boundp hook) (set-default hook nil))
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

(defun magit-run-section-hook (hook)
  "Run HOOK, warning about invalid entries."
  (--if-let (-remove #'functionp (symbol-value hook))
      (progn
        (message "`%s' contains entries that are no longer valid.
%s\nUsing standard value instead.  Please re-configure hook variable."
                 hook
                 (mapconcat (lambda (sym) (format "  `%s'" sym)) it "\n"))
        (sit-for 5)
        (defvar magit--hook-standard-value nil)
        (let ((magit--hook-standard-value
               (eval (car (get hook 'standard-value)))))
          (run-hooks 'magit---hook-standard-value)))
    (run-hooks hook)))

(provide 'magit-section)
;;; magit-section.el ends here
