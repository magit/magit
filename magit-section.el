;;; magit-section.el --- section functionality

;; Copyright (C) 2010-2014  The Magit Project Developers
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

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

;;; Code:

(require 'cl-lib)
(require 'dash)

;;; Options

(defgroup magit-section nil
  "Expandable sections."
  :group 'magit)

(defcustom magit-section-show-child-count nil
  "Whether to append the number of childen to section headings."
  :package-version '(magit . "2.1.0")
  :group 'magit-section
  :type 'boolean)

(defcustom magit-section-highlight-hook
  '(magit-diff-highlight magit-section-highlight)
  "Functions used to highlight the current section.
Each function is run with the current section as only argument
until one of them returns non-nil."
  :package-version '(magit . "2.1.0")
  :group 'magit-section
  :type 'hook
  :options '(magit-diff-highlight magit-section-highlight))

(defcustom magit-section-unhighlight-hook
  '(magit-diff-unhighlight)
  "Functions used to unhighlight the previously current section.
Each function is run with the current section as only argument
until one of them returns non-nil."
  :package-version '(magit . "2.1.0")
  :group 'magit-section
  :type 'hook
  :options '(magit-diff-unhighlight))

(defface magit-section-highlight
  '((((class color) (background light)) :background "grey85")
    (((class color) (background  dark)) :background "grey20"))
  "Face for highlighting the current section."
  :group 'magit-faces)

(defface magit-section-heading
  '((((class color) (background light)) :background "grey80" :weight bold)
    (((class color) (background  dark)) :background "grey25" :weight bold))
  "Face for section headings."
  :group 'magit-faces)

;;; Core

(cl-defstruct magit-section
  type value start content end hidden washer
  source process parent children)

(defvar-local magit-root-section nil
  "The root section in the current buffer.
All other sections are descendants of this section.  The value
of this variable is set by `magit-insert-section' and you should
never modify it.")
(put 'magit-root-section 'permanent-local t)

(defvar-local magit-current-section nil
  "For internal use only.  Instead use function by same name.")

(defun magit-current-section ()
  "Return the section at point."
  (or (get-text-property (point) 'magit-section) magit-root-section))

(defun magit-section-ident (section)
  "Return an unique identifier for SECTION.
The return value has the form ((TYPE . VALUE)...)."
  (cons (cons (magit-section-type section)
              (magit-section-value section))
        (--when-let (magit-section-parent section)
          (magit-section-ident it))))

(defun magit-get-section (ident &optional root)
  "Return the section identified by IDENT.
IDENT has to be a list as returned by `magit-section-ident'."
  (setq ident (reverse ident))
  (let ((section (or root magit-root-section)))
    (when (eq (car (pop ident)) (magit-section-type section))
      (while (and ident
                  (setq section
                        (--first
                         (and (eq    (caar ident) (magit-section-type it))
                              (equal (cdar ident) (magit-section-value it)))
                         (magit-section-children section))))
        (pop ident))
      section)))

;;; Commands
;;;; Movement

(defun magit-section-forward ()
  "Move to the beginning of the next section."
  (interactive)
  (if (eobp)
      (error "No next section")
    (let ((section (magit-current-section)))
      (if (magit-section-parent section)
          (let ((next (and (not (magit-section-hidden   section))
                           (car (magit-section-children section)))))
            (while (and section (not next))
              (unless (setq next (car (magit-section-siblings section 'next)))
                (setq section (magit-section-parent section))))
            (if next
                (magit-goto-section next)
              (error "No next section")))
        (forward-line 1)))))

(defun magit-section-backward ()
  "Move to the beginning of the previous section."
  (interactive)
  (if (bobp)
      (error "No previous section")
    (let ((section (magit-current-section)) children)
      (if (and (eq (point) (1- (magit-section-end section)))
               (setq children (magit-section-children section)))
          (magit-goto-section (car (last children)))
        (let ((prev (car (magit-section-siblings section 'prev))))
          (if prev
              (while (and (not (magit-section-hidden prev))
                          (setq children (magit-section-children prev)))
                (setq prev (car (last children))))
            (setq prev (magit-section-parent section)))
          (if prev
              (magit-goto-section prev)
            (if (magit-section-parent section)
                (error "No previous section")
              (forward-line -1))))))))

(defun magit-section-up ()
  "Go to the parent section."
  (interactive)
  (--if-let (magit-section-parent (magit-current-section))
      (magit-goto-section it)
    (error "No parent section")))

(defun magit-section-forward-sibling ()
  "Move to the beginning of the next sibling section.
If there is no next sibling section, then move to the parent."
  (interactive)
  (let ((current (magit-current-section)))
    (if (magit-section-parent current)
        (--if-let (car (magit-section-siblings current 'next))
            (magit-goto-section it)
          (magit-section-forward))
      (forward-line 1))))

(defun magit-section-backward-sibling ()
  "Move to the beginning of the previous sibling section.
If there is no previous sibling section, then move to the parent."
  (interactive)
  (let ((current (magit-current-section)))
    (if (magit-section-parent current)
        (--if-let (car (magit-section-siblings current 'prev))
            (magit-goto-section it)
          (magit-section-backward))
      (forward-line -1))))

(defun magit-goto-section (section)
  (goto-char (magit-section-start section))
  (run-hook-with-args 'magit-goto-section-hook section))

(defvar magit-goto-section-hook
  '(magit-hunk-set-window-start
    magit-log-maybe-show-commit
    magit-log-maybe-show-more-entries))

(defun magit-section-set-window-start (section)
  (unless (pos-visible-in-window-p (magit-section-end section))
    (set-window-start (selected-window) (magit-section-start section))))

(defun magit-hunk-set-window-start (section)
  (when (eq (magit-section-type section) 'hunk)
    (magit-section-set-window-start section)))

(defmacro magit-define-section-jumper (sym title)
  "Define an interactive function to go to section SYM.
TITLE is the displayed title of the section."
  (let ((fun (intern (format "magit-jump-to-%s" sym))))
    `(progn
       (defun ,fun (&optional expand) ,(format "\
Jump to section '%s'.
With a prefix argument also expand it." title)
         (interactive "P")
         (--if-let (magit-get-section
                    (cons '(,sym) (magit-section-ident magit-root-section)))
             (progn (goto-char (magit-section-start it))
                    (when expand
                      (with-local-quit (magit-section-show it))
                      (recenter 0)))
           (message ,(format "Section '%s' wasn't found" title))))
       (put ',fun 'definition-name ',sym))))

;;;; Visibility

(defun magit-section-show (section)
  "Show the body of the current section."
  (interactive (list (magit-current-section)))
  (setf (magit-section-hidden section) nil)
  (-when-let (washer (magit-section-washer section))
    (setf (magit-section-washer section) nil)
    (let ((inhibit-read-only t)
          (magit-insert-section--parent section))
      (save-excursion
        (goto-char (magit-section-end section))
        (setf (magit-section-content section) (point-marker))
        (funcall washer)
        (setf (magit-section-end section) (point-marker))))
    (magit-section-update-highlight t))
  (-when-let (beg (magit-section-content section))
    (let ((inhibit-read-only t))
      (put-text-property beg (magit-section-end section) 'invisible nil)))
  (dolist (child (magit-section-children section))
    (if (magit-section-hidden child)
        (magit-section-hide child)
      (magit-section-show child))))

(defun magit-section-hide (section)
  "Hide the body of the current section."
  (interactive (list (magit-current-section)))
  (if (eq section magit-root-section)
      (user-error "Cannot hide root section")
    (setf (magit-section-hidden section) t)
    (-when-let (beg (magit-section-content section))
      (let ((inhibit-read-only t))
        (put-text-property beg (magit-section-end section) 'invisible t)))))

(defun magit-section-toggle (section)
  "Toggle visibility of the body of the current section."
  (interactive (list (magit-current-section)))
  (if (eq section magit-root-section)
      (user-error "Cannot hide root section")
    (goto-char (magit-section-start section))
    (if (magit-section-hidden section)
        (magit-section-show section)
      (magit-section-hide section))))

(defun magit-section-toggle-children (section)
  "Toggle visibility of bodies of children of the current section."
  (interactive (list (magit-current-section)))
  (goto-char (magit-section-start section))
  (let* ((children (magit-section-children section))
         (show (-any? 'magit-section-hidden children)))
    (dolist (c children)
      (setf (magit-section-hidden c) show)))
  (magit-section-show section))

(defun magit-section-show-children (section &optional depth)
  "Recursively show the bodies of children of the current section.
With a prefix argument show children that deep and hide deeper
children."
  (interactive (list (magit-current-section)))
  (magit-section-show-children-1 section depth)
  (magit-section-show section))

(defun magit-section-show-children-1 (section &optional depth)
  (dolist (s (magit-section-children section))
    (setf (magit-section-hidden s) nil)
    (if depth
        (if (> depth 0)
            (magit-section-show-children-1 s (1- depth))
          (magit-section-hide s))
      (magit-section-show-children-1 s))))

(defun magit-section-hide-children (section)
  "Recursively hide the bodies of children of the current section."
  (interactive (list (magit-current-section)))
  (mapc 'magit-section-hide (magit-section-children section)))

(defun magit-section-show-headings (section)
  "Recursively how headings of children of the current section.
Only show the headings, previously shown text-only bodies are
hidden."
  (interactive (list (magit-current-section)))
  (magit-section-show-headings-1 section)
  (magit-section-show section))

(defun magit-section-show-headings-1 (section)
  (dolist (s (magit-section-children section))
    (if (or (magit-section-children s)
            (not (magit-section-content s)))
        (progn (setf (magit-section-hidden s) nil)
               (magit-section-show-headings-1 s))
      (setf (magit-section-hidden s) t))))

(defun magit-section-cycle (section)
  "Cycle visibility of current section."
  (interactive (list (magit-current-section)))
  (goto-char (magit-section-start section))
  (if (magit-section-hidden section)
      (progn (magit-section-show section)
             (magit-section-hide-children section))
    (let ((children (magit-section-children section)))
      (cond ((and (-any? 'magit-section-hidden   children)
                  (-any? 'magit-section-children children))
             (magit-section-show-headings section))
            ((-any? 'magit-section-hidden-body children)
             (magit-section-show-children section))
            (t
             (magit-section-hide section))))))

(defun magit-section-cycle-global ()
  "Cycle visibility of all sections in the current buffer."
  (interactive)
  (let ((children (magit-section-children magit-root-section)))
    (cond ((and (-any? 'magit-section-hidden   children)
                (-any? 'magit-section-children children))
           (magit-section-show-headings magit-root-section))
          ((-any? 'magit-section-hidden-body children)
           (magit-section-show-children magit-root-section))
          (t
           (mapc 'magit-section-hide children)))))

(defun magit-section-cycle-diffs ()
  "Cycle visibility of diff-related sections in the current buffer."
  (interactive)
  (-when-let (sections
              (cond ((derived-mode-p 'magit-status-mode)
                     (and (magit-get-section '((staged)   (status)))
                          (magit-get-section '((unstaged) (status)))))
                    ((derived-mode-p 'magit-diff-mode)
                     (--filter (eq (magit-section-type it) 'file)
                               (magit-section-children magit-root-section)))))
      (if (-any? 'magit-section-hidden sections)
          (dolist (s sections)
            (magit-section-show s)
            (magit-section-hide-children s))
        (let ((children (cl-mapcan 'magit-section-children sections)))
          (cond ((and (-any? 'magit-section-hidden   children)
                      (-any? 'magit-section-children children))
                 (mapc 'magit-section-show-headings sections))
                ((-any? 'magit-section-hidden-body children)
                 (mapc 'magit-section-show-children sections))
                (t
                 (mapc 'magit-section-hide sections)))))))

(defun magit-section-hidden-body (section &optional pred)
  (--if-let (magit-section-children section)
      (funcall (or pred '-any?) 'magit-section-hidden-body it)
    (and (magit-section-content section)
         (magit-section-hidden  section))))

(defun magit-show-level (level)
  "Show surrounding sections up to LEVEL.
If LEVEL (the prefix argument) is negative show up to the
absolute value.  Sections at higher levels are hidden."
  (interactive "p")
  (if (< level 0)
      (let ((s (magit-current-section)))
        (setq level (- level))
        (while (> (1- (length (magit-section-ident s))) level)
          (setq s (magit-section-parent s))
          (goto-char (magit-section-start s)))
        (magit-section-show-children magit-root-section (1- level)))
    (cl-do* ((s (magit-current-section) (magit-section-parent s))
             (i (1- (length (magit-section-ident s))) (cl-decf i)))
        ((cond ((< i level) (magit-section-show-children s (- level i 1)) t)
               ((= i level) (magit-section-hide s) t))
         (magit-goto-section s)))))

(defun magit-show-level-1 ()
  "Show surrounding sections on first level."
  (interactive)
  (magit-show-level 1))

(defun magit-show-level-1-all ()
  "Show all sections on first level."
  (interactive)
  (magit-show-level -1))

(defun magit-show-level-2 ()
  "Show surrounding sections up to second level."
  (interactive)
  (magit-show-level 2))

(defun magit-show-level-2-all ()
  "Show all sections up to second level."
  (interactive)
  (magit-show-level -2))

(defun magit-show-level-3 ()
  "Show surrounding sections up to third level."
  (interactive)
  (magit-show-level 3))

(defun magit-show-level-3-all ()
  "Show all sections up to third level."
  (interactive)
  (magit-show-level -3))

(defun magit-show-level-4 ()
  "Show surrounding sections up to fourth level."
  (interactive)
  (magit-show-level 4))

(defun magit-show-level-4-all ()
  "Show all sections up to fourth level."
  (interactive)
  (magit-show-level -4))

;;;; Auxiliary

(defun magit-describe-section ()
  "Show information about the section at point.
This command is intended for debugging purposes."
  (interactive)
  (let ((section (magit-current-section)))
    (message "%S %S %s-%s"
             (magit-section-value section)
             (apply 'vector (mapcar 'car (magit-section-ident section)))
             (marker-position (magit-section-start section))
             (marker-position (magit-section-end section)))))

;;; Match

(defun magit-section-match (condition &optional ident)
  "Return t if the section at point matches CONDITION.

Conditions can take the following forms:
  (CONDITION...)  matches if any of the CONDITIONs matches.
  [TYPE...]       matches if the first TYPE matches the type
                  of the section at point, the second matches
                  that of its parent, and so on.
  [* TYPE...]     matches sections that match [TYPE...] and
                  also recursively all their child sections.
  TYPE            matches TYPE regardless of its parents.

Each TYPE is a symbol.  Note that is not necessary to specify all
TYPEs up to the root section as printed by `magit-describe-type',
unless of course your want to be that precise.
\n(fn CONDITION)" ; IDENT is for internal use
  (when (or ident (--when-let (magit-current-section)
                    (mapcar 'car (magit-section-ident it))))
    (if (listp condition)
        (--first (magit-section-match it ident) condition)
      (magit-section-match-1 (if (symbolp condition)
                                 (list condition)
                               (append condition nil))
                             ident))))

(defun magit-section-match-1 (l1 l2)
  (or (null l1)
      (if (eq (car l1) '*)
          (or (magit-section-match-1 (cdr l1) l2)
              (and l2
                   (magit-section-match-1 l1 (cdr l2))))
        (and l2
             (equal (car l1) (car l2))
             (magit-section-match-1 (cdr l1) (cdr l2))))))

(defmacro magit-section-when (condition &rest body)
  "If the section at point matches CONDITION evaluate BODY.

If the section matches evaluate BODY forms sequentially and
return the value of the last one, or if there are no BODY forms
return the value of the section.  If the section does not match
return nil.

See `magit-section-match' for the forms CONDITION can take."
  (declare (indent 1)
           (debug (sexp body)))
  `(--when-let (magit-current-section)
     (when (magit-section-match ',condition
                                (mapcar 'car (magit-section-ident it)))
       ,@(or body '((magit-section-value it))))))

(defmacro magit-section-case (&rest clauses)
  "Choose among clauses on the type of the section at point.

Each clause looks like (CONDITION BODY...).  The type of the
section is compared against each CONDITION; the BODY forms of the
first match are evaluated sequentially and the value of the last
form is returned.  Inside BODY the symbol `it' is bound to the
section at point.  If no clause succeeds or if there is no
section at point return nil.

See `magit-section-match' for the forms CONDITION can take.
Additionall a CONDITION of t is allowed in the final clause, and
matches if no other CONDITION match, even if there is no section
at point."
  (declare (indent 0)
           (debug (&rest (sexp body))))
  (let ((ident (cl-gensym "id")))
    `(let* ((it (magit-current-section))
            (,ident (and it (mapcar 'car (magit-section-ident it)))))
       (cond ,@(mapcar (lambda (clause)
                         `(,(or (eq (car clause) t)
                                `(and it (magit-section-match
                                          ',(car clause) ,ident)))
                           ,@(cdr clause)))
                       clauses)))))
;;; Create

(defvar magit-insert-section--current nil "For internal use only.")
(defvar magit-insert-section--parent  nil "For internal use only.")
(defvar magit-insert-section--oldroot nil "For internal use only.")

(defmacro magit-insert-section (&rest args)
  "\n\n(fn [NAME] (TYPE &optional VALUE HIDE) &rest BODY)"
  (declare (indent defun)
           (debug ([&optional symbolp] (symbolp &optional sexp sexp) body)))
  (let ((s (if (symbolp (car args))
               (pop args)
             (cl-gensym "section"))))
    `(let* ((,s (make-magit-section
                 :type ',(nth 0 (car args))
                 :value ,(nth 1 (car args))
                 :start (point-marker)
                 :parent magit-insert-section--parent)))
       (setf (magit-section-hidden ,s)
             (--if-let (and magit-insert-section--oldroot
                            (magit-get-section
                             (magit-section-ident ,s)
                             magit-insert-section--oldroot))
                 (magit-section-hidden it)
               ,(nth 2 (car args))))
       (let ((magit-insert-section--current ,s)
             (magit-insert-section--parent  ,s)
             (magit-insert-section--oldroot
              (or magit-insert-section--oldroot
                  (unless magit-insert-section--parent
                    (prog1 magit-root-section
                      (setq magit-root-section ,s))))))
         (catch 'cancel-section
           ,@(cdr args)
           (magit-insert-child-count ,s)
           (set-marker-insertion-type (magit-section-start ,s) t)
           (let* ((end (setf (magit-section-end ,s) (point-marker)))
                  (map (intern (format "magit-%s-section-map"
                                       (magit-section-type ,s))))
                  (map (and (boundp map) (symbol-value map))))
             (save-excursion
               (goto-char (magit-section-start ,s))
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
               (magit-section-show ,s)
             (setf (magit-section-children (magit-section-parent ,s))
                   (nconc (magit-section-children (magit-section-parent ,s))
                          (list ,s)))))
         ,s))))

(defun magit-cancel-section ()
  (when magit-insert-section--current
    (if (not (magit-section-parent magit-insert-section--current))
        (insert "(empty)\n")
      (delete-region (magit-section-start magit-insert-section--current)
                     (point))
      (setq magit-insert-section--current nil)
      (throw 'cancel-section nil))))

(defun magit-insert-heading (&rest args)
  (declare (indent defun))
  (when args
    (let ((heading (apply #'concat args)))
      (magit-insert
       (if (next-single-property-change 0 'face (concat "0" heading))
           heading
         (propertize heading 'face 'magit-section-heading)))
      (unless (bolp)
        (insert ?\n))))
  (setf (magit-section-content magit-insert-section--current) (point-marker)))

(defun magit-insert-child-count (section)
  ;; This has to be fast, not pretty!
  (let (content count)
    (when (and magit-section-show-child-count
               (setq count (length (magit-section-children section)))
               (> count 0)
               (setq content (magit-section-content section))
               (eq (char-before (1- content)) ?:))
      (save-excursion
        (goto-char (- content 2))
        (insert (format " (%s)" count))
        (delete-char 1)))))

(defun magit-insert (string &optional face &rest args)
  (if face
      (let ((start (point)))
        (insert string)
        (let ((ov (make-overlay start (point) nil t)))
          (overlay-put ov 'face face)
          (overlay-put ov 'evaporate t)))
    (let ((buf (current-buffer))
          (offset (1- (point))))
      (with-temp-buffer
        (save-excursion (insert string))
        (while (not (eobp))
          (let* ((beg (point))
                 (end (or (next-single-property-change beg 'face)
                          (point-max)))
                 (face (get-text-property beg 'face))
                 (text (buffer-substring-no-properties beg end)))
            (with-current-buffer buf
              (insert text)
              (when face
                (let ((ov (make-overlay (+ beg offset)
                                        (+ end offset) nil t)))
                  (overlay-put ov 'face face)
                  (overlay-put ov 'evaporate t))))
            (goto-char end))))))
  (apply #'insert args))

(defun magit-put-face-property (start end face)
  (let ((ov (make-overlay start end nil t)))
    (overlay-put ov 'face face)
    (overlay-put ov 'evaporate t)))

;;; Update

(defvar-local magit-section-highlight-overlays nil)

(defun magit-section-update-highlight (&optional force)
  (let ((inhibit-read-only t)
        (deactivate-mark nil)
        (old  magit-current-section)
        (new (magit-current-section)))
    (when (or force (not (eq old new)))
      (when old
        (mapc #'delete-overlay magit-section-highlight-overlays)
        (run-hook-with-args-until-success 'magit-section-unhighlight-hook old))
      (unless (or (eq new magit-root-section)
                  (and (use-region-p)
                       (= (region-beginning) (magit-section-start new))
                       (not (magit-section-content new))))
        (run-hook-with-args-until-success 'magit-section-highlight-hook new)))
    (setq magit-current-section new)))

(defun magit-section-highlight (section &optional end face)
  "Highlight the SECTION using an overlay."
  (let ((ov (make-overlay (magit-section-start section)
                          (or end (magit-section-end section)))))
    (overlay-put ov 'face (or face 'magit-section-highlight))
    (overlay-put ov 'evaporate t)
    (push ov magit-section-highlight-overlays)))

(defun magit-section-goto-successor (section line char)
  (let ((ident (magit-section-ident section)))
    (--if-let (magit-get-section ident)
        (let ((start (magit-section-start it)))
          (goto-char start)
          (when (> (length ident) 1)
            (ignore-errors
              (forward-line (1- line))
              (forward-char char))
            (unless (eq (magit-current-section) it)
              (goto-char start))))
      (goto-char (--if-let (magit-section-goto-successor-1 section)
                     (magit-section-start it)
                   (point-min))))))

(defun magit-section-goto-successor-1 (section)
  (or (--when-let (cl-case (magit-section-type section)
                    (staged 'unstaged)
                    (unstaged 'staged)
                    (unpushed 'unpulled)
                    (unpulled 'unpushed))
        (magit-get-section `((,it) (status))))
      (--when-let (car (magit-section-siblings section 'next))
        (or (magit-get-section (magit-section-ident it))
            (when (eq (magit-section-type it) 'hunk)
              (let ((text (car (magit-section-value it))))
                (--first (equal (car (magit-section-value it)) text)
                         (magit-section-children
                          (magit-get-section
                           (cdr (magit-section-ident it)))))))))
      (--when-let (car (magit-section-siblings section 'prev))
        (magit-get-section (magit-section-ident it)))
      (--when-let (magit-section-parent section)
        (or (magit-get-section (magit-section-ident it))
            (magit-section-goto-successor-1 it)))))

;;; Utilities

(defun magit-section-parent-value (section)
  (setq section (magit-section-parent section))
  (when section (magit-section-value  section)))

(defun magit-section-siblings (section &optional direction)
  (-when-let (parent (magit-section-parent section))
    (let ((siblings  (magit-section-children parent)))
      (cl-ecase direction
        (prev (cdr (member section (reverse siblings))))
        (next (cdr (member section siblings)))
        (nil  (delq section siblings))))))

(defun magit-section-region-siblings (&optional key)
  (let ((beg (get-text-property (region-beginning) 'magit-section))
        (end (get-text-property (region-end) 'magit-section)))
    (if (eq beg end)
        (list (if key (funcall key beg) beg))
      (goto-char (region-end))
      (when (bolp)
        (setq end (get-text-property (1- (point)) 'magit-section)))
      (while (> (length (magit-section-ident beg))
                (length (magit-section-ident end)))
        (setq beg (magit-section-parent beg)))
      (while (> (length (magit-section-ident end))
                (length (magit-section-ident beg)))
        (setq end (magit-section-parent end)))
      (let* ((parent   (magit-section-parent beg))
             (siblings (magit-section-children parent))
             (stop     (cadr (memq end siblings))))
        (if (eq parent (magit-section-parent end))
            (mapcar (or key #'identity)
                    (--take-while (not (eq it stop))
                                  (memq beg siblings)))
          (user-error "Ambitious cross-section region"))))))

(defun magit-current-sections (&optional type)
  (let ((sections (or (and (use-region-p)
                           (magit-section-region-siblings))
                      (--when-let (magit-current-section)
                        (list it)))))
    (when (or (not type)
              (eq (magit-section-type (car sections)) type))
      sections)))

(defun magit-map-sections (function section)
  "Apply FUNCTION to SECTION and recursively its subsections."
  (funcall function section)
  (mapc (apply-partially 'magit-map-sections function)
        (magit-section-children section)))

(defun magit-wash-sequence (function)
  "Repeatedly call FUNCTION until it returns nil or eob is reached.
FUNCTION has to move point forward or return nil."
  (while (and (not (eobp)) (funcall function))))

(defun magit-add-section-hook (hook function &optional at append local)
  "Add to the value of section hook HOOK the function FUNCTION.

Add FUNCTION at the beginning of the hook list unless optional
APPEND is non-nil, in which case FUNCTION is added at the end.
If FUNCTION already is a member then move it to the new location.

If optional AT is non-nil and a member of the hook list, then add
FUNCTION next to that instead.  Add before or after AT depending
on APPEND.  If only FUNCTION is a member of the list, then leave
it where ever it already is.

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
  (or (boundp hook) (set hook nil))
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
          (if append
              (push function (cdr at))
            (push (car at) (cdr at))
            (setcar at function)))
      (setq value (delq function value)))
    (unless (member function value)
      (setq value (if append
                      (append value (list function))
                    (cons function value))))
    (if local
        (set hook value)
      (set-default hook value))))

;;; magit-section.el ends soon
(provide 'magit-section)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-section.el ends here
