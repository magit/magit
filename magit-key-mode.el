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
(require 'magit)

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
  (let ((fsym (intern (format "magit-key-mode-popup-%s" name)))
        (vsym (intern (format "magit-popup-%s" name)))
        (msym (intern (format "magit-popup-%s-map" name))))
    `(progn
       (defun ,fsym () ,doc
         (interactive)
         (magit-popup-mode-setup ',name))
       (defvar ,vsym
         (list ,@plist))
       (defvar ,msym
         (magit-define-popup-keymap ',name ,vsym))
       (put ',fsym 'definition-name ',name)
       (put ',vsym 'definition-name ',name)
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
  (let* ((spec (symbol-value (intern (format "magit-popup-%s" popup))))
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
   (symbol-value (intern (format "magit-popup-%s-map" popup))))
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
        (spec (symbol-value (intern (format "magit-popup-%s" popup)))))
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

;;; (being refactored)

(magit-define-popup dispatch
  "Key menu for dispatch."
  :actions '(("b" "Branching"       magit-key-mode-popup-branching)
             ("B" "Bisecting"       magit-key-mode-popup-bisecting)
             ("c" "Committing"      magit-key-mode-popup-committing)
             ("d" "Diff worktree"   magit-diff-working-tree)
             ("D" "Diff"            magit-diff)
             ("f" "Fetching"        magit-key-mode-popup-fetching)
             ("F" "Pulling"         magit-key-mode-popup-pulling)
             ("g" "Refresh Buffers" magit-refresh-all)
             ("l" "Logging"         magit-key-mode-popup-logging)
             ("m" "Merging"         magit-key-mode-popup-merging)
             ("M" "Remoting"        magit-key-mode-popup-remoting)
             ("P" "Pushing"         magit-key-mode-popup-pushing)
             ("o" "Submoduling"     magit-key-mode-popup-submodule)
             ("r" "Rewriting"       magit-key-mode-popup-rewriting)
             ("R" "Rebasing"        magit-rebase-step)
             ("s" "Show Status"     magit-status)
             ("S" "Stage all"       magit-stage-all)
             ("t" "Tagging"         magit-key-mode-popup-tagging)
             ("U" "Unstage all"     magit-unstage-all)
             ("v" "Show Commit"     magit-show-commit)
             ("V" "Show File"       magit-show)
             ("w" "Wazzup"          magit-wazzup)
             ("X" "Reset worktree"  magit-reset-working-tree)
             ("y" "Cherry"          magit-cherry)
             ("z" "Stashing"        magit-key-mode-popup-stashing)
             ("!" "Running"         magit-key-mode-popup-running)
             ("$" "Show Process"    magit-display-process)))

(magit-define-popup logging
  "Key menu for logging."
  :man-page "git-log"
  :switches '(("-m" "Only merge commits" "--merges")
              ("-do" "Date Order" "--date-order")
              ("-f" "First parent" "--first-parent")
              ("-i" "Case insensitive patterns" "-i")
              ("-pr" "Pickaxe regex" "--pickaxe-regex")
              ("-g" "Show Graph" "--graph")
              ("-n" "Name only" "--name-only")
              ("-am" "All match" "--all-match")
              ("-al" "All" "--all"))
  :options  '(("=r" "Relative" "--relative=" read-directory-name)
              ("=c" "Committer" "--committer=" read-from-minibuffer)
              ("=>" "Since" "--since=" read-from-minibuffer)
              ("=<" "Before" "--before=" read-from-minibuffer)
              ("=a" "Author" "--author=" read-from-minibuffer)
              ("=g" "Grep messages" "--grep=" read-from-minibuffer)
              ("=G" "Grep patches" "-G" read-from-minibuffer)
              ("=L" "Trace evolution of line range [long log only]"
               "-L" magit-read-file-trace)
              ("=s" "Pickaxe search" "-S" read-from-minibuffer)
              ("=b" "Branches" "--branches=" read-from-minibuffer)
              ("=R" "Remotes" "--remotes=" read-from-minibuffer))
  :actions  '(("l" "Short" magit-log)
              ("L" "Long" magit-log-long)
              ("h" "Head Reflog" magit-reflog-head)
              ("f" "File log" magit-file-log)
              ("rl" "Ranged short" magit-log-ranged)
              ("rL" "Ranged long" magit-log-long-ranged)
              ("rh" "Reflog" magit-reflog)))

(magit-define-popup running
  "Key menu for running."
  :actions '(("!" "Git Subcommand (from root)" magit-git-command-topdir)
             (":" "Git Subcommand (from pwd)" magit-git-command)
             ("g" "Git Gui" magit-run-git-gui)
             ("k" "Gitk" magit-run-gitk)))

(magit-define-popup fetching
  "Key menu for fetching."
  :man-page "git-fetch"
  :switches '(("-p" "Prune" "--prune"))
  :actions  '(("f" "Current" magit-fetch-current)
              ("a" "All" magit-remote-update)
              ("o" "Other" magit-fetch)))

(magit-define-popup pushing
  "Key menu for pushing."
  :man-page "git-push"
  :switches '(("-f" "Force" "--force")
              ("-d" "Dry run" "-n")
              ("-u" "Set upstream" "-u"))
  :actions  '(("P" "Push" magit-push)
              ("t" "Push tags" magit-push-tags)))

(magit-define-popup pulling
  "Key menu for pulling."
  :man-page "git-pull"
  :switches '(("-f" "Force" "--force")
              ("-r" "Rebase" "--rebase"))
  :actions  '(("F" "Pull" magit-pull)))

(magit-define-popup branching
  "Key menu for branching."
  :man-page "git-branch"
  :switches '(("-t" "Set upstream configuration" "--track")
              ("-m" "Merged to HEAD" "--merged")
              ("-M" "Merged to master" "--merged=master")
              ("-n" "Not merged to HEAD" "--no-merged")
              ("-N" "Not merged to master" "--no-merged=master"))
  :options  '(("=c" "Contains" "--contains=" magit-read-rev-with-default)
              ("=m" "Merged" "--merged=" magit-read-rev-with-default)
              ("=n" "Not merged" "--no-merged=" magit-read-rev-with-default))
  :actions  '(("v" "Branch manager" magit-branch-manager)
              ("b" "Checkout" magit-checkout)
              ("c" "Create" magit-create-branch)
              ("r" "Rename" magit-rename-branch)
              ("k" "Delete" magit-delete-branch)))

(magit-define-popup remoting
  "Key menu for remoting."
  :man-page "git-remote"
  :actions  '(("v" "Remote manager" magit-branch-manager)
              ("a" "Add" magit-add-remote)
              ("r" "Rename" magit-rename-remote)
              ("k" "Remove" magit-remove-remote)))

(magit-define-popup tagging
  "Key menu for tagging."
  :man-page "git-tag"
  :switches '(("-a" "Annotate" "--annotate")
              ("-f" "Force" "--force")
              ("-s" "Sign" "--sign"))
  :actions  '(("t" "Create" magit-tag)
              ("k" "Delete" magit-delete-tag)))

(magit-define-popup stashing
  "Key menu for stashing."
  :man-page "git-stash"
  :switches '(("-k" "Keep index" "--keep-index")
              ("-u" "Include untracked files" "--include-untracked")
              ("-a" "Include all files" "--all"))
  :actions  '(("v" "View" magit-diff-stash)
              ("z" "Save" magit-stash)
              ("s" "Snapshot" magit-stash-snapshot)
              ("a" "Apply" magit-stash-apply)
              ("p" "Pop" magit-stash-pop)
              ("k" "Drop" magit-stash-drop)))

(magit-define-popup committing
  "Key menu for committing."
  :man-page "git-commit"
  :switches '(("-r" "Replace the tip of current branch" "--amend")
              ("-R" "Claim authorship and reset author date" "--reset-author")
              ("-a" "Stage all modified and deleted files" "--all")
              ("-e" "Allow empty commit" "--allow-empty")
              ("-v" "Show diff of changes to be committed" "--verbose")
              ("-n" "Bypass git hooks" "--no-verify")
              ("-s" "Add Signed-off-by line" "--signoff"))
  :options  '(("=S" "Sign using gpg" "--gpg-sign=" magit-read-gpg-secret-key))
  :actions  '(("c" "Commit" magit-commit)
              ("a" "Amend"  magit-commit-amend)
              ("e" "Extend" magit-commit-extend)
              ("r" "Reword" magit-commit-reword)
              ("f" "Fixup"  magit-commit-fixup)
              ("s" "Squash" magit-commit-squash)))

(magit-define-popup merging
  "Key menu for merging."
  :man-page "git-merge"
  :switches '(("-ff" "Fast-forward only" "--ff-only")
              ("-nf" "No fast-forward" "--no-ff")
              ("-sq" "Squash" "--squash"))
  :options  '(("-st" "Strategy" "--strategy=" read-from-minibuffer))
  :actions  '(("m" "Merge" magit-merge)
              ("A" "Abort" magit-merge-abort)))

(magit-define-popup rewriting
  "Key menu for rewriting."
  :actions '(("b" "Begin" magit-rewrite-start)
             ("s" "Stop" magit-rewrite-stop)
             ("a" "Abort" magit-rewrite-abort)
             ("f" "Finish" magit-rewrite-finish)
             ("*" "Set unused" magit-rewrite-set-unused)
             ("." "Set used" magit-rewrite-set-used)))

(magit-define-popup apply-mailbox
  "Key menu for apply-mailbox."
  :man-page "git-am"
  :switches '(("-s" "add a Signed-off-by line to the commit message" "--signoff")
              ("-3" "allow fall back on 3way merging if needed" "--3way")
              ("-k" "pass -k flag to git-mailinfo" "--keep")
              ("-c" "strip everything before a scissors line" "--scissors")
              ("-p" "pass it through git-apply" "-p")
              ("-r" "override error message when patch failure occurs" "--resolvemsg")
              ("-d" "lie about committer date" "--committer-date-is-author-date")
              ("-D" "use current timestamp for author date" "--ignore-date")
              ("-b" "pass -b flag to git-mailinfo" "--keep-non-patch"))
  :options  '(("=p" "format the patch(es) are in" "--patch-format"))
  :actions  '(("J" "Apply Mailbox" magit-apply-mailbox)))

(magit-define-popup submodule
  "Key menu for submodule."
  :man-page "git-submodule"
  :actions  '(("u" "Update" magit-submodule-update)
              ("b" "Both update and init" magit-submodule-update-init)
              ("i" "Init" magit-submodule-init)
              ("s" "Sync" magit-submodule-sync)))

(magit-define-popup bisecting
  "Key menu for bisecting."
  :man-page "git-bisect"
  :actions  '(("b" "Bad" magit-bisect-bad)
              ("g" "Good" magit-bisect-good)
              ("k" "Skip" magit-bisect-skip)
              ("r" "Reset" magit-bisect-reset)
              ("s" "Start" magit-bisect-start)
              ("u" "Run" magit-bisect-run)))

(provide 'magit-key-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-key-mode.el ends here
