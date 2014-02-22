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

(defvar magit-pre-key-mode-window-conf)

;;; Options

(defcustom magit-key-mode-show-usage t
  "Whether to show usage information when entering a popup."
  :group 'magit
  :type 'boolean)

;;; Faces

(defface magit-key-mode-header-face
  '((t :inherit font-lock-keyword-face))
  "Face for key mode header lines."
  :group 'magit-faces)

(defface magit-key-mode-button-face
  '((t :inherit font-lock-builtin-face))
  "Face for key mode buttons."
  :group 'magit-faces)

(defface magit-key-mode-switch-face
  '((t :inherit font-lock-warning-face))
  "Face used to display state of switches in popups."
  :group 'magit-faces)

(defface magit-key-mode-option-face
  '((t :inherit widget-field))
  "Face used to display option values in popups."
  :group 'magit-faces)

;;; (being refactored)

(defvar magit-key-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map 'nodigits)
    (define-key map (kbd "C-p") 'backward-button)
    (define-key map (kbd "DEL") 'backward-button)
    (define-key map (kbd "C-n") 'forward-button)
    (define-key map (kbd "TAB") 'forward-button)
    (define-key map (kbd "C-g") 'magit-key-mode-abort)
    (define-key map (kbd "q")   'magit-key-mode-abort)
    map))

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
    (set-keymap-parent map magit-key-mode-map)
    (dolist (e (cdr (assoc 'switches spec)))
      (magit-define-popup-switch popup map (car e) (nth 2 e)))
    (dolist (e (cdr (assoc 'options spec)))
      (magit-define-popup-option popup map (car e) (nth 2 e) (nth 3 e)))
    (dolist (e (cdr (assoc 'actions spec)))
      (magit-define-popup-action popup map (car e) (nth 2 e)))
    (define-key map "?" `(lambda ()
                           (interactive)
                           (magit-key-mode-help ',popup)))
    map))

(defvar-local magit-popup-prefix-arg nil)

(defvar-local magit-popup-current-options nil)
(defvar-local magit-popup-current-switches nil)

(defvar magit-custom-options nil)

(defun magit-invoke-popup-switch (popup switch)
  (if (member switch magit-popup-current-switches)
      (setq magit-popup-current-switches
            (delete switch magit-popup-current-switches))
    (add-to-list 'magit-popup-current-switches switch))
  (magit-refresh-popup-buffer popup))

(defun magit-invoke-popup-option (popup arg-name input-func)
  (let ((elt (assoc arg-name magit-popup-current-options))
        (val (funcall input-func (concat arg-name ": "))))
    (if elt
        (setcdr elt val)
      (push (cons arg-name val) magit-popup-current-options))
    (magit-refresh-popup-buffer popup)))

(defun magit-invoke-popup-action (popup func)
  (let ((current-prefix-arg (or current-prefix-arg magit-popup-prefix-arg))
        (magit-custom-options
         (nconc magit-popup-current-switches
                (mapcar (lambda (elt)
                          (concat (car elt) (cdr elt)))
                        magit-popup-current-options))))
    (let ((buf (current-buffer)))
      (set-window-configuration magit-pre-key-mode-window-conf)
      (kill-buffer buf))
    (when func
      (call-interactively func))))

(defun magit-key-mode-help (popup)
  (let* ((spec (symbol-value (intern (format "magit-popup-%s" popup))))
         (man-page (cadr (assoc 'man-page spec)))
         (seq (read-key-sequence
               (format "Enter command prefix%s: "
                       (if man-page
                           (format ", `?' for man `%s'" man-page)
                         ""))))
         (actions (cdr (assoc 'actions spec))))
    (cond
      ((assoc seq actions) (describe-function (nth 2 (assoc seq actions))))
      ((equal seq "?")
       (if man-page
           (man man-page)
         (error "No man page associated with `%s'" popup)))
      (t (error "No help associated with `%s'" seq)))))

(defun magit-key-mode-abort ()
  (interactive)
  (magit-invoke-popup-action nil nil))

;;; Mode

(defvar magit-key-mode-buf-name "*magit-key: %s*")

(defvar-local magit-pre-key-mode-window-conf nil)

(defun magit-key-mode (popup)
  (interactive)
  (let ((winconf (current-window-configuration))
        (buf (get-buffer-create (format magit-key-mode-buf-name
                                        (symbol-name popup)))))
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer buf)
    (kill-all-local-variables)
    (set (make-local-variable 'scroll-margin) 0)
    (make-local-variable 'font-lock-defaults)
    (setq buffer-read-only t)
    (setq magit-pre-key-mode-window-conf winconf
          magit-popup-prefix-arg current-prefix-arg
          mode-name "magit-key-mode"
          major-mode 'magit-key-mode)
    (use-local-map
     (symbol-value (intern (format "magit-popup-%s-map" popup))))
    (magit-refresh-popup-buffer popup)
    (fit-window-to-buffer))
  (when magit-key-mode-show-usage
    (message (concat "Type a prefix key to toggle it. "
                     "Run actions with their prefixes. "
                     "'?' for more help."))))

(defun magit-refresh-popup-buffer (popup)
  (let ((inhibit-read-only t)
        (key (ignore-errors
               (car (button-get (button-at (point)) 'args))))
        (spec (symbol-value (intern (format "magit-popup-%s" popup)))))
    (erase-buffer)
    (save-excursion
      (magit-popup-insert-buttons popup "Switches" " %k: %d %a"
                                  'magit-invoke-popup-switch
                                  (cdr (assoc 'switches spec)))
      (magit-popup-insert-buttons popup "Options"  " %k: %d %a%v"
                                  'magit-invoke-popup-option
                                  (cdr (assoc 'options spec))
                                  (not magit-key-mode-options-in-cols))
      (magit-popup-insert-buttons popup "Actions"  " %k: %d"
                                  'magit-invoke-popup-action
                                  (cdr (assoc 'actions spec))))
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
    (insert (propertize heading 'face 'magit-key-mode-header-face) "\n")
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
  (let* ((k (propertize (car arg) 'face 'magit-key-mode-button-face))
         (d (nth 1 arg))
         (a (unless (symbolp (nth 2 arg)) (nth 2 arg)))
         (v (and a (cdr (assoc a magit-popup-current-options)))))
    (when (member a magit-popup-current-switches)
      (setq a (propertize a 'face 'magit-key-mode-switch-face)))
    (when v
      (setq v (propertize v 'face 'magit-key-mode-option-face)))
    (format-spec format
                 `((?k . ,k)
                   (?d . ,d)
                   (?a . ,(concat "(" a ")"))
                   (?v . ,(if v (concat " " v) ""))))))

;;; (being refactored)

(defvar magit-popup-dispatch
  '((actions
     ("b" "Branching"       magit-key-mode-popup-branching)
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
     ("$" "Show Process"    magit-display-process))))

(defvar magit-popup-dispatch-map
  (magit-define-popup-keymap 'dispatch magit-popup-dispatch))

(defun magit-key-mode-popup-dispatch ()
  "Key menu for dispatch."
  (interactive)
  (magit-key-mode 'dispatch))

(defvar magit-popup-logging
  '((man-page "git-log")
    (switches
     ("-m" "Only merge commits" "--merges")
     ("-do" "Date Order" "--date-order")
     ("-f" "First parent" "--first-parent")
     ("-i" "Case insensitive patterns" "-i")
     ("-pr" "Pickaxe regex" "--pickaxe-regex")
     ("-g" "Show Graph" "--graph")
     ("-n" "Name only" "--name-only")
     ("-am" "All match" "--all-match")
     ("-al" "All" "--all"))
    (options
     ("=r" "Relative" "--relative=" read-directory-name)
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
    (actions
     ("l" "Short" magit-log)
     ("L" "Long" magit-log-long)
     ("h" "Head Reflog" magit-reflog-head)
     ("f" "File log" magit-file-log)
     ("rl" "Ranged short" magit-log-ranged)
     ("rL" "Ranged long" magit-log-long-ranged)
     ("rh" "Reflog" magit-reflog))))

(defvar magit-popup-logging-map
  (magit-define-popup-keymap 'logging magit-popup-logging))

(defun magit-key-mode-popup-logging ()
  "Key menu for logging."
  (interactive)
  (magit-key-mode 'logging))

(defvar magit-popup-running
  '((actions
     ("!" "Git Subcommand (from root)" magit-git-command-topdir)
     (":" "Git Subcommand (from pwd)" magit-git-command)
     ("g" "Git Gui" magit-run-git-gui)
     ("k" "Gitk" magit-run-gitk))))

(defvar magit-popup-running-map
  (magit-define-popup-keymap 'running magit-popup-running))

(defun magit-key-mode-popup-running ()
  "Key menu for running."
  (interactive)
  (magit-key-mode 'running))

(defvar magit-popup-fetching
  '((man-page "git-fetch")
    (actions
     ("f" "Current" magit-fetch-current)
     ("a" "All" magit-remote-update)
     ("o" "Other" magit-fetch))
    (switches
     ("-p" "Prune" "--prune"))))

(defvar magit-popup-fetching-map
  (magit-define-popup-keymap 'fetching magit-popup-fetching))

(defun magit-key-mode-popup-fetching ()
  "Key menu for fetching."
  (interactive)
  (magit-key-mode 'fetching))

(defvar magit-popup-pushing
  '((man-page "git-push")
    (switches
     ("-f" "Force" "--force")
     ("-d" "Dry run" "-n")
     ("-u" "Set upstream" "-u"))
    (actions
     ("P" "Push" magit-push)
     ("t" "Push tags" magit-push-tags))))

(defvar magit-popup-pushing-map
  (magit-define-popup-keymap 'pushing magit-popup-pushing))

(defun magit-key-mode-popup-pushing ()
  "Key menu for pushing."
  (interactive)
  (magit-key-mode 'pushing))

(defvar magit-popup-pulling
  '((man-page "git-pull")
    (switches
     ("-f" "Force" "--force")
     ("-r" "Rebase" "--rebase"))
    (actions
     ("F" "Pull" magit-pull))))

(defvar magit-popup-pulling-map
  (magit-define-popup-keymap 'pulling magit-popup-pulling))

(defun magit-key-mode-popup-pulling ()
  "Key menu for pulling."
  (interactive)
  (magit-key-mode 'pulling))

(defvar magit-popup-branching
  '((man-page "git-branch")
    (switches
     ("-t" "Set upstream configuration" "--track")
     ("-m" "Merged to HEAD" "--merged")
     ("-M" "Merged to master" "--merged=master")
     ("-n" "Not merged to HEAD" "--no-merged")
     ("-N" "Not merged to master" "--no-merged=master"))
    (options
     ("=c" "Contains" "--contains=" magit-read-rev-with-default)
     ("=m" "Merged" "--merged=" magit-read-rev-with-default)
     ("=n" "Not merged" "--no-merged=" magit-read-rev-with-default))
    (actions
     ("v" "Branch manager" magit-branch-manager)
     ("b" "Checkout" magit-checkout)
     ("c" "Create" magit-create-branch)
     ("r" "Rename" magit-rename-branch)
     ("k" "Delete" magit-delete-branch))))

(defvar magit-popup-branching-map
  (magit-define-popup-keymap 'branching magit-popup-branching))

(defun magit-key-mode-popup-branching ()
  "Key menu for branching."
  (interactive)
  (magit-key-mode 'branching))

(defvar magit-popup-remoting
  '((man-page "git-remote")
    (actions
     ("v" "Remote manager" magit-branch-manager)
     ("a" "Add" magit-add-remote)
     ("r" "Rename" magit-rename-remote)
     ("k" "Remove" magit-remove-remote))))

(defvar magit-popup-remoting-map
  (magit-define-popup-keymap 'remoting magit-popup-remoting))

(defun magit-key-mode-popup-remoting ()
  "Key menu for remoting."
  (interactive)
  (magit-key-mode 'remoting))

(defvar magit-popup-tagging
  '((man-page "git-tag")
    (switches
     ("-a" "Annotate" "--annotate")
     ("-f" "Force" "--force")
     ("-s" "Sign" "--sign"))
    (actions
     ("t" "Create" magit-tag)
     ("k" "Delete" magit-delete-tag))))

(defvar magit-popup-tagging-map
  (magit-define-popup-keymap 'tagging magit-popup-tagging))

(defun magit-key-mode-popup-tagging ()
  "Key menu for tagging."
  (interactive)
  (magit-key-mode 'tagging))

(defvar magit-popup-stashing
  '((man-page "git-stash")
    (switches
     ("-k" "Keep index" "--keep-index")
     ("-u" "Include untracked files" "--include-untracked")
     ("-a" "Include all files" "--all"))
    (actions
     ("v" "View" magit-diff-stash)
     ("z" "Save" magit-stash)
     ("s" "Snapshot" magit-stash-snapshot)
     ("a" "Apply" magit-stash-apply)
     ("p" "Pop" magit-stash-pop)
     ("k" "Drop" magit-stash-drop))))

(defvar magit-popup-stashing-map
  (magit-define-popup-keymap 'stashing magit-popup-stashing))

(defun magit-key-mode-popup-stashing ()
  "Key menu for stashing."
  (interactive)
  (magit-key-mode 'stashing))

(defvar magit-popup-committing
  '((man-page "git-commit")
    (switches
     ("-r" "Replace the tip of current branch" "--amend")
     ("-R" "Claim authorship and reset author date" "--reset-author")
     ("-a" "Stage all modified and deleted files" "--all")
     ("-e" "Allow empty commit" "--allow-empty")
     ("-v" "Show diff of changes to be committed" "--verbose")
     ("-n" "Bypass git hooks" "--no-verify")
     ("-s" "Add Signed-off-by line" "--signoff"))
    (arguments
     ("=S" "Sign using gpg" "--gpg-sign=" magit-read-gpg-secret-key))
    (actions
     ("c" "Commit" magit-commit)
     ("a" "Amend"  magit-commit-amend)
     ("e" "Extend" magit-commit-extend)
     ("r" "Reword" magit-commit-reword)
     ("f" "Fixup"  magit-commit-fixup)
     ("s" "Squash" magit-commit-squash))))

(defvar magit-popup-committing-map
  (magit-define-popup-keymap 'committing magit-popup-committing))

(defun magit-key-mode-popup-committing ()
  "Key menu for committing."
  (interactive)
  (magit-key-mode 'committing))

(defvar magit-popup-merging
  '((man-page "git-merge")
    (switches
     ("-ff" "Fast-forward only" "--ff-only")
     ("-nf" "No fast-forward" "--no-ff")
     ("-sq" "Squash" "--squash"))
    (options
     ("-st" "Strategy" "--strategy=" read-from-minibuffer))
    (actions
     ("m" "Merge" magit-merge)
     ("A" "Abort" magit-merge-abort))))

(defvar magit-popup-merging-map
  (magit-define-popup-keymap 'merging magit-popup-merging))

(defun magit-key-mode-popup-merging ()
  "Key menu for merging."
  (interactive)
  (magit-key-mode 'merging))

(defvar magit-popup-rewriting
  '((actions
     ("b" "Begin" magit-rewrite-start)
     ("s" "Stop" magit-rewrite-stop)
     ("a" "Abort" magit-rewrite-abort)
     ("f" "Finish" magit-rewrite-finish)
     ("*" "Set unused" magit-rewrite-set-unused)
     ("." "Set used" magit-rewrite-set-used))))

(defvar magit-popup-rewriting-map
  (magit-define-popup-keymap 'rewriting magit-popup-rewriting))

(defun magit-key-mode-popup-rewriting ()
  "Key menu for rewriting."
  (interactive)
  (magit-key-mode 'rewriting))

(defvar magit-popup-apply-mailbox
  '((man-page "git-am")
    (actions
     ("J" "Apply Mailbox" magit-apply-mailbox))
    (switches
     ("-s" "add a Signed-off-by line to the commit message" "--signoff")
     ("-3" "allow fall back on 3way merging if needed" "--3way")
     ("-k" "pass -k flag to git-mailinfo" "--keep")
     ("-c" "strip everything before a scissors line" "--scissors")
     ("-p" "pass it through git-apply" "-p")
     ("-r" "override error message when patch failure occurs" "--resolvemsg")
     ("-d" "lie about committer date" "--committer-date-is-author-date")
     ("-D" "use current timestamp for author date" "--ignore-date")
     ("-b" "pass -b flag to git-mailinfo" "--keep-non-patch"))
    (arguments
     ("=p" "format the patch(es) are in" "--patch-format"))))

(defvar magit-popup-apply-mailbox-map
  (magit-define-popup-keymap 'apply-mailbox magit-popup-apply-mailbox))

(defun magit-key-mode-popup-apply-mailbox ()
  "Key menu for apply-mailbox."
  (interactive)
  (magit-key-mode 'apply-mailbox))

(defvar magit-popup-submodule
  '((man-page "git-submodule")
    (actions
     ("u" "Update" magit-submodule-update)
     ("b" "Both update and init" magit-submodule-update-init)
     ("i" "Init" magit-submodule-init)
     ("s" "Sync" magit-submodule-sync))))

(defvar magit-popup-submodule-map
  (magit-define-popup-keymap 'submodule magit-popup-submodule))

(defun magit-key-mode-popup-submodule ()
  "Key menu for submodule."
  (interactive)
  (magit-key-mode 'submodule))

(defvar magit-popup-bisecting
  '((man-page "git-bisect")
    (actions
     ("b" "Bad" magit-bisect-bad)
     ("g" "Good" magit-bisect-good)
     ("k" "Skip" magit-bisect-skip)
     ("r" "Reset" magit-bisect-reset)
     ("s" "Start" magit-bisect-start)
     ("u" "Run" magit-bisect-run))))

(defvar magit-popup-bisecting-map
  (magit-define-popup-keymap 'bisecting magit-popup-bisecting))

(defun magit-key-mode-popup-bisecting ()
  "Key menu for bisecting."
  (interactive)
  (magit-key-mode 'bisecting))

(provide 'magit-key-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-key-mode.el ends here
