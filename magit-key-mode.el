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

;; This library implements `magit-key-mode' which is used throughout
;; Magit to let the user interactively select the command, switches
;; and options to call Git with.  It can be though of as a way to
;; provide "postfix" arguments.

;;; Code:

(require 'magit)

(eval-when-compile (require 'cl-lib))

(defvar magit-key-mode-keymaps)
(defvar magit-key-mode-last-buffer)
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
  "Face for key mode switches."
  :group 'magit-faces)

(defface magit-key-mode-args-face
  '((t :inherit widget-field))
  "Face for key mode switch arguments."
  :group 'magit-faces)

;;; Keygroups
;;;###autoload
(defvar magit-key-mode-groups
  '((dispatch
     (actions
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
      ("$" "Show Process"    magit-display-process)))

    (logging
     (man-page "git-log")
     (actions
      ("l" "Short" magit-log)
      ("L" "Long" magit-log-long)
      ("h" "Head Reflog" magit-reflog-head)
      ("f" "File log" magit-file-log)
      ("rl" "Ranged short" magit-log-ranged)
      ("rL" "Ranged long" magit-log-long-ranged)
      ("rh" "Reflog" magit-reflog))
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
     (arguments
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
      ("=R" "Remotes" "--remotes=" read-from-minibuffer)))

    (running
     (actions
      ("!" "Git Subcommand (from root)" magit-git-command-topdir)
      (":" "Git Subcommand (from pwd)" magit-git-command)
      ("g" "Git Gui" magit-run-git-gui)
      ("k" "Gitk" magit-run-gitk)))

    (fetching
     (man-page "git-fetch")
     (actions
      ("f" "Current" magit-fetch-current)
      ("a" "All" magit-remote-update)
      ("o" "Other" magit-fetch))
     (switches
      ("-p" "Prune" "--prune")))

    (pushing
     (man-page "git-push")
     (actions
      ("P" "Push" magit-push)
      ("t" "Push tags" magit-push-tags))
     (switches
      ("-f" "Force" "--force")
      ("-d" "Dry run" "-n")
      ("-u" "Set upstream" "-u")))

    (pulling
     (man-page "git-pull")
     (actions
      ("F" "Pull" magit-pull))
     (switches
      ("-f" "Force" "--force")
      ("-r" "Rebase" "--rebase")))

    (branching
     (man-page "git-branch")
     (actions
      ("v" "Branch manager" magit-branch-manager)
      ("b" "Checkout" magit-checkout)
      ("c" "Create" magit-create-branch)
      ("r" "Rename" magit-rename-branch)
      ("k" "Delete" magit-delete-branch))
     (switches
      ("-t" "Set upstream configuration" "--track")
      ("-m" "Merged to HEAD" "--merged")
      ("-M" "Merged to master" "--merged=master")
      ("-n" "Not merged to HEAD" "--no-merged")
      ("-N" "Not merged to master" "--no-merged=master"))
     (arguments
      ("=c" "Contains" "--contains=" magit-read-rev-with-default)
      ("=m" "Merged" "--merged=" magit-read-rev-with-default)
      ("=n" "Not merged" "--no-merged=" magit-read-rev-with-default)))

    (remoting
     (man-page "git-remote")
     (actions
      ("v" "Remote manager" magit-branch-manager)
      ("a" "Add" magit-add-remote)
      ("r" "Rename" magit-rename-remote)
      ("k" "Remove" magit-remove-remote)))

    (tagging
     (man-page "git-tag")
     (actions
      ("t" "Create" magit-tag)
      ("k" "Delete" magit-delete-tag))
     (switches
      ("-a" "Annotate" "--annotate")
      ("-f" "Force" "--force")
      ("-s" "Sign" "--sign")))

    (stashing
     (man-page "git-stash")
     (actions
      ("v" "View" magit-diff-stash)
      ("z" "Save" magit-stash)
      ("s" "Snapshot" magit-stash-snapshot)
      ("a" "Apply" magit-stash-apply)
      ("p" "Pop" magit-stash-pop)
      ("k" "Drop" magit-stash-drop))
     (switches
      ("-k" "Keep index" "--keep-index")
      ("-u" "Include untracked files" "--include-untracked")
      ("-a" "Include all files" "--all")))

    (committing
     (man-page "git-commit")
     (actions
      ("c" "Commit" magit-commit)
      ("a" "Amend"  magit-commit-amend)
      ("e" "Extend" magit-commit-extend)
      ("r" "Reword" magit-commit-reword)
      ("f" "Fixup"  magit-commit-fixup)
      ("s" "Squash" magit-commit-squash))
     (switches
      ("-a" "Stage all modified and deleted files" "--all")
      ("-e" "Allow empty commit" "--allow-empty")
      ("-v" "Show diff of changes to be committed" "--verbose")
      ("-n" "Bypass git hooks" "--no-verify")
      ("-s" "Add Signed-off-by line" "--signoff")
      ("-R" "Claim authorship and reset author date" "--reset-author"))
     (arguments
      ("=A" "Override the author" "--author=" read-from-minibuffer)
      ("=S" "Sign using gpg" "--gpg-sign=" magit-read-gpg-secret-key)))

    (merging
     (man-page "git-merge")
     (actions
      ("m" "Merge" magit-merge)
      ("A" "Abort" magit-merge-abort))
     (switches
      ("-ff" "Fast-forward only" "--ff-only")
      ("-nf" "No fast-forward" "--no-ff")
      ("-sq" "Squash" "--squash"))
     (arguments
      ("-st" "Strategy" "--strategy=" read-from-minibuffer)))

    (rewriting
     (actions
      ("b" "Begin" magit-rewrite-start)
      ("s" "Stop" magit-rewrite-stop)
      ("a" "Abort" magit-rewrite-abort)
      ("f" "Finish" magit-rewrite-finish)
      ("*" "Set unused" magit-rewrite-set-unused)
      ("." "Set used" magit-rewrite-set-used)))

    (apply-mailbox
     (man-page "git-am")
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
      ("=p" "format the patch(es) are in" "--patch-format=" read-from-minibuffer)))

    (submodule
     (man-page "git-submodule")
     (actions
      ("u" "Update" magit-submodule-update)
      ("b" "Both update and init" magit-submodule-update-init)
      ("i" "Init" magit-submodule-init)
      ("s" "Sync" magit-submodule-sync)))

    (bisecting
     (man-page "git-bisect")
     (actions
      ("b" "Bad" magit-bisect-bad)
      ("g" "Good" magit-bisect-good)
      ("k" "Skip" magit-bisect-skip)
      ("r" "Reset" magit-bisect-reset)
      ("s" "Start" magit-bisect-start)
      ("u" "Run" magit-bisect-run)))

    (diff-options
     (actions
      ("s" "Set" magit-set-diff-options)
      ("d" "Set default" magit-set-default-diff-options)
      ("c" "Save default" magit-save-default-diff-options)
      ("r" "Reset to default" magit-reset-diff-options)
      ("h" "Toggle Hunk Refinement" magit-toggle-diff-refine-hunk))
     (switches
      ("-m" "Show smallest possible diff" "--minimal")
      ("-p" "Use patience diff algorithm" "--patience")
      ("-h" "Use histogram diff algorithm" "--histogram")
      ("-b" "Ignore whitespace changes" "--ignore-space-change")
      ("-w" "Ignore all whitespace" "--ignore-all-space")
      ("-W" "Show surrounding functions" "--function-context"))
     ))
  "Holds the key, help, function mapping for the log-mode.
If you modify this make sure you reset `magit-key-mode-keymaps'
to nil.")

(defun magit-key-mode-delete-group (group)
  "Delete a group from `magit-key-mode-keymaps'."
  (let ((items (assoc group magit-key-mode-groups)))
    (when items
      ;; reset the cache
      (setq magit-key-mode-keymaps nil)
      ;; delete the whole group
      (setq magit-key-mode-groups
            (delq items magit-key-mode-groups))
      ;; unbind the defun
      (magit-key-mode-de-generate group))
    magit-key-mode-groups))

(defun magit-key-mode-add-group (group)
  "Add a new group to `magit-key-mode-keymaps'.
If there already is a group of that name then this will
completely remove it and put in its place an empty one of the
same name."
  (when (assoc group magit-key-mode-groups)
    (magit-key-mode-delete-group group))
  (setq magit-key-mode-groups
        (cons (list group (list 'actions) (list 'switches))
              magit-key-mode-groups)))

(defun magit-key-mode-key-defined-p (for-group key)
  "Return t if KEY is defined as any option within FOR-GROUP.
The option may be a switch, argument or action."
  (catch 'result
    (let ((options (magit-key-mode-options-for-group for-group)))
      (dolist (type '(actions switches arguments))
        (when (assoc key (assoc type options))
          (throw 'result t))))))

(defun magit-key-mode-update-group (for-group thing &rest args)
  "Abstraction for setting values in `magit-key-mode-keymaps'."
  (let* ((options (magit-key-mode-options-for-group for-group))
         (things (assoc thing options))
         (key (car args)))
    (if (cdr things)
        (if (magit-key-mode-key-defined-p for-group key)
            (error "%s is already defined in the %s group." key for-group)
          (setcdr (cdr things) (cons args (cddr things))))
      (setcdr things (list args)))
    (setq magit-key-mode-keymaps nil)
    things))

(defun magit-key-mode-insert-argument (for-group key desc arg read-func)
  "Add a new binding KEY in FOR-GROUP which will use READ-FUNC
to receive input to apply to argument ARG git is run.  DESC should
be a brief description of the binding."
  (magit-key-mode-update-group for-group 'arguments key desc arg read-func))

(defun magit-key-mode-insert-switch (for-group key desc switch)
  "Add a new binding KEY in FOR-GROUP which will add SWITCH to git's
command line when it runs.  DESC should be a brief description of
the binding."
  (magit-key-mode-update-group for-group 'switches key desc switch))

(defun magit-key-mode-insert-action (for-group key desc func)
  "Add a new binding KEY in FOR-GROUP which will run command FUNC.
DESC should be a brief description of the binding."
  (magit-key-mode-update-group for-group 'actions key desc func))

(defun magit-key-mode-options-for-group (for-group)
  "Retrieve the options for the group FOR-GROUP.
This includes switches, commands and arguments."
  (or (cdr (assoc for-group magit-key-mode-groups))
      (error "Unknown group '%s'" for-group)))

;;; Commands

(defun magit-key-mode-help (for-group)
  "Provide help for a key within FOR-GROUP.
The user is prompted for the key."
  (let* ((opts (magit-key-mode-options-for-group for-group))
         (man-page (cadr (assoc 'man-page opts)))
         (seq (read-key-sequence
               (format "Enter command prefix%s: "
                       (if man-page
                           (format ", `?' for man `%s'" man-page)
                         ""))))
         (actions (cdr (assoc 'actions opts))))
    (cond
      ;; if it is an action popup the help for the to-be-run function
      ((assoc seq actions) (describe-function (nth 2 (assoc seq actions))))
      ;; if there is "?" show a man page if there is one
      ((equal seq "?")
       (if man-page
           (man man-page)
         (error "No man page associated with `%s'" for-group)))
      (t (error "No help associated with `%s'" seq)))))

(defun magit-key-mode-exec-at-point ()
  "Run action/args/option at point."
  (interactive)
  (let ((key (or (get-text-property (point) 'key-group-executor)
                 (error "Nothing at point to do."))))
    (call-interactively (lookup-key (current-local-map) key))))

(defun magit-key-mode-jump-to-next-exec ()
  "Jump to the next action/args/option point."
  (interactive)
  (let* ((oldp (point))
         (old  (get-text-property oldp 'key-group-executor))
         (p    (if (= oldp (point-max)) (point-min) (1+ oldp))))
    (while (let ((new (get-text-property p 'key-group-executor)))
             (and (not (= p oldp)) (or (not new) (eq new old))))
      (setq p (if (= p (point-max)) (point-min) (1+ p))))
    (goto-char p)
    (skip-chars-forward " ")))

;;; Keymaps

(defvar magit-key-mode-keymaps nil
  "This will be filled lazily with proper keymaps.
These keymaps are created using `define-key' as they're requested.")

(defun magit-key-mode-build-keymap (for-group)
  "Construct a normal looking keymap for the key mode to use.
Put it in `magit-key-mode-keymaps' for fast lookup."
  (let* ((options (magit-key-mode-options-for-group for-group))
         (actions (cdr (assoc 'actions options)))
         (switches (cdr (assoc 'switches options)))
         (arguments (cdr (assoc 'arguments options)))
         (map (make-sparse-keymap)))
    (suppress-keymap map 'nodigits)
    ;; ret dwim
    (define-key map (kbd "RET") 'magit-key-mode-exec-at-point)
    ;; tab jumps to the next "button"
    (define-key map (kbd "TAB") 'magit-key-mode-jump-to-next-exec)

    ;; all maps should `quit' with `C-g' or `q'
    (define-key map (kbd "C-g") `(lambda ()
                                   (interactive)
                                   (magit-key-mode-command nil)))
    (define-key map (kbd "q")   `(lambda ()
                                   (interactive)
                                   (magit-key-mode-command nil)))
    ;; run help
    (define-key map (kbd "?") `(lambda ()
                                 (interactive)
                                 (magit-key-mode-help ',for-group)))

    (let ((defkey (lambda (k action)
                    (when (and (lookup-key map (car k))
                               (not (numberp (lookup-key map (car k)))))
                      (message "Warning: overriding binding for `%s' in %S"
                               (car k) for-group)
                      (ding)
                      (sit-for 2))
                    (define-key map (car k)
                      `(lambda () (interactive) ,action)))))
      (dolist (k actions)
        (funcall defkey k `(magit-key-mode-command ',(nth 2 k))))
      (dolist (k switches)
        (funcall defkey k `(magit-key-mode-toggle-option ',for-group ,(nth 2 k))))
      (dolist (k arguments)
        (funcall defkey k `(magit-key-mode-add-argument
                            ',for-group ,(nth 2 k) ',(nth 3 k)))))

    (push (cons for-group map) magit-key-mode-keymaps)
    map))

;;; Toggling and Running

(defvar magit-key-mode-prefix nil
  "Prefix argument to the command that brought up the key-mode window.
For internal use.  Used by the command that's eventually invoked.")

(defvar magit-key-mode-current-args nil
  "A hash-table of current argument set.
These will eventually make it to the git command-line.")

(defvar magit-key-mode-current-options nil
  "Current option set.
These will eventually make it to the git command-line.")

(defvar magit-custom-options nil
  "List of custom options to pass to Git.
Do not customize this (used in the `magit-key-mode' implementation).")

(defun magit-key-mode-command (func)
  (let ((current-prefix-arg (or current-prefix-arg magit-key-mode-prefix))
        (magit-custom-options magit-key-mode-current-options))
    (maphash (lambda (k v)
               (push (concat k v) magit-custom-options))
             magit-key-mode-current-args)
    (set-window-configuration magit-pre-key-mode-window-conf)
    (kill-buffer magit-key-mode-last-buffer)
    (when func
      (call-interactively func))))

(defun magit-key-mode-add-argument (for-group arg-name input-func)
  (let ((input (funcall input-func (concat arg-name ": "))))
    (puthash arg-name input magit-key-mode-current-args)
    (magit-key-mode-redraw for-group)))

(defun magit-key-mode-toggle-option (for-group option-name)
  "Toggles the appearance of OPTION-NAME in `magit-key-mode-current-options'."
  (if (member option-name magit-key-mode-current-options)
      (setq magit-key-mode-current-options
            (delete option-name magit-key-mode-current-options))
    (add-to-list 'magit-key-mode-current-options option-name))
  (magit-key-mode-redraw for-group))

;;; Mode

(defvar magit-key-mode-buf-name "*magit-key: %s*"
  "Format string to create the name of the magit-key buffer.")

(defvar magit-key-mode-last-buffer nil
  "Store the last magit-key buffer used.")

(defvar magit-pre-key-mode-window-conf nil
  "Will hold the pre-menu configuration of magit.")

(defun magit-key-mode (for-group &optional original-opts)
  "Mode for magit key selection.
All commands, switches and options can be toggled/actioned with
the key combination highlighted before the description."
  (interactive)
  ;; save the window config to restore it as was (no need to make this
  ;; buffer local)
  (setq magit-pre-key-mode-window-conf
        (current-window-configuration))
  ;; setup the mode, draw the buffer
  (let ((buf (get-buffer-create (format magit-key-mode-buf-name
                                        (symbol-name for-group)))))
    (setq magit-key-mode-last-buffer buf)
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer buf)
    (kill-all-local-variables)
    (set (make-local-variable 'scroll-margin) 0)
    (set (make-local-variable
          'magit-key-mode-current-options)
         original-opts)
    (set (make-local-variable
          'magit-key-mode-current-args)
         (make-hash-table))
    (set (make-local-variable 'magit-key-mode-prefix) current-prefix-arg)
    (magit-key-mode-redraw for-group))
  (when magit-key-mode-show-usage
    (message (concat "Type a prefix key to toggle it. "
                     "Run 'actions' with their prefixes. "
                     "'?' for more help."))))

(defun magit-key-mode-get-key-map (for-group)
  "Get or build the keymap for FOR-GROUP."
  (or (cdr (assoc for-group magit-key-mode-keymaps))
      (magit-key-mode-build-keymap for-group)))

(defun magit-key-mode-redraw (for-group)
  "(re)draw the magit key buffer."
  (let ((buffer-read-only nil)
        (current-exec (get-text-property (point) 'key-group-executor))
        (new-exec-pos)
        (old-point (point))
        (is-first (zerop (buffer-size)))
        (actions-p nil))
    (erase-buffer)
    (make-local-variable 'font-lock-defaults)
    (use-local-map (magit-key-mode-get-key-map for-group))
    (setq actions-p (magit-key-mode-draw for-group))
    (delete-trailing-whitespace)
    (setq mode-name "magit-key-mode" major-mode 'magit-key-mode)
    (when current-exec
      (setq new-exec-pos
            (cdr (assoc current-exec
                        (magit-key-mode-build-exec-point-alist)))))
    (cond ((and is-first actions-p)
           (goto-char actions-p)
           (magit-key-mode-jump-to-next-exec))
          (new-exec-pos
           (goto-char new-exec-pos)
           (skip-chars-forward " "))
          (t
           (goto-char old-point))))
  (setq buffer-read-only t)
  (fit-window-to-buffer))

(defun magit-key-mode-build-exec-point-alist ()
  (save-excursion
    (goto-char (point-min))
    (let* ((exec (get-text-property (point) 'key-group-executor))
           (exec-alist (and exec `((,exec . ,(point))))))
      (cl-do nil ((eobp) (nreverse exec-alist))
        (when (not (eq exec (get-text-property (point) 'key-group-executor)))
          (setq exec (get-text-property (point) 'key-group-executor))
          (when exec (push (cons exec (point)) exec-alist)))
        (forward-char)))))

;;; Draw Buffer

(defun magit-key-mode-draw-header (header)
  "Draw a header with the correct face."
  (insert (propertize header 'face 'magit-key-mode-header-face) "\n"))

(defvar magit-key-mode-args-in-cols nil
  "When true, draw arguments in columns as with switches and options.")

(defun magit-key-mode-draw-args (args)
  "Draw the args part of the menu."
  (magit-key-mode-draw-buttons
   "Args"
   args
   (lambda (x)
     (format "(%s) %s"
             (nth 2 x)
             (propertize (gethash (nth 2 x) magit-key-mode-current-args "")
                         'face 'magit-key-mode-args-face)))
   (not magit-key-mode-args-in-cols)))

(defun magit-key-mode-draw-switches (switches)
  "Draw the switches part of the menu."
  (magit-key-mode-draw-buttons
   "Switches"
   switches
   (lambda (x)
     (format "(%s)" (let ((s (nth 2 x)))
                      (if (member s magit-key-mode-current-options)
                          (propertize s 'face 'magit-key-mode-switch-face)
                        s))))))

(defun magit-key-mode-draw-actions (actions)
  "Draw the actions part of the menu."
  (magit-key-mode-draw-buttons "Actions" actions nil))

(defun magit-key-mode-draw-buttons (section xs maker
                                    &optional one-col-each)
  (when xs
    (magit-key-mode-draw-header section)
    (magit-key-mode-draw-in-cols
     (mapcar (lambda (x)
               (let* ((head (propertize (car x) 'face 'magit-key-mode-button-face))
                      (desc (nth 1 x))
                      (more (and maker (funcall maker x)))
                      (text (format " %s: %s%s%s"
                                    head desc (if more " " "") (or more ""))))
                 (propertize text 'key-group-executor (car x))))
             xs)
     one-col-each)))

(defun magit-key-mode-draw-in-cols (strings one-col-each)
  "Given a list of strings, print in columns (using `insert').
If ONE-COL-EACH is true then don't columify, but rather, draw
each item on one line."
  (let ((longest-act (apply 'max (mapcar 'length strings))))
    (while strings
      (let ((str (car strings)))
        (let ((padding (make-string (- (+ longest-act 3) (length str)) ? )))
          (insert str)
          (if (or one-col-each
                  (and (> (+ (length padding) ;
                             (current-column)
                             longest-act)
                          (window-width))
                       (cdr strings)))
              (insert "\n")
            (insert padding))))
      (setq strings (cdr strings))))
  (insert "\n"))

(defun magit-key-mode-draw (for-group)
  "Draw actions, switches and parameters.
Return the point before the actions part, if any, nil otherwise."
  (let* ((options (magit-key-mode-options-for-group for-group))
         (switches (cdr (assoc 'switches options)))
         (arguments (cdr (assoc 'arguments options)))
         (actions (cdr (assoc 'actions options)))
         (p nil))
    (magit-key-mode-draw-switches switches)
    (magit-key-mode-draw-args arguments)
    (when actions (setq p (point-marker)))
    (magit-key-mode-draw-actions actions)
    (insert "\n")
    p))

;;; Generate Groups

(defun magit-key-mode-de-generate (group)
  "Unbind the function for GROUP."
  (fmakunbound
   (intern (concat "magit-key-mode-popup-" (symbol-name group)))))

(defun magit-key-mode-generate (group)
  "Generate the key-group menu for GROUP."
  (let ((opts (magit-key-mode-options-for-group group)))
    (eval
     `(defun ,(intern (concat "magit-key-mode-popup-" (symbol-name group))) nil
        ,(concat "Key menu for " (symbol-name group))
        (interactive)
        (magit-key-mode
         (quote ,group)
         ;; As a tempory kludge it is okay to do this here.
         ,(cl-case group
            (logging
             '(list "--graph"))
            (diff-options
             '(when (local-variable-p 'magit-diff-options)
                magit-diff-options))))))))

;; create the interactive functions for the key mode popups (which are
;; applied in the top-level key maps)
(mapc (lambda (g)
        (magit-key-mode-generate (car g)))
      magit-key-mode-groups)

;;;###autoload (mapc (lambda (g) (eval `(autoload ',(intern (concat "magit-key-mode-popup-" (symbol-name (car g)))) "magit-key-mode" ,(concat "Key menu for " (symbol-name (car g))) t))) magit-key-mode-groups)

(provide 'magit-key-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-key-mode.el ends here
