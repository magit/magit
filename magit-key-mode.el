(require 'magit)

(eval-when-compile (require 'cl))

(defvar magit-key-mode-key-maps '()
  "This will be filled lazily with proper `define-key' built
  keymaps as they're requested.")

(defvar magit-key-mode-buf-name "*magit-key*"
  "Name of the buffer.")

(defvar magit-key-mode-current-args '()
  "Will contain the arguments to be passed to git.")

(defvar magit-key-mode-current-options '()
  "Will contain the arguments to be passed to git.")

(defvar magit-log-mode-window-conf nil
  "Will hold the pre-menu configuration of magit.")

(defvar magit-key-mode-groups
  '((logging
     (man-page "git-log")
     (actions
      ("l" "Short" magit-log)
      ("L" "Long" magit-log-long)
      ("h" "Reflog" magit-reflog)
      ("rl" "Ranged short" magit-log-ranged)
      ("rL" "Ranged long" magit-log-long-ranged)
      ("rh" "Ranged reflog" magit-reflog-ranged))
     (switches
      ("-m" "Only merge commits" "--merges")
      ("-f" "First parent" "--first-parent")
      ("-i" "Case insensitive patterns" "-i")
      ("-pr" "Pickaxe regex" "--pickaxe-regex")
      ("-n" "Name only" "--name-only")
      ("-am" "All match" "--all-match")
      ("-al" "All" "--all"))
     (arguments
      ("=r" "Relative" "--relative=" read-directory-name)
      ("=c" "Committer" "--committer=" read-from-minibuffer)
      ("=>" "Since" "--since=" read-from-minibuffer)
      ("=<" "Before" "--before=" read-from-minibuffer)
      ("=s" "Pickaxe search" "-S" read-from-minibuffer)
      ("=a" "Author" "--author=" read-from-minibuffer)
      ("=g" "Grep" "--grep=" read-from-minibuffer)))

    (running
     (actions
      ("!" "Command from root" magit-shell-command)
      (":" "Git command" magit-git-command)
      ("g" "git gui" magit-run-git-gui)
      ("k" "gitk" magit-run-gitk)))

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
      ("-r" "Rebase" "--rebase")))

    (branching
     (man-page "git-branch")
     (actions
      ("v" "Branch manager" magit-branch-manager)
      ("c" "Create" magit-create-branch)
      ("r" "Rename" magit-move-branch)
      ("k" "Delete" magit-delete-branch)
      ("b" "Checkout" magit-checkout)))

    (remoting
     (man-page "git-remote")
     (actions
      ("v" "Branch manager" magit-branch-manager)
      ("a" "Add" magit-add-remote)
      ("r" "Rename" magit-rename-remote)
      ("k" "Remove" magit-remove-remote)))

    (tagging
     (man-page "git-tag")
     (actions
      ("t" "Lightweight" magit-tag)
      ("a" "Annotated" magit-annotated-tag))
     (switches
      ("-f" "Force" "-f")))

    (stashing
     (man-page "git-stash")
     (actions
      ("z" "Save" magit-stash)
      ("s" "Snapshot" magit-stash-snapshot))
     (switches
      ("-k" "Keep index" "--keep-index")
      ("-u" "Include untracked files" "--include-untracked")
      ("-a" "Include all files" "--all")))

    (merging
     (man-page "git-merge")
     (actions
      ("m" "Merge" magit-manual-merge))
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
      ("l" "Log" magit-bisect-log)
      ("r" "Reset" magit-bisect-reset)
      ("s" "Start" magit-bisect-start)
      ("u" "Run" magit-bisect-run)
      ("v" "Visualize" magit-bisect-visualize))))
  "Holds the key, help, function mapping for the log-mode. If you
  modify this make sure you reset `magit-key-mode-key-maps' to
  nil.")

(defun magit-key-mode-delete-group (group)
  "Delete a group from `magit-key-mode-key-maps'."
  (let ((items (assoc group magit-key-mode-groups)))
    (when items
      ;; reset the cache
      (setq magit-key-mode-key-maps nil)
      ;; delete the whole group
      (setq magit-key-mode-groups
            (delq items magit-key-mode-groups))
      ;; unbind the defun
      (magit-key-mode-de-generate group))
    magit-key-mode-groups))

(defun magit-key-mode-add-group (group)
  "Add a new group to `magit-key-mode-key-maps'. If there's
already a group of that name then this will completely remove it
and put in its place an empty one of the same name."
  (when (assoc group magit-key-mode-groups)
    (magit-key-mode-delete-group group))
  (setq magit-key-mode-groups
        (cons (list group (list 'actions)) magit-key-mode-groups)))

(defun magit-key-mode-key-defined-p (for-group key)
  "If KEY is defined as any of switch, argument or action within
FOR-GROUP then return t"
  (catch 'result
    (let ((options (magit-key-mode-options-for-group for-group)))
      (dolist (type '(actions switches arguments))
        (when (assoc key (assoc type options))
          (throw 'result t))))))

(defun magit-key-mode-update-group (for-group thing &rest args)
  "Abstraction for setting values in `magit-key-mode-key-maps'."
  (let* ((options (magit-key-mode-options-for-group for-group))
         (things (assoc thing options))
         (key (car args)))
    (if (cdr things)
        (if (magit-key-mode-key-defined-p for-group key)
            (error "%s is already defined in the %s group." key for-group)
          (setcdr (cdr things) (cons args (cddr things))))
      (setcdr things (list args)))
    (setq magit-key-mode-key-maps nil)
    things))

(defun magit-key-mode-insert-argument (for-group key desc arg read-func)
  "Add a new binding (KEY) in FOR-GROUP which will use READ-FUNC
to receive input to apply to argument ARG git is run. DESC should
be a brief description of the binding."
  (magit-key-mode-update-group for-group 'arguments key desc arg read-func))

(defun magit-key-mode-insert-switch (for-group key desc switch)
  "Add a new binding (KEY) in FOR-GROUP which will add SWITCH to git's
command line when it runs. DESC should be a brief description of
the binding."
  (magit-key-mode-update-group for-group 'switches key desc switch))

(defun magit-key-mode-insert-action (for-group key desc func)
  "Add a new binding (KEY) in FOR-GROUP which will run command
FUNC. DESC should be a brief description of the binding."
  (magit-key-mode-update-group for-group 'actions key desc func))

(defun magit-key-mode-options-for-group (for-group)
  "Retrieve the options (switches, commands and arguments) for
the group FOR-GROUP."
  (or (cdr (assoc for-group magit-key-mode-groups))
      (error "Unknown group '%s'" for-group)))

(defun magit-key-mode-help (for-group)
  "Provide help for a key (which the user is prompted for) within
FOR-GROUP."
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
  (let* ((key (or (get-text-property (point) 'key-group-executor)
                  (error "Nothing at point to do.")))
         (def (lookup-key (current-local-map) key)))
    (call-interactively def)))
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

(defun magit-key-mode-build-keymap (for-group)
  "Construct a normal looking keymap for the key mode to use and
put it in magit-key-mode-key-maps for fast lookup."
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
    (dolist (k actions)
      (define-key map (car k)
        `(lambda ()
           (interactive)
           (magit-key-mode-command ',(nth 2 k)))))
    (dolist (k switches)
      (define-key map (car k)
        `(lambda ()
           (interactive)
           (magit-key-mode-add-option ',for-group ,(nth 2 k)))))
    (dolist (k arguments)
      (define-key map (car k)
        `(lambda ()
           (interactive)
           (magit-key-mode-add-argument ',for-group ,(nth 2 k) ',(nth 3 k)))))
    (push (cons for-group map) magit-key-mode-key-maps)
    map))

(defvar magit-key-mode-prefix nil
  "For internal use.  Holds the prefix argument to the command
that brought up the key-mode window, so it can be used by the
command that's eventually invoked.")

(defun magit-key-mode-command (func)
  (let ((args '()))
    ;; why can't maphash return a list?!
    (maphash (lambda (k v)
               (push (concat k (shell-quote-argument v)) args))
             magit-key-mode-current-args)
    (let ((magit-custom-options (append args magit-key-mode-current-options))
          (current-prefix-arg (or current-prefix-arg magit-key-mode-prefix)))
      (set-window-configuration magit-log-mode-window-conf)
      (when func
        (call-interactively func))
      (magit-key-mode-kill-buffer))))

(defvar magit-key-mode-current-args nil
  "A hash-table of current argument set (which will eventually
  make it to the git command-line).")

(defun magit-key-mode-add-argument (for-group arg-name input-func)
  (let ((input (funcall input-func (concat arg-name ": "))))
    (puthash arg-name input magit-key-mode-current-args)
   (magit-key-mode-redraw for-group)))

(defvar magit-key-mode-current-options '()
  "Current option set (which will eventually make it to the git
  command-line).")

(defun magit-key-mode-add-option (for-group option-name)
  "Toggles the appearance of OPTION-NAME in
`magit-key-mode-current-options'."
  (if (not (member option-name magit-key-mode-current-options))
      (add-to-list 'magit-key-mode-current-options option-name)
    (setq magit-key-mode-current-options
          (delete option-name magit-key-mode-current-options)))
  (magit-key-mode-redraw for-group))

(defun magit-key-mode-kill-buffer ()
  (interactive)
  (kill-buffer magit-key-mode-buf-name))

(defvar magit-log-mode-window-conf nil
  "Pre-popup window configuration.")

(defun magit-key-mode (for-group &optional original-opts)
  "Mode for magit key selection. All commands, switches and
options can be toggled/actioned with the key combination
highlighted before the description."
  (interactive)
  ;; save the window config to restore it as was (no need to make this
  ;; buffer local)
  (setq magit-log-mode-window-conf
        (current-window-configuration))
  ;; setup the mode, draw the buffer
  (let ((buf (get-buffer-create magit-key-mode-buf-name)))
    (delete-other-windows)
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer buf)
    (kill-all-local-variables)
    (set (make-local-variable
          'magit-key-mode-current-options)
         original-opts)
    (set (make-local-variable
          'magit-key-mode-current-args)
         (make-hash-table))
    (set (make-local-variable 'magit-key-mode-prefix) current-prefix-arg)
    (magit-key-mode-redraw for-group))
  (message
   (concat
    "Type a prefix key to toggle it. Run 'actions' with their prefixes. "
    "'?' for more help.")))

(defun magit-key-mode-get-key-map (for-group)
  "Get or build the keymap for FOR-GROUP."
  (or (cdr (assoc for-group magit-key-mode-key-maps))
      (magit-key-mode-build-keymap for-group)))

(defun magit-key-mode-redraw (for-group)
  "(re)draw the magit key buffer."
  (let ((buffer-read-only nil)
        (old-point (point))
        (is-first (zerop (buffer-size)))
        (actions-p nil))
    (erase-buffer)
    (make-local-variable 'font-lock-defaults)
    (use-local-map (magit-key-mode-get-key-map for-group))
    (setq actions-p (magit-key-mode-draw for-group))
    (delete-trailing-whitespace)
    (setq mode-name "magit-key-mode" major-mode 'magit-key-mode)
    (if (and is-first actions-p)
      (progn (goto-char actions-p)
             (magit-key-mode-jump-to-next-exec))
      (goto-char old-point)))
  (setq buffer-read-only t)
  (fit-window-to-buffer))

(defun magit-key-mode-draw-header (header)
  "Draw a header with the correct face."
  (insert (propertize header 'face 'font-lock-keyword-face) "\n"))

(defvar magit-key-mode-args-in-cols nil
  "When true, draw arguments in columns as with switches and
  options.")

(defun magit-key-mode-draw-args (args)
  "Draw the args part of the menu."
  (magit-key-mode-draw-buttons
   "Args"
   args
   (lambda (x)
     (format "(%s) %s"
             (nth 2 x)
             (propertize (gethash (nth 2 x) magit-key-mode-current-args "")
                         'face 'widget-field)))
   (not magit-key-mode-args-in-cols)))

(defun magit-key-mode-draw-switches (switches)
  "Draw the switches part of the menu."
  (magit-key-mode-draw-buttons
   "Switches"
   switches
   (lambda (x)
     (format "(%s)" (let ((s (nth 2 x)))
                      (if (member s magit-key-mode-current-options)
                        (propertize s 'face 'font-lock-warning-face)
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
               (let* ((head (propertize (car x) 'face 'font-lock-builtin-face))
                      (desc (nth 1 x))
                      (more (and maker (funcall maker x)))
                      (text (format " %s: %s%s%s"
                                    head desc (if more " " "") (or more ""))))
                 (propertize text 'key-group-executor (car x))))
             xs)
     one-col-each)))

(defun magit-key-mode-draw-in-cols (strings one-col-each)
  "Given a list of strings, print in columns (using `insert'). If
ONE-COL-EACH is true then don't columify, but rather, draw each
item on one line."
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
  "Function used to draw actions, switches and parameters.

Returns the point before the actions part, if any."
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

(defun magit-key-mode-de-generate (group)
  "Unbind the function for GROUP."
  (fmakunbound
   (intern (concat "magit-key-mode-popup-" (symbol-name group)))))

(defun magit-key-mode-generate (group)
  "Generate the key-group menu for GROUP"
  (let ((opts (magit-key-mode-options-for-group group)))
    (eval
     `(defun ,(intern (concat "magit-key-mode-popup-" (symbol-name group))) nil
        ,(concat "Key menu for " (symbol-name group))
        (interactive)
        (magit-key-mode (quote ,group))))))

;; create the interactive functions for the key mode popups (which are
;; applied in the top-level key maps)
(mapc (lambda (g)
        (magit-key-mode-generate (car g)))
      magit-key-mode-groups)

(provide 'magit-key-mode)
