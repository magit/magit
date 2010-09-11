(require 'assoc)

(defvar magit-key-mode-key-maps '()
  "This will be filled lazily with proper `define-key' built
  keymaps as they're reqeusted.")

(defvar magit-key-mode-buf-name "*magit-key*"
  "Name of the buffer.")

(defvar magit-key-mode-groups
  '((logging
     (actions
      ("l" "Short" magit-log)
      ("L" "Long" magit-log-long)
      ("h" "Reflog" magit-reflog)
      ("H" "Reflog on head" magit-reflog-head))
     (switches
      ("-m" "Only merge commits" "--merges")
      ("-f" "First parent" "--first-parent")
      ("-i" "Case insesnitive patterns" "-i")
      ("-a" "All" "--all"))
     (arguments
      ("=b" "Branches" "--branches" read-from-minibuffer)
      ("=a" "Author" "--author" read-from-minibuffer)
      ("=g" "Grep" "--grep" read-from-minibuffer)))

    (running
     (actions
      ("!" "Command from root" magit-shell-command)
      (":" "Git command" magit-git-command)))

    (committing
     (actions
      ("c" "Commit" magit-log-edit-commit))
     (switches
      ("-s" "Signoff" "--signoff")
      ("-am" "Amend" "--amend")
      ("-al" "All" "--all"))
      ("-e" "Allow empty" "--allow-empty")
     (arguments
      ("=au" "Author" "--author" read-from-minibuffer)))

    (fetching
     (actions
      ("f" "Fetch" magit-fetch)
      ("r" "Remote update" magit-remote-update)))

    (pushing
     (actions
      ("p" "Push" magit-push))
     (switches
      ("-d" "Dry run" "-n")))

    (pulling
     (actions
      ("p" "Pull" magit-pull))
     (switches
      ("-r" "Rebase" "--rebase")))

    (branching
     (actions
      ("V" "Branch manager" magit-show-branches)
      ("B" "Create" magit-create-branch)
      ("m" "Move" magit-move-branch)
      ("d" "Delete" magit-delete-branch)
      ("c" "Checkout" magit-checkout)))

    (tagging
     (actions
      ("t" "Lightweight" magit-tag)
      ("T" "Annotated" magit-annotated-tag))
     (switches
      ("-f" "Force" "-f")))

    (stashing
     (actions
      ("s" "Save" magit-stash)
      ("S" "Snapshot" magit-stash-snapshot))
     (switches
      ("-k" "Keep index" "--keep-index")))

    (merging
     (actions
      ("m" "Merge" magit-merge))
     (switches
      ("-nf" "No fast-forward" "--no-ff")
      ("-nc" "No commit" "--no-commit")
      ("-sq" "Squash" "--squash"))
     (arguments
      ("-st" "Strategy" "--strategy" read-from-minibuffer)))

    (rewriting
     (actions
      ("b" "Begin" magit-rewrite-start)
      ("s" "Stop" magit-rewrite-stop)
      ("a" "Abort" magit-rewrite-abort)
      ("f" "Finish" magit-rewrite-finish)
      ("*" "Set unused" magit-rewrite-set-unused)
      ("." "Set used" magit-rewrite-set-used))))
  "Holds the key, help, function mapping for the log-mode. If you
  modify this make sure you reset `magit-key-mode-key-maps' to
  nil.")

(defun magit-key-mode-add-group (name)
  "Add a new group to `magit-key-mode-key-maps'."
  (unless (assoc name magit-key-mode-groups)
    (push (list name '(actions)) magit-key-mode-groups)))

(defun magit-key-mode-update-group (for-group thing &rest args)
  "Abstraction for setting values in `magit-key-mode-key-maps'."
  (let* ((options (magit-key-mode-options-for-group for-group))
         (things (assoc thing options)))
    (if (cdr things)
        (setcdr (cdr things) (cons args (cddr things)))
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
commandline when it runs. DESC should be a brief description of
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

(defun magit-key-mode-build-keymap (for-group)
  "Construct a normal looking keymap for the key mode to use and
put it in magit-key-mode-key-maps for fast lookup."
  (let* ((options (magit-key-mode-options-for-group for-group))
         (actions (cdr (assoc 'actions options)))
         (switches (cdr (assoc 'switches options)))
         (arguments (cdr (assoc 'arguments options))))
    (let ((map (make-sparse-keymap)))
      ;; all maps should 'quit' with C-g
      (define-key map (kbd "C-g") (lambda ()
                                    (interactive)
                                    (magit-key-mode-command nil)))
      (when actions
        (dolist (k actions)
          (define-key map (car k) `(lambda ()
                                     (interactive)
                                     (magit-key-mode-command ',(nth 2 k))))))
      (when switches
        (dolist (k switches)
          (define-key map (car k) `(lambda ()
                                     (interactive)
                                     (magit-key-mode-add-option
                                      ',for-group
                                      ,(nth 2 k))))))
      (when arguments
        (dolist (k arguments)
          (define-key map (car k) `(lambda ()
                                     (interactive)
                                     (magit-key-mode-add-argument
                                      ',for-group
                                      ,(nth 2 k)
                                      ',(nth 3 k))))))
      (aput 'magit-key-mode-key-maps for-group map)
      map)))

(defun magit-key-mode-command (func)
  (let ((args '()))
    ;; why can't maphash return a list?!
    (maphash (lambda (k v)
               (push (concat k "=" (shell-quote-argument v)) args))
             magit-key-mode-current-args)
    (let ((magit-custom-options (append args magit-key-mode-current-options)))
      (set-window-configuration magit-log-mode-window-conf)
      (when func
        (call-interactively func))
      (magit-key-mode-kill-buffer))))

(defvar magit-key-mode-current-args nil
  "A hash-table of current argument set (which will eventually
  make it to the git command-line).")

(defun debug-args ()
  (interactive)
  (maphash (lambda (k v) (print (format "%s: %s" k v))) magit-key-mode-current-args))

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
  "Mode for magit key selection."
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
    (magit-key-mode-redraw for-group)))

(defun magit-key-mode-redraw (for-group)
  "(re)draw the magit key buffer."
  (let ((buffer-read-only nil))
    (erase-buffer)
    (make-local-variable 'font-lock-defaults)
    (use-local-map
     (or (cdr (assoc for-group magit-key-mode-key-maps))
         (magit-key-mode-build-keymap for-group)))
    (magit-key-mode-draw for-group)
    (delete-trailing-whitespace)
    (setq mode-name "magit-key-mode" major-mode 'magit-key-mode))
  (setq buffer-read-only t)
  (goto-char (point-min))
  (fit-window-to-buffer))

(defun magit-key-mode-draw-header (header)
  "Draw a header with the correct face."
  (insert (propertize header 'face 'font-lock-keyword-face)))

(defun magit-key-mode-draw-args (args)
  "Draw the args part of the menu."
  (when args
    (magit-key-mode-draw-header "Args\n")
    (dolist (argument args)
      (insert
       (format " %s: (%s) %s %s\n"
               (propertize
               (car argument)
                'face 'font-lock-builtin-face)
               (nth 1 argument)
               (nth 2 argument)
               (propertize
                (gethash (nth 2 argument) magit-key-mode-current-args "")
                'face 'widget-field))))))

(defun magit-key-mode-draw-switches (switches)
  "Draw the switches part of the menu."
  (when switches
    (let ((switch-strs (mapcar
                        (lambda (s)
                          (let ((option (nth 2 s)))
                            (format " %s: %s (%s)"
                                    (propertize (car s)
                                                'face 'font-lock-builtin-face)
                                    (nth 1 s)
                                    (if (member option magit-key-mode-current-options)
                                        (propertize
                                         option
                                         'face 'font-lock-warning-face)
                                      option))))
                          switches)))
      (magit-key-mode-draw-header "Switches\n")
      (magit-key-mode-draw-in-cols switch-strs))))

(defun magit-key-mode-draw-actions (actions)
  "Draw the actions part of the menu."
  (when actions
    (let ((action-strs (mapcar
                        (lambda (a)
                          (format
                           " %s: %s"
                           (propertize (car a)
                                       'face 'font-lock-builtin-face)
                           (nth 1 a)))
                        actions)))
      (magit-key-mode-draw-header "Actions\n")
      (magit-key-mode-draw-in-cols action-strs))))

(defun magit-key-mode-draw-in-cols (strings)
  "Given a list of strings, print in columns (using `insert')."
  (let ((longest-act (apply 'max (mapcar 'length strings))))
    (while strings
      (let ((str (car strings)))
        (let ((padding (make-string (- (+ longest-act 3) (length str)) ? )))
          (insert str)
          (if (and (> (+ (current-column) longest-act) (window-width))
                   (cdr strings))
              (insert "\n")
            (insert padding))))
      (setq strings (cdr strings))))
  (insert "\n"))

(defun magit-key-mode-draw (for-group)
  "Function used to draw actions, switches and parameters."
  (let* ((options (magit-key-mode-options-for-group for-group))
         (switches (cdr (assoc 'switches options)))
         (arguments (cdr (assoc 'arguments options)))
         (actions (cdr (assoc 'actions options))))
    (magit-key-mode-draw-actions actions)
    (magit-key-mode-draw-switches switches)
    (magit-key-mode-draw-args arguments)))

(defun magit-key-mode-generate (sym)
  "Generate the key-group menu for SYM"
  (let ((opts (magit-key-mode-options-for-group sym)))
    (eval
     `(defun ,(intern  (concat "magit-key-mode-popup-" (symbol-name sym))) nil
        ,(concat "Key menu for " (symbol-name sym))
        (interactive)
        (magit-key-mode (quote ,sym))))))

;; create the interactive functions for the key mode popups
(mapc (lambda (g)
        (magit-key-mode-generate (car g)))
      magit-key-mode-groups)

(provide 'magit-key-mode)
