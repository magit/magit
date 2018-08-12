---
title: 
name: Bug report
about: Report a defect
---

Please explain
  (1) what behavior you expected
  (2) what behavior you observed
  (3) and how we can reproduce the issue.

Also post the output of

  M-x magit-version RET

Before reporting a defect please try to reproduce it using an Emacs instance in which only Magit and its dependencies have been loaded. Other packages or your configuration should not be loaded. This makes it easier to determine whether the issue lays with Magit or something else.

If you run Magit from its Git repository, then you can do so using:

  $ cd /path/to/magit
  $ make emacs-Q

Alternatively, run `M-x magit-emacs-Q-command RET` to save a shell command to the `kill-ring` and the system's clip-board, which you can then copy into a shell to run.

Finally, if that didn't work and you have installed Magit from Melpa, then run commands similar to the ones below, but use tab completion to replace the various Ns with the correct versions:

  $ cd ~/.emacs.d/elpa/magit-N
  $ emacs -Q --debug-init --eval '(setq debug-on-error t)' -L ../dash-N -L ../git-commit-N -L ../magit-popup-N -L ../with-editor-N -L . -l magit

More debugging tools are described in the manual [6].

  [6]: https://magit.vc/manual/magit/Debugging-Tools.html

---- now delete this line and everything above ----
