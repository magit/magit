---
title: 
name: Bug report
about: Report a defect
---

Please do not ignore these instructions.

If you have just updated Magit, then restart Emacs. If that does not fix the issue, then also uninstall Magit and all dependencies that were updated at the same time, restart Emacs and then reinstall Magit.

The reason why this might fix the issue is that updating a package does not cause the old version to be unloaded, so you might end up with a franken-version; a mixture of parts of the old and new version being loaded at the same time. Worse it is possible for the old version to leak into the byte-code of the new version, which is why reinstalling might help.

Please explain
    (1) what behavior you expected
    (2) what behavior you observed
    (3) and how we can reproduce the issue.

Please include a backtrace in your report.  In most cases doing:

    M-x toggle-debug-on-error RET

and then going through the steps again should result in a backtrace.

Also post the output of:

    M-x magit-version RET

Before reporting a defect please try to reproduce it using an Emacs instance in which only Magit and its dependencies have been loaded. Other packages or your configuration should not be loaded. This makes it easier to determine whether the issue lays with Magit or something else.

If you run Magit from its Git repository, then you can do so using:

    $ cd /path/to/magit
    $ make emacs-Q

Alternatively, run:

    M-x magit-emacs-Q-command RET

to save a shell command to the `kill-ring` and the system's clip-board, which you can then copy into a shell to run.

Finally, if that didn't work and you have installed Magit from Melpa, then run commands similar to the ones above, but use tab completion to replace the various Ns with the correct versions:

    $ cd ~/.emacs.d/elpa/magit-N
    $ emacs -Q --debug-init --eval '(setq debug-on-error t)' -L ../dash-N -L ../git-commit-N -L ../transient-N -L ../with-editor-N -L ../ghub-N -L . -l magit

More debugging tools are described in the manual.

    https://magit.vc/manual/magit/Debugging-Tools.html

---- now delete this line and everything above ----
