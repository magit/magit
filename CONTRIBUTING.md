How to contribute
=================

Contributions to Magit are highly welcome.  Here are a few guidelines
that will help your patches hit upstream as soon as possible.

Making changes
--------------

Please make sure your commits are well-organized and "atomic" (hitting
a single well-defined target each).

Please also make sure you check that byte-compilation completes
without errors or warnings, and that tests run without failures.

First you will have to add dependencies of magit `git-rebase-mode` and `git-commit-mode` in
your working directory.

    $ curl https://raw.github.com/magit/git-modes/master/git-commit-mode.el -o git-commit-mode.el
    $ curl https://raw.github.com/magit/git-modes/master/git-rebase-mode.el -o git-rebase-mode.e

After that run

    $ make lisp test

Bonus points if you add tests to cover the feature you're hacking.

Submitting changes
------------------

Please submit your changes by opening a pull request on Magit's Github
repository at https://github.com/magit/magit/pulls.
