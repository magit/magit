[![Build Status](https://travis-ci.org/magit/magit.svg?branch=master)](https://travis-ci.org/magit/magit)
[![Gratipay](http://img.shields.io/gratipay/magit.png)](https://www.gratipay.com/magit)

It's Magit!  An Emacs mode for Git
==================================

Magit is an interface to the version control system [Git][git],
implemented as an [Emacs][emacs] extension.

Unlike the [Version Control][vc] package which is part of Emacs and
strives to provide a unified interface to various version control
systems, Magit only supports Git and can therefore better take
advantage of its native features.

Magit requires at least GNU Emacs 24.1 and Git 1.7.2.5.

Installation
============

You are looking at the `README.md` of the development branch.  For
instructions on how to install the stable version of Magit see the
respective [`README.md`][development] instead.

First **uninstall** all instances of `magit`, `git-commit-mode` and
`git-rebase-mode` that you have currently installed.  Doing this,
and doing it carefully (e.g. did you also install Magit using your
distributions package manager at some point?), is **very important**.
It only takes a few minutes to do this, but if everyone does it, then
that will save me hours of investigating "impossible" issues, which I
cannot actually reproduce myself.

Then install package `dash` using `install-package`.  This is now
the only external dependency; the libraries `git-commit.el` and
`git-rebase.el` are now part of the Magit repository.  (These
libraries were previously part of the Git-Modes repository and their
names used to end with `-mode.el`).

Then clone the Magit repository and compile the libraries:

    $ git clone git://github.com/magit/magit.git ~/.emacs.d/site-lisp/magit
    $ cd ~/.emacs.d/site-lisp/magit
    $ make lisp

Finally add this to you init file:

    (add-to-list 'load-path "~/emacs.d/site-lisp/magit")
    (require 'magit)

To update Magit use:

    $ git pull
    $ make clean lisp

To view all available targets use `make help`.

Getting Help
============

When something breaks please see the [FAQ][faq].  If that doesn't help
check the list of [all open issues][issues].

If everything else fails please open a [new issue][issues] or ask for
help on the [mailing list][group].

Contributions
=============

Magit is [hosted on Github][development].  Please contribute by
suggesting features on the [issue tracker][issues] or by making code
contributions using [pull requests][pulls].  Before opening a pull
request make sure to read the brief [guidelines][contributing].

Please also consider supporting development using
[gratipay][gratipay].  Thank you!

Magit was started by [Marius Vollmer][marius] and is now maintained
by [Jonas Bernoulli][jonas].  Other Magitians (former maintainers)
are [Nicolas Dudebout][nicolas], [Peter J. Weisberg][peter],
[Phil Jackson][phil], [Rémi Vanicat][remi], and [Yann Hodique][yann].

Many more people have [contributed code][contributors] and suggested
features.

Thanks to all of you, may (the history of) the source be with you!


[contributing]: https://github.com/magit/magit/blob/master/CONTRIBUTING.md
[contributors]: https://github.com/magit/magit/contributors
[development]: http://github.com/magit/magit
[download]: https://github.com/magit/magit/releases/download/1.2.1/magit-1.2.1.tar.gz
[faq]: https://github.com/magit/magit/wiki/FAQ
[group]: https://groups.google.com/forum/?fromgroups#!forum/magit
[issues]: https://github.com/magit/magit/issues
[knownissues]: https://github.com/magit/magit/wiki/Known-issues
[manual]: http://magit.github.io/documentation.html
[owners]: https://github.com/magit?tab=members
[pulls]: https://github.com/magit/magit/pulls
[screencast]: http://vimeo.com/2871241
[website]: http://magit.github.io

[jonas]: https://github.com/tarsius
[marius]: https://github.com/mvollmer
[nicolas]: https://github.com/dudebout
[peter]: https://github.com/pjweisberg
[phil]: https://github.com/philjackson
[remi]: https://github.com/vanicat
[yann]: https://github.com/sigma

[cl-lib]: http://elpa.gnu.org/packages/cl-lib.html
[dash]: https://github.com/magnars/dash.el
[emacs]: http://www.gnu.org/software/emacs
[ert]: https://github.com/ohler/ert
[git-wip]: https://github.com/bartman/git-wip
[git]: http://git-scm.com
[gitflow]: https://github.com/nvie/gitflow
[gnuelpa]: https://elpa.gnu.org
[gratipay]: https://gratipay.com/magit
[git-modes]: https://github.com/magit/git-modes
[marmalade]: http://marmalade-repo.org
[mastering-intro]: http://www.masteringemacs.org/article/introduction-magit-emacs-mode-git
[melpa]: http://melpa.milkbox.net
[melpa-intro]: http://melpa.milkbox.net/#/getting-started
[stgit]: http://www.procode.org/stgit
[topgit]: https://github.com/greenrd/topgit
[vc]: http://www.gnu.org/software/emacs/manual/html_node/emacs/Version-Control.html
