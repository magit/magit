[![Build Status](https://travis-ci.org/magit/magit.svg?branch=master)](https://travis-ci.org/magit/magit)
[![Gratipay](http://img.shields.io/gratipay/magit.png)](https://www.gratipay.com/magit)

It's Magit!  A Git Porcelain inside Emacs
=========================================

**The Magit wiki contains a list of [FREQUENTLY ASKED QUESTIONS][faq],
please do consult it.**

**Magit is in [FEATURE FREEZE][roadmap], keep that in mind when making
feature requests.**

Magit is an interface to the version control system [Git][git],
implemented as an [Emacs][emacs] package.

Unlike the [VC][vc] package which is part of Emacs and strives to
provide a unified interface to various version control systems, Magit
only supports Git and can therefore better take advantage of its
native features.

*Magit 2.0.50 requires at least GNU Emacs 24.2 and Git 1.9.4.*

Installation
============

You are looking at the `README.md` of the development branch.  For
instructions on how to install the stable version of Magit see the
extended [installation instructions][install] instead.

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

Then clone the Magit repository and check out the "next" branch:

    $ git clone git://github.com/magit/magit.git ~/.emacs.d/site-lisp/magit
    $ cd ~/.emacs.d/site-lisp/magit
    $ git checkout next

Then compile the libraries:

    $ make lisp

Finally add this to your init file:

    (add-to-list 'load-path "~/emacs.d/site-lisp/magit")
    (require 'magit)

To update Magit use:

    $ git pull
    $ make clean lisp

To view all available targets use `make help`.

Getting Help
============

When something doesn't work as expected please see the [FAQ][faq], and
the list of [open issues][issues], and use the search box at the top
of the latter to find older related issues.  If everything else fails
ask for help on the [Emacs Stackexchange][emacsstack] site or the
[mailing list][group].

Contributions
=============

Magit is [hosted on Github][development].  Please contribute by
reporting bugs and suggesting features on the [issue tracker][issues]
or by making code contributions using [pull requests][pulls].  Before
opening a pull request read the brief
[contribution guidelines][contributing].

Please also consider supporting development by making a
[monetary donation][donations].  Thank you!

Magit was started by [Marius Vollmer][marius] and is now maintained
by [Jonas Bernoulli][jonas].  Other Magitians (former maintainers)
are [Nicolas Dudebout][nicolas], [Peter J. Weisberg][peter],
[Phil Jackson][phil], [Rémi Vanicat][remi], and [Yann Hodique][yann].

Many more people have [contributed code][contributors] and suggested
features.

Thanks to all of you, may (the history of) the source be with you!


[contributing]: https://github.com/magit/magit/blob/next/CONTRIBUTING.md
[development]:  https://github.com/magit/magit
[faq]:          https://github.com/magit/magit/wiki/FAQ
[install]:      https://github.com/magit/magit/wiki/Installation
[issues]:       https://github.com/magit/magit/issues
[pulls]:        https://github.com/magit/magit/pulls
[roadmap]:      https://github.com/magit/magit/issues/1645

[contributors]: http://magit.vc/stats/authors.html
[donations]:    http://magit.vc/donations.html
[manual]:       http://magit.vc/manual/next

[emacsstack]:   http://emacs.stackexchange.com/questions/tagged/magit
[group]:        https://groups.google.com/forum/?fromgroups#!forum/magit

[emacs]: http://www.gnu.org/software/emacs
[git]:   http://git-scm.com
[vc]:    http://www.gnu.org/software/emacs/manual/html_node/emacs/Version-Control.html

[jonas]:   http://emacsair.me
[marius]:  https://github.com/mvollmer
[nicolas]: http://dudebout.com
[peter]:   https://github.com/pjweisberg
[phil]:    https://github.com/philjackson
[remi]:    https://github.com/vanicat
[yann]:    http://www.hodique.info
