[![Build Status](https://travis-ci.org/magit/magit.svg?branch=master)](https://travis-ci.org/magit/magit)
[![Gratipay](http://img.shields.io/gratipay/magit.png)](https://www.gratipay.com/magit)

It's Magit!  An Emacs mode for Git
==================================

**The Magit wiki contains a list of [FREQUENTLY ASKED QUESTIONS][faq],
please do consult it.**

**Magit is in [FEATURE FREEZE][roadmap], keep that in mind when making
feature requests.**

Magit is an interface to the version control system [Git][git],
implemented as an [Emacs][emacs] extension.

Unlike the [VC][vc] package which is part of Emacs and strives to
provide a unified interface to various version control systems, Magit
only supports Git and can therefore better take advantage of its
native features.

Magit requires at least GNU Emacs 24.1 and Git 1.9.1.

Installation
============

You are looking at the `README.md` of the development branch.  For
instructions on how to install the stable version of Magit see the
respective [`README.md`][magit] instead.  Also see
[Which version should I install?][faq-which]

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

When something breaks please see the [FAQ][faq].  If that doesn't help
check the list of [all open issues][issues].

If everything else fails please open a [new issue][issues] or ask for
help on the [mailing list][group].

Contributions
=============

Magit is [hosted on Github][magit].  Please contribute by suggesting
features on the [issue tracker][issues] or by making code
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

[magit]:        https://github.com/magit/magit
[contributing]: https://github.com/magit/magit/blob/next/CONTRIBUTING.md
[contributors]: https://github.com/magit/magit/contributors
[faq]:          https://github.com/magit/magit/wiki/FAQ
[faq-which]:    https://github.com/magit/magit/wiki/FAQ#which-version-should-i-install
[issues]:       https://github.com/magit/magit/issues
[pulls]:        https://github.com/magit/magit/pulls
[roadmap]:      https://github.com/magit/magit/issues/1645

[group]:    https://groups.google.com/forum/?fromgroups#!forum/magit
[gratipay]: https://gratipay.com/magit

[emacs]: http://www.gnu.org/software/emacs
[git]:   http://git-scm.com
[vc]:    http://www.gnu.org/software/emacs/manual/html_node/emacs/Version-Control.html

[jonas]:   https://github.com/tarsius
[marius]:  https://github.com/mvollmer
[nicolas]: https://github.com/dudebout
[peter]:   https://github.com/pjweisberg
[phil]:    https://github.com/philjackson
[remi]:    https://github.com/vanicat
[yann]:    https://github.com/sigma
