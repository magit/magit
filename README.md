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

### Table of Contents

* [Getting Started](#getting-started)
* [Getting Help](#getting-help)
* [Contributions](#contributions)
* [Installation](#installation)
* [Dependencies](#dependencies)

Getting Started
===============

To get started with Magit, run <kbd>M-x magit-status</kbd>.  If you
are inside a Git repository this opens a buffer that summarizes its
status.  Otherwise you are first prompted for a repository.  Read the
short help for `magit-status-mode` (<kbd>C-h m</kbd> in the status
buffer).

Then edit and save some files, refresh the status buffer
(<kbd>g</kbd>), stage changes (<kbd>s</kbd>) and commit (<kbd>c</kbd>)
them.

For more details consult the Magit user manual.  You can read it with
<kbd>C-u C-h i magit.info</kbd> or [on the web][manual].

We can also recommend [this][mastering-intro] introduction from the
Mastering Emacs blog.  It even describes some new features that are
not yet documented in the manual.

Magit also has a [website][website].

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

Installation
============

You are looking at the `README.md` of the development branch.  For
instructions on how to install the stable version of Magit see the
respective [readme][development] instead.

First install the third-party `dash` using `install-package`.

Then get the Git-Modes and Magit repositories:

```sh
$ cd /path/to/common/parent
$ git clone git://github.com/magit/git-modes.git
$ git clone git://github.com/magit/magit.git
```

Then, in each of these repositories, byte compile the libraries
and generate the documentation, though that is not required:

```sh
$ make lisp docs
```

If you installed the Git-Modes repository to a different location,
the tell `make` about it when compiling Magit:

```sh
$ EFLAGS="-L /path/to/git-modes" make lisp docs
```

Then add this to you init file:

```lisp
(add-to-list 'load-path "/path/to/git-modes")
(add-to-list 'load-path "/path/to/magit")
(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list "/path/to/magit/")))
(require 'magit)
```

If you are using an Emacs version before 24.3, then you also have to
install `cl-lib` and tell `make` as well as Emacs where to find it.

To view available make targets use:

```sh
$ make help
```

To update use:

```sh
$ git pull
$ make lisp docs
```

Before creating a pull request always run:

```sh
$ make lisp test
```

You may also build Magit manually:

```sh
$ emacs -Q --batch -L . -L /path/to/DEPENCENCY -f batch-byte-compile *.el
$ makeinfo -o magit.info magit.texi
$ install-info --dir=dir magit.info
```

Dependencies
============

Magit requires at least GNU Emacs 24.1 and Git 1.7.2.5.

If you install Magit using `install-package` then dependencies are
automatically being taken care of.  Otherwise you have to track down
dependencies and install them manually.

In Emacs 24.3 the old `cl` library was replaced with `cl-lib`.  They
both provide various Common Lisp forms, but the new library prefixes
symbols with `cl-`.  Magit requires the latter library, but luckily a
forward compatibility implementation of `cl-lib` for older Emacs
releases is available from the [GNU Elpa][gnuelpa] repository.  You
can install it using `install-package` or get it [here][cl-lib].

The library `dash` makes writing Emacs lisp more pleasant, and so
Magit uses it.  It is available from Melpa, its repository can be found
[here][dash].

The libraries `git-commit-mode`, `git-rebase-mode` and `with-editor`
are developed in parallel to Magit, but because they can be used
without Magit are located in their own [git-modes][git-modes].  While
they don't need Magit, Magit does need them.  Each of these libraries
is available from Melpa as a separate package by the same name.

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
[mastering-intro]: http://www.masteringemacs.org/articles/2013/12/06/introduction-magit-emacs-mode-git
[melpa]: http://melpa.milkbox.net
[melpa-intro]: http://melpa.milkbox.net/#/getting-started
[stgit]: http://www.procode.org/stgit
[topgit]: https://github.com/greenrd/topgit
[vc]: http://www.gnu.org/software/emacs/manual/html_node/emacs/Version-Control.html
