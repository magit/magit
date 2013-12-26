[![Build Status](http://img.shields.io/travis-ci/magit/magit.png)](https://travis-ci.org/magit/magit)
[![Gittip](http://img.shields.io/gittip/magit.png)](https://www.gittip.com/magit)

It's Magit!  An Emacs mode for Git
==================================

Magit is an interface to the version control system [Git][git],
implemented as an [Emacs][emacs] extension.

Unlike Emacs' native [Version Control][vc] package which strives to
provide a unified interface to various version control systems, Magit
only supports Git and can therefore better take advantage of its native
features.

Magit supports GNU Emacs 23.2 or later; 24.1 or later is recommended.
Magit supports Git 1.7.2.5 or later; 1.8.2 or later is recommended.
The minimal versions are those available in Debian oldstable.

Table of Contents
=================

* [Getting Started](#getting-started)
* [Getting Help](#getting-help)
* [Contributions](#contributions)
* [Installation](#installation)
  * [Installing from Melpa](#installing-from-melpa)
  * [Installing from Marmalade](#installing-from-marmalade)
  * [Installing from Git](#installing-from-git)
  * [Installing from Tarball](#installing-from-tarball)
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

We can also strongly recommend [this][mastering-intro] introduction
from the Mastering Emacs blog.  It even describes some new features
that are not yet documented in the manual.

Magit also has a [website][website].

Getting Help
============

When something breaks *please* see the
[curated list of known issues][knownissues] and the [FAQ][faq].
If that doesn't help check the list of [all open issues][issues].

If everything else fails please open a [new issue][issues] or ask for
help on the [mailing list][group].

Contributions
=============

Magit is [hosted on Github][development].  Please contribute by
suggesting features on the [issue tracker][issues] or by making code
contributions using [pull requests][pulls].  Before opening a pull
request make sure to read the brief [guidelines][contributing].

Please also consider supporting development using [gittip][gittip].
Thank you!

Magit was started by [Marius Vollmer][marius] and is now maintained
by [Jonas Bernoulli][jonas].  Other Magitians (former maintainers)
are [Nicolas Dudebout][nicolas], [Peter J. Weisberg][peter],
[Phil Jackson][phil], [Rémi Vanicat][remi], and [Yann Hodique][yann].

Many more people have [contributed code][contributors] and suggested
features.

Thanks to all of you, may (the history of) the source be with you!

Installation
============

Beginning with version 24.1 Emacs includes a package management
facility known as Elpa or `package.el`.  Using an Elpa package
repository is the easiest and recommended way to install and update
Magit and its dependencies.  Among other things using `package.el`
is recommended because that automatically takes care of installing
dependencies.

Magit is available from both of the two popular Elpa repositories,
[Marmalade][marmalade] (stable releases) and [Melpa][melpa]
(snapshots).

### Installing from Melpa

If you have already used Melpa to install some other package then
all you have to do is:

<kbd>M-x package-install RET magit RET</kbd>

This installs Magit as well as all of its dependencies and makes
them available in the current and future Emacs sessions.

If this is the first time you are using Melpa, then you have to
configure `package.el` once.

```lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
```

Then evaluate these forms, update the package cache, and install
Magit as above.  To update the cache use:

<kbd>M-x package-refresh-contents RET</kbd>

You might also want to have a look at the more detailed
[instructions][melpa-intro] provided by the Melpa project.  Among
other things it explains how to install only some packages from Melpa
and others from Marmalade, and how to use `package.el` with older
versions of Emacs.

### Installing from Marmalade

For the time being we recommend that you install the development
version available from Melpa, because the latest Magit release (which
is what you get from Marmalade) is very outdated.  If you are using
the development version of Emacs, then you have to do so, because it
contains an incompatible change that breaks the last Magit release.

### Installing from Git

If you want to contribute to Magit you should run it directly from the
Git repository.

First get the repository:

```sh
$ git clone git://github.com/magit/magit.git
```

Then you should byte compile the libraries and generate the
documentation, though that is not required:

```sh
$ make lisp docs
```

Unless all dependencies are installed at `../DEPENDENCY` you have to
tell `make` where to find them, e.g.:

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

### Installing from Tarball

This is only intended for users who have been doing this sort of thing
for years.  Installing from a tarball isn't particularly difficult but
because we are only providing this as an alternative method we are a
bit light on documentation, so it helps to have done this before.

Also most steps have to be repeated every time you want to update.

Because the latest Magit release is very outdated, please consider
installing the development version even if tarballs are your thing.

Download and unpack [magit-1.2.0.tar.gz][download]. Then build and
install as usual:

```sh
$ wget https://github.com/downloads/magit/magit/magit-1.2.0.tar.gz
$ tar -xf magit-1.2.0.tar.gz
$ cd magit-1.2.0
$ make
$ sudo make install
```

This installs the Emacs lisp libraries, as well as the prebuilt
documentation from the tarball.  You may alternatively build the
documentation yourself:

```sh
$ make docs
$ sudo make install-docs
```

By default the Emacs lisp libraries are installed in
`/usr/local/share/emacs/site-lisp/magit/`.  Unless Emacs itself is
also installed in `/usr/local/` you have to add that directory to the
`load-path`.

```lisp
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/magit")
```

Then `magit` can be loaded:

```lisp
(require 'magit)
```

Add the above lines to your init file and restart Emacs.

Dependencies
============

If you install Magit using `package.el` then dependencies are
automatically being taken care of.  Otherwise you have to track down
dependencies and install them manually.

* `cl-lib` is a new library in Emacs 24.3.  Like the old `cl` it
  provides various Common Lisp forms, but differs in that symbols are
  prefixed with `cl-`.  A forward compatibility `cl-lib` for older
  versions of Emacs is available from the GNU Elpa repository.  You
  can install it using `package.el` or get it [here][cl-lib].

* `git-commit-mode` which is part of the [git-modes][git-modes]
  repository and available as a separate package from Melpa.

* `git-rebase-mode` which is part of the [git-modes][git-modes]
  repository and available as a separate package from Melpa.

Optional Dependencies
---------------------

The following libraries build on third-party tools or git subcommands
that are not installed by the Git base-package on some distributions:

* `magit-stgit.el` requires [`stgit`][stgit].
* `magit-svn.el` requires the official Git subcommand `svn`.
* `magit-topgit.el` requires [`topgit`][topgit].
* `magit-wip.el` requires [`git-wip`][git-wip].

Dependencies of Tests
---------------------

To run tests the following libraries are also required:

* `ert` is a tool for automated testing.  It is part of Emacs
  starting with version 24.1.  You can also obtain an old version from
  the former development [repository][ert].


[contributing]: https://github.com/magit/magit/blob/master/CONTRIBUTING.md
[contributors]: https://github.com/magit/magit/contributors
[development]: http://github.com/magit/magit
[download]: https://github.com/downloads/magit/magit/magit-1.2.0.tar.gz
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
[emacs]: http://www.gnu.org/software/emacs
[ert]: https://github.com/ohler/ert
[git-wip]: https://github.com/bartman/git-wip
[git]: http://git-scm.com
[gitflow]: https://github.com/nvie/gitflow
[gittip]: https://www.gittip.com/magit
[git-modes]: https://github.com/magit/git-modes
[marmalade]: http://marmalade-repo.org
[mastering-intro]: http://www.masteringemacs.org/articles/2013/12/06/introduction-magit-emacs-mode-git
[melpa]: http://melpa.milkbox.net
[melpa-intro]: http://melpa.milkbox.net/#/getting-started
[stgit]: http://www.procode.org/stgit
[topgit]: https://github.com/greenrd/topgit
[vc]: http://www.gnu.org/software/emacs/manual/html_node/emacs/Version-Control.html
