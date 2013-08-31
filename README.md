[![Build Status](https://travis-ci.org/magit/magit.png?branch=maint,master,next)](https://travis-ci.org/magit/magit)

It's Magit!  An Emacs mode for Git
==================================

Magit is an interface to [Git][git] for [Emacs][emacs].

Unlike Emacs's native [version control support][vc] which strives to
provide a unified interface to various version control systems, Magit
only supports Git and can therefor take full advantage of its native
features.

Magit supports GNU Emacs 23.2 or later; 24.1 or later is recommended.
Magit supports Git 1.7.2.5 or later; 1.8.2 or later is recommended.
These are the versions in ancient Debian oldstable.

Getting Help
============

When something breaks please see the
[curated list of known issues][knownissues] and the [FAQ][faq].  If
that doesn't help check the list of [all open issues][issues].

If everything else fails please open a [new issue][issues] or ask for
help on the [mailing list][group].

Getting Started
===============

To get started with Magit, run <kbd>M-x magit-status</kbd>.  If you
are inside a Git repository this opens a buffer that summarizes its
status.  Otherwise you are first prompted for a repository.  Read the
short help for `magit-mode` (<kbd>C-h m</kbd> in the status buffer),
make some changes to your files, then stage (<kbd>s</kbd>) and commit
(<kbd>c</kbd>) them.

For more details consult the Magit user manual.  You can read it
[on the web][manual] or in Emacs with <kbd>C-u C-h i magit.info</kbd>.

Magit also has a [website][website].

Installation
============

Emacs >=24.1 includes a facility that lets you easily download and
install packages.  Using `package.el` is the easiest and recommended
way to install Magit and its dependencies.  If you don't want to use
it see the extended installation instructions in
[INSTALL.md][installing].

The stable Magit version is available from the [Marmalade][marmalade]
package repository.  If you want to install the development version
(the `master` branch) use the [Melpa][melpa] repository instead.
Please note that *all* packages on Melpa are built from the upstream
`master` branch.  If you generally want stable versions but the latest
Magit use Marmalade and install Magit from Git.

First tell `package.el` to use one of the package repositories:

```lisp
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
```

**or**

```lisp
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
```

For details please see the website of the package repository of your
choosing.

Then install Magit (and its run-time dependencies):

<kbd>M-x package-install RET magit RET</kbd>

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

Some Magit libraries depend on third-party tools or optional git
subcommands.  See [INSTALL.md][installing] for more information.

Development
===========

Magit's canonical source repository is
[hosted on Github][development].

Magit was started by Marius Vollmer and is now collectively maintained
by the [Magit Owners Team][owners].  [Many more people][contributors]
have contributed.

To report bugs and make feature requests please use the
[issue tracker][issues] and Github [pull requests][pulls].  You may
also use Magit's [Google group][group].  Before making a pull request
please read [CONTRIBUTING.md][contributing].


[contributing]: https://github.com/magit/magit/blob/maint/CONTRIBUTING.md
[contributors]: https://github.com/magit/magit/contributors
[development]: http://github.com/magit/magit
[download]: https://github.com/downloads/magit/magit/magit-1.2.0.tar.gz
[faq]: https://github.com/magit/magit/wiki/FAQ
[group]: https://groups.google.com/forum/?fromgroups#!forum/magit
[installing]: https://github.com/magit/magit/blob/maint/INSTALL.md
[issues]: https://github.com/magit/magit/issues
[knownissues]: https://github.com/magit/magit/wiki/Known-issues
[manual]: http://magit.github.com/magit/magit.html
[owners]: https://github.com/magit?tab=members
[pulls]: https://github.com/magit/magit/pulls
[screencast]: http://vimeo.com/2871241
[website]: http://magit.github.com/magit

[cl-lib]: http://elpa.gnu.org/packages/cl-lib.html
[emacs]: http://www.gnu.org/software/emacs
[ert]: https://github.com/ohler/ert
[git-wip]: https://github.com/bartman/git-wip
[git]: http://git-scm.com
[marmalade]: http://marmalade-repo.org
[melpa]: http://melpa.milkbox.net
[vc]: http://www.gnu.org/software/emacs/manual/html_node/emacs/Version-Control.html
