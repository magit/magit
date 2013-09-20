Installing with `package.el`
============================

Emacs >=24.1 includes a facility that lets you easily download and
install packages.  Using `package.el` is the easiest and recommended
way to install Magit and its dependencies.

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

<kbd>M-x install-package RET magit RET</kbd>

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

Installing from Git
===================

If you want to contribute to Magit you should run it directly from the
git repository.

First get the repository:

    $ git clone git://github.com/magit/magit.git

You then have the choice between the `maint` (stable), `master`
(development), and `next` (experimental) branches.  Unless you have
chosen `master` you need to create and checkout the branch.

    $ git checkout -b LOCAL-BRANCH REMOTE-BRANCH

Then you should byte compile the libraries and generate the
documentation, though that is not required.

    $ make lisp docs

You can also do so manually.

    $ emacs -Q --batch -L . [-L ../path/to/cl-lib] -f batch-byte-compile *.el
    $ makeinfo -o magit.info magit.texi
    $ install-info --dir=dir magit.info

Then add this to you init file:

```lisp
(add-to-list 'load-path "/path/to/magit")
(require 'magit)
```

And optionally tell `info` about the documentation:

```lisp
(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list "/path/to/magit/")))
```

For a list of all make targets see:

    $ make help

Installing from Tarball
=======================

Please consider using `package.el` instead.  Still here? Download and
unpack [magit-1.3.0.tar.gz][download]. Then build and install as usual:

    $ wget https://github.com/downloads/magit/magit/magit-1.3.0.tar.gz
    $ tar -xf magit-1.3.0.tar.gz
	$ cd magit-1.3.0
    $ make
    $ sudo make install

This installs the Emacs lisp libraries, as well as the prebuilt
documentation from the tarball.  You may alternatively build the
documentation yourself:

    $ make docs
    $ sudo make install-docs

The `magit` shell script can be installed using:

    $ sudo make install-script

For a list of all make targets see:

    $ make help

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

Add the above lines to your init file (`~/.emacs.el` or
`~/.emacs.d/init.el`) and restart Emacs.


[contributing]: https://github.com/magit/magit/blob/maint/CONTRIBUTING.md
[contributors]: https://github.com/magit/magit/contributors
[development]: http://github.com/magit/magit
[download]: https://github.com/downloads/magit/magit/magit-1.2.0.tar.gz
[group]: https://groups.google.com/forum/?fromgroups#!forum/magit
[installing]: https://github.com/magit/magit/blob/maint/INSTALL.md
[issues]: https://github.com/magit/magit/issues
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
[gitflow]: https://github.com/nvie/gitflow
[git-modes]: https://github.com/magit/git-modes
[marmalade]: http://marmalade-repo.org
[melpa]: http://melpa.milkbox.net
[stgit]: http://www.procode.org/stgit
[topgit]: https://github.com/greenrd/topgit
[vc]: http://www.gnu.org/software/emacs/manual/html_node/emacs/Version-Control.html
