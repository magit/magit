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

First tell `package.el` to use one of the package repository:

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

Release `1.2.0` and branch `maint` (stable)
-------------------------------------------

This release/branch only depends on libraries that are built into
Emacs.

Branch `master` (development)
-----------------------------

* `cl-lib` is a new library in Emacs 24.3.  Like the old `cl` it
  provides various Common Lisp forms, but differs in that symbols are
  prefixed with `cl-`.  A forward compatibility `cl-lib` for older
  versions of Emacs is available from the GNU Elpa repository.  You
  can install it using `package.el` or get it [here][cl-lib].

* `contrib/magit-inotify.el` requires `inotify` with is only available
  from Emacs' `trunk` branch; it will be released with Emacs 24.4.

* `magit-wip.el` requires the `git-wip` shell script available
  [here][git-wip].

Branch `next` (experimental)
----------------------------

Same as for `master`.

Running tests
-------------

To run tests the following libraries are also required:

* `ert` is a tool for automated testing.  It is part of Emacs
  starting with version 24.1.  You can also obtain an old version from
  the former development [repository][ert].

* [`mocker`][mocker] a mocking framework

* [`el-x`][el-x] various utilities (required by mocker)

Installing from Git
===================

If you want to live on the bleeding edge or just aren't that much into
`package.el` the next best thing is to get Magit from Git.  This is
also necessary if you want to contribute to Magit.

    git clone git://github.com/magit/magit.git

You then have the choice between the `maint` (stable), `master`
(development), and `next` (experimental) branches.  The latter two
depend on third-party libraries, which have to be properly installed
for the following to succeed.

Start by adding the following lines to your init file:

```lisp
(add-to-list 'load-path "/path/to/magit-repo")
(require 'magit)
```

If you intend to use contributed libraries also add this:

```lisp
(add-to-list 'load-path "/path/to/magit-repo/contrib")
```

To finish the installation you can use `make` as explained in the
tarball section below.  The only difference is that you have to skip
the final `make install` step, since we already added the repository
itself to the `load-path`.

Or you could just do it manually, by running something like this:

    $ emacs -Q -batch -L . -L ../DEPENDENCY -b batch-byte-compile *.el
    $ makeinfo -o magit.info magit.texi
    $ ginstall-info --dir=dir magit.info

and adding this to your initialization file:

```lisp
(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list "/path/to/magit-repo/")))
```

Installing from Tarball
=======================

Download and unpack [magit-1.2.0.tar.gz][download]. Then build and
install as usual:

    $ wget https://github.com/downloads/magit/magit/magit-1.2.0.tar.gz
    $ tar -xf magit-1.2.0.tar.gz
	$ cd magit-1.2.0
    $ make
    $ sudo make install

This requires the `makeinfo` binary (usually available from a package
named `texinfo`).  If you don't have it you can skip creating the info
page by using this instead:

    $ make core
    $ sudo make install_core

You might also want to compile and install the additional libraries in
`contrib/`:

    $ make contrib
    $ sudo make install_contrib

In either case Emacs lisp libraries are put into
`/usr/local/share/emacs/site-lisp/`, where Emacs should be able to
find them.  Finally add this to your initialization file.

    (require 'magit)


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
[el-x]: https://github.com/sigma/el-x
[emacs]: http://www.gnu.org/software/emacs
[ert]: https://github.com/ohler/ert
[git-wip]: https://github.com/bartman/git-wip
[git]: http://git-scm.com
[marmalade]: http://marmalade-repo.org
[melpa]: http://melpa.milkbox.net
[mocker]: https://github.com/sigma/mocker.el
[vc]: http://www.gnu.org/software/emacs/manual/html_node/emacs/Version-Control.html
