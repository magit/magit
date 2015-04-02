It's Magit!  A Git Porcelain inside Emacs
=========================================

Magit is an interface to the version control system [Git][git],
implemented as an [Emacs][emacs] package.

Unlike the [VC][vc] package which is part of Emacs and strives to
provide a unified interface to various version control systems, Magit
only supports Git and can therefore better take advantage of its
native features.

*Magit supports GNU Emacs 23.2 or later; 24.1 or later is recommended.*
*Magit supports Git 1.7.2.5 or later; 1.8.2 or later is recommended.*

### Table of Contents

* [Installation](#installation)
* [Getting Started](#getting-started)
* [Getting Help](#getting-help)
* [Contributions](#contributions)
* [Dependencies](#dependencies)

Installation
============

We recommend that you use Emacs' package manager (`package.el`),
to install Magit from your preferred Elpa archive ([Melpa][melpa],
[Melpa-Stable][melpa-stable], or [Marmalade][marmalade]).  If you
haven't used the package manager before, or would like to install
directly from Magit's Git repository, then consult the detailed
[installation instructions][install].

Getting Started
===============

To get started with Magit show a summary of its status in a
new buffer using <kbd>M-x magit-status RET</kbd>.  Navigate the
buffer using <kbd>n</kbd> and <kbd>p</kbd>, and toggle sections using
<kbd>TAB</kbd>.  Edit and save some files, refresh the status buffer
using <kbd>g</kbd>, stage files or individual hunks using <kbd>s</kbd>
and initiate a commit using <kbd>c c</kbd> the staged changes.  When
done writing the commit message use <kbd>C-c C-c</kbd> to actually
create the commit.

For more details consult the user manual.  You can read it with
<kbd>C-u C-h i magit.info RET</kbd> or [on the web][manual].  Magit
also has a [website][website].

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

Many more people have [contributed code][stats-authors] and suggested
features.

Thanks to all of you, may (the history of) the source be with you!

Dependencies
============

If you install Magit using Emacs' package manager then dependencies
are automatically being taken care of.  Otherwise you have to track
down dependencies and install them manually.

Magit requires the libraries `git-commit-mode` and `git-rebase-mode`
which are part of the [magit/git-modes][git-modes] repository and are
distributed as separate packages.

The library `magit-wip` additionally requires the [`git-wip`][git-wip]
script, which you have to install manually.


[contributing]: https://github.com/magit/magit/blob/master/CONTRIBUTING.md
[development]: http://github.com/magit/magit
[donations]: http://magit.vc/donations.html
[emacsstack]: http://emacs.stackexchange.com/questions/tagged/magit
[faq]: https://github.com/magit/magit/wiki/FAQ
[group]: https://groups.google.com/forum/?fromgroups#!forum/magit
[install]: https://github.com/magit/magit/wiki/Installation
[issues]: https://github.com/magit/magit/issues
[manual]: http://magit.vc/manual/master
[pulls]: https://github.com/magit/magit/pulls
[stats-authors]: http://magit.vc/stats/authors.html
[website]: http://magit.vc

[jonas]: http://emacsair.me
[marius]: https://github.com/mvollmer
[nicolas]: http://dudebout.com
[peter]: https://github.com/pjweisberg
[phil]: https://github.com/philjackson
[remi]: https://github.com/vanicat
[yann]: http://www.hodique.info

[emacs]: http://www.gnu.org/software/emacs
[git-wip]: https://github.com/bartman/git-wip
[git]: http://git-scm.com
[git-modes]: https://github.com/magit/git-modes
[marmalade]: http://marmalade-repo.org
[melpa]: http://melpa.org
[melpa-stable]: http://stable.melpa.org
[vc]: http://www.gnu.org/software/emacs/manual/html_node/emacs/Version-Control.html
