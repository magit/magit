<a href="#readme"></a>
<p align="center"><img src="http://magit.vc/img/magit.png"/></p>
<h1 align="center">It's Magit!</h1>
<h2 align="center">A Git Porcelain inside Emacs</h2>
<p align="center">
  <a href="http://magit.vc"><b>homepage</b></a> |
  <a href="http://magit.vc/manual"><b>manual</b></a> |
  <a href="http://magit.vc/manual/magit/FAQ.html"><b>faq</b></a> |
  <a href="https://github.com/magit/magit/wiki"><b>wiki</b></a> |
  <a href="https://groups.google.com/forum/?fromgroups#!forum/magit"><b>mailing list</b></a> |
  <a href="http://emacs.stackexchange.com/questions/tagged/magit"><b>forum</b></a> |
  <a href="https://gitter.im/magit/magit"><b>chat</b></a> |
  <a href="https://twitter.com/magit_emacs"><b>news</b></a>
</p>

***
<p align="justify">
  Magit is an interface to the version control system
  <a href="http://git-scm.com">Git</a>, implemented as an
  <a href="http://www.gnu.org/software/emacs">Emacs</a> package.
  Magit aspires to be a complete Git porcelain.  While we cannot
  (yet) claim, that Magit wraps and improves upon each and every Git
  command, it is complete enough to allow even experienced Git users
  to perform almost all of their daily version control tasks directly
  from within Emacs.  While many fine Git clients exist, only Magit
  and Git itself deserve to be called porcelains.
</p>
***

<p align="center">
  Please consider supporting development by making a
  <a href="http://magit.vc/donations.html">donation</a>.
</p>
***

Magit 2.1.0 will be released on the 1th of July
===============================================

- To install the soon to be obsolete `1.4.2` release, follow
  [these](https://github.com/magit/magit/wiki/Installation) old
  instructions.

- To install the `2.0.50` pre-release now, follow [these][install]
  instructions.  Note that until `2.1.0` has been released you have
  to install directly from the development repository.

- If you are the maintainer of a package that depends on Magit, then
  please start using `2.0.50` now and try to adapt your package before
  the 1th of July.  Thanks.

- For more information see [this][roadmap] issue.

Before you install the new version (regardless of whether you install
the `2.0.50` pre-release now, or the `2.1.0` release once that has
arrived) you have to **first uninstall `1.4.0`**.  See [this][update]
page for more information.

Support
=======

**When something doesn't work as expected then please first see the
[FAQ][faq].** Then also try the list of [open issues][issues] and use
the search box at the top of that page to find older related issues.
You should also consult the [manual][manual] and ask a general-purpose
search engine.

If everything else fails then ask for help on the
**[Emacs Stackexchange site][forum]**, the
[mailing list][list], or the
[Gitter chat][chat].

**Please do NOT use the Github issue tracker for support requests.**
**We only use it for feature requests and bug reports.**

Contributing
============

To report bugs and suggest new feature use the
[issue tracker][issues].  If you have some code which you would like
to be merged, then open a [pull requests][pulls]. Please also see
[CONTRIBUTING.md][contrib].

Acknowledgments
===============

Magit was started by [Marius Vollmer][marius] and is now maintained
by [Jonas Bernoulli][jonas].  Other former maintainers are
[Nicolas Dudebout][nicolas], [Peter J. Weisberg][peter],
[Phil Jackson][phil], [Rémi Vanicat][remi], and [Yann Hodique][yann].
Many more people have [contributed code][contributors] and suggested
features.

Thanks to all of you, may (the history of) the source be with you!

***
[![Build Status](https://travis-ci.org/magit/magit.svg?branch=master)](https://travis-ci.org/magit/magit)


[contrib]: https://github.com/magit/magit/blob/next/CONTRIBUTING.md
[issues]:  https://github.com/magit/magit/issues
[pulls]:   https://github.com/magit/magit/pulls
[roadmap]: https://github.com/magit/magit/issues/1645

[contributors]: http://magit.vc/stats/authors.html
[donations]:    http://magit.vc/donations.html
[faq]:          http://magit.vc/manual/magit/FAQ.html
[install]:      http://magit.vc/manual/magit/Installation.html
[manual]:       http://magit.vc/manual
[update]:       http://magit.vc/manual/magit/Updating-from-an-older-release.html

[forum]: http://emacs.stackexchange.com/questions/tagged/magit
[chat]:  https://gitter.im/magit/magit
[list]:  https://groups.google.com/forum/?fromgroups#!forum/magit

[emacs]: http://www.gnu.org/software/emacs
[git]:   http://git-scm.com

[jonas]:   http://emacsair.me
[marius]:  https://github.com/mvollmer
[nicolas]: http://dudebout.com
[peter]:   https://github.com/pjweisberg
[phil]:    https://github.com/philjackson
[remi]:    https://github.com/vanicat
[yann]:    http://www.hodique.info
