<div align="center"><img src="https://magit.vc/assets/magit-168x200px.png"/></div>
<h2 align="center">A Git Porcelain inside Emacs</h2>
<p align="center">
  <a href="https://magit.vc"><b>homepage</b></a> |
  <a href="https://magit.vc/manual"><b>manual</b></a> |
  <a href="https://magit.vc/manual/magit/FAQ.html"><b>faq</b></a> |
  <a href="https://github.com/magit/magit/wiki"><b>wiki</b></a> |
  <a href="https://groups.google.com/forum/?fromgroups#!forum/magit"><b>mailing list</b></a> |
  <a href="https://emacs.stackexchange.com/questions/tagged/magit"><b>forum</b></a> |
  <a href="https://twitter.com/magit_emacs"><b>news</b></a>
</p>
<hr>

<p align="justify">
  Magit is an interface to the version control system
  <a href="https://git-scm.com">Git</a>, implemented as an
  <a href="https://www.gnu.org/software/emacs">Emacs</a> package.
  Magit aspires to be a complete Git porcelain.  While we cannot
  (yet) claim that Magit wraps and improves upon each and every Git
  command, it is complete enough to allow even experienced Git users
  to perform almost all of their daily version control tasks directly
  from within Emacs.  While many fine Git clients exist, only Magit
  and Git itself deserve to be called porcelains.
</p>
<hr>

<div align="center">
  Keeping its users <a href= "https://magit.vc/quotes">this excited</a> is
  <a href="https://magit.vc/stats/authors.html#cumulated_added_lines_of_code_per_author">
    a lot of work
  </a>.
  If Magit makes you <br> more productive too,
  then <b>please consider making a donation</b>.
</div>
<div align="center">
  <em>Thank you! &mdash; Jonas Bernoulli</em>
</div>
<br>
<div align="center">
  <a href="https://github.com/sponsors/tarsius">
    <img title="Donate using Github Sponsors"
         alt="Donate using Github Sponsors"
         src="https://magit.vc/assets/github-sponsors-50px.png"></a>
  &nbsp;&nbsp;
  <a href="https://magit.vc/donate/#iban">
     <img title="Donate using IBAN"
          alt="Donate using IBAN"
          src="https://magit.vc/assets/iban-50px.png"></a>
  &nbsp;&nbsp;
  <a href="https://magit.vc/donate/#zelle">
     <img title="Donate using Zelle (USA bank transfer)"
          alt="Donate using Zelle (USA bank transfer)"
          src="https://magit.vc/assets/zelle-50px.png"></a>
  &nbsp;&nbsp;
  <a href="https://www.paypal.me/JonasBernoulli/20">
    <img title="Donate using PayPal"
         alt="Donate using PayPal"
         src="https://magit.vc/assets/paypal.png"></a>
</div>
<br>
<div align="center">
  Some alternative donation methods are <a href="https://magit.vc/donate">available</a>.
</div>
<hr>

Getting Started
===============

If you are new to Magit, then either one of the following two
articles should help understanding how it differs from other Git
clients.

- [Visual Magit walk-through](https://emacsair.me/2017/09/01/magit-walk-through)

  If you are completely new to Magit, then this article is a good
  visual introduction.

  Almost everything that you see in Magit can be acted on by pressing
  some key, but that's not obvious from just seeing how Magit looks.
  The screenshots and accompanying text of this article explain how to
  perform a variety of actions on Magit's output.

- [Magit, the magical Git interface](https://emacsair.me/2017/09/01/the-magical-git-interface)

  Magit differs significantly from other Git interfaces, and its
  advantages are not immediately obvious simply from looking at a few
  screenshots as presented in the preceding article.

  This article discusses Magit's properties in somewhat more abstract
  terms.

Support
=======

When something doesn't work as expected then please first see the
[FAQ][faq].  Then also try the list of [open issues][issues] and use
the search box at the top of that page to find older related issues.
You should also consult the [manual][manual] and ask a general-purpose
search engine.

If that doesn't answer your question, then ask for help on the
**[Emacs Stackexchange site][forum]** or the [mailing list][list].
We only use the GitHub issue tracker for feature requests and bug
reports, so please don't ask for help there.

Contributing
============

To report bugs and suggest new feature use the
[issue tracker][issues].  If you have some code which you would like
to be merged, then open a [pull request][pulls]. Please also see
[CONTRIBUTING.md][contrib].

Acknowledgments
===============

Magit was started by [Marius Vollmer][marius], and is now maintained
by [Jonas Bernoulli][jonas], [Kyle Meyer][kyle], and
[Noam Postavsky][noam].  Other former maintainers are
[Nicolas Dudebout][nicolas], [Peter J. Weisberg][peter],
[Phil Jackson][phil], [Rémi Vanicat][remi], and [Yann Hodique][yann].
Many more people have [contributed code][authors] and suggested
features.

Over the years a lot of people supported development financially,
including the [1987 backers][backers] of the 2017 crowdfunding
campaign.

Thanks to all of you, may (the history of) the source be with you!

***
[![Paren Xkcb](https://img.shields.io/badge/%28-%20%20%20-red.svg)](https://xkcd.com/859)
[![GPL v3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/magit/magit.svg?branch=master)](https://travis-ci.org/magit/magit)
[![Melpa](https://melpa.org/packages/magit-badge.svg)](https://melpa.org/#/magit)
[![Melpa Stable](https://stable.melpa.org/packages/magit-badge.svg)](https://stable.melpa.org/#/magit)
[![Eierlegende Wollmilchsau](https://img.shields.io/badge/eierlegende-Wollmilchsau-green.svg)](https://magit.vc/manual/magit)
[![Swiss Made](https://img.shields.io/badge/swiss-made-red.svg?colorA=E11A27&colorB=555555)](https://magit.vc/stats/authors.html#commits_per_author)
[![Netscape](https://magit.vc/assets/netscape-20px.png)](https://en.wikipedia.org/wiki/Browser_wars)


[backers]: https://github.com/magit/magit/blob/master/Documentation/BACKERS.md
[contrib]: https://github.com/magit/magit/blob/master/.github/CONTRIBUTING.md
[issues]:  https://github.com/magit/magit/issues
[pulls]:   https://github.com/magit/magit/pulls

[authors]: https://magit.vc/stats/authors.html
[faq]:     https://magit.vc/manual/magit/FAQ.html
[manual]:  https://magit.vc/manual

[forum]:   https://emacs.stackexchange.com/questions/tagged/magit
[list]:    https://groups.google.com/forum/?fromgroups#!forum/magit

[jonas]:   https://emacsair.me
[kyle]:    https://github.com/kyleam
[marius]:  https://github.com/mvollmer
[nicolas]: http://dudebout.com
[noam]:    https://github.com/npostavs
[peter]:   https://github.com/pjweisberg
[phil]:    https://github.com/philjackson
[remi]:    https://github.com/vanicat
[yann]:    http://www.hodique.info
