<div align="center"><img src="https://magit.vc/assets/magit-168x200px.png"/></div>
<h2 align="center">A Git Porcelain inside Emacs</h2>
<p align="center">
  <a href="https://magit.vc"><b>homepage</b></a> |
  <a href="https://docs.magit.vc"><b>manual</b></a> |
  <a href="https://docs.magit.vc/magit/FAQ.html"><b>faq</b></a> |
  <a href="https://github.com/magit/magit/wiki"><b>wiki</b></a> |
  <a href="https://emacs.ch/@tarsius"><b>mastodon</b></a>
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
  Keeping its users <a href= "https://magit.vc/quotes/">this excited</a> is
  <a href="https://magit.vc/stats/magit/authors.html#cumulated_added_lines_of_code_per_author">
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
  <a href="https://github.com/sponsors/tarsius/">
    <img title="Sponsor my work using Github Sponsors"
         alt="Sponsor my work using Github Sponsors"
         src="https://magit.vc/assets/donate/github-sponsors-50px.png"></a>
  &nbsp;&nbsp;
  <a href="https://liberapay.com/magit/">
    <img title="Sponsor my work using Liberapay"
         alt="Sponsor my work using Liberapay"
         src="https://magit.vc/assets/donate/liberapay-50px.png"></a>
  <br>
  <a href="https://opencollective.com/magit/">
    <img title="Sponsor my work using Opencollective"
         alt="Sponsor my work using Opencollective"
         src="https://magit.vc/assets/donate/opencollective-50px.png"></a>
  &nbsp;&nbsp;
  <a href="https://magit.vc/donate/paypal.html">
    <img title="Sponsor my work using PayPal"
         alt="Sponsor my work using PayPal"
         src="https://magit.vc/assets/donate/paypal-50px.png"></a>
</div>
<br>
<div align="center">
  Some alternative donation methods are <a href="https://magit.vc/donate/">available</a>.
</div>
<hr>

### Getting Started

If you are new to Magit, then either one of the following two
articles should help understanding how it differs from other Git
clients.

#### [Visual Magit walk-through](https://emacsair.me/2017/09/01/magit-walk-through)

If you are completely new to Magit, then this article is a good
visual introduction.

Almost everything that you see in Magit can be acted on by pressing
some key, but that's not obvious from just seeing how Magit looks.
The screenshots and accompanying text of this article explain how to
perform a variety of actions on Magit's output.

#### [Magit, the magical Git interface](https://emacsair.me/2017/09/01/the-magical-git-interface)

Magit differs significantly from other Git interfaces, and its
advantages are not immediately obvious simply from looking at a few
screenshots as presented in the preceding article.

This article discusses Magit's properties in somewhat more abstract
terms.

#### Video introductions

If you prefer [video](https://magit.vc/screencasts/) introductions,
head over to that page, where find a collection of such introductions
and other videos about Magit, by various creators.

***
### Support and Contributing

Magit has many users and very few maintainers, so we kindly ask to read
the appropriate guidelines before getting in contact. &mdash; Thanks!

- 🆘 [How to ask for help](https://github.com/magit/magit/discussions/4630)
- 🪳 [How to report a bug](https://github.com/magit/magit/wiki/How-to-report-a-bug)
- 💡 [How to suggest a feature](https://github.com/magit/magit/discussions/4631)
- 🏗️ [Pull request guidelines](https://github.com/magit/magit/wiki/Pull-request-guidelines)
- ℹ️ [FAQ](https://docs.magit.vc/magit/FAQ.html)
- ℹ️ [Manual](https://docs.magit.vc/magit)

TL;DR We now use discussions for feature requests (not issues) and prefer
if you ask the community for support instead of the overworked maintainers.

Please also consider to contribute by supporting other users or by making
a [monetary donation](https://magit.vc/donate). &mdash; Thanks!

***
### Acknowledgments

Magit was started by [Marius Vollmer][marius], and is now maintained by
[Jonas Bernoulli][jonas] and [Kyle Meyer][kyle].  Former maintainers are
[Nicolas Dudebout][nicolas], [Noam Postavsky][noam],
[Peter J. Weisberg][peter], [Phil Jackson][phil], [Rémi Vanicat][remi] and
[Yann Hodique][yann].  Many more people have [contributed code][authors],
suggested features or made monetary contributions.

Thanks to all of you, may (the history of) the source be with you!

***
[![Compile](https://github.com/magit/magit/actions/workflows/compile.yml/badge.svg)](https://github.com/magit/magit/actions/workflows/compile.yml)
[![Test](https://github.com/magit/magit/actions/workflows/test.yml/badge.svg)](https://github.com/magit/magit/actions/workflows/test.yml)
[![Manual](https://github.com/magit/magit/actions/workflows/manual.yml/badge.svg)](https://github.com/magit/magit/actions/workflows/manual.yml)
[![NonGNU ELPA](https://emacsair.me/assets/badges/nongnu-elpa.svg)](https://elpa.nongnu.org/nongnu/magit.html)
[![Melpa](https://melpa.org/packages/magit-badge.svg)](https://melpa.org/#/magit)
[![Melpa Stable](https://stable.melpa.org/packages/magit-badge.svg)](https://stable.melpa.org/#/magit)

[![Packaging status](https://repology.org/badge/vertical-allrepos/emacs%3Amagit.svg?header=&columns=4&minversion=4&exclude_unsupported=1)](https://repology.org/project/emacs%3Amagit/versions)

[authors]: https://magit.vc/stats/magit/authors.html
[jonas]:   https://emacsair.me
[kyle]:    https://kyleam.com
[marius]:  https://github.com/mvollmer
[nicolas]: http://dudebout.com
[noam]:    https://github.com/npostavs
[peter]:   https://github.com/pjweisberg
[phil]:    https://github.com/philjackson
[remi]:    https://github.com/vanicat
[yann]:    https://yann.hodique.info
