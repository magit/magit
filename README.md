It's Magit!  An Emacs mode for Git.
===================================

Magit is an interface to Git for Emacs, supporting GNU Emacs 22 or
later.

 Unlike Emacs's native [version control support][vc], Magit can take
advantage of Git's native features without breaking compatibility with
other systems.

To get started see the [Magit User Manual][manual] or perhaps the
[cheatsheet][cheatsheet] if you're in a hurry. There's also an
excellent [Magit screencast][screencast] by Alex Vollmer which
demonstrates some of the major features.

Installing
----------

Download the latest tarball from [the github download page][download],
then Magit can be installed with the popular recipe of:

    $ make && sudo make install

This requires `emacs` and `makeinfo` binaries, so please make sure the relevant
packages (generally `emacs` and `texinfo`) are installed on your system.
This will put magit.el into `/usr/local/share/emacs/site-lisp`, where
Emacs should be able to find it.  Then add

    (require 'magit)

to your `.emacs` file.

Magit also now supports extensions:

### git-svn

`(require 'magit-svn)` - integrates with git-svn. Hit 'N' to see your
options.

### git-topgit

`(require 'magit-topgit)` - integrates with topgit.

### git-stgit

`(require 'magit-stgit)` - integrates with StGit.

Getting started
---------------

To get started with Magit, open any file in a Git repository in Emacs
and run `M-x magit-status`.  Read the short help for magit-mode (`C-h
m` in the Magit buffer), make some changes to your files, and try to
commit them.

Learning more
-------------

The [Magit User Manual][manual] describes things with more words than
the online help.  You can read it in Emacs with `C-u C-h i
magit.info`, or [on the web][manual].

If you have any questions, please use [the mailing list][google group]
at Google Groups.

Magit's website is currently hosted [on GitHub][website].

Development
-----------

Magit was started by Marius Vollmer. It is now collectively maintained by the
Magit Owners Team: https://github.com/organizations/magit/teams/53130

For a full list of contributors have a look at `magit.el` in the
source distribution.

Magit's canonical source repository is currently
[hosted on GitHub][development].

[vc]: http://www.gnu.org/software/emacs/manual/html_node/emacs/Version-Control.html#Version-Control
[website]: http://magit.github.com/magit
[development]: http://github.com/magit/magit
[manual]: http://magit.github.com/magit/magit.html
[cheatsheet]: http://daemianmack.com/magit-cheatsheet.html
[screencast]: http://vimeo.com/2871241
[download]: http://github.com/magit/magit/downloads
[google group]: http://groups.google.com/group/magit/
