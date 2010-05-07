It's Magit!  A Emacs mode for Git.
==================================

I started to write Magit to learn about Git and to figure out how I
would be using Git in a 'natural' way.  Magit will grow and hopefully
become more coherent as I learn more about Git and good ways to use
it.  Feedback is welcome!

Magit is owned by Marius Vollmer and is currently maintained by Phil
Jackson. For a full list of contributors have a look at magit.el.

Installing
----------

Download the latest tarball from
[the github download page](http://github.com/philjackson/magit/downloads).

And then Magit can be installed with the popular recipe of:

    $ ./autogen.sh     # If you got the sources directly from Git
    $ ./configure
    $ make install

This will put magit.el into /usr/local/share/emacs/site-lisp, where
Emacs should be able to find it.  Then add

    (require 'magit)

to your .emacs file.

Getting started
---------------

To get started with Magit, open any file in a Git repository in Emacs
and run 'M-x magit-status'.  Read the online help of magit-mode ('C-h
m' in the Magit buffer), make some changes to your files, and try to
commit them.

Learning more
-------------

The Magit User Manual describes things with more words than the online
help.  You can read it in Emacs with 'C-u C-h i magit.info' for
example, or on the web at

  http://zagadka.vm.bytemark.co.uk/magit/magit.html

If you have questions, please use the mailing list at

  http://groups.google.com/group/magit/

Magit's web home is currently at

  http://zagadka.vm.bytemark.co.uk/magit/
