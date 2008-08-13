lispdir = $(datadir)/emacs/site-lisp
sitestartdir = $(sysconfdir)/emacs/site-start.d

lisp_DATA = magit.el magit.elc
sitestart_DATA = 50magit.el

info_TEXINFOS = magit.texi

CLEANFILES = magit.elc
EXTRA_DIST = magit.el 50magit.el

%.elc: %.el
	@if [ $(builddir) != $(srcdir) ]; then ln $(srcdir)/$*.el .; fi
	emacs --batch --eval '(byte-compile-file "$*.el")'
	@if [ $(builddir) != $(srcdir) ]; then rm -f $*.el; fi
