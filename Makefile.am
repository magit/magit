lispdir = $(datadir)/emacs/site-lisp
sitestartdir = @SITESTART@

lisp_DATA = magit.el magit.elc magit-svn.el magit-svn.elc magit-topgit.el magit-topgit.elc
sitestart_DATA = 50magit.el

info_TEXINFOS = magit.texi

CLEANFILES = magit-*.elc
EXTRA_DIST = magit.el 50magit.el magit.spec magit-svn.el magit-topgit.el

%.elc: %.el
	@if [ $(builddir) != $(srcdir) ]; then ln $(srcdir)/$*.el .; fi
	emacs --batch --eval "(add-to-list 'load-path \"$(srcdir)\")" \
	              --eval '(byte-compile-file "$*.el")'
	@if [ $(builddir) != $(srcdir) ]; then rm -f $*.el; fi


elpa: magit-pkg.el info
	-@rm -rf magit-$(VERSION)
	mkdir magit-$(VERSION)
	cp magit.el magit-pkg.el magit.info magit-$(VERSION)
	(cd magit-$(VERSION); ginstall-info --dir-file=dir magit.info)
	tar cf magit-$(VERSION).tar magit-$(VERSION)
	@rm -rf magit-$(VERSION)
