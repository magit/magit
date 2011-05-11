VERSION=$(shell git describe --tags --dirty)
EMACS=emacs
PREFIX=/usr/local
ELS=magit.el magit-svn.el magit-topgit.el magit-stgit.el magit-key-mode.el magit-bisect.el
ELS_CONTRIB=contrib/magit-simple-keys.el contrib/magit-classic-theme.el
ELCS=$(ELS:.el=.elc)
ELCS_CONTRIB=$(ELS_CONTRIB:.el=.elc)
DIST_FILES=$(ELS) Makefile magit.texi README.md magit.spec.in magit-pkg.el.in 50magit.el
DIST_FILES_CONTRIB=$(ELS_CONTRIB) contrib/magit

.PHONY=install

BATCH=$(EMACS) -batch -q -no-site-file -eval \
  "(setq load-path (cons (expand-file-name \".\") load-path))"

%.elc: %.el
	$(BATCH) --eval '(byte-compile-file "$<")'

all: core docs contrib

core: $(ELCS) magit.spec magit-pkg.el

docs: magit.info

contrib: $(ELCS_CONTRIB)

magit.spec: magit.spec.in
	sed -e s/@VERSION@/$(VERSION)/ < $< > $@

magit-pkg.el: magit-pkg.el.in
	sed -e s/@VERSION@/$(VERSION)/ < $< > $@

magit.elc:
magit-key-mode.elc:
magit-svn.elc:
magit-topgit.elc:
magit-stgit.elc:
magit.info:

# yuck - this needs cleaning up a bit...
dist: $(DIST_FILES) $(DIST_FILES_CONTRIB)
	mkdir -p magit-$(VERSION)/contrib
	cp $(DIST_FILES) magit-$(VERSION)
	cp $(DIST_FILES_CONTRIB) magit-$(VERSION)/contrib
	sed -i -e "1 s/=.*/=$(VERSION)/" magit-$(VERSION)/Makefile #NO_DIST
	sed -i -e "/NO_DIST/d" magit-$(VERSION)/Makefile #NO_DIST
	sed -i "s/@GIT_DEV_VERSION@/$(VERSION)/" magit-$(VERSION)/magit.el #NO_DIST
	tar -cvzf magit-$(VERSION).tar.gz magit-$(VERSION)
	rm -rf magit-$(VERSION)

install: install_core install_docs

install_core: core
	mkdir -p $(DESTDIR)/$(PREFIX)/share/emacs/site-lisp
	install -m 644 $(ELS) $(ELCS) $(DESTDIR)/$(PREFIX)/share/emacs/site-lisp
	sed -i "s/@GIT_DEV_VERSION@/$(VERSION)/" $(DESTDIR)/$(PREFIX)/share/emacs/site-lisp/magit.el #NO_DIST
	mkdir -p $(DESTDIR)/etc/emacs/site-start.d
	install -m 644 50magit.el $(DESTDIR)/etc/emacs/site-start.d/50magit.el

install_docs: docs
	mkdir -p $(DESTDIR)/$(PREFIX)/share/info
	install -m 644 magit.info $(DESTDIR)/$(PREFIX)/share/info
	install-info --info-dir=$(DESTDIR)/$(PREFIX)/share/info $(DESTDIR)/$(PREFIX)/share/info/magit.info

install_contrib: contrib
	mkdir -p $(DESTDIR)/$(PREFIX)/share/emacs/site-lisp
	install -m 644 $(ELS_CONTRIB) $(ELCS_CONTRIB) $(DESTDIR)/$(PREFIX)/share/emacs/site-lisp
	mkdir -p $(DESTDIR)/$(PREFIX)/bin
	install -m 755 contrib/magit $(DESTDIR)/$(PREFIX)/bin

install_all: install install_contrib

clean:
	rm -fr magit-pkg.el magit.spec magit.info $(ELCS) $(ELCS_CONTRIB) *.tar.gz magit-$(VERSION)
