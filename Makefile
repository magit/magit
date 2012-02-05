VERSION=$(shell git describe --tags --dirty)
EMACS=emacs
PREFIX=/usr/local
SYSCONFDIR=/etc
ELS=magit.el magit-svn.el magit-topgit.el magit-stgit.el magit-key-mode.el magit-bisect.el magit-wip.el rebase-mode.el
ELS_CONTRIB=contrib/magit-simple-keys.el contrib/magit-classic-theme.el
ELCS=$(ELS:.el=.elc)
ELCS_CONTRIB=$(ELS_CONTRIB:.el=.elc)
DIST_FILES=$(ELS) Makefile magit.texi magit.info README.md magit.spec.in magit-pkg.el.in
DIST_FILES_CONTRIB=$(ELS_CONTRIB) contrib/magit
ELPA_FILES=$(ELS) magit.info magit-pkg.el

.PHONY=install

BATCH=$(EMACS) -batch -q -no-site-file -eval \
  "(setq load-path (cons (expand-file-name \".\") load-path))"

%.elc: %.el
	$(BATCH) --eval '(byte-compile-file "$<")'

all: core docs contrib

core: $(ELCS) magit.spec magit-pkg.el 50magit.el

docs: magit.info

contrib: $(ELCS_CONTRIB)

magit.spec: magit.spec.in
	sed -e s/@VERSION@/$(VERSION)/ < $< > $@

magit-pkg.el: magit-pkg.el.in
	sed -e s/@VERSION@/$(VERSION)/ < $< > $@

50magit.el: $(ELS)
	$(BATCH) --eval "(let ((generated-autoload-file \"$(PWD)/50magit.el\") (make-backup-files nil)) (update-directory-autoloads \".\"))"

magit.elc: magit.el
	sed -e "s/@GIT_DEV_VERSION@/$(VERSION)/" < magit.el > magit.tmp.el #NO_DIST
	$(BATCH) --eval '(byte-compile-file "magit.tmp.el")' #NO_DIST
	mv magit.tmp.elc magit.elc #NO_DIST
	rm magit.tmp.el #NO_DIST
magit.info:

dist: magit-$(VERSION).tar.gz

magit-$(VERSION).tar.gz: $(DIST_FILES) $(DIST_FILES_CONTRIB)
	mkdir -p magit-$(VERSION)/contrib
	cp --preserve=timestamps $(DIST_FILES) magit-$(VERSION)
	cp --preserve=timestamps $(DIST_FILES_CONTRIB) magit-$(VERSION)/contrib
	sed -i -e "1 s/=.*/=$(VERSION)/" magit-$(VERSION)/Makefile #NO_DIST
	sed -i -e "/NO_DIST/d" magit-$(VERSION)/Makefile #NO_DIST
	sed -i -e "s/@GIT_DEV_VERSION@/$(VERSION)/" magit-$(VERSION)/magit.el #NO_DIST
	tar -cvzf magit-$(VERSION).tar.gz magit-$(VERSION)
	rm -rf magit-$(VERSION)

elpa: magit-$(VERSION).tar

magit-$(VERSION).tar: $(ELPA_FILES)
	mkdir magit-$(VERSION)
	cp --preserve=timestamps $(ELPA_FILES) magit-$(VERSION)
	sed -i -e "s/@GIT_DEV_VERSION@/$(VERSION)/" magit-$(VERSION)/magit.el #NO_DIST
	tar -cvf magit-$(VERSION).tar magit-$(VERSION)
	rm -rf magit-$(VERSION)

install: install_core install_docs

install_core: core
	mkdir -p $(DESTDIR)$(PREFIX)/share/emacs/site-lisp
	install -m 644 $(ELS) $(ELCS) $(DESTDIR)$(PREFIX)/share/emacs/site-lisp
	sed -i -e "s/@GIT_DEV_VERSION@/$(VERSION)/" $(DESTDIR)$(PREFIX)/share/emacs/site-lisp/magit.el #NO_DIST
	mkdir -p $(DESTDIR)$(SYSCONFDIR)/emacs/site-start.d
	install -m 644 50magit.el $(DESTDIR)$(SYSCONFDIR)/emacs/site-start.d/50magit.el

install_docs: docs
	mkdir -p $(DESTDIR)$(PREFIX)/share/info
	install -m 644 magit.info $(DESTDIR)$(PREFIX)/share/info
	install-info --info-dir=$(DESTDIR)$(PREFIX)/share/info $(DESTDIR)$(PREFIX)/share/info/magit.info

install_contrib: contrib
	mkdir -p $(DESTDIR)$(PREFIX)/share/emacs/site-lisp
	install -m 644 $(ELS_CONTRIB) $(ELCS_CONTRIB) $(DESTDIR)$(PREFIX)/share/emacs/site-lisp
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	install -m 755 contrib/magit $(DESTDIR)$(PREFIX)/bin

install_all: install install_contrib

clean:
	rm -f magit.info #NO_DIST
	rm -fr magit-pkg.el magit.spec 50magit.el $(ELCS) $(ELCS_CONTRIB) *.tar.gz magit-$(VERSION)
