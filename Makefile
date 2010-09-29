VERSION=0.8.2
EMACS=emacs
PREFIX=/usr/local
ELS=magit.el magit-svn.el magit-topgit.el magit-key-mode.el
ELCS=$(ELS:.el=.elc)
DIST_FILES=$(ELS) Makefile magit.texi README.md magit.spec.in magit-pkg.el.in 50magit.el

.PHONY=install

BATCH=$(EMACS) -batch -q -no-site-file -eval \
  "(setq load-path (cons (expand-file-name \".\") load-path))"

%.elc: %.el
	$(BATCH) --eval '(byte-compile-file "$<")'

all: $(ELCS) magit.info magit.spec magit-pkg.el

magit.spec: magit.spec.in
	sed -e s/@VERSION@/$(VERSION)/ < $< > $@

magit-pkg.el: magit-pkg.el.in
	sed -e s/@VERSION@/$(VERSION)/ < $< > $@

magit.elc:
magit-key-mode.elc:
magit-svn.elc:
magit-topgit.elc:
magit.info:

# yuck - this needs cleaning up a bit...
dist: $(DIST_FILES)
	mkdir -p magit-$(VERSION)
	cp $(DIST_FILES) magit-$(VERSION)
	tar -cvzf magit-$(VERSION).tar.gz magit-$(VERSION)
	rm -rf magit-$(VERSION)

install: all
	mkdir -p $(DESTDIR)/$(PREFIX)/share/emacs/site-lisp
	install -m 644 $(ELS) $(ELCS) $(DESTDIR)/$(PREFIX)/share/emacs/site-lisp
	mkdir -p $(DESTDIR)/$(PREFIX)/share/info
	install -m 644 magit.info $(DESTDIR)/$(PREFIX)/share/info
	install-info --info-dir=$(DESTDIR)/$(PREFIX)/share/info $(DESTDIR)/$(PREFIX)/share/info/magit.info
	mkdir -p $(DESTDIR)/etc/emacs/site-start.d
	install -m 644 50magit.el $(DESTDIR)/etc/emacs/site-start.d/50magit.el

clean:
	rm -fr magit-pkg.el magit.spec magit.info $(ELCS) *.tar.gz magit-$(VERSION)
