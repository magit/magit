VERSION=0.8.2
PREFIX=/usr/local
ELS=magit.el magit-svn.el magit-topgit.el
ELCS=$(ELS:.el=.elc)

.PHONY=install

%.elc: %.el
	emacs --batch --eval "(add-to-list 'load-path \"$(CURDIR)\")" \
	              --eval '(byte-compile-file "$<")'

all: $(ELCS) magit.info magit.spec magit-pkg.el

magit.spec: magit.spec.in
	sed -e s/@VERSION@/$(VERSION)/ < $< > $@

magit-pkg.el: magit-pkg.el.in
	sed -e s/@VERSION@/$(VERSION)/ < $< > $@

magit.elc: magit.el
magit-svn.elc: magit-svn.el 
magit-topgit.elc: magit-topgit.el
magit.info:

install: all
	mkdir -p $(DESTDIR)/$(PREFIX)/share/emacs/site-lisp
	install -m 644 $(ELS) $(ELCS) $(DESTDIR)/$(PREFIX)/share/emacs/site-lisp
	mkdir -p $(DESTDIR)/$(PREFIX)/share/info
	install -m 644 magit.info $(DESTDIR)/$(PREFIX)/share/info
	install-info --info-dir=$(DESTDIR)/$(PREFIX)/share/info $(DESTDIR)/$(PREFIX)/share/info/magit.info
	mkdir -p $(DESTDIR)/etc/emacs/site-start.d
	install -m 644 50magit.el $(DESTDIR)/etc/emacs/site-start.d/50magit.el

clean:
	rm -f magit-pkg.el magit.spec magit.info $(ELCS)

