VERSION=0.8.2
PREFIX=/usr

build:
	sed -e s/@VERSION@/$(VERSION)/ < magit-pkg.el.in > magit-pkg.el
	sed -e s/@VERSION@/$(VERSION)/ < magit.spec.in > magit.spec
	emacs --batch --eval '(byte-compile-file "magit.el")'
	makeinfo -I . -o magit.info magit.texi

install: build
	mkdir -p $(DESTDIR)/$(PREFIX)/share/emacs/site-lisp
	install -m 644 magit.el magit.elc $(DESTDIR)/$(PREFIX)/share/emacs/site-lisp
	mkdir -p $(DESTDIR)/$(PREFIX)/share/info
	install -m 644 magit.info $(DESTDIR)/$(PREFIX)/share/info
	install-info --info-dir=$(DESTDIR)/$(PREFIX)/share/info $(DESTDIR)/$(PREFIX)/share/info/magit.info
	mkdir -p $(DESTDIR)/etc/emacs/site-start.d
	install -m 644 50magit.el $(DESTDIR)/etc/emacs/site-start.d/50magit.el

clean:
	rm -f magit-pkg.el magit.spec magit.elc magit.info
