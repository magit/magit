VERSION=0.8.2

install: build
	mkdir -p $(DESTDIR)/share/emacs/site-lisp
	install -m 644 magit.el magit.elc $(DESTDIR)/share/emacs/site-lisp
	mkdir -p $(DESTDIR)/share/info
	install -m 644 magit.info $(DESTDIR)/share/info
	install-info --info-dir=$(DESTDIR)/share/info $(DESTDIR)/share/info/magit.info
	mkdir -p $(DESTDIR)/etc/emacs/site-start.d
	install -m 644 50magit.el $(DESTDIR)/etc/emacs/site-start.d/50magit.el

build:
	sed -e s/@VERSION@/$(VERSION)/ < magit-pkg.el.in > magit-pkg.el
	sed -e s/@VERSION@/$(VERSION)/ < magit.spec.in > magit.spec
	emacs --batch --eval '(byte-compile-file "magit.el")'
	makeinfo -I . -o magit.info magit.texi

clean:
	rm -f magit-pkg.el magit.spec magit.elc magit.info
