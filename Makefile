PREFIX  ?= /usr/local
lispdir ?= $(PREFIX)/share/emacs/site-lisp/magit
infodir ?= $(PREFIX)/share/info
execdir ?= $(PREFIX)/bin

LOADDEFS_FILE ?= magit-autoloads.el
LOADDEFS_DIR  ?= $(lispdir)

ELS  = magit.el
ELS += magit-bisect.el
ELS += magit-blame.el
ELS += magit-cherry.el
ELS += magit-compat.el
ELS += magit-flow.el
ELS += magit-key-mode.el
ELS += magit-stgit.el
ELS += magit-svn.el
ELS += magit-topgit.el
ELS += magit-wip.el
ELCS = $(ELS:.el=.elc)

CP    ?= install -p -m 644
CPBIN ?= install -p -m 755
MKDIR ?= install -p -m 755 -d

MAKEINFO     ?= makeinfo
INSTALL_INFO ?= install-info

EMACS ?= emacs
BATCH  = $(EMACS) $(EFLAGS) -batch -Q -L .
BATCHC = $(BATCH) -f batch-byte-compile

VERSION=$(shell \
  test -e .git && git describe --tags --dirty 2> /dev/null || \
  $(BATCH) --eval "\
(progn\
  (require 'cl)\
  (flet ((message (&rest _) _))\
    (load-file \"magit-version.el\"))\
  (princ magit-version))")

.PHONY: clean magit-version.el
.PHONY: install-lisp install-docs install-script
.PHONY: melpa melpa-push

lisp:     $(ELCS) magit-version.el loaddefs
all:      lisp docs

help:
	$(info Getting Help)
	$(info ============)
	$(info )
	$(info make help             - show brief help)
	$(info )
	$(info Build)
	$(info =====)
	$(info )
	$(info make                  - build elisp files)
	$(info make lisp             - ditto)
	$(info make all              - build elisp files and documentation)
	$(info make docs             - generate documentation)
	$(info )
	$(info Install)
	$(info =======)
	$(info )
	$(info make install          - install elisp files and documentation)
	$(info make install-lisp     - install elisp files)
	$(info make install-docs     - install documentation)
	$(info make install-script   - install shell script)
	$(info make install-all      - install elisp files, script, and docs)
	$(info )
	$(info Test)
	$(info ====)
	$(info )
	$(info make test             - run tests)
	$(info )
	$(info Release Managment)
	$(info =================)
	$(info )
	$(info make dist             - create old-school tarball)
	$(info make marmalade        - create marmalade tarball)
	@echo ""

%.elc: %.el
	@$(BATCHC) $<

magit-version.el:
	@printf "Generating magit-version.el\n"
	@printf ";;; magit-version.el --- the Magit version you are using\n\n" > $@
	@printf "(setq magit-version \""$(VERSION)"\")\n\n" >> $@
	@printf "(provide 'magit-version)\n\n" >> $@
	@printf ";; Local Variables:\n" >> $@
	@printf ";; version-control: never\n" >> $@
	@printf ";; no-byte-compile: t\n" >> $@
	@printf ";; no-update-autoloads: t\n" >> $@
	@printf ";; coding: utf-8\n" >> $@
	@printf ";; End:\n" >> $@
	@printf ";;; magit-version.el ends here\n" >> $@

loaddefs: $(LOADDEFS_FILE)

$(LOADDEFS_FILE): $(ELS)
	@$(BATCH) -eval "\
(progn (defvar generated-autoload-file nil)\
  (let ((generated-autoload-file \"$(CURDIR)/$(LOADDEFS_FILE)\")\
        (make-backup-files nil))\
    (update-directory-autoloads \".\")))"

docs: magit.info dir

%.info: %.texi
	$(MAKEINFO) $< -o $@

dir: magit.info
	$(INSTALL_INFO) --dir=$@ $<

install: install-lisp install-docs
install-all: install install-script

install-lisp: lisp
	$(MKDIR) $(DESTDIR)$(lispdir)
	$(CP) $(ELS) $(ELCS) magit-version.el $(DESTDIR)$(lispdir)
	$(MKDIR) $(DESTDIR)$(LOADDEFS_DIR)
	$(CP) $(LOADDEFS_FILE) $(DESTDIR)$(LOADDEFS_DIR)/$(LOADDEFS_FILE)

install-docs: docs
	$(MKDIR) $(DESTDIR)$(infodir)
	$(CP) magit.info $(DESTDIR)$(infodir)
	$(INSTALL_INFO) --info-dir=$(DESTDIR)$(infodir) $(DESTDIR)$(infodir)/magit.info

install-script: bin/magit
	$(MKDIR) $(DESTDIR)$(execdir)
	$(CPBIN) bin/magit $(DESTDIR)$(execdir)

test: $(ELCS)
	@$(BATCH) -eval "(progn (require 'cl) \
	(put 'flet 'byte-obsolete-info nil))" \
	-l tests/magit-tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -f $(ELCS) $(LOADDEFS_FILE) magit-version.el magit.info
	rm -fr magit-$(VERSION) magit.spec *.tar.gz *.tar
	test -e .git || rm -f magit.info

DIST_FILES  = $(ELS) magit-version.el Makefile
DIST_FILES += README.md INSTALL.md magit.texi magit.info dir
DIST_FILES_BIN  = bin/magit

ELPA_FILES = $(ELS) magit.info dir

dist: magit-$(VERSION).tar.gz

magit-$(VERSION).tar.gz: $(DIST_FILES)
	$(MKDIR) magit-$(VERSION)/bin
	$(CP) $(DIST_FILES) magit-$(VERSION)
	$(CPBIN) $(DIST_FILES_BIN) magit-$(VERSION)/bin
	tar -cvz --mtime=./magit-$(VERSION) -f magit-$(VERSION).tar.gz magit-$(VERSION)
	rm -rf magit-$(VERSION)

marmalade: magit-$(VERSION).tar

magit-$(VERSION).tar: $(ELPA_FILES)
	$(MKDIR) magit-$(VERSION)
	$(CP) $(ELPA_FILES) magit-$(VERSION)
	tar -cv --mtime=./magit-$(VERSION) -f magit-$(VERSION).tar magit-$(VERSION)
	rm -rf magit-$(VERSION)
