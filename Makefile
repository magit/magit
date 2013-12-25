PREFIX  ?= /usr/local
datarootdir ?= $(PREFIX)/share
lispdir ?= $(datarootdir)/emacs/site-lisp/magit
infodir ?= $(datarootdir)/info
docdir  ?= $(datarootdir)/doc/magit
execdir ?= $(PREFIX)/bin

LOADDEFS_FILE ?= magit-autoloads.el
LOADDEFS_DIR  ?= $(lispdir)

ELS  = magit.el
ELS += magit-blame.el
ELS += magit-key-mode.el
ELS += magit-stgit.el
ELS += magit-svn.el
ELS += magit-topgit.el
ELS += magit-wip.el
ELCS = $(ELS:.el=.elc)

CP    ?= install -p -m 644
CPBIN ?= install -p -m 755
MKDIR ?= install -p -m 755 -d
RMDIR ?= rm -rf

MAKEINFO     ?= makeinfo
INSTALL_INFO ?= install-info

EFLAGS ?= -L ../git-modes -L ../cl-lib
EMACS  ?= emacs
BATCH   = $(EMACS) $(EFLAGS) -batch -Q -L .
BATCHE  = $(BATCH) -eval
BATCHC  = $(BATCH) -f batch-byte-compile

VERSION=$(shell \
  test -e .git && git describe --tags --dirty 2> /dev/null || \
  $(BATCHE) "(progn\
  (require 'cl)\
  (flet ((message (&rest _) _))\
    (load-file \"magit-version.el\"))\
  (princ magit-version))")

.PHONY: lisp
lisp: $(ELCS) magit-version.el loaddefs

.PHONY: all
all: lisp docs

.PHONY: help
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
	$(info make test-interactive - run tests interactively)
	$(info )
	$(info Release Managment)
	$(info =================)
	$(info )
	$(info make authors          - regenerate the AUTHORS.md file)
	$(info make dist             - create old-school tarball)
	$(info make marmalade        - create marmalade tarball)
	@printf "\n"

%.elc: %.el
	@$(BATCHC) $<

# Not a phony target, but needs to run *every* time.
.PHONY: magit-version.el
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

# Not a phony target, but needs to run *every* time.
.PHONY: magit-pkg.el
magit-pkg.el:
	@printf "Generating magit-pkg.el\n"
	@printf "(define-package \"magit\" \""$(VERSION)"\"\n" > $@
	@printf "  \"Control Git from Emacs.\"\n"      >> $@
	@printf "  '((cl-lib \"0.3\")\n"               >> $@
	@printf "    (git-commit-mode \"0.14.0\")\n"   >> $@
	@printf "    (git-rebase-mode \"0.14.0\")))\n" >> $@

.PHONY: loaddefs
loaddefs: $(LOADDEFS_FILE)

$(LOADDEFS_FILE): $(ELS)
	@$(BATCHE) "(progn\
	(setq vc-handled-backends nil)\
	(defvar generated-autoload-file nil)\
	(let ((generated-autoload-file \"$(CURDIR)/$(LOADDEFS_FILE)\")\
	      (make-backup-files nil))\
	  (update-directory-autoloads \".\")))"

.PHONY: docs
docs: magit.info dir

%.info: %.texi
	$(MAKEINFO) $< -o $@

dir: magit.info
	$(INSTALL_INFO) --dir=$@ $<

define MAILMAP
Alex Ott <alexott@gmail.com> <ott@flash.lan>
Seong-Kook Shin <cinsky@gmail.com>
David Abrahams <dave@boostpro.com>
Evgkeni Sampelnikof <esabof@gmail.com> <faceoffuture@yahoo.gr>
Evgkeni Sampelnikof <esabof@gmail.com> <sabof@example.com>
Graham Clark <grclark@gmail.com> <gcla@moria.(none)>
Jesse Alama <jesse.alama@gmail.com> <alama@stanford.edu>
Jonas Bernoulli <jonas@bernoul.li> <jonasbernoulli@gmail.com>
Leo Liu <sdl.web@gmail.com>
Marc Herbert <marc.herbert@gmail.com> <marc.herbert+git@gmail.com>
Marc Herbert <marc.herbert@gmail.com> <Marc.Herbert+git@gmail.com>
Marcel Wolf <mwolf@ml1.net> marcel-wolf
Marius Vollmer <marius.vollmer@gmail.com> <marius.vollmer@nokia.com>
Marius Vollmer <marius.vollmer@gmail.com> <marius.vollmer@uni-dortmund.de>
Marius Vollmer <marius.vollmer@gmail.com> <mvo@bright.(none)>
Marius Vollmer <marius.vollmer@gmail.com> <mvo@esdhcp03984.research.nokia.com>
Marius Vollmer <marius.vollmer@gmail.com> <mvo@manamana.(none)>
Noam Postavsky <npostavs@users.sourceforge.net>
Óscar Fuentes <ofv@wanadoo.es> Oscar Fuentes <ofv@wanadoo.es>
Óscar Fuentes <ofv@wanadoo.es> <oscar@nc10>
Óscar Fuentes <ofv@wanadoo.es> <oscar@qcore>
Peter J. Weisberg <pj@irregularexpressions.net>
Rémi Vanicat <vanicat@debian.org> <github.20.vanicat@mamber.net>
Sébastien Gross <seb@chezwam.org> <seb•ɑƬ•chezwam•ɖɵʈ•org>
Yann Hodique <yann.hodique@gmail.com> <yann.hodique@bromium.com>
Yann Hodique <yann.hodique@gmail.com> <yhodique@vmware.com>
endef
export MAILMAP

# Not a phony target, but needs to run *every* time.
.PHONY: .mailmap
.mailmap:
	@printf "Generating .mailmap..."
	@printf "$$MAILMAP\n" > $@
	@printf "done\n"

CONTRIBUTORS_URL = https://github.com/magit/magit/graphs/contributors
define AUTHORS_HEADER
Authors
=======

Also see $(CONTRIBUTORS_URL).
Names below are sorted alphabetically.

Author
------

- Marius Vollmer <marius.vollmer@gmail.com>

Maintainer
----------

- Jonas Bernoulli <jonas@bernoul.li>

Retired Maintainers
-------------------

- Nicolas Dudebout <nicolas.dudebout@gatech.edu>
- Peter J. Weisberg <pj@irregularexpressions.net>
- Phil Jackson <phil@shellarchive.co.uk>
- Rémi Vanicat <vanicat@debian.org>
- Yann Hodique <yann.hodique@gmail.com>

Contributors
------------

endef
export AUTHORS_HEADER

# Not a phony target, but needs to run *every* time.
.PHONY: AUTHORS.md
AUTHORS.md: .mailmap
	@printf "Generating AUTHORS.md..."
	@test -d .git \
		&& (printf "$$AUTHORS_HEADER\n" > $@ \
			&& git log --pretty=format:'- %aN <%aE>' | sort -u >> $@ \
			&& printf "done\n" ; ) \
		|| printf "FAILED (non-fatal)\n"

.PHONY: authors
authors: AUTHORS.md

.PHONY: install
install: install-lisp install-docs

.PHONY: install-all
install-all: install-lisp install-docs install-script

.PHONY: install-lisp
install-lisp: lisp
	$(MKDIR) $(DESTDIR)$(lispdir)
	$(CP) $(ELS) $(ELCS) magit-version.el $(DESTDIR)$(lispdir)
	$(MKDIR) $(DESTDIR)$(LOADDEFS_DIR)
	$(CP) $(LOADDEFS_FILE) $(DESTDIR)$(LOADDEFS_DIR)/$(LOADDEFS_FILE)

.PHONY: install-docs
install-docs: docs
	$(MKDIR) $(DESTDIR)$(infodir)
	$(CP) magit.info $(DESTDIR)$(infodir)
	$(INSTALL_INFO) --info-dir=$(DESTDIR)$(infodir) $(DESTDIR)$(infodir)/magit.info
	$(MKDIR) $(DESTDIR)$(docdir)
	$(CP) AUTHORS.md $(DESTDIR)$(docdir)

.PHONY: install-script
install-script: bin/magit
	$(MKDIR) $(DESTDIR)$(execdir)
	$(CPBIN) bin/magit $(DESTDIR)$(execdir)

.PHONY: test
test: $(ELCS)
	@$(BATCHE) "(progn\
	(require 'cl) \
	(put 'flet 'byte-obsolete-info nil))" \
	-l tests/magit-tests.el -f ert-run-tests-batch-and-exit

.PHONY: test-interactive
test-interactive: $(ELCS)
	@$(EMACS) $(EFLAGS) -Q -L "." --eval "(progn\
	(require 'cl)\
	(put 'flet 'byte-obsolete-info nil)\
	(load-file \"tests/magit-tests.el\")\
	(ert t))"

.PHONY: clean
clean:
	@echo "Cleaning..."
	@$(RM) $(ELCS) $(LOADDEFS_FILE) magit-version.el *.tar.gz *.tar .mailmap
	@$(RMDIR) magit-$(VERSION)
	@test ! -e .git || $(RM) magit.info

DIST_FILES  = $(ELS) magit-version.el Makefile AUTHORS.md
DIST_FILES += README.md magit.texi magit.info dir
DIST_FILES_BIN  = bin/magit

ELPA_FILES = $(ELS) magit.info dir AUTHORS.md

.PHONY: dist
dist: magit-$(VERSION).tar.gz

magit-$(VERSION).tar.gz: $(DIST_FILES)
	$(MKDIR) magit-$(VERSION)/bin
	$(CP) $(DIST_FILES) magit-$(VERSION)
	$(CPBIN) $(DIST_FILES_BIN) magit-$(VERSION)/bin
	tar -cvz --mtime=./magit-$(VERSION) -f magit-$(VERSION).tar.gz magit-$(VERSION)
	$(RMDIR) magit-$(VERSION)

.PHONY: marmalade
marmalade: magit-$(VERSION).tar

magit-$(VERSION).tar: $(ELPA_FILES) magit-pkg.el
	$(MKDIR) magit-$(VERSION)
	$(CP) $(ELPA_FILES) magit-$(VERSION)
	$(CP) magit-pkg.el magit-$(VERSION)
	tar -cv --mtime=./magit-$(VERSION) -f magit-$(VERSION).tar magit-$(VERSION)
	$(RMDIR) magit-$(VERSION)
