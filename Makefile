-include config.mk
include .common.mk

ELPA_DIR ?= $(HOME)/.emacs.d/elpa

DASH_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/dash-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(DASH_DIR)" ""
  DASH_DIR = ../dash
endif

CYGPATH := $(shell cygpath --version 2>/dev/null)

ifdef CYGPATH
  LOAD_PATH ?= -L . -L $(shell cygpath --mixed $(DASH_DIR))
else
  LOAD_PATH ?= -L . -L $(DASH_DIR)
endif

BATCH = $(EMACSBIN) -batch -Q $(LOAD_PATH)

VERSION=$(shell \
  test -e .git\
  && git describe --tags --dirty 2> /dev/null\
  || $(BATCH) --eval "(progn\
  (fset 'message (lambda (&rest _)))\
  (load-file \"magit-version.el\")\
  (princ magit-version))")

.PHONY: help magit-version.el AUTHORS.md \
	install-lisp install-docs test test-interactive clean clean-docs \
	dist magit-$(VERSION).tar magit-$(VERSION).tar.gz genstats

all: lisp docs

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
	$(info make marmalade-upload - create and upload marmalade tarball)
	$(info make genstats         - generate stats)
	@printf "\n"

ELS  = with-editor.el
ELS += git-commit.el
ELS += magit-popup.el
ELS += magit-utils.el
ELS += magit-section.el
ELS += magit-git.el
ELS += magit-mode.el
ELS += magit-process.el
ELS += magit-core.el
ELS += magit-diff.el
ELS += magit-wip.el
ELS += magit-apply.el
ELS += magit-log.el
ELS += magit.el
ELS += magit-sequence.el
ELS += magit-commit.el
ELS += magit-remote.el
ELS += magit-bisect.el
ELS += magit-stash.el
ELS += magit-blame.el
ELS += magit-ediff.el
ELS += magit-extras.el
ELS += git-rebase.el
ELCS = $(ELS:.el=.elc)

with-editor.elc:
git-commit.elc:		with-editor.elc
magit-utils.elc:
magit-section.elc:	magit-utils.elc
magit-git.elc:		magit-utils.elc magit-section.elc
magit-mode.elc:		magit-section.elc magit-git.elc
magit-popup.elc:
magit-process.elc:	with-editor.elc magit-utils.elc magit-section.elc \
			magit-git.elc magit-mode.elc
magit-core.elc:		magit-utils.elc magit-section.elc magit-git.elc \
			magit-mode.elc magit-popup.elc magit-process.elc
magit-diff.elc:		git-commit.elc magit-core.elc
magit-wip.elc:		magit-core.elc
magit-apply.elc:	magit-core.elc magit-diff.elc magit-wip.elc
magit-log.elc:		magit-core.elc magit-diff.elc
magit.elc:		with-editor.elc git-commit.elc \
			magit-core.elc magit-diff.elc magit-apply.elc magit-log.elc
magit-sequence.elc:	magit.elc
magit-commit.elc:	magit.elc magit-sequence.elc
magit-remote.elc:	magit.elc
magit-bisect.elc:	magit.elc
magit-stash.elc:	magit.elc
magit-blame.elc:	magit.elc
magit-ediff.elc:	magit.elc
magit-extras.elc:	magit.elc
git-rebase.elc:		magit.elc with-editor.elc

lisp: $(ELCS) magit-version.el magit-autoloads.el

%.elc: %.el
	@printf "Compiling %s\n" $<
	@$(BATCH) --eval "(progn\
	(when (file-exists-p \"$@\")\
	  (delete-file \"$@\"))\
	(setq with-editor-emacsclient-executable nil)\
	(fset 'message* (symbol-function 'message))\
	(fset 'message  (lambda (f &rest a)\
	                  (unless (equal f \"Wrote %s\")\
	                    (apply 'message* f a)))))" \
	-f batch-byte-compile $<

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

magit-autoloads.el: $(ELS)
	@printf "Generating magit-autoloads.el\n"
	@$(BATCH) --eval "(progn\
	(fset 'message (lambda (&rest _)))\
	(setq make-backup-files nil)\
	(setq vc-handled-backends nil)\
	(setq default-directory (file-truename default-directory))\
	(setq generated-autoload-file (expand-file-name \"magit-autoloads.el\"))\
	(setq find-file-visit-truename t)\
	(update-directory-autoloads default-directory)))"

docs:
	@$(MAKE) -C Documentation all

AUTHORS_URL = http://magit.vc/stats/authors.html
define AUTHORS_HEADER
Authors
=======

For statistics see $(AUTHORS_URL).

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

authors: AUTHORS.md
AUTHORS.md: .mailmap
	@printf "Generating AUTHORS.md..."
	@test -d .git \
		&& (printf "$$AUTHORS_HEADER\n" > $@ \
			&& git log --pretty=format:'- %aN <%aE>' | sort -u >> $@ \
			&& printf "done\n" ; ) \
		|| printf "FAILED (non-fatal)\n"

install: install-lisp install-docs

install-lisp: lisp
	@$(MKDIR) $(DESTDIR)$(lispdir)
	$(CP) $(ELS) $(ELCS) magit-autoloads.el magit-version.el $(DESTDIR)$(lispdir)

install-docs: install-info
	@$(MKDIR) $(DESTDIR)$(docdir)
	$(CP) AUTHORS.md $(DESTDIR)$(docdir)

install-info: docs
	@$(MAKE) -C Documentation install

test:
	@$(BATCH) --eval "(progn\
	(load-file \"t/magit-tests.el\")\
	(ert-run-tests-batch-and-exit))"

test-interactive:
	@$(EMACSBIN) -Q $(LOAD_PATH) --eval "(progn\
	(setq magit-last-seen-setup-instructions \"9999\")\
	(load-file \"t/magit-tests.el\")\
	(ert t))"

clean: clean-docs
	@$(RM) $(ELCS) magit-autoloads.el magit-version.el *.tar.gz *.tar
	@$(RMDIR) magit-$(VERSION)

clean-docs:
	@$(MAKE) -C Documentation clean

DIST_FILES = $(ELS) magit-version.el Makefile \
	AUTHORS.md README.md COPYING Documentation/magit.texi

dist: magit-$(VERSION).tar.gz
magit-$(VERSION).tar.gz:
	@printf "Packing $@\n"
	@$(MKDIR) magit-$(VERSION)
	@$(CP) $(DIST_FILES) magit-$(VERSION)
	@$(TAR) cz --mtime=./magit-$(VERSION) -f magit-$(VERSION).tar.gz magit-$(VERSION)
	@$(RMDIR) magit-$(VERSION)

ELPA_FILES = $(ELS) magit-pkg.el AUTHORS.md Documentation/magit.info Documentation/dir

marmalade: magit-$(VERSION).tar
marmalade-upload: marmalade
	@printf "Uploading magit-$(VERSION)\n"
	@marmalade-upload magit-$(VERSION).tar
	@$(RMDIR) marmalade
magit-$(VERSION).tar:
	@printf "Packing $@\n"
	@$(MKDIR) magit-$(VERSION)
	@$(CP) $(ELPA_FILES) magit-$(VERSION)
	@$(TAR) c --mtime=./magit-$(VERSION) -f magit-$(VERSION).tar magit-$(VERSION)
	@$(RMDIR) magit-$(VERSION)

genstats:
	@printf "Generating stats\n"
	@gitstats -c style=/css/stats.css -c max_authors=200 . $(statsdir)
