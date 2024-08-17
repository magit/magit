-include config.mk
include default.mk

## ###################################################################

.PHONY: lisp docs \
	install install-lisp install-docs install-info \
	test test-interactive magit \
	clean clean-lisp clean-docs clean-archives \
	stats \
	dist versionlib magit-$(VERSION).tar.gz

all: lisp docs

help:
	$(info )
	$(info See default.mk for variables you might want to set.)
	$(info )
	$(info Build)
	$(info =====)
	$(info )
	$(info make [all]            - compile elisp and documentation)
	$(info make lisp             - compile elisp)
	$(info make redo             - re-compile elisp)
	$(info make docs             - generate all manual formats)
	$(info make texi             - generate texi manuals)
	$(info make info             - generate info manuals)
	$(info make html             - generate html manual files)
	$(info make html-dir         - generate html manual directories)
	$(info make pdf              - generate pdf manuals)
	$(info make epub             - generate epub manuals)
	$(info )
	$(info Install)
	$(info =======)
	$(info )
	$(info make install          - install elisp and documentation)
	$(info make install-lisp     - install elisp)
	$(info make install-docs     - install all documentation)
	$(info make install-info     - install info manuals only)
	$(info )
	$(info Clean)
	$(info ====)
	$(info )
	$(info make clean            - clean elisp, documentation and tarball)
	$(info make clean-lisp       - clean elisp)
	$(info make clean-docs       - clean docs)
	$(info make clean-archives   - clean release tarball)
	$(info make clean-all        - clean everything except tracked texi)
	$(info make clean-stats      - clean stats)
	$(info )
	$(info Test)
	$(info ====)
	$(info )
	$(info make test             - run tests)
	$(info make test-interactive - run tests interactively)
	$(info make emacs-Q          - run emacs -Q plus Magit)
	$(info make check-declare    - check function declarations)
	$(info )
	$(info Release Management)
	$(info ==================)
	$(info )
	$(info make authors          - regenerate AUTHORS.md)
	$(info make publish          - publish snapshot manuals)
	$(info make release          - publish release manuals)
	$(info make dist             - create tarballs)
	$(info make stats            - regenerate statistics)
	$(info make stats-upload     - publish statistics)
	@printf "\n"

## Build #############################################################

redo: clean-lisp lisp

lisp:
	@$(MAKE) -C lisp lisp
	@$(MAKE) -C test lisp

docs:
	@$(MAKE) -C docs docs

texi:
	@$(MAKE) -C docs texi

info:
	@$(MAKE) -C docs info

html:
	@$(MAKE) -C docs html

html-dir:
	@$(MAKE) -C docs html-dir

pdf:
	@$(MAKE) -C docs pdf

epub:
	@$(MAKE) -C docs epub

## Install ###########################################################

install: install-lisp install-docs

install-lisp: lisp
	@$(MAKE) -C lisp install

install-docs: docs
	@$(MAKE) -C docs install-docs

install-info: info
	@$(MAKE) -C docs install-info

## Test ##############################################################

test:
	@$(MAKE) -C test test

test-interactive:
	@$(MAKE) -C test test-interactive

emacs-Q: clean-lisp
	@$(EMACS) -Q $(LOAD_PATH) --debug-init --eval "(progn\
	(setq debug-on-error t)\
	(require 'magit)\
	(global-set-key \"\\C-xg\" 'magit-status))"

check-declare:
	@$(MAKE) -C lisp check-declare

## Clean #############################################################

clean: clean-lisp clean-docs clean-archives
	@printf "Cleaning...\n"
	@$(RM) *.elc $(ELGS) # temporary cleanup kludge
	@$(RM) docs/*.texi~ docs/*.info-1 docs/*.info-2
	@$(RM) magit-pkg.el test/magit-tests.elc

clean-lisp:
	@$(MAKE) -C lisp clean

clean-docs:
	@$(MAKE) -C docs clean

clean-archives:
	@$(RM) *.tar.gz *.tar lisp/magit-version.el
	@$(RMDIR) magit-$(VERSION)

clean-all: clean clean-stats

clean-stats:
	@$(MAKE) -C docs clean-stats

## Release management ################################################

authors:
	@$(MAKE) -C docs authors

publish:
	@$(MAKE) -C docs publish

release:
	@$(MAKE) -C docs release

dist: magit-$(VERSION).tar.gz
	@$(RMDIR) magit-$(VERSION)
	@$(RM) magit-version.el

versionlib:
	@$(MAKE) -C lisp versionlib

DIST_ROOT_FILES = LICENSE default.mk Makefile README.md CHANGELOG
DIST_LISP_FILES = $(addprefix lisp/,$(ELS) magit-version.el Makefile)
DIST_DOCS_FILES = $(addprefix docs/,$(TEXIPAGES) AUTHORS.md Makefile)

magit-$(VERSION).tar.gz: lisp versionlib info
	@printf "Packing $@\n"
	@$(MKDIR) magit-$(VERSION)
	@$(CP) $(DIST_ROOT_FILES) magit-$(VERSION)
	@$(MKDIR) magit-$(VERSION)/lisp
	@$(CP) $(DIST_LISP_FILES) magit-$(VERSION)/lisp
	@$(MKDIR) magit-$(VERSION)/docs
	@$(CP) $(DIST_DOCS_FILES) magit-$(VERSION)/docs
	@$(TAR) cz --mtime=./magit-$(VERSION) -f magit-$(VERSION).tar.gz magit-$(VERSION)

## Statistics ########################################################

stats:
	@$(MAKE) -C docs stats

stats-upload:
	@$(MAKE) -C docs stats-upload

