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
	$(info make [all]            -- Compile elisp and documentation)
	$(info make lisp             -- Compile elisp)
	$(info make redo             -- Re-compile elisp)
	$(info make docs             -- Generate all manual formats)
	$(info make redo-docs        -- Re-generate all manual formats)
	$(info make texi             -- Generate texi manuals)
	$(info make info             -- Generate info manuals)
	$(info make html             -- Generate html manual files)
	$(info make html-dir         -- Generate html manual directories)
	$(info make pdf              -- Generate pdf manuals)
	$(info make epub             -- Generate epub manuals)
	$(info )
	$(info Install)
	$(info =======)
	$(info )
	$(info make install          -- Install elisp and documentation)
	$(info make install-lisp     -- Install elisp)
	$(info make install-docs     -- Install all documentation)
	$(info make install-info     -- Install info manuals only)
	$(info )
	$(info Clean)
	$(info ====)
	$(info )
	$(info make clean            -- Clean elisp, documentation and tarball)
	$(info make clean-lisp       -- Clean elisp)
	$(info make clean-docs       -- Clean docs)
	$(info make clean-archives   -- Clean release tarball)
	$(info make clean-all        -- Clean everything except tracked texi)
	$(info make clean-stats      -- Clean stats)
	$(info )
	$(info Test)
	$(info ====)
	$(info )
	$(info make test             -- Run tests)
	$(info make test-interactive -- Run tests interactively)
	$(info make emacs-Q          -- Run emacs -Q plus Magit)
	$(info make check-declare    -- Check function declarations)
	$(info )
	$(info Release Management)
	$(info ==================)
	$(info )
	$(info make authors          -- Regenerate AUTHORS.md)
	$(info make publish          -- Publish snapshot manuals)
	$(info make release          -- Publish release manuals)
	$(info make dist             -- Create tarballs)
	$(info make stats            -- Regenerate statistics)
	$(info make stats-upload     -- Publish statistics)
	@printf "\n"

## Build #############################################################

lisp:
	@$(MAKE) -C lisp lisp
	@$(MAKE) -C test lisp

redo: clean-lisp lisp

docs:
	@$(MAKE) -C docs docs

redo-docs:
	@$(MAKE) -C docs redo-docs

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

dist: clean-docs clean-archives versionlib info magit-$(VERSION).tar.gz
	@$(RMDIR) magit-$(VERSION)
	@$(RM) magit-version.el

versionlib:
	@$(MAKE) -C lisp versionlib

DIST_ROOT_FILES = LICENSE default.mk Makefile README.md CHANGELOG
DIST_LISP_FILES = $(addprefix lisp/,$(ELS) magit-version.el Makefile)
DIST_DOCS_FILES = $(addprefix docs/,$(TEXIPAGES) AUTHORS.md Makefile)

magit-$(VERSION).tar.gz:
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

