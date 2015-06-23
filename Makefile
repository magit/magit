include default.mk
-include config.mk

.PHONY: lisp \
	install install-lisp install-docs install-info \
	test test-interactive \
	clean clean-lisp clean-docs \
	genstats \
	dist magit-$(VERSION).tar.gz elpa magit-$(VERSION).tar

all: lisp docs

help:
	$(info )
	$(info Current version: magit-$(VERSION))
	$(info )
	$(info Build)
	$(info =====)
	$(info )
	$(info make [all]            - compile elisp and documentation)
	$(info make lisp             - compile elisp)
	$(info make docs             - generate info manuals)
	$(info make info             - generate info manuals)
	$(info )
	$(info Install)
	$(info =======)
	$(info )
	$(info make install          - install elisp and documentation)
	$(info make install-lisp     - install elisp)
	$(info make install-docs     - install all documentation)
	$(info make install-info     - install info manuals only)
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
	$(info make texi             - regenerate texi from org)
	$(info make genstats         - regenerate statistics)
	$(info make authors          - regenerate AUTHORS.md)
	$(info make dist             - create tarballs)
	$(info make elpa             - create elpa tarballs)
	$(info make marmalade        - upload elpa tarballs to marmalade)
	@printf "\n"

lisp:
	@$(RM) $(ELCS) $(ELGS) # temporary cleanup kludge
	@$(MAKE) -C lisp lisp

docs:
	@$(MAKE) -C Documentation all

info:
	@$(MAKE) -C Documentation info

texi:
	@$(MAKE) -C Documentation texi

install: install-lisp install-docs

install-lisp: lisp
	@$(MAKE) -C lisp install

install-docs: docs
	@$(MAKE) -C Documentation install-docs

install-info: info
	@$(MAKE) -C Documentation install-info

test:
	@$(BATCH) --eval "(progn\
	(load-file \"t/magit-tests.el\")\
	(ert-run-tests-batch-and-exit))"

test-interactive:
	@$(EMACSBIN) -Q $(LOAD_PATH) --eval "(progn\
	(load-file \"t/magit-tests.el\")\
	(ert t))"

clean: clean-lisp clean-docs
	@$(RM) $(ELCS) $(ELGS) # temporary cleanup kludge
	@$(RM) *.tar.gz *.tar Documentation/*.texi~
	@$(RMDIR) magit-$(VERSION)

clean-lisp:
	@$(MAKE) -C lisp clean

clean-docs:
	@$(MAKE) -C Documentation clean

# Release management

genstats:
	@printf "Generating stats\n"
	@gitstats -c style=/css/stats.css -c max_authors=200 . $(statsdir)

authors:
	@$(MAKE) -C Documentation authors

dist: magit-$(VERSION).tar.gz

DIST_ROOT_FILES = COPYING default.mk Makefile README.md
DIST_LISP_FILES = $(addprefix lisp/,$(ELS) magit-version.el Makefile)
DIST_DOCS_FILES = $(addprefix Documentation/,$(TEXIPAGES) AUTHORS.md Makefile)
ifneq ("$(wildcard RelNotes/$(VERSION).txt)","")
  DIST_DOCS_FILES += RelNotes/$(VERSION).txt
endif

magit-$(VERSION).tar.gz:
	@printf "Packing $@\n"
	@$(MKDIR) magit-$(VERSION)
	@$(CP) $(DIST_ROOT_FILES) magit-$(VERSION)
	@$(MKDIR) magit-$(VERSION)/lisp
	@$(CP) $(DIST_LISP_FILES) magit-$(VERSION)/lisp
	@$(MKDIR) magit-$(VERSION)/Documentation
	@$(CP) $(DIST_DOCS_FILES) magit-$(VERSION)/Documentation
	@$(TAR) cz --mtime=./magit-$(VERSION) -f magit-$(VERSION).tar.gz magit-$(VERSION)
	@$(RMDIR) magit-$(VERSION)

marmalade: elpa
	@printf "Uploading magit-$(VERSION)\n"
	@marmalade-upload magit-$(VERSION).tar

elpa: magit-$(VERSION).tar

ELPA_ROOT_FILES = COPYING README.md magit-pkg.el
ELPA_LISP_FILES = $(addprefix lisp/,$(ELS) magit-version.el)
ELPA_DOCS_FILES = $(addprefix Documentation/,$(INFOPAGES) AUTHORS.md dir)

magit-$(VERSION).tar: info
	@printf "Packing $@\n"
	@$(MKDIR) magit-$(VERSION)
	@$(CP) $(ELPA_ROOT_FILES) magit-$(VERSION)
	@$(CP) $(ELPA_LISP_FILES) magit-$(VERSION)
	@$(CP) $(ELPA_DOCS_FILES) magit-$(VERSION)
	@$(TAR) c --mtime=./magit-$(VERSION) -f magit-$(VERSION).tar magit-$(VERSION)
	@$(RMDIR) magit-$(VERSION)
