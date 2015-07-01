include default.mk
-include config.mk

.PHONY: lisp \
	install install-lisp install-docs install-info \
	test test-interactive \
	clean clean-lisp clean-docs \
	genstats \
	dist magit-$(VERSION).tar.gz elpa $(ELPA_ARCHIVES)

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
	@printf "Cleaning...\n"
	@$(RM) $(ELCS) $(ELGS) # temporary cleanup kludge
	@$(RM) git-commit-*.el *.tar.gz *.tar Documentation/*.texi~
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

magit-$(VERSION).tar.gz: lisp info
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
	@printf "Uploading with-editor-$(VERSION)\n"
	@marmalade-upload with-editor-$(VERSION).tar
	@printf "Uploading git-commit-$(VERSION)\n"
	@marmalade-upload git-commit-$(VERSION).tar
	@printf "Uploading magit-popup-$(VERSION)\n"
	@marmalade-upload magit-popup-$(VERSION).tar
	@printf "Uploading magit-$(VERSION)\n"
	@marmalade-upload magit-$(VERSION).tar

ELPA_ARCHIVES  = with-editor-$(VERSION).tar
ELPA_ARCHIVES += git-commit-$(VERSION).el
ELPA_ARCHIVES += magit-popup-$(VERSION).tar
ELPA_ARCHIVES += magit-$(VERSION).tar

elpa: $(ELPA_ARCHIVES)

define with_editor_pkg
(define-package "with-editor" "$(VERSION)"
  "Use the Emacsclient as $$EDITOR"
  '((emacs "24.4")
    (dash "2.10.0")))
endef
# '
export with_editor_pkg
with-editor-$(VERSION).tar: info
	@printf "Packing $@\n"
	@$(MKDIR) with-editor-$(VERSION)
	@printf "$$with_editor_pkg\n" > with-editor-$(VERSION)/with-editor-pkg.el
	@$(CP) lisp/with-editor.el with-editor-$(VERSION)
	@$(CP) Documentation/with-editor.info Documentation/dir with-editor-$(VERSION)
	@$(TAR) c --mtime=./with-editor-$(VERSION) \
	  -f with-editor-$(VERSION).tar with-editor-$(VERSION)
	@$(RMDIR) with-editor-$(VERSION)

git-commit-$(VERSION).el:
	@printf "Packing $@\n"
	@$(CP) lisp/git-commit.el git-commit-$(VERSION).el

define magit_popup_pkg
(define-package "magit-popup" "$(VERSION)"
  "Define prefix-infix-suffix command combos"
  '((emacs "24.4")
    (dash "2.10.0")))
endef
# '
export magit_popup_pkg
magit-popup-$(VERSION).tar: info
	@printf "Packing $@\n"
	@$(MKDIR) magit-popup-$(VERSION)
	@printf "$$magit_popup_pkg\n" > magit-popup-$(VERSION)/magit-popup-pkg.el
	@$(CP) lisp/magit-popup.el magit-popup-$(VERSION)
	@$(CP) Documentation/magit-popup.info Documentation/dir magit-popup-$(VERSION)
	@$(TAR) c --mtime=./magit-popup-$(VERSION) \
	  -f magit-popup-$(VERSION).tar magit-popup-$(VERSION)
	@$(RMDIR) magit-popup-$(VERSION)

ELPA_ROOT_FILES = COPYING
ELPA_LISP_FILES = $(addprefix lisp/,$(ELMS) magit-version.el)
ELPA_DOCS_FILES = $(addprefix Documentation/,AUTHORS.md dir magit.info)

define magit_pkg
(define-package "magit" "$(VERSION)"
  "A Git porcelain inside Emacs"
  '((emacs "24.4")
    (dash "2.10.0")
    (with-editor "2.1.0")
    (git-commit "2.1.0")
    (magit-popup "2.1.0")))
endef
# '
export magit_pkg
magit-$(VERSION).tar: lisp info
	@printf "Packing $@\n"
	@$(MKDIR) magit-$(VERSION)
	@printf "$$magit_pkg\n" > magit-$(VERSION)/magit-pkg.el
	@$(CP) $(ELPA_ROOT_FILES) magit-$(VERSION)
	@$(CP) $(ELPA_LISP_FILES) magit-$(VERSION)
	@$(CP) $(ELPA_DOCS_FILES) magit-$(VERSION)
	@$(TAR) c --mtime=./magit-$(VERSION) -f magit-$(VERSION).tar magit-$(VERSION)
	@$(RMDIR) magit-$(VERSION)
