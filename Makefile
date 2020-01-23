-include config.mk
include default.mk

## ###################################################################

.PHONY: lisp \
	install install-lisp install-docs install-info \
	test test-interactive magit \
	clean clean-lisp clean-docs clean-archives \
	stats bump-version melpa-post-release \
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
	$(info make docs             - generate info manuals)
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
	$(info )
	$(info Release Management)
	$(info ==================)
	$(info )
	$(info make texi             - regenerate texi from org)
	$(info make stats            - regenerate statistics)
	$(info make authors          - regenerate AUTHORS.md)
	$(info make publish-stats    - publish statistics)
	$(info make publish-manuals  - publish snapshot manuals)
	$(info make release-manuals  - publish release manuals)
	$(info make dist             - create tarballs)
	$(info make bump-versions    - bump versions for release)
	$(info make bump-snapshots   - bump versions after release)
	@printf "\n"

## Build #############################################################

lisp:
	@$(MAKE) -C lisp lisp

docs:
	@$(MAKE) -C Documentation all

info:
	@$(MAKE) -C Documentation info

html:
	@$(MAKE) -C Documentation html

html-dir:
	@$(MAKE) -C Documentation html-dir

pdf:
	@$(MAKE) -C Documentation pdf

epub:
	@$(MAKE) -C Documentation epub

## Install ###########################################################

install: install-lisp install-docs

install-lisp: lisp
	@$(MAKE) -C lisp install

install-docs: docs
	@$(MAKE) -C Documentation install-docs

install-info: info
	@$(MAKE) -C Documentation install-info

## Test ##############################################################

test:
	@$(BATCH) --eval "(progn\
        $$suppress_warnings\
	(load-file \"t/magit-tests.el\")\
	(ert-run-tests-batch-and-exit))"

test-interactive:
	@$(EMACSBIN) -Q $(LOAD_PATH) --eval "(progn\
	(load-file \"t/magit-tests.el\")\
	(ert t))"

emacs-Q: clean-lisp
	@$(EMACSBIN) -Q $(LOAD_PATH) --debug-init --eval "(progn\
	(setq debug-on-error t)\
	(require 'magit)\
	(global-set-key \"\\C-xg\" 'magit-status))"

## Clean #############################################################

clean: clean-lisp clean-docs clean-archives
	@printf "Cleaning...\n"
	@$(RM) *.elc $(ELGS) # temporary cleanup kludge
	@$(RM) Documentation/*.texi~ Documentation/*.info-1 Documentation/*.info-2
	@$(RM) magit-pkg.el t/magit-tests.elc

clean-lisp:
	@$(MAKE) -C lisp clean

clean-docs:
	@$(MAKE) -C Documentation clean

clean-archives:
	@$(RM) *.tar.gz *.tar lisp/magit-version.el
	@$(RMDIR) magit-$(VERSION)

clean-all: clean clean-stats

clean-stats:
	@$(RMDIR) $(statsdir)

## Release management ################################################

texi:
	@$(MAKE) -C Documentation texi

stats:
	@$(MAKE) -C Documentation stats

authors:
	@$(MAKE) -C Documentation authors

publish-stats:
	@$(MAKE) -C Documentation publish-stats

publish-manuals:
	@$(MAKE) -C Documentation publish-manuals

release-manuals:
	@$(MAKE) -C Documentation release-manuals

dist: magit-$(VERSION).tar.gz

versionlib:
	@$(MAKE) -C lisp versionlib

DIST_ROOT_FILES = LICENSE default.mk Makefile README.md
DIST_LISP_FILES = $(addprefix lisp/,$(ELS) magit-version.el Makefile)
DIST_DOCS_FILES = $(addprefix Documentation/,$(TEXIPAGES) AUTHORS.md Makefile)
ifneq ("$(wildcard Documentation/RelNotes/$(VERSION).txt)","")
  DIST_DOCS_FILES += Documentation/RelNotes/$(VERSION).txt
endif

magit-$(VERSION).tar.gz: lisp versionlib info
	@printf "Packing $@\n"
	@$(MKDIR) magit-$(VERSION)
	@$(CP) $(DIST_ROOT_FILES) magit-$(VERSION)
	@$(MKDIR) magit-$(VERSION)/lisp
	@$(CP) $(DIST_LISP_FILES) magit-$(VERSION)/lisp
	@$(MKDIR) magit-$(VERSION)/Documentation
	@$(CP) $(DIST_DOCS_FILES) magit-$(VERSION)/Documentation
	@$(TAR) cz --mtime=./magit-$(VERSION) -f magit-$(VERSION).tar.gz magit-$(VERSION)
	@$(RMDIR) magit-$(VERSION)

define set_package_requires
(with-temp-file "lisp/git-commit.el"
  (insert-file-contents "lisp/git-commit.el")
  (re-search-forward "^;; Package-Requires: ")
  (delete-region (point) (line-end-position))
  (insert (format "%S"
`((emacs ,emacs-version) ;`
  (dash ,dash-version)
  (transient ,transient-version)
  (with-editor ,with-editor-version)))))
(with-temp-file "lisp/magit-libgit.el"
  (insert-file-contents "lisp/magit-libgit.el")
  (re-search-forward "^;; Package-Requires: ")
  (delete-region (point) (line-end-position))
  (insert (format "%S"
`((emacs "$(LIBGIT_EMACS_VERSION)") ;`
  (magit "$(LIBGIT_MAGIT_VERSION)")
  (libgit ,libgit-version)))))
(with-temp-file "lisp/magit-section.el"
  (insert-file-contents "lisp/magit-section.el")
  (re-search-forward "^;; Package-Requires: ")
  (delete-region (point) (line-end-position))
  (insert (format "%S"
`((emacs ,emacs-version) ;`
  (dash ,dash-version)))))
(with-temp-file "lisp/magit-pkg.el"
  (insert (pp-to-string
`(define-package "magit" "$(VERSION)" ;`
   "A Git porcelain inside Emacs."
   '((emacs ,emacs-version) ;'
     (async ,async-version)
     (dash ,dash-version)
     (git-commit ,git-commit-version)
     (magit-section ,magit-section-version)
     (transient ,transient-version)
     (with-editor ,with-editor-version))
   :keywords '("git" "tools" "vc")))) ;'
  (goto-char (point-min))
  (re-search-forward " \"A")
  (goto-char (match-beginning 0))
  (insert "\n "))
endef
export set_package_requires

bump-versions: bump-versions-1 texi
bump-versions-1:
	@$(BATCH) --eval "(let (\
	(emacs-version \"$(EMACS_VERSION)\")\
        (async-version \"$(ASYNC_VERSION)\")\
        (dash-version \"$(DASH_VERSION)\")\
        (git-commit-version \"$(GIT_COMMIT_VERSION)\")\
        (libgit-version \"$(LIBGIT_VERSION)\")\
        (magit-section-version \"$(MAGIT_SECTION_VERSION)\")\
        (transient-version \"$(TRANSIENT_VERSION)\")\
        (with-editor-version \"$(WITH_EDITOR_VERSION)\"))\
        $$set_package_requires)"

bump-snapshots:
	@$(BATCH) --eval "(let (\
	(emacs-version \"$(EMACS_VERSION)\")\
        (async-version \"$(ASYNC_MELPA_SNAPSHOT)\")\
        (dash-version \"$(DASH_MELPA_SNAPSHOT)\")\
        (git-commit-version \"$(GIT_COMMIT_MELPA_SNAPSHOT)\")\
        (libgit-version \"$(LIBGIT_MELPA_SNAPSHOT)\")\
        (magit-section-version \"$(MAGIT_SECTION_SNAPSHOT)\")\
        (transient-version \"$(TRANSIENT_MELPA_SNAPSHOT)\")\
        (with-editor-version \"$(WITH_EDITOR_MELPA_SNAPSHOT)\"))\
        $$set_package_requires)"
	@git commit -a -m "Reset Package-Requires for Melpa"
