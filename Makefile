-include config.mk
include default.mk

## ###################################################################

.PHONY: lisp docs \
	install install-lisp install-docs install-info \
	test test-interactive magit \
	clean clean-lisp clean-docs clean-archives \
	stats bump-versions bump-snapshots \
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
	$(info make check-declare    - check function declarations)
	$(info )
	$(info Release Management)
	$(info ==================)
	$(info )
	$(info make texi             - regenerate texi from org)
	$(info make authors          - regenerate AUTHORS.md)
	$(info make publish          - publish snapshot manuals)
	$(info make release          - publish release manuals)
	$(info make dist             - create tarballs)
	$(info make bump-versions    - bump versions for release)
	$(info make bump-snapshots   - bump versions after release)
	$(info make stats            - regenerate statistics)
	$(info make stats-upload     - publish statistics)
	@printf "\n"

## Build #############################################################

lisp:
	@$(MAKE) -C lisp lisp

docs:
	@$(MAKE) -C docs all

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
	@$(RM) magit-pkg.el t/magit-tests.elc

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

texi:
	@$(MAKE) -C docs texi

authors:
	@$(MAKE) -C docs authors
	@git commit --gpg-sign -m "AUTHORS.md: Update list of contributors" \
	-o -- docs/AUTHORS.md .mailmap
	@git show --pretty= -p HEAD

publish:
	@$(MAKE) -C docs publish

release:
	@$(MAKE) -C docs release

dist: magit-$(VERSION).tar.gz

versionlib:
	@$(MAKE) -C lisp versionlib

DIST_ROOT_FILES = LICENSE default.mk Makefile README.md
DIST_LISP_FILES = $(addprefix lisp/,$(ELS) magit-version.el Makefile)
DIST_DOCS_FILES = $(addprefix docs/,$(TEXIPAGES) AUTHORS.md Makefile)
ifneq ("$(wildcard docs/RelNotes/$(VERSION).txt)","")
  DIST_DOCS_FILES += docs/RelNotes/$(VERSION).txt
endif

magit-$(VERSION).tar.gz: lisp versionlib info
	@printf "Packing $@\n"
	@$(MKDIR) magit-$(VERSION)
	@$(CP) $(DIST_ROOT_FILES) magit-$(VERSION)
	@$(MKDIR) magit-$(VERSION)/lisp
	@$(CP) $(DIST_LISP_FILES) magit-$(VERSION)/lisp
	@$(MKDIR) magit-$(VERSION)/docs
	@$(CP) $(DIST_DOCS_FILES) magit-$(VERSION)/docs
	@$(TAR) cz --mtime=./magit-$(VERSION) -f magit-$(VERSION).tar.gz magit-$(VERSION)
	@$(RMDIR) magit-$(VERSION)

define set_package_requires_nongnu

(with-temp-file "lisp/git-commit.el"
  (insert-file-contents "lisp/git-commit.el")
  (re-search-forward "^;; Package-Requires: ")
  (delete-region (point) (line-end-position))
  (insert (format "%S"
`((emacs ,emacs-version) ;`
  (dash ,dash-version)
  (transient ,transient-version)
  (with-editor ,with-editor-version))))
  (re-search-forward "^;; Package-Version: ")
  (delete-region (point) (line-end-position))
  (insert "$(GIT_COMMIT_VERSION)"))

(with-temp-file "lisp/magit.el"
  (insert-file-contents "lisp/magit.el")
  (re-search-forward "^;; Package-Requires: ")
  (delete-region (point) (line-end-position))
  (insert (format "%S"
`((emacs ,emacs-version) ;`
  (dash ,dash-version)
  (git-commit ,git-commit-version)
  (magit-section ,magit-section-version)
  (transient ,transient-version)
  (with-editor ,with-editor-version))))
  (re-search-forward "^;; Package-Version: ")
  (delete-region (point) (line-end-position))
  (insert "$(MAGIT_SECTION_VERSION)"))

(with-temp-file "lisp/magit-libgit.el"
  (insert-file-contents "lisp/magit-libgit.el")
  (re-search-forward "^;; Package-Requires: ")
  (delete-region (point) (line-end-position))
  (insert (format "%S"
`((emacs "$(LIBGIT_EMACS_VERSION)") ;`
  (libgit ,libgit-version)
  (magit ,magit-version))))
  (re-search-forward "^;; Package-Version: ")
  (delete-region (point) (line-end-position))
  (insert "$(MAGIT_LIBGIT_VERSION)"))

(with-temp-file "lisp/magit-section.el"
  (insert-file-contents "lisp/magit-section.el")
  (re-search-forward "^;; Package-Requires: ")
  (delete-region (point) (line-end-position))
  (insert (format "%S"
`((emacs ,emacs-version) ;`
  (dash ,dash-version))))
  (re-search-forward "^;; Package-Version: ")
  (delete-region (point) (line-end-position))
  (insert "$(MAGIT_SECTION_VERSION)"))
endef
export set_package_requires_nongnu

define set_package_requires_melpa

(with-temp-file "lisp/git-commit-pkg.el"
  (insert (format
"(define-package \"git-commit\" \"$(GIT_COMMIT_VERSION)$(DEV_SUFFIX)\"
  \"Edit Git commit messages.\"
  '((emacs %S)
    (transient %S)
    (with-editor %S))
  :homepage \"https://magit.vc\"
  :keywords '(\"git\" \"tools\" \"vc\"))
"   emacs-version
    dash-version
    transient-version
    with-editor-version)))

(with-temp-file "lisp/magit-pkg.el"
  (insert (format
"(define-package \"magit\" \"$(MAGIT_VERSION)$(DEV_SUFFIX)\"
  \"A Git porcelain inside Emacs.\"
  '((emacs %S)
    (dash %S)
    (git-commit %S)
    (magit-section %S)
    (transient %S)
    (with-editor %S))
  :homepage \"https://magit.vc\"
  :keywords '(\"git\" \"tools\" \"vc\"))
"   emacs-version
    dash-version
    git-commit-version
    magit-section-version
    transient-version
    with-editor-version)))

(with-temp-file "lisp/magit-libgit-pkg.el"
  (insert (format
"(define-package \"magit-libgit\" \"$(MAGIT_LIBGIT_VERSION)$(DEV_SUFFIX)\"
  \".\"
  '((emacs %S)
    (libgit %S)
    (magit %S))
  :homepage \"https://magit.vc\"
  :keywords '(\"git\" \"tools\" \"vc\"))
"   emacs-version
    libgit-version
    magit-version)))

(with-temp-file "lisp/magit-section-pkg.el"
  (insert (format
"(define-package \"magit-section\" \"$(MAGIT_SECTION_VERSION)$(DEV_SUFFIX)\"
  \"Sections for read-only buffers\"
  '((emacs %S)
    (dash %S))
  :homepage \"https://magit.vc\"
  :keywords '(\"tools\"))
"   emacs-version
    dash-version)))
endef
export set_package_requires_melpa

define set_package_versions
(emacs-version "$(EMACS_VERSION)")
(dash-version "$(DASH_VERSION)")
(git-commit-version "$(GIT_COMMIT_VERSION)")
(libgit-version "$(LIBGIT_VERSION)")
(magit-version "$(MAGIT_VERSION)")
(magit-libgit-version "$(MAGIT_LIBGIT_VERSION)")
(magit-section-version "$(MAGIT_SECTION_VERSION)")
(transient-version "$(TRANSIENT_VERSION)")
(with-editor-version "$(WITH_EDITOR_VERSION)")
endef
export set_package_versions

define set_package_snapshots
(emacs-version "$(EMACS_VERSION)")
(dash-version "$(DASH_MELPA_SNAPSHOT)")
(git-commit-version "$(GIT_COMMIT_MELPA_SNAPSHOT)")
(libgit-version "$(LIBGIT_MELPA_SNAPSHOT)")
(magit-version "$(MAGIT_MELPA_SNAPSHOT)")
(magit-libgit-version "$(MAGIT_LIBGIT_MELPA_SNAPSHOT)")
(magit-section-version "$(MAGIT_SECTION_MELPA_SNAPSHOT)")
(transient-version "$(TRANSIENT_MELPA_SNAPSHOT)")
(with-editor-version "$(WITH_EDITOR_MELPA_SNAPSHOT)")
endef
export set_package_snapshots

bump-versions: _bump-versions texi
_bump-versions:
	@$(BATCH) --eval "(let (\
        $$set_package_versions)\
        $$set_package_requires_nongnu\
        $$set_package_requires_melpa)"

bump-snapshots:
	@$(eval DEV_SUFFIX := -git)
	@$(BATCH) --eval "(let (\
        $$set_package_versions)\
        $$set_package_requires_nongnu)"
	@$(BATCH) --eval "(let (\
        $$set_package_snapshots)\
        $$set_package_requires_melpa)"
	@git commit -a --gpg-sign -m "Reset Package-Requires for Melpa"
	@git show --pretty= -p HEAD

## Statistics ########################################################

stats:
	@$(MAKE) -C docs stats

stats-upload:
	@$(MAKE) -C docs stats-upload

