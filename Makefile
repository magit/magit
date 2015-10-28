include default.mk
-include config.mk

.PHONY: lisp \
	install install-lisp install-docs install-info \
	test test-interactive magit \
	clean clean-lisp clean-docs clean-archives \
	genstats bump-version melpa-post-release \
	dist magit-$(VERSION).tar.gz elpa $(ELPA_ARCHIVES)

all: lisp docs

help:
	$(info )
	$(info Current version: magit-$(VERSION))
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
	$(info make magit            - run emacs -Q plus Magit)
	$(info )
	$(info Release Managment)
	$(info =================)
	$(info )
	$(info make texi             - regenerate texi from org)
	$(info make genstats         - regenerate statistics)
	$(info make authors          - regenerate AUTHORS.md)
	$(info make dist             - create tarballs)
	$(info make elpa             - create elpa tarballs)
	$(info make VERSION=... bump-version)
	$(info make VERSION=... melpa-post-release)
	$(info -                     - fixup version strings)
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

magit: clean-lisp
	@$(EMACSBIN) -Q $(LOAD_PATH) --eval "(progn\
	(require 'magit)\
	(global-set-key \"\\C-xg\" 'magit-status)\
	(tool-bar-mode 0)\
	(menu-bar-mode 0)\
	(scroll-bar-mode 0))"

clean: clean-lisp clean-docs clean-archives
	@printf "Cleaning...\n"
	@$(RM) $(ELCS) $(ELGS) # temporary cleanup kludge
	@$(RM) Documentation/*.texi~ Documentation/*.info-1 Documentation/*.info-2
	@$(RM) magit-pkg.el t/magit-tests.elc

clean-lisp:
	@$(MAKE) -C lisp clean

clean-docs:
	@$(MAKE) -C Documentation clean

clean-archives:
	@$(RM) git-commit-*.el *.tar.gz *.tar
	@$(RMDIR) magit-$(VERSION)

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
ifneq ("$(wildcard Documentation/RelNotes/$(VERSION).txt)","")
  DIST_DOCS_FILES += Documentation/RelNotes/$(VERSION).txt
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
	@marmalade-upload git-commit-$(VERSION).el
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
  '((emacs "$(EMACS_VERSION)")
    (async "$(ASYNC_VERSION)")
    (dash "$(DASH_VERSION)")))
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
	@$(SED) -i git-commit-$(VERSION).el \
	  -e "s/^;; Keywords:/;; Package-Version: $(VERSION)\n;; Keywords:/"

define magit_popup_pkg
(define-package "magit-popup" "$(VERSION)"
  "Define prefix-infix-suffix command combos"
  '((emacs "$(EMACS_VERSION)")
    (async "$(ASYNC_VERSION)")
    (dash "$(DASH_VERSION)")))
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
  '((emacs "$(EMACS_VERSION)")
    (async "$(ASYNC_VERSION)")
    (dash "$(DASH_VERSION)")
    (with-editor "$(VERSION)")
    (git-commit "$(VERSION)")
    (magit-popup "$(VERSION)")))
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

define set_package_requires
(require 'dash)
(dolist (lib (list "with-editor" "git-commit" "magit-popup" "magit"))
  (with-current-buffer (find-file-noselect (format "lisp/%s.el" lib))
    (goto-char (point-min))
    (re-search-forward "^;; Package-Requires: ")
    (let ((s (read (buffer-substring (point) (line-end-position)))))
      (--when-let (assq 'async       s) (setcdr it (list async-version)))
      (--when-let (assq 'dash        s) (setcdr it (list dash-version)))
      (--when-let (assq 'with-editor s) (setcdr it (list "$(VERSION)")))
      (--when-let (assq 'git-commit  s) (setcdr it (list "$(VERSION)")))
      (--when-let (assq 'magit-popup s) (setcdr it (list "$(VERSION)")))
      (delete-region (point) (line-end-position))
      (insert (format "%S" s))
      (save-buffer))))
endef
# '
export set_package_requires

define set_manual_version
(let ((version (split-string "$(VERSION)" "\\.")))
  (setq version (concat (car version) "." (cadr version)))
  (dolist (file (list "with-editor" "magit-popup" "magit"))
    (with-current-buffer (find-file-noselect (format "Documentation/%s.org" file))
      (goto-char (point-min))
      (re-search-forward "^#\\+SUBTITLE: for version ")
      (delete-region (point) (line-end-position))
      (insert version)
      (save-buffer))))
endef
#'
export set_manual_version

bump-version:
	@$(BATCH) --eval "(progn\
        (setq async-version \"$(ASYNC_VERSION)\")\
        (setq dash-version \"$(DASH_VERSION)\")\
        $$set_package_requires\
        $$set_manual_version)"

melpa-post-release:
	@$(BATCH) --eval "(progn\
        (setq async-version \"$(ASYNC_MELPA_SNAPSHOT)\")\
        (setq dash-version \"$(DASH_MELPA_SNAPSHOT)\")\
        $$set_package_requires)"
