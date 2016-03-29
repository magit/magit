include default.mk
-include config.mk

.PHONY: lisp \
	install install-lisp install-docs install-info \
	test test-interactive magit \
	clean clean-lisp clean-docs clean-archives \
	genstats bump-version melpa-post-release \
	dist magit-$(VERSION).tar.gz

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
	$(info make emacs-Q          - run emacs -Q plus Magit)
	$(info )
	$(info Release Managment)
	$(info =================)
	$(info )
	$(info make texi             - regenerate texi from org)
	$(info make genstats         - regenerate statistics)
	$(info make authors          - regenerate AUTHORS.md)
	$(info make dist             - create tarballs)
	$(info make bump-versions    - bump versions for release)
	$(info make bump-snapshots   - bump versions after release)
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

emacs-Q: clean-lisp
	@$(EMACSBIN) -Q $(LOAD_PATH) --debug-init --eval "(progn\
	(setq debug-on-error t)\
	(require 'magit)\
	(global-set-key \"\\C-xg\" 'magit-status))"

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

define set_package_requires
(require 'dash)
(dolist (lib (list "git-commit" "magit-popup" "magit"))
  (with-current-buffer (find-file-noselect (format "lisp/%s.el" lib))
    (goto-char (point-min))
    (re-search-forward "^;; Package-Requires: ")
    (let ((s (read (buffer-substring (point) (line-end-position)))))
      (--when-let (assq 'async       s) (setcdr it (list async-version)))
      (--when-let (assq 'dash        s) (setcdr it (list dash-version)))
      (--when-let (assq 'with-editor s) (setcdr it (list with-editor-version)))
      (--when-let (assq 'git-commit  s) (setcdr it (list git-commit-version)))
      (--when-let (assq 'magit-popup s) (setcdr it (list magit-popup-version)))
      (delete-region (point) (line-end-position))
      (insert (format "%S" s))
      (save-buffer))))
endef
export set_package_requires

define set_manual_version
(let ((version (split-string "$(MAGIT_VERSION)" "\\.")))
  (setq version (concat (car version) "." (cadr version)))
  (dolist (file (list "magit-popup" "magit"))
    (with-current-buffer (find-file-noselect (format "Documentation/%s.org" file))
      (goto-char (point-min))
      (re-search-forward "^#\\+SUBTITLE: for version ")
      (delete-region (point) (line-end-position))
      (insert version)
      (save-buffer))))
endef
export set_manual_version

bump-versions: bump-versions-1 texi
bump-versions-1:
	@$(BATCH) --eval "(progn\
        (setq async-version \"$(ASYNC_VERSION)\")\
        (setq dash-version \"$(DASH_VERSION)\")\
        (setq with-editor-version \"$(WITH_EDITOR_VERSION)\")\
        (setq git-commit-version \"$(GIT_COMMIT_VERSION)\")\
        (setq magit-popup-version \"$(MAGIT_POPUP_VERSION)\")\
        $$set_package_requires\
        $$set_manual_version)"

bump-snapshots:
	@$(BATCH) --eval "(progn\
        (setq async-version \"$(ASYNC_MELPA_SNAPSHOT)\")\
        (setq dash-version \"$(DASH_MELPA_SNAPSHOT)\")\
        (setq with-editor-version \"$(WITH_EDITOR_MELPA_SNAPSHOT)\")\
        (setq git-commit-version \"$(GIT_COMMIT_MELPA_SNAPSHOT)\")\
        (setq magit-popup-version \"$(MAGIT_POPUP_MELPA_SNAPSHOT)\")\
        $$set_package_requires)"
	git commit -a -m "reset Package-Requires for Melpa"
