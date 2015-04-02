PREFIX      ?= /usr/local
datarootdir ?= $(PREFIX)/share
lispdir     ?= $(datarootdir)/emacs/site-lisp/magit
infodir     ?= $(datarootdir)/info
docdir      ?= $(datarootdir)/doc/magit

statsdir    ?= $(HOME)/Repos/magit/page/stats

LOADDEFS_FILE ?= magit-autoloads.el
LOADDEFS_DIR  ?= $(lispdir)

ELS  = with-editor.el
ELS += git-commit.el
ELS += git-rebase.el
ELS += magit-popup.el
ELS += magit-utils.el
ELS += magit-section.el
ELS += magit-git.el
ELS += magit-mode.el
ELS += magit-process.el
ELS += magit-core.el
ELS += magit-diff.el
ELS += magit-apply.el
ELS += magit-log.el
ELS += magit-wip.el
ELS += magit.el
ELS += magit-sequence.el
ELS += magit-stash.el
ELS += magit-backup.el
ELS += magit-commit.el
ELS += magit-remote.el
ELS += magit-bisect.el
ELS += magit-blame.el
ELS += magit-ediff.el
ELS += magit-extras.el
ELCS = $(ELS:.el=.elc)

CP    ?= install -p -m 644
MKDIR ?= install -p -m 755 -d
RMDIR ?= rm -rf

MAKEINFO     ?= makeinfo
INSTALL_INFO ?= $(shell \
  hash ginstall-info 2> /dev/null\
  && printf ginstall-info\
  || printf install-info)

ELPA_DIR ?= ~/.emacs.d/elpa

CL_LIB_DIR ?= $(shell \
  find $(ELPA_DIR) -maxdepth 1 -regex '.*/cl-lib-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(CL_LIB_DIR)" ""
  CL_LIB_DIR = ../cl-lib
endif

DASH_DIR ?= $(shell \
  find $(ELPA_DIR) -maxdepth 1 -regex '.*/dash-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(DASH_DIR)" ""
  DASH_DIR = ../dash
endif

LOAD_PATH ?= -L . -L $(CL_LIB_DIR) -L $(DASH_DIR)

EMACSBIN ?= emacs
BATCH    = $(EMACSBIN) -batch -Q $(LOAD_PATH)

VERSION=$(shell \
  test -e .git\
  && git describe --tags --dirty 2> /dev/null\
  || $(BATCH) -eval "(progn\
  (fset 'message (lambda (&rest _)))\
  (load-file \"magit-version.el\")\
  (princ magit-version))")

.PHONY: all
all: lisp

.PHONY: lisp
lisp: $(ELCS) loaddefs magit-version.el

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
	$(info )
	$(info Web Page)
	$(info ========)
	$(info )
	$(info make stats            - generate stats)
	@printf "\n"

%.elc: %.el
	@printf "Compiling %s\n" $<
	@$(BATCH) -eval "(progn\
	(setq with-editor-emacsclient-executable nil)\
	(fset 'message* (symbol-function 'message))\
	(fset 'message  (lambda (f &rest a)\
	                  (unless (equal f \"Wrote %s\")\
	                    (apply 'message* f a)))))" \
	-f batch-byte-compile $<

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

.PHONY: loaddefs
loaddefs: $(LOADDEFS_FILE)

$(LOADDEFS_FILE): $(ELS)
	@printf "Generating magit-autoloads.el\n"
	@$(BATCH) -eval "(progn\
	(fset 'message (lambda (&rest _)))\
	(setq vc-handled-backends nil)\
	(defvar generated-autoload-file nil)\
	(let ((generated-autoload-file \"$(CURDIR)/$(LOADDEFS_FILE)\")\
	      (make-backup-files nil))\
	  (update-directory-autoloads \".\")))"

%.info: %.texi
	@$(MAKEINFO) $< -o $@

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

.PHONY: install-lisp
install-lisp: lisp
	$(MKDIR) $(DESTDIR)$(lispdir)
	$(CP) $(ELS) $(ELCS) magit-version.el $(DESTDIR)$(lispdir)
	$(MKDIR) $(DESTDIR)$(LOADDEFS_DIR)
	$(CP) $(LOADDEFS_FILE) $(DESTDIR)$(LOADDEFS_DIR)/$(LOADDEFS_FILE)

.PHONY: install-docs
install-docs:
	$(MKDIR) $(DESTDIR)$(docdir)
	$(CP) AUTHORS.md $(DESTDIR)$(docdir)

.PHONY: test
test:
	@$(BATCH) -l t/magit-tests.el -f ert-run-tests-batch-and-exit

.PHONY: test-interactive
test-interactive:
	@$(EMACSBIN) -Q $(LOAD_PATH) --eval "\
	(progn (load-file \"t/magit-tests.el\") (ert t))"

.PHONY: clean
clean:
	@printf "Cleaning...\n"
	@$(RM) $(ELCS) $(LOADDEFS_FILE) magit-version.el *.tar.gz *.tar dir
	@$(RMDIR) magit-$(VERSION)

DIST_FILES = $(ELS) magit-version.el Makefile AUTHORS.md README.md COPYING

.PHONY: dist
dist: magit-$(VERSION).tar.gz
magit-$(VERSION).tar.gz: $(DIST_FILES)
	@printf "Packing $@\n"
	@$(MKDIR) magit-$(VERSION)
	@$(CP) $(DIST_FILES) magit-$(VERSION)
	@tar -cz --mtime=./magit-$(VERSION) -f magit-$(VERSION).tar.gz magit-$(VERSION)
	@$(RMDIR) magit-$(VERSION)

ELPA_FILES = $(ELS) magit-pkg.el AUTHORS.md

.PHONY: marmalade-upload marmalade
marmalade-upload: magit-$(VERSION).tar
	@marmalade-upload
marmalade: magit-$(VERSION).tar
magit-$(VERSION).tar: $(ELPA_FILES)
	@printf "Packing $@\n"
	@$(MKDIR) magit-$(VERSION)
	@$(CP) $(ELPA_FILES) magit-$(VERSION)
	@tar -c --mtime=./magit-$(VERSION) -f magit-$(VERSION).tar magit-$(VERSION)
	@$(RMDIR) magit-$(VERSION)

.PHONY: stats
stats:
	@printf "Generating stats\n"
	@gitstats -c style=/css/stats.css -c max_authors=200 . $(statsdir)
