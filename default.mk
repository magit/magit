TOP := $(dir $(lastword $(MAKEFILE_LIST)))

## User options ######################################################
#
# You can override these settings in "config.mk" or on the command
# line.
#
# You might also want to set LOAD_PATH.  If you do, then it must
# contain "-L .".
#
# If you don't do so, then the default is set in the "Load-Path"
# section below.  The default assumes that all dependencies are
# installed either at "../<DEPENDENCY>", or when using package.el
# at "ELPA_DIR/<DEPENDENCY>-<HIGHEST-VERSION>".

PREFIX   ?= /usr/local
sharedir ?= $(PREFIX)/share
lispdir  ?= $(sharedir)/emacs/site-lisp/magit
infodir  ?= $(sharedir)/info
docdir   ?= $(sharedir)/doc/magit
statsdir ?= $(TOP)/Documentation/stats

CP       ?= install -p -m 644
MKDIR    ?= install -p -m 755 -d
RMDIR    ?= rm -rf
TAR      ?= tar
SED      ?= sed

EMACSBIN ?= emacs
BATCH     = $(EMACSBIN) -Q --batch $(LOAD_PATH)

INSTALL_INFO     ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO         ?= makeinfo
MANUAL_HTML_ARGS ?= --css-ref /assets/page.css

## Files #############################################################

PKG              = magit
PACKAGES         = magit magit-popup git-commit
PACKAGE_VERSIONS = $(addsuffix -$(VERSION),$(PACKAGES))

TEXIPAGES = $(addsuffix .texi,$(filter-out git-commit,$(PACKAGES)))
INFOPAGES = $(addsuffix .info,$(filter-out git-commit,$(PACKAGES)))
HTMLFILES = $(addsuffix .html,$(filter-out git-commit,$(PACKAGES)))
HTMLDIRS  = $(filter-out git-commit,$(PACKAGES))
PDFFILES  = $(addsuffix .pdf,$(filter-out git-commit,$(PACKAGES)))

ELS  = git-commit.el
ELS += magit-popup.el
ELS += magit-utils.el
ELS += magit-section.el
ELS += magit-git.el
ELS += magit-mode.el
ELS += magit-margin.el
ELS += magit-process.el
ELS += magit-autorevert.el
ELS += magit-core.el
ELS += magit-diff.el
ELS += magit-log.el
ELS += magit-wip.el
ELS += magit-apply.el
ELS += magit-repos.el
ELS += magit.el
ELS += magit-status.el
ELS += magit-refs.el
ELS += magit-files.el
ELS += magit-branch.el
ELS += magit-worktree.el
ELS += magit-notes.el
ELS += magit-obsolete.el
ELS += magit-sequence.el
ELS += magit-commit.el
ELS += magit-remote.el
ELS += magit-bisect.el
ELS += magit-stash.el
ELS += magit-blame.el
ELS += magit-submodule.el
ELS += magit-subtree.el
ELS += magit-ediff.el
ELS += magit-extras.el
ELS += git-rebase.el
ELS += magit-imenu.el
ELS += magit-bookmark.el
ELCS = $(ELS:.el=.elc)
ELMS = magit.el $(filter-out $(addsuffix .el,$(PACKAGES)),$(ELS))
ELGS = magit-autoloads.el magit-version.el

## Versions ##########################################################

VERSION := $(shell \
  test -e $(TOP).git\
  && git describe --tags --dirty 2> /dev/null\
  || $(BATCH) --eval "(progn\
  (fset 'message (lambda (&rest _)))\
  (load-file \"magit-version.el\")\
  (princ magit-version))")

MAGIT_VERSION       = 2.11
ASYNC_VERSION       = 1.9.2
DASH_VERSION        = 2.13.0
WITH_EDITOR_VERSION = 2.6.0
GIT_COMMIT_VERSION  = 2.10.3
MAGIT_POPUP_VERSION = 2.10.3

ASYNC_MELPA_SNAPSHOT       = 20170823
DASH_MELPA_SNAPSHOT        = 20170810
WITH_EDITOR_MELPA_SNAPSHOT = 20170817
GIT_COMMIT_MELPA_SNAPSHOT  = 20170823
MAGIT_POPUP_MELPA_SNAPSHOT = 20170824

EMACS_VERSION = 24.4

EMACSOLD := $(shell $(BATCH) --eval \
  "(and (version< emacs-version \"$(EMACS_VERSION)\") (princ \"true\"))")
ifeq "$(EMACSOLD)" "true"
  $(error At least version $(EMACS_VERSION) of Emacs is required)
endif

## Load-Path #########################################################

ifndef LOAD_PATH

ELPA_DIR ?= $(HOME)/.emacs.d/elpa

DASH_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/dash-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(DASH_DIR)" ""
  DASH_DIR = $(TOP)../dash
endif

WITH_EDITOR_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/with-editor-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(WITH_EDITOR_DIR)" ""
  WITH_EDITOR_DIR = $(TOP)../with-editor
endif

SYSTYPE := $(shell $(EMACSBIN) -Q --batch --eval "(princ system-type)")
ifeq ($(SYSTYPE), windows-nt)
  CYGPATH := $(shell cygpath --version 2>/dev/null)
endif

LOAD_PATH = -L $(TOP)/lisp

ifdef CYGPATH
  LOAD_PATH += -L $(shell cygpath --mixed $(DASH_DIR))
  LOAD_PATH += -L $(shell cygpath --mixed $(WITH_EDITOR_DIR))
else
  LOAD_PATH += -L $(DASH_DIR)
  LOAD_PATH += -L $(WITH_EDITOR_DIR)
endif

endif # ifndef LOAD_PATH

DOC_LOAD_PATH  ?= $(LOAD_PATH) \
-L ../../org/lisp -L ../../org/contrib/lisp -L ../../ox-texinfo+
