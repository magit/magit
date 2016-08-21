TOP := $(dir $(lastword $(MAKEFILE_LIST)))

PREFIX   ?= /usr/local
sharedir ?= $(PREFIX)/share
lispdir  ?= $(sharedir)/emacs/site-lisp/magit
infodir  ?= $(sharedir)/info
docdir   ?= $(sharedir)/doc/magit
statsdir ?= ./stats

# You might also want to set LOAD_PATH.  If you do, then it must
# contain "-L .".  If you don't then the default is set, assuming
# that all dependencies are installed either at ../<DEPENDENCY>,
# or using package.el at ELPA_DIR/<DEPENDENCY>-<HIGHEST-VERSION>.

CP    ?= install -p -m 644
MKDIR ?= install -p -m 755 -d
RMDIR ?= rm -rf
TAR   ?= tar
SED   ?= sed

PACKAGES = git-commit magit-popup magit
PACKAGE_VERSIONS = $(addsuffix -$(VERSION),$(PACKAGES))

INFOPAGES = $(addsuffix .info,$(filter-out git-commit,$(PACKAGES)))
TEXIPAGES = $(addsuffix .texi,$(filter-out git-commit,$(PACKAGES)))

ELS  = git-commit.el
ELS += magit-popup.el
ELS += magit-utils.el
ELS += magit-section.el
ELS += magit-git.el
ELS += magit-autorevert.el
ELS += magit-mode.el
ELS += magit-process.el
ELS += magit-core.el
ELS += magit-diff.el
ELS += magit-wip.el
ELS += magit-apply.el
ELS += magit-log.el
ELS += magit.el
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
ELCS = $(ELS:.el=.elc)
ELMS = magit.el $(filter-out $(addsuffix .el,$(PACKAGES)),$(ELS))
ELGS = magit-autoloads.el magit-version.el

EMACS_VERSION = 24.4

MAGIT_VERSION       = 2.8
ASYNC_VERSION       = 1.9
DASH_VERSION        = 2.13.0
WITH_EDITOR_VERSION = 2.5.1
GIT_COMMIT_VERSION  = 2.7.0
MAGIT_POPUP_VERSION = 2.7.0

ASYNC_MELPA_SNAPSHOT       = 20160711.223
DASH_MELPA_SNAPSHOT        = 20160820.501
WITH_EDITOR_MELPA_SNAPSHOT = 20160812.1457
GIT_COMMIT_MELPA_SNAPSHOT  = 20160519.950
MAGIT_POPUP_MELPA_SNAPSHOT = 20160813.642

EMACSBIN ?= emacs

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

BATCH = $(EMACSBIN) -batch -Q $(LOAD_PATH)

EMACSOLD := $(shell $(BATCH) --eval \
  "(and (version< emacs-version \"$(EMACS_VERSION)\") (princ \"true\"))")
ifeq "$(EMACSOLD)" "true"
  $(error At least version $(EMACS_VERSION) of Emacs is required)
endif

VERSION := $(shell \
  test -e $(TOP).git\
  && git describe --tags --dirty 2> /dev/null\
  || $(BATCH) --eval "(progn\
  (fset 'message (lambda (&rest _)))\
  (load-file \"magit-version.el\")\
  (princ magit-version))")
