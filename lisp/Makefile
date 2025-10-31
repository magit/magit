-include ../config.mk
include ../default.mk

## ###################################################################

.PHONY: magit-version.el check-declare install

all: lisp

## Build order #######################################################

magit-section.elc:
magit-base.elc:            magit-section.elc
magit-git.elc:             magit-base.elc
magit-mode.elc:            magit-base.elc magit-git.elc
magit-margin.elc:          magit-base.elc magit-mode.elc
magit-process.elc:         magit-base.elc magit-git.elc magit-mode.elc
git-commit.elc:            magit-git.elc magit-mode.elc magit-process.elc
magit-transient.elc:       magit-git.elc magit-mode.elc magit-process.elc
magit-autorevert.elc:      magit-git.elc magit-process.elc
magit-core.elc:            magit-base.elc magit-git.elc magit-mode.elc \
                           magit-margin.elc magit-process.elc \
                           magit-transient.elc magit-autorevert.elc
magit-diff.elc:            git-commit.elc magit-core.elc
magit-log.elc:             magit-core.elc magit-diff.elc
magit-wip.elc:             magit-core.elc magit-log.elc
magit-reflog.elc:          magit-core.elc magit-log.elc
magit-apply.elc:           magit-core.elc magit-diff.elc magit-wip.elc
magit-repos.elc:           magit-core.elc
magit.elc:                 git-commit.elc magit-core.elc magit-diff.elc \
                           magit-log.elc magit-apply.elc magit-repos.elc
magit-status.elc:          magit.elc
magit-refs.elc:            magit.elc
magit-files.elc:           magit.elc
magit-reset.elc:           magit.elc
magit-branch.elc:          magit.elc magit-reset.elc
magit-merge.elc:           magit.elc magit-diff.elc
magit-tag.elc:             magit.elc
magit-worktree.elc:        magit.elc
magit-notes.elc:           magit.elc
magit-sequence.elc:        magit.elc
magit-commit.elc:          magit.elc magit-sequence.elc
magit-remote.elc:          magit.elc
magit-clone.elc:           magit.elc
magit-fetch.elc:           magit.elc
magit-pull.elc:            magit.elc magit-remote.elc
magit-push.elc:            magit.elc
magit-bisect.elc:          magit.elc
magit-stash.elc:           magit.elc magit-sequence.elc magit-reflog.elc
magit-blame.elc:           magit.elc
magit-submodule.elc:       magit.elc
magit-patch.elc:           magit.elc
magit-subtree.elc:         magit.elc
magit-ediff.elc:           magit.elc
magit-gitignore.elc:       magit.elc
magit-sparse-checkout.elc: magit.elc
magit-bundle.elc:          magit.elc
magit-extras.elc:          magit.elc magit-merge.elc
magit-dired.elc:           magit.elc
git-rebase.elc:            magit.elc
magit-bookmark.elc:        magit.elc

## Build #############################################################

lisp:       $(ELCS) autoloads $(LISP_EXTRA_TARGETS)
autoloads:   $(PKG)-autoloads.el
versionlib: $(PKG)-version.el

%.elc: %.el
	@printf "Compiling $<\n"
	@$(BATCH) --eval "(progn\
	(when (file-exists-p \"$@\")\
	  (delete-file \"$@\"))\
	(setq with-editor-emacsclient-executable nil)\
	(when (< emacs-major-version 30)\
	  (require 'transient)))" \
	-f batch-byte-compile $<

$(PKG)-autoloads.el: $(ELS)
	@printf " Creating $@\n"
	@$(EMACS) -Q --batch -l autoload --eval "\
(let* ((file (expand-file-name \"$@\"))\
       (generated-autoload-file file)\
       (coding-system-for-write 'utf-8-emacs-unix)\
       (backup-inhibited t)\
       (version-control 'never)\
       (inhibit-message t))\
  (write-region (autoload-rubric file \"package\" t) nil file)\
  (update-directory-autoloads default-directory))" \
	2>&1 | sed "/^Package autoload is deprecated$$/d"

$(PKG)-version.el:
	@printf " Creating $@\n"
	@printf "%s" "$$VERSIONLIB_TMPL" > $@

check-declare:
	@printf "Checking function declarations\n"
	@$(BATCH) -L $(TOP)../borg -L $(TOP)../forge/lisp \
	--eval "(check-declare-directory default-directory)"

## Install ###########################################################

install: lisp versionlib
	@$(MKDIR) $(DESTDIR)$(lispdir)
	$(CP) $(ELS) magit-autoloads.el magit-version.el $(ELCS) $(DESTDIR)$(lispdir)

## Clean #############################################################

clean:
	@printf " Cleaning lisp/*...\n"
	@$(RM) *.elc $(ELGS)
	@$(RM) ../test/magit-tests.elc

## Templates #########################################################

define VERSIONLIB_TMPL
;;; $(PKG)-version.el --- The Magit version you are using  -*- lexical-binding:t -*-

(setq $(PKG)-version "$(VERSION)")

(provide '$(PKG)-version)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; $(PKG)-version.el ends here

endef
export VERSIONLIB_TMPL
#'
