-include ../config.mk
include ../default.mk

## ###################################################################

.PHONY: magit-version.el install

all: lisp

## Build order #######################################################

git-commit.elc:
magit-utils.elc:
magit-section.elc:	magit-utils.elc
magit-git.elc:		magit-utils.elc magit-section.elc
magit-mode.elc:		magit-section.elc magit-git.elc
magit-margin.elc:	magit-section.elc magit-mode.elc
magit-process.elc:	magit-utils.elc magit-section.elc \
			magit-git.elc magit-mode.elc
magit-autorevert.elc:	magit-git.elc magit-process.elc
magit-core.elc:		magit-margin.elc magit-utils.elc \
			magit-section.elc magit-git.elc magit-mode.elc \
			magit-process.elc magit-autorevert.elc
magit-diff.elc:		git-commit.elc magit-core.elc
magit-log.elc:		magit-core.elc magit-diff.elc
magit-wip.elc:		magit-core.elc magit-log.elc
magit-apply.elc:	magit-core.elc magit-diff.elc magit-wip.elc
magit-repos.elc:	magit-core.elc
magit.elc:		git-commit.elc magit-core.elc magit-diff.elc \
			magit-log.elc magit-apply.elc magit-repos.elc
magit-status.elc:	magit.elc
magit-refs.elc: 	magit.elc
magit-files.elc:	magit.elc
magit-reset.elc:        magit.elc
magit-branch.elc:	magit.elc magit-reset.elc
magit-merge.elc:        magit.elc
magit-tag.elc:          magit.elc
magit-worktree.elc:	magit.elc
magit-notes.elc:	magit.elc
magit-sequence.elc:	magit.elc
magit-commit.elc:	magit.elc magit-sequence.elc
magit-remote.elc:	magit.elc
magit-clone.elc:	magit.elc
magit-fetch.elc:	magit.elc
magit-pull.elc:		magit.elc
magit-push.elc:		magit.elc
magit-patch.elc:	magit.elc
magit-bisect.elc:	magit.elc
magit-stash.elc:	magit.elc
magit-blame.elc:	magit.elc
magit-obsolete.elc:	magit.elc
magit-submodule.elc:	magit.elc
magit-subtree.elc:	magit.elc
magit-ediff.elc:	magit.elc
magit-gitignore.elc:	magit.elc
magit-extras.elc:	magit.elc magit-merge.elc
git-rebase.elc:		magit.elc
magit-imenu.elc:	magit.elc git-rebase.elc
magit-bookmark.elc:	magit.elc
magit-obsolete.elc:	magit.elc

## Build #############################################################

lisp:       $(ELCS) loaddefs
loaddefs:   $(PKG)-autoloads.el
versionlib: $(PKG)-version.el

%.elc: %.el
	@printf "Compiling $<\n"
	@$(BATCH) --eval "(progn\
	(when (file-exists-p \"$@\")\
	  (delete-file \"$@\"))\
	(setq with-editor-emacsclient-executable nil)\
	$$suppress_warnings)" \
	-f batch-byte-compile $<

$(PKG)-autoloads.el: $(ELS)
	@printf "Generating $@\n"
	@printf "%s" "$$LOADDEFS_TMPL" > $@
	@$(BATCH) --eval "(progn\
	(fset 'message (lambda (&rest _)))\
	(setq make-backup-files nil)\
	(setq vc-handled-backends nil)\
	(setq default-directory (file-truename default-directory))\
	(setq generated-autoload-file (expand-file-name \"$@\"))\
	(setq find-file-visit-truename t)\
	(update-directory-autoloads default-directory))"

$(PKG)-version.el:
	@printf "Generating $@\n"
	@printf "%s" "$$VERSIONLIB_TMPL" > $@

## Install ###########################################################

install: lisp versionlib
	@$(MKDIR) $(DESTDIR)$(lispdir)
	$(CP) $(ELS) magit-autoloads.el magit-version.el $(ELCS) $(DESTDIR)$(lispdir)

## Clean #############################################################

clean:
	@printf "Cleaning lisp/*...\n"
	@$(RM) *.elc $(ELGS)

## Templates #########################################################

define LOADDEFS_TMPL
;;; $(PKG)-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name \
(or (file-name-directory #$$) (car load-path))))

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; $(PKG)-autoloads.el ends here

endef
export LOADDEFS_TMPL
#'

define VERSIONLIB_TMPL
;;; $(PKG)-version.el --- the Magit version you are using

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

define suppress_warnings
(fset 'original-message (symbol-function 'message))
(fset 'message ;'
      (lambda (f &rest a)
        (unless (or (equal f "Wrote %s")
                    (equal f "pcase-memoize: equal first branch, yet different")
                    (and (equal f "Warning: Unknown defun property `%S' in %S")
                         (memq (car a) '(pure side-effect-free interactive-only))))
          (apply 'original-message f a))))
endef
export suppress_warnings
