-include ../config.mk
include ../default.mk

.PHONY: magit-version.el install

all: lisp

git-commit.elc:
magit-utils.elc:
magit-section.elc:	magit-utils.elc
magit-git.elc:		magit-utils.elc magit-section.elc
magit-mode.elc:		magit-section.elc magit-git.elc
magit-popup.elc:
magit-process.elc:	magit-utils.elc magit-section.elc \
			magit-git.elc magit-mode.elc
magit-autorevert.elc:	magit-git.elc magit-process.elc
magit-core.elc:		magit-utils.elc magit-section.elc magit-git.elc \
			magit-mode.elc magit-popup.elc magit-process.elc
magit-diff.elc:		git-commit.elc magit-core.elc
magit-log.elc:		magit-core.elc magit-diff.elc
magit-wip.elc:		magit-core.elc magit-log.elc
magit-apply.elc:	magit-core.elc magit-diff.elc magit-wip.elc
magit.elc:		git-commit.elc magit-core.elc magit-diff.elc \
			magit-apply.elc magit-log.elc
magit-sequence.elc:	magit.elc
magit-commit.elc:	magit.elc magit-sequence.elc
magit-remote.elc:	magit.elc
magit-bisect.elc:	magit.elc
magit-stash.elc:	magit.elc
magit-blame.elc:	magit.elc
magit-submodule.elc:	magit.elc
magit-subtree.elc:	magit.elc
magit-ediff.elc:	magit.elc
magit-extras.elc:	magit.elc
git-rebase.elc:		magit.elc

lisp: $(ELCS) magit-version.el magit-autoloads.el

%.elc: %.el
	@printf "Compiling $<\n"
	@$(BATCH) --eval "(progn\
	(when (file-exists-p \"$@\")\
	  (delete-file \"$@\"))\
	(setq with-editor-emacsclient-executable nil)\
	(fset 'message* (symbol-function 'message))\
	(fset 'message  (lambda (f &rest a)\
	                  (unless (equal f \"Wrote %s\")\
	                    (apply 'message* f a)))))" \
	-f batch-byte-compile $<

magit-version.el:
	@printf "Generating $@\n"
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

magit-autoloads.el: $(ELS)
	@printf "Generating $@\n"
	@$(RM) $@
	@printf ";;; $@ --- automatically extracted autoloads\n" >> $@
	@printf ";;\n" >> $@
	@printf ";;; Code:\n" >> $@ >> $@
	@printf "(add-to-list 'load-path (directory-file-name (or (file-name-directory #$$) (car load-path))))\n" >> $@
	@printf "\n" >> $@
	@printf ";; Local Variables:\n" >> $@
	@printf ";; version-control: never\n" >> $@
	@printf ";; no-byte-compile: t\n" >> $@
	@printf ";; no-update-autoloads: t\n" >> $@
	@printf ";; End:\n" >> $@
	@printf ";;; $@ ends here\n" >> $@
	@$(BATCH) --eval "(progn\
	(fset 'message (lambda (&rest _)))\
	(setq make-backup-files nil)\
	(setq vc-handled-backends nil)\
	(setq default-directory (file-truename default-directory))\
	(setq generated-autoload-file (expand-file-name \"magit-autoloads.el\"))\
	(setq find-file-visit-truename t)\
	(update-directory-autoloads default-directory)))"

install: lisp
	@$(MKDIR) $(DESTDIR)$(lispdir)
	$(CP) $(ELS) magit-autoloads.el magit-version.el $(ELCS) $(DESTDIR)$(lispdir)

clean:
	@printf "Cleaning lisp/*...\n"
	@$(RM) $(ELCS) $(ELGS)
