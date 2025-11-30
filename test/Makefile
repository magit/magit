-include ../config.mk
include ../default.mk

test: lisp
	@printf "Testing...\n"
	@$(EMACS_BATCH) --eval "(progn\
	$$suppress_warnings\
	(load-file \"$(TOP)test/magit-tests.el\")\
	(ert-run-tests-batch-and-exit))"

test-interactive:
	@$(EMACS_INTR) --eval "(progn\
	(load-file \"$(TOP)test/magit-tests.el\")\
	(ert t))"

lisp: magit-tests.elc

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS_BATCH) --eval "(progn\
	(when (file-exists-p \"$@\")\
	  (delete-file \"$@\"))\
	(setq with-editor-emacsclient-executable nil)\
	$$suppress_warnings)" \
	-f batch-byte-compile $<
