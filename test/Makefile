-include ../config.mk
include ../default.mk

test:
	@printf "Testing...\n"
	@$(BATCH) --eval "(progn\
	$$suppress_warnings\
	(load-file \"$(TOP)test/magit-tests.el\")\
	(ert-run-tests-batch-and-exit))"

test-interactive:
	@$(EMACS) -Q $(LOAD_PATH) --eval "(progn\
	(load-file \"$(TOP)test/magit-tests.el\")\
	(ert t))"
