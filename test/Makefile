-include ../config.mk
include ../default.mk

LOAD_PATH += -L ../lisp

test: lisp
	@printf "  Testing...\n"
	@$(EMACS_BATCH) -l $(PKG)-tests -f ert-run-tests-batch-and-exit

test-interactive:
	@$(EMACS_INTR) -l $(PKG)-tests --eval "(ert t)"

lisp: $(addprefix ../lisp/,$(ELCS)) $(PKG)-tests.elc

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS_BATCH) --funcall batch-byte-compile $<

clean:
	@printf " Cleaning test/*...\n"
	@rm -rf $(PKG)-tests.elc
