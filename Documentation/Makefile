-include ../config.mk
include ../.common.mk

MAKEINFO ?= makeinfo

INSTALL_INFO ?= $(shell \
  hash ginstall-info 2> /dev/null\
  && printf ginstall-info\
  || printf install-info)

DOC_LOAD_PATH ?= -L ../../dash -L ../../org/lisp -L ../../ox-texinfo+

.PHONY: install clean

all: magit.info with-editor.info dir

%.texi: %.org
	@printf "Generating $<\n"
	@$(EMACSBIN) -batch -Q $(DOC_LOAD_PATH) \
	-l ox-texinfo+.el $< -f org-texinfo+export-to-texinfo
	@echo >> $@

%.info: %.texi
	@printf "Generating $<\n"
	@$(MAKEINFO) $< -o $@

dir: magit.info with-editor.info
	@printf "Generating dir\n"
	@$(INSTALL_INFO) --dir=$@ $<

install: all
	@$(MKDIR) $(DESTDIR)$(infodir)
	$(CP) magit.info with-editor.info $(DESTDIR)$(infodir)
clean:
	@$(RM) dir magit.info
