-include ../config.mk
include ../.common.mk

MAKEINFO ?= makeinfo

INSTALL_INFO ?= $(shell \
  hash ginstall-info 2> /dev/null\
  && printf ginstall-info\
  || printf install-info)

.PHONY: install clean

all: magit.info dir

%.info: %.texi
	@printf "Generating $<\n"
	@$(MAKEINFO) $< -o $@

dir: magit.info
	@printf "Generating dir\n"
	@$(INSTALL_INFO) --dir=$@ $<

install: all
	@$(MKDIR) $(DESTDIR)$(infodir)
	$(CP) magit.info $(DESTDIR)$(infodir)
clean:
	@$(RM) dir magit.info
