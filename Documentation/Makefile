-include ../config.mk
include ../default.mk

MAKEINFO ?= makeinfo

INSTALL_INFO ?= $(shell command -v ginstall-info || printf install-info)

DOC_LOAD_PATH ?= -L ../../dash -L ../../org/lisp -L ../../ox-texinfo+

.PHONY: texi install clean AUTHORS.md

all: info

# Because most users don't have the necessary tools we cannot
# use "%.texi: %.org".  Instead we have to hardcode each file
# using a shared target.
texi: $(TEXIPAGES)
	@printf "Generating magit.texi\n"
	@$(EMACSBIN) -batch -Q $(DOC_LOAD_PATH) \
	-l ox-texinfo+.el magit.org -f org-texinfo+export-to-texinfo
	@echo >> magit.texi
	@printf "Generating magit-popup.texi\n"
	@$(EMACSBIN) -batch -Q $(DOC_LOAD_PATH) \
	-l ox-texinfo+.el magit-popup.org -f org-texinfo+export-to-texinfo
	@echo >> magit-popup.texi

info: $(INFOPAGES) dir

%.info: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --no-split $< -o $@

dir: magit.info magit-popup.info
	@printf "Generating dir\n"
	@echo $^ | xargs -n 1 $(INSTALL_INFO) --dir=$@

install: install-info install-docs

install-docs: install-info
	@$(MKDIR) $(DESTDIR)$(docdir)
	$(CP) AUTHORS.md $(DESTDIR)$(docdir)

install-info: info
	@$(MKDIR) $(DESTDIR)$(infodir)
	$(CP) $(INFOPAGES) $(DESTDIR)$(infodir)

clean:
	@printf "Cleaning Documentation/*...\n"
	@$(RM) $(INFOPAGES) dir

authors: AUTHORS.md
AUTHORS.md:
	@printf "Generating AUTHORS.md..."
	@test -e $(TOP).git \
	&& (printf "$$AUTHORS_HEADER\n" > $@ \
	&& git log --pretty=format:'- %aN <%aE>' | sort -u >> $@ \
	&& printf "done\n" ; ) \
	|| printf "FAILED (non-fatal)\n"

define AUTHORS_HEADER
Authors
=======

The following people have contributed to Magit, including the
libraries `git-commit.el`, `magit-popup.el`, and `with-editor.el`
which are distributed as separate Elpa packages.

For statistics see https://magit.vc/stats/authors.html.

Names below are sorted alphabetically.

Author
------

- Marius Vollmer <marius.vollmer@gmail.com>

Maintainer
----------

- Jonas Bernoulli <jonas@bernoul.li>

Developers
----------

- Kyle Meyer <kyle@kyleam.com>
- Noam Postavsky <npostavs@users.sourceforge.net>

Retired Maintainers and Developers
----------------------------------

- Nicolas Dudebout <nicolas.dudebout@gatech.edu>
- Peter J. Weisberg <pj@irregularexpressions.net>
- Pieter Praet <pieter@praet.org>
- Phil Jackson <phil@shellarchive.co.uk>
- Rémi Vanicat <vanicat@debian.org>
- Yann Hodique <yann.hodique@gmail.com>

Contributors
------------

endef
export AUTHORS_HEADER
