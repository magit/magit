-include ../config.mk
include ../default.mk

## ###################################################################

.PHONY: texi install clean AUTHORS.md stats

all: info

## Build #############################################################

info: $(INFOPAGES) dir
html: $(HTMLFILES)
pdf:  $(PDFFILES)

%.info: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --no-split $< -o $@

dir: magit.info magit-popup.info
	@printf "Generating dir\n"
	@echo $^ | xargs -n 1 $(INSTALL_INFO) --dir=$@

%.html: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --html --no-split $(MANUAL_HTML_ARGS) $<

html-dir: $(TEXIFILES)
	@printf "Generating magit/*.html\n"
	@$(MAKEINFO) --html $(MANUAL_HTML_ARGS) magit.texi
	@printf "Generating magit-popup/*.html\n"
	@$(MAKEINFO) --html $(MANUAL_HTML_ARGS) magit-popup.texi

%.pdf: %.texi
	@printf "Generating $@\n"
	@texi2pdf --clean $< > /dev/null

## Install ###########################################################

install: install-info install-docs

install-docs: install-info
	@$(MKDIR) $(DESTDIR)$(docdir)
	$(CP) AUTHORS.md $(DESTDIR)$(docdir)

install-info: info
	@$(MKDIR) $(DESTDIR)$(infodir)
	$(CP) $(INFOPAGES) $(DESTDIR)$(infodir)

## Clean #############################################################

clean:
	@printf "Cleaning Documentation/*...\n"
	@$(RMDIR) dir $(INFOPAGES) $(HTMLFILES) $(HTMLDIRS) $(PDFFILES)

## Release management ################################################

# Because most users don't have the necessary tools we cannot
# use "%.texi: %.org".  Instead we have to hardcode each file
# using a shared target.

DOC_ARGS  = --batch -Q $(DOC_LOAD_PATH)
DOC_ARGS += -l magit-utils.el -l ox-extra -l org-man -l ox-texinfo+.el
DOC_EVAL  = --eval "(ox-extras-activate '(ignore-headlines))"
DOC_EVAL += -f org-texinfo-export-to-texinfo

texi: $(TEXIPAGES)
	@printf "Generating magit.texi\n"
	@$(EMACSBIN) $(DOC_ARGS) magit.org $(DOC_EVAL)
	@echo >> magit.texi
	@printf "Generating magit-popup.texi\n"
	@$(EMACSBIN) $(DOC_ARGS) magit-popup.org $(DOC_EVAL)
	@echo >> magit-popup.texi

stats:
	@printf "Generating statistics\n"
	@gitstats -c style=/assets/stats.css -c max_authors=999 $(TOP) $(statsdir)

authors: AUTHORS.md

AUTHORS.md:
	@printf "Generating AUTHORS.md..."
	@test -e $(TOP).git \
	&& (printf "$$AUTHORS_HEADER\n" > $@ \
	&& git log --pretty=format:'- %aN <%aE>' | sort -u >> $@ \
	&& printf "done\n" ; ) \
	|| printf "FAILED (non-fatal)\n"

DOMAIN         ?= magit.vc
CFRONT_DIST    ?= E2LUHBKU1FBV02
PUBLISH_BUCKET ?= s3://$(DOMAIN)
PREVIEW_BUCKET ?= s3://preview.$(DOMAIN)
PUBLISH_URL    ?= http://$(DOMAIN).s3-website.eu-central-1.amazonaws.com
PREVIEW_URL    ?= http://preview.$(DOMAIN).s3-website.eu-central-1.amazonaws.com

preview-stats: stats
	@printf "Uploading statistics...\n"
	@aws s3 sync $(statsdir)      $(PREVIEW_BUCKET)/stats/
	@aws cloudfront create-invalidation \
	--distribution-id $(CFRONT_DIST) --paths "/stats/*"
	@printf "Uploaded to $(PREVIEW_URL)/stats/\n"

publish-stats: stats
	@printf "Uploading statistics...\n"
	@aws s3 sync $(statsdir)      $(PUBLISH_BUCKET)/stats/
	@aws cloudfront create-invalidation \
	--distribution-id $(CFRONT_DIST) --paths "/stats/*"
	@printf "Uploaded to $(PUBLISH_URL)/stats/\n"

preview-manuals: html html-dir pdf
	@printf "Uploading manuals...\n"
	@aws s3 sync magit            $(PREVIEW_BUCKET)/manual/magit/
	@aws s3 cp   magit.html       $(PREVIEW_BUCKET)/manual/
	@aws s3 cp   magit.pdf        $(PREVIEW_BUCKET)/manual/
	@aws s3 sync magit-popup      $(PREVIEW_BUCKET)/manual/magit-popup/
	@aws s3 cp   magit-popup.html $(PREVIEW_BUCKET)/manual/
	@aws s3 cp   magit-popup.pdf  $(PREVIEW_BUCKET)/manual/
	@aws cloudfront create-invalidation \
	--distribution-id $(CFRONT_DIST) --paths "/manual/*"
	@printf "Uploaded to $(PREVIEW_URL)/manual/\n"

publish-manuals: html html-dir pdf
	@printf "Uploading manuals...\n"
	@aws s3 sync magit            $(PUBLISH_BUCKET)/manual/magit/
	@aws s3 cp   magit.html       $(PUBLISH_BUCKET)/manual/
	@aws s3 cp   magit.pdf        $(PUBLISH_BUCKET)/manual/
	@aws s3 sync magit-popup      $(PUBLISH_BUCKET)/manual/magit-popup/
	@aws s3 cp   magit-popup.html $(PUBLISH_BUCKET)/manual/
	@aws s3 cp   magit-popup.pdf  $(PUBLISH_BUCKET)/manual/
	@aws cloudfront create-invalidation \
	--distribution-id $(CFRONT_DIST) --paths "/manual/*"
	@printf "Uploaded to $(PUBLISH_URL)/manual/\n"

# Templates ##########################################################

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
