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

dir: magit.info
	@printf "Generating dir\n"
	@echo $^ | xargs -n 1 $(INSTALL_INFO) --dir=$@

HTML_FIXUP_CSS    = '/<link rel="stylesheet" type="text\/css" href="\/assets\/page.css">/a\
<link class="s-css-s--style" rel="stylesheet"           title="Default"               href="/assets/themes/default.css">\
\n<link class="s-css-s--style" rel="stylesheet alternate" title="Default high contrast" href="/assets/themes/default-high-contrast.css">\
\n<link class="s-css-s--style" rel="stylesheet alternate" title="Solarized dark xterm"  href="/assets/themes/solarized-dark-xterm.css">\
\n<link class="s-css-s--style" rel="stylesheet alternate" title="Black on white"        href="/assets/themes/black-on-white.css">\
\n<script src="/assets/js/simple-css-switch.js"></script>'
HTML_FIXUP_ONLOAD = 's/<body lang="en">/<body lang="en" onload="simpleCssSwitch()">/'
HTML_FIXUP_MENU   = '/<\/body>/i<div id="s-css-s--menu"><\/div>'

%.html: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --html --no-split $(MANUAL_HTML_ARGS) $<
	@sed -i -e $(HTML_FIXUP_CSS) -e $(HTML_FIXUP_ONLOAD) -e $(HTML_FIXUP_MENU) $@

html-dir: $(TEXIFILES)
	@printf "Generating magit/*.html\n"
	@$(MAKEINFO) --html $(MANUAL_HTML_ARGS) magit.texi
	@for f in $$(find magit -name '*.html') ; do \
	sed -i -e $(HTML_FIXUP_CSS) -e $(HTML_FIXUP_ONLOAD) -e $(HTML_FIXUP_MENU) $$f ; \
	done

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

ORG_ARGS  = --batch -Q $(ORG_LOAD_PATH)
ORG_ARGS += -l magit-utils.el -l org-man -l ox-extra -l ox-texinfo+.el
ORG_EVAL  = --eval "(ox-extras-activate '(ignore-headlines))"
ORG_EVAL += --eval "(setq indent-tabs-mode nil)"
ORG_EVAL += --eval "(setq org-src-preserve-indentation nil)"
ORG_EVAL += --funcall org-texinfo-export-to-texinfo

# This target first bumps version strings in the Org source.  The
# necessary tools might be missing so other targets do not depend
# on this target and it has to be run explicitly when appropriate.
#
#   AMEND=t make texi    Update manual to be amended to HEAD.
#   VERSION=N make texi  Update manual for release.
#
texi:
	@$(EMACSBIN) $(ORG_ARGS) $(PKG).org $(ORG_EVAL)
	@printf "\n" >> $(PKG).texi
	@rm -f $(PKG).texi~

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
	@printf "Uploaded to $(PREVIEW_URL)/stats/\n"

publish-stats: stats
	@printf "Uploading statistics...\n"
	@aws s3 sync $(statsdir)      $(PUBLISH_BUCKET)/stats/
	@printf "Uploaded to $(PUBLISH_URL)/stats/\n"
	@printf "Generating CDN invalidation\n"
	@aws cloudfront create-invalidation \
	--distribution-id $(CFRONT_DIST) --paths "/stats/*" > /dev/null

preview-manuals: html html-dir pdf
	@printf "Uploading manuals...\n"
	@aws s3 sync magit            $(PREVIEW_BUCKET)/manual/magit/
	@aws s3 cp   magit.html       $(PREVIEW_BUCKET)/manual/
	@aws s3 cp   magit.pdf        $(PREVIEW_BUCKET)/manual/
	@printf "Uploaded to $(PREVIEW_URL)/manual/\n"

publish-manuals: html html-dir pdf
	@printf "Uploading manuals...\n"
	@aws s3 sync magit            $(PUBLISH_BUCKET)/manual/magit/
	@aws s3 cp   magit.html       $(PUBLISH_BUCKET)/manual/
	@aws s3 cp   magit.pdf        $(PUBLISH_BUCKET)/manual/
	@printf "Uploaded to $(PUBLISH_URL)/manual/\n"
	@printf "Generating CDN invalidation\n"
	@aws cloudfront create-invalidation \
	--distribution-id $(CFRONT_DIST) --paths "/manual/*" > /dev/null

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
