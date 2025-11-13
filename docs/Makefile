-include ../config.mk
include ../default.mk

.PHONY: install clean AUTHORS.md stats

## Build #############################################################

docs: texi info html html-dir pdf

texi:     $(TEXIPAGES)
info:     $(INFOPAGES) dir
html:     $(HTMLFILES)
html-dir: $(HTMLTOPS)
pdf:      $(PDFFILES)
epub:     $(EPUBFILES)

ORG_ARGS  = --batch -Q $(ORG_LOAD_PATH) -l ol-man
ORG_EVAL += --eval "(progn $$ORG_MAN_EXPORT)"
ORG_EVAL += --eval "(setq indent-tabs-mode nil)"
ORG_EVAL += --eval "(setq org-src-preserve-indentation nil)"
ORG_EVAL += --eval "\
(defun org-texinfo--sanitize-content (text)\
  (replace-regexp-in-string \"[@@{}]\" \"@@\\&\" text))"
ORG_EVAL += --funcall org-texinfo-export-to-texinfo

redo-docs:
	@touch $(ORGPAGES)
	@make docs

.revdesc: ;
_    := $(shell test "$(REVDESC)" = "$$(cat .revdesc 2> /dev/null)" ||\
        echo "$(REVDESC)" > .revdesc)

%.texi: %.org .orgconfig .revdesc
	@printf "Generating $@\n"
	@$(EMACS) $(ORG_ARGS) $< $(ORG_EVAL)

%.info: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --no-split $< -o $@

dir: magit.info magit-section.info
	@printf "Generating dir\n"
	@echo $^ | xargs -n 1 $(INSTALL_INFO) --dir=$@

HTML_FIXUP_CSS    = '/<link rel="stylesheet" type="text\/css" href="https:\/\/$(DOMAIN)\/assets\/page.css">/a\
<link rel="icon" href="https://$(DOMAIN)/assets/magit_alt1.ico">\
\n<link class="s-css-s--style" rel="stylesheet"           title="Default"               href="https://$(DOMAIN)/assets/themes/default.css">\
\n<link class="s-css-s--style" rel="stylesheet alternate" title="Default high contrast" href="https://$(DOMAIN)/assets/themes/default-high-contrast.css">\
\n<link class="s-css-s--style" rel="stylesheet alternate" title="Solarized dark xterm"  href="https://$(DOMAIN)/assets/themes/solarized-dark-xterm.css">\
\n<link class="s-css-s--style" rel="stylesheet alternate" title="Black on white"        href="https://$(DOMAIN)/assets/themes/black-on-white.css">\
\n<script src="https://$(DOMAIN)/assets/js/simple-css-switch.js"></script>'
HTML_FIXUP_ONLOAD = 's/<body lang="en">/<body lang="en" onload="simpleCssSwitch()">/'
HTML_FIXUP_MENU   = '/<\/body>/i<div id="s-css-s--menu"><\/div>'

%.html: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --html --no-split $(MANUAL_HTML_ARGS) $<
	@sed -i -e $(HTML_FIXUP_CSS) -e $(HTML_FIXUP_ONLOAD) -e $(HTML_FIXUP_MENU) $@

%/index.html: %.texi
	@$(RMDIR) $(HTMLDIRS)
	@printf "Generating magit/*.html\n"
	@$(MAKEINFO) --html -o $(PKG)/ $(MANUAL_HTML_ARGS) magit.texi
	@for f in $$(find magit -name '*.html') ; do \
	sed -i -e $(HTML_FIXUP_CSS) -e $(HTML_FIXUP_ONLOAD) -e $(HTML_FIXUP_MENU) $$f ; \
	done
	@printf "Generating magit-section/*.html\n"
	@$(MAKEINFO) --html -o $(PKG)-section/ $(MANUAL_HTML_ARGS) magit-section.texi
	@for f in $$(find magit-section -name '*.html') ; do \
	sed -i -e $(HTML_FIXUP_CSS) -e $(HTML_FIXUP_ONLOAD) -e $(HTML_FIXUP_MENU) $$f ; \
	done

%.pdf: %.texi
	@printf "Generating $@\n"
	@texi2pdf --clean $< > /dev/null

%.epub: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --docbook $< -o epub.xml
	@xsltproc $(DOCBOOK_XSL) epub.xml 2> /dev/null
	@echo "application/epub+zip" > mimetype
	@zip -X --quiet --recurse-paths -0 $@ mimetype
	@zip -X --quiet --recurse-paths -9 --no-dir-entries $@ META-INF OEBPS
	@$(RMDIR) $(EPUBTRASH)

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
	@printf " Cleaning docs/*...\n"
	@$(RMDIR) dir $(INFOPAGES) $(HTMLFILES) $(HTMLDIRS) $(PDFFILES)
	@$(RMDIR) $(EPUBFILES) $(EPUBTRASH)
	@$(RMDIR) $(GENSTATS_DIR)

## Release management ################################################

authors: AUTHORS.md

AUTHORS.md:
	@printf "Generating AUTHORS.md..."
	@test -e $(TOP).git \
	&& (printf "$$AUTHORS_HEADER\n" > $@ \
	&& git log --pretty=format:'- %aN' | sort -u | \
	grep -v dependabot >> $@ \
	&& printf "done\n" ; ) \
	|| printf "FAILED (non-fatal)\n"
	@git commit --gpg-sign -m "AUTHORS.md: Update list of contributors" \
	-o -- $@ ../.mailmap && git show --pretty= -p HEAD || true

DOCS_DOMAIN = docs.$(DOMAIN)
SNAP_TARGET = $(subst .,_,$(DOCS_DOMAIN)):devel/
DOCS_TARGET = $(subst .,_,$(DOCS_DOMAIN)):

publish: redo-docs
	@printf "Publishing snapshot manual...\n"
	@cp $(PKG).pdf         $(PKG)/$(PKG).pdf
	@cp $(PKG)-section.pdf $(PKG)-section/$(PKG)-section.pdf
	@$(RCLONE) sync $(RCLONE_ARGS) $(PKG)         $(SNAP_TARGET)/$(PKG)/
	@$(RCLONE) sync $(RCLONE_ARGS) $(PKG)-section $(SNAP_TARGET)/$(PKG)-section/

release: redo-docs
	@printf "Publishing release manual...\n"
	@cp $(PKG).pdf         $(PKG)/$(PKG).pdf
	@cp $(PKG)-section.pdf $(PKG)-section/$(PKG)-section.pdf
	@$(RCLONE) sync $(RCLONE_ARGS) $(PKG)         $(DOCS_TARGET)/$(PKG)/
	@$(RCLONE) sync $(RCLONE_ARGS) $(PKG)-section $(DOCS_TARGET)/$(PKG)-section/

## Statistics ########################################################

STAT_DOMAIN = stats.$(DOMAIN)
STAT_TARGET = $(subst .,_,$(STAT_DOMAIN)):$(PKG)/

stats:
	@printf "Generating statistics\n"
	@$(GITSTATS) $(GITSTATS_ARGS) $(TOP) $(GITSTATS_DIR)

stats-upload:
	@printf "Uploading statistics...\n"
	@$(RCLONE) sync $(RCLONE_ARGS) stats $(STAT_TARGET)

## Lisp ##############################################################

# When making changes here, then also adjust the copy in magit-base.el.
define ORG_MAN_EXPORT
(define-advice org-man-export (:around (fn link description format) gitman)
  (if (and (eq format 'texinfo) ;'
           (string-match-p "\\`git" link))
      (replace-regexp-in-string "%s" link "
@ifinfo
@ref{%s,,,gitman,}.
@end ifinfo
@ifhtml
@html
the <a href=\"http://git-scm.com/docs/%s\">%s(1)</a> manpage.
@end html
@end ifhtml
@iftex
the %s(1) manpage.
@end iftex
")
    (funcall fn link description format)))
endef
export ORG_MAN_EXPORT

## Templates #########################################################

define AUTHORS_HEADER
The following people have contributed to Magit.
For statistics see https://$(STAT_DOMAIN)/magit/authors.html.

Authors
-------

- Marius Vollmer
- Jonas Bernoulli

Active Maintainers
------------------

- Jonas Bernoulli
- Kyle Meyer

Former Maintainers
------------------

- Nicolas Dudebout
- Noam Postavsky
- Peter J. Weisberg
- Phil Jackson
- Rémi Vanicat
- Yann Hodique

All Contributors
----------------

endef
export AUTHORS_HEADER
