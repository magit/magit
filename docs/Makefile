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

HTML_FIXUP_CSS    = '/<link rel="stylesheet" type="text\/css" href="\/assets\/page.css">/a\
<link rel="icon" href="/assets/magit_alt1.ico">\
\n<link class="s-css-s--style" rel="stylesheet"           title="Default"               href="/assets/themes/default.css">\
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

PUBLISH_PATH   ?= /manual/
RELEASE_PATH   ?= /manual/$(VERSION)/
S3_BUCKET      ?= s3://$(DOMAIN)
PUBLISH_TARGET  = $(S3_BUCKET)$(PUBLISH_PATH)
RELEASE_TARGET  = $(S3_BUCKET)$(RELEASE_PATH)
CFRONT_PATHS    = $(PKG).html $(PKG).pdf $(PKG)/*

comma := ,
empty :=
space := $(empty) $(empty)

publish: redo-docs
	@printf "Uploading snapshot manuals...\n"
	@aws s3 cp $(PKG).html $(PUBLISH_TARGET)
	@aws s3 cp $(PKG).pdf  $(PUBLISH_TARGET)
	@printf "upload: ./$(PKG)/* to $(PUBLISH_TARGET)*\n"
	@aws s3 sync --delete $(PKG) $(PUBLISH_TARGET)$(PKG)/ > /dev/null
	@aws s3 cp magit-section.html $(PUBLISH_TARGET)
	@aws s3 cp magit-section.pdf  $(PUBLISH_TARGET)
	@printf "upload: ./magit-section/* to $(PUBLISH_TARGET)*\n"
	@aws s3 sync --delete magit-section $(PUBLISH_TARGET)magit-section/ > /dev/null
	@printf "Generating CDN invalidation\n"
	@aws cloudfront create-invalidation --distribution-id $(CFRONT_DIST) --paths \
	"$(subst $(space),$(comma),$(addprefix $(PUBLISH_PATH),$(CFRONT_PATHS)))" > /dev/null

release: redo-docs
	@printf "Uploading release manuals...\n"
	@aws s3 cp $(PKG).html $(RELEASE_TARGET)
	@aws s3 cp $(PKG).pdf  $(RELEASE_TARGET)
	@printf "upload: ./$(PKG)/* to $(RELEASE_TARGET)*\n"
	@aws s3 sync --delete $(PKG) $(RELEASE_TARGET)$(PKG)/ > /dev/null
	@aws s3 cp magit-section.html $(RELEASE_TARGET)
	@aws s3 cp magit-section.pdf  $(RELEASE_TARGET)
	@printf "upload: ./magit-section/* to $(RELEASE_TARGET)*\n"
	@aws s3 sync --delete magit-section $(RELEASE_TARGET)magit-section/ > /dev/null
	@aws s3 cp $(PUBLISH_TARGET)dir.html $(RELEASE_TARGET)dir.html
	@aws s3 cp $(PUBLISH_TARGET)dir/index.html $(RELEASE_TARGET)dir/index.html
	@printf "Generating CDN invalidation\n"
	@aws cloudfront create-invalidation --distribution-id $(CFRONT_DIST) --paths \
	"$(subst $(space),$(comma),$(addprefix $(RELEASE_PATH),$(CFRONT_PATHS)))" > /dev/null

## Statistics ########################################################

stats:
	@printf "Generating statistics\n"
	@$(GITSTATS) $(GITSTATS_ARGS) $(TOP) $(GITSTATS_DIR)

stats-upload:
	@printf "Uploading statistics...\n"
	@aws s3 sync $(GITSTATS_DIR) $(S3_BUCKET)/stats/$(PKG)
	@printf "Uploaded to $(S3_BUCKET)/stats/$(PKG)\n"
	@printf "Generating CDN invalidation\n"
	@aws cloudfront create-invalidation \
	--distribution-id $(CFRONT_DIST) --paths "/stats/*" > /dev/null

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
For statistics see https://magit.vc/stats/magit/authors.html.

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
