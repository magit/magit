all: index.html contribute.html manual.html

clean:
	-rm -rf *.html magit.texi

%.html : %.tt
	tpage --define "user=${USER}" $< > $@

contribute.html: contribute.tt lib/head-foot.tt
index.html: index.tt lib/head-foot.tt

# will build from the latest tag
manual.html: lib/head-foot.tt
	./bin/build-manual.bash `git tag | head -n 1`
