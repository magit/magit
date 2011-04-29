all: index.html contribute.html manual.html

clean:
	-rm -rf *.html magit.texi

%.html : %.tt
	tpage --define "user=${USER}" $< > $@

contribute.html: contribute.tt lib/head-foot.tt
index.html: index.tt lib/head-foot.tt

# will build from the latest tag whose name starts with a digit
manual.html: lib/head-foot.tt
	./bin/build-manual.bash `git tag -l '[0-9].*' | sort | tail -n 1`
