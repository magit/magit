all: index.html

clean:
	-rm -rf *.html

%.html : %.tt
	tpage --define "date=$(date)" $< > $@

index.html: index.tt lib/head-foot.tt

