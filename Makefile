all: bluenoise.html

index.html: bluenoise.md
	pandoc $< -s -t html > $@

%.html: %.md
	pandoc $< -s -t html > $@

%.md: %.fut lib
	futhark literate --backend=opencl $<

lib:
	futhark pkg sync

.PHONY: clean

clean:
	rm -f bluenoise.html bluenoise.md

.PHONY: clean-all

clean-all: clean
	rm -rf lib
