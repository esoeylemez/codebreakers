.PHONY: all

targets = \
	$(patsubst %.hs,%,$(wildcard [a-z]*.hs))


all: $(targets)


GHCFLAGS = -O -threaded -rtsopts -fdefer-typed-holes -ferror-spans


%: %.hs
	@mkdir -p tmp/$@
	ghc $(GHCFLAGS) -outputdir=tmp/$@ -o $@ $<
	@touch $@
