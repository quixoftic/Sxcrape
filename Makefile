all: all-event-urls

all-event-urls: all-event-urls.hs
	ghc --make -optl"-Wl,-read_only_relocs,suppress" $<
