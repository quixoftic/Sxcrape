all: all-event-urls event-details

all-event-urls: all-event-urls.hs
	ghc --make -optl"-Wl,-read_only_relocs,suppress" $<

event-details: event-details.hs
	ghc --make -optl"-Wl,-read_only_relocs,suppress" $<
