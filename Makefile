all: all-event-urls event-details

all-event-urls: EventURLs.hs
	ghc --make -optl"-Wl,-read_only_relocs,suppress" -o $@ $<

event-details: EventDetails.hs
	ghc --make -optl"-Wl,-read_only_relocs,suppress" -o $@ $<

clean:
	rm -f  *.hi *.o event-details all-event-urls
