all: all-event-urls event-details

all-event-urls: AllEventURLs.hs EventURLs.hs
	ghc --make -optl"-Wl,-read_only_relocs,suppress" -o $@ $^

event-details: GetEventDetails.hs Event.hs ParserUtils.hs
	ghc --make -optl"-Wl,-read_only_relocs,suppress" -o $@ $^

clean:
	rm -f  *.hi *.o event-details all-event-urls
