all: all-event-urls get-event store-event

all-event-urls: AllEventURLs.hs EventURLs.hs
	ghc --make -optl"-Wl,-read_only_relocs,suppress" -o $@ $^

get-event: GetEvent.hs Event.hs ParserUtils.hs
	ghc --make -optl"-Wl,-read_only_relocs,suppress" -o $@ $^

store-event: StoreEvent.hs Redis.hs
	ghc --make -optl"-Wl,-read_only_relocs,suppress" -o $@ $^

clean:
	rm -f  *.hi *.o get-event all-event-urls store-event
