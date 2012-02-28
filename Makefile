all: store-event store-all-events sxcrape

store-event: StoreEvent.hs Redis.hs
	ghc --make -O2 -o $@ $^

store-all-events: StoreAllEvents.hs Redis.hs EventURLs.hs
	ghc --make -O2 -o $@ $^

sxcrape: Sxcrape.hs EventURLs.hs Event.hs Redis.hs
	ghc --make -O2 -o $@ $^

clean:
	rm -f  *.hi *.o store-event store-all-events sxcrape
