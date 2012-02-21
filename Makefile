all: store-event store-all-events sxcrape

export GHC_EXTRA_FLAGS

store-event: StoreEvent.hs Redis.hs
	ghc --make -O2 $(GHC_EXTRA_FLAGS) -o $@ $^

store-all-events: StoreAllEvents.hs Redis.hs EventURLs.hs
	ghc --make -O2 $(GHC_EXTRA_FLAGS) -o $@ $^

sxcrape: Sxcrape.hs EventURLs.hs Event.hs ParserUtils.hs Redis.hs
	ghc --make -O2 $(GHC_EXTRA_FLAGS) -o $@ $^

clean:
	rm -f  *.hi *.o store-event store-all-events sxcrape
