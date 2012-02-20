all: get-event store-event store-all-events sxcrape

export GHC_EXTRA_FLAGS

get-event: GetEvent.hs Event.hs ParserUtils.hs
	ghc --make $(GHC_EXTRA_FLAGS) -o $@ $^

store-event: StoreEvent.hs Redis.hs
	ghc --make $(GHC_EXTRA_FLAGS) -o $@ $^

store-all-events: StoreAllEvents.hs Redis.hs
	ghc --make $(GHC_EXTRA_FLAGS) -o $@ $^

sxcrape: Sxcrape.hs EventURLs.hs Event.hs ParserUtils.hs Redis.hs
	ghc --make $(GHC_EXTRA_FLAGS) -o $@ $^

clean:
	rm -f  *.hi *.o get-event store-event store-all-events sxcrape
