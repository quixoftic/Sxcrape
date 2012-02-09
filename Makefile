all: all-event-urls get-event store-event store-all-events

export GHC_EXTRA_FLAGS

all-event-urls: AllEventURLs.hs EventURLs.hs
	ghc --make $(GHC_EXTRA_FLAGS) -o $@ $^

get-event: GetEvent.hs Event.hs ParserUtils.hs
	ghc --make $(GHC_EXTRA_FLAGS) -o $@ $^

store-event: StoreEvent.hs Redis.hs
	ghc --make $(GHC_EXTRA_FLAGS) -o $@ $^

store-all-events: StoreAllEvents.hs Redis.hs
	ghc --make $(GHC_EXTRA_FLAGS) -o $@ $^

clean:
	rm -f  *.hi *.o get-event all-event-urls store-event store-all-events
