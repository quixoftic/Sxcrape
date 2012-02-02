all: all-event-urls get-event redis-test

all-event-urls: AllEventURLs.hs EventURLs.hs
	ghc --make -optl"-Wl,-read_only_relocs,suppress" -o $@ $^

get-event: GetEvent.hs Event.hs ParserUtils.hs
	ghc --make -optl"-Wl,-read_only_relocs,suppress" -o $@ $^

redis-test: RedisTest.hs
	ghc --make -optl"-Wl,-read_only_relocs,suppress" -o $@ $^

clean:
	rm -f  *.hi *.o get-event all-event-urls redis-test
