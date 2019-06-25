PREFIX=/usr/local

.PHONY: all test clean

all: meta-rpc.so

# Development tests
test:
	$(PREFIX)/bin/csc -I tests/ tests/tests.scm -o run
	./run

# Module interface
meta-rpc.so: src/meta-rpc.scm src/client.scm src/server.scm src/common.scm
	$(PREFIX)/bin/csc -s -j meta-rpc -o $@ $<
	$(PREFIX)/bin/csc meta-rpc.import.scm -dynamic

clean:
	rm -f tests/*.o *.o run *.c tests/*.c *.so meta-rpc.import.scm tests/run src/*.c src/*.so
	rm -f meta-rpc.*.sh meta-rpc.link
