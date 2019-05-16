PREFIX=/usr/local

.PHONY: all test clean

all: meta-rpc.so

# Development tests
test: meta-rpc-imple.so
	$(PREFIX)/bin/csc -I tests/ tests/tests.scm -o run
	./run

# Development interface
meta-rpc-imple.so:
	$(PREFIX)/bin/csc -s src/meta-rpc-imple.scm

# Module interface
meta-rpc.so:
	$(PREFIX)/bin/csc -s -j meta-rpc -o meta-rpc.so src/meta-rpc.scm
	$(PREFIX)/bin/csc meta-rpc.import.scm -dynamic

clean:
	rm -f tests/*.o *.o run *.c tests/*.c *.so meta-rpc.import.scm tests/run src/*.c src/*.so
	rm -f meta-rpc.*.sh meta-rpc.link
