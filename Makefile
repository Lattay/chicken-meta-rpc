PREFIX=/usr/local

.PHONY: all test clean

all: meta-rpc.so meta-rpc.transport.so meta-rpc.interface.so

# Development tests
test:
	$(PREFIX)/bin/csc -I tests/ tests/tests.scm -o run
	./run

# main module
meta-rpc.so: src/meta-rpc.scm src/client.scm src/server.scm src/common.scm
	$(PREFIX)/bin/csc -s -j meta-rpc -o $@ $<
	$(PREFIX)/bin/csc meta-rpc.import.scm -dynamic

# transport submodule
meta-rpc.transport.so: src/transport.scm src/transport/file-io.scm src/transport/tcp.scm
	$(PREFIX)/bin/csc -s -j meta-rpc.transport -o $@ $<
	$(PREFIX)/bin/csc meta-rpc.transport.import.scm -dynamic

# interface submodule
meta-rpc.interface.so: src/interface.scm src/interface/message-format.scm src/interface/transport.scm
	$(PREFIX)/bin/csc -s -j meta-rpc.interface -o $@ $<
	$(PREFIX)/bin/csc meta-rpc.interface.import.scm -dynamic

clean:
	rm -f tests/*.o *.o run *.c tests/*.c *.so *.import.scm tests/run src/*.c src/*.so
	rm -f meta-rpc.*.sh *.link
