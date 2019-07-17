PREFIX=
CSC=
ifeq ($(CSC),)
ifeq ($(PREFIX),)
CSC=csc
else
CSC=$(PREFIX)/bin/csc
endif
endif

.PHONY: all test-test unit unit-server unit-client test clean

all: meta-rpc.so meta-rpc.transport.so meta-rpc.interface.so

# Development test
test-test: test/test-tools.scm test/tunnel.scm test/pseudo-format.scm test/pseudo-transport.scm meta-rpc.interface.so
	$(CSC) test/test-tools.scm -o run && ./run

test: meta-rpc.so meta-rpc.transport.so meta-rpc.interface.so test/test.scm test/pseudo-format.scm test/pseudo-transport.scm
	$(CSC) test/test.scm -o run && ./run

unit: unit-server unit-client

unit-client: test/client-unit.scm test/pseudo-format.scm test/pseudo-transport.scm test/tunnel.scm src/main/common.scm src/main/client.scm meta-rpc.interface.so
	$(CSC) -X meta-rpc.interface.so test/client-unit.scm -o unit-client && ./unit-client

unit-server: test/server-unit.scm test/pseudo-format.scm test/pseudo-transport.scm test/tunnel.scm src/main/common.scm src/main/actor.scm src/main/server.scm meta-rpc.interface.so 
	$(CSC) -X meta-rpc.interface.so test/server-unit.scm -o unit-server && ./unit-server

# main module
meta-rpc.so: src/meta-rpc.scm src/main/client.scm src/main/server.scm src/main/common.scm src/main/actor.scm meta-rpc.interface.so
	$(CSC) -X meta-rpc.interface.so -s -j meta-rpc -o $@ $<
	$(CSC) meta-rpc.import.scm -dynamic

# transport submodule
meta-rpc.transport.so: src/transport.scm src/transport/file-io.scm src/transport/tcp.scm meta-rpc.interface.so
	$(CSC) -X meta-rpc.interface.so -s -j meta-rpc.transport -o $@ $<
	$(CSC) meta-rpc.transport.import.scm -dynamic

# interface submodule
meta-rpc.interface.so: src/interface.scm src/interface/message-format.scm src/interface/transport.scm
	$(CSC) -s -j meta-rpc.interface -o $@ $<
	$(CSC) meta-rpc.interface.import.scm -dynamic

clean:
	rm -f test/*.o *.o run unit-* *.c test/*.c *.so *.import.scm test/run src/*.c src/*.so
	rm -f meta-rpc.*.sh *.link
