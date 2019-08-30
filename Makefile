PREFIX=
CSC=
ifeq ($(CSC),)
ifeq ($(PREFIX),)
CSC=csc
else
CSC=$(PREFIX)/bin/csc
endif
endif

CSC_OPTIONS=-disable-interrupts

.PHONY: all test-test unit unit-server unit-client test clean

all: meta-rpc.so meta-rpc.transport.so meta-rpc.interface.so

help:
	@echo "Usage: make [PREFIX=<chicken installation prefix>] [CSC=<csc command name>] <target>"
	@echo "Available target:"
	@echo "  Test targets:"
	@echo "    test                   proceed to all tests"
	@echo "    test-test              test the test tools"
	@echo "    transport              test the transports layers"
	@echo "    server-client          test server and client implementations"
	@echo "    unit                   unit testing of server and client"
	@echo "    unit-client            unit testing of the client"
	@echo "    unit-server            unit testing of the server"
	@echo ""
	@echo "  Lib targets:"
	@echo "    meta-rpc.so            main module"
	@echo "    meta-rpc.transport.so  transport module"
	@echo "    meta-rpc.interface.so  transport and format interface definition"
	@echo ""
	@echo "  Other targets:"
	@echo "    all                    compile all libs"
	@echo "    clean                  remove every build product"

# Development test

test: test-test server-client transport

test-test: test/test-tools.scm test/tunnel.scm test/pseudo-format.scm test/pseudo-transport.scm meta-rpc.interface.so
	$(CSC) $(CSC_OPTIONS) test/test-tools.scm -o run && ./run

transport: test/transport.scm test/pseudo-format.scm meta-rpc.interface.so meta-rpc.transport.so
	$(CSC) $(CSC_OPTIONS) test/transport.scm -o run && ./run

server-client: meta-rpc.so meta-rpc.transport.so meta-rpc.interface.so test/test.scm test/pseudo-format.scm test/pseudo-transport.scm
	$(CSC) $(CSC_OPTIONS) test/test.scm -o run && ./run

unit: unit-server unit-client

unit-client: test/client-unit.scm test/pseudo-format.scm test/pseudo-transport.scm test/tunnel.scm src/main/common.scm src/main/client.scm meta-rpc.interface.so
	$(CSC) $(CSC_OPTIONS) -X meta-rpc.interface.so test/client-unit.scm -o unit-client && ./unit-client

unit-server: test/server-unit.scm test/pseudo-format.scm test/pseudo-transport.scm test/tunnel.scm src/main/common.scm src/main/actor.scm src/main/server.scm meta-rpc.interface.so 
	$(CSC) $(CSC_OPTIONS) -X meta-rpc.interface.so test/server-unit.scm -o unit-server && ./unit-server

# main module
meta-rpc.so: src/meta-rpc.scm src/main/client.scm src/main/server.scm src/main/common.scm src/main/actor.scm meta-rpc.interface.so
	$(CSC) $(CSC_OPTIONS) -X meta-rpc.interface.so -s -j meta-rpc -o $@ $<
	$(CSC) $(CSC_OPTIONS) meta-rpc.import.scm -dynamic

# transport submodule
meta-rpc.transport.so: src/transport.scm src/transport/file-io.scm src/transport/tcp.scm meta-rpc.interface.so
	$(CSC) $(CSC_OPTIONS) -X meta-rpc.interface.so -s -j meta-rpc.transport -o $@ $<
	$(CSC) $(CSC_OPTIONS) meta-rpc.transport.import.scm -dynamic

# interface submodule
meta-rpc.interface.so: src/interface.scm src/interface/message-format.scm src/interface/transport.scm
	$(CSC) $(CSC_OPTIONS) -s -j meta-rpc.interface -o $@ $<
	$(CSC) $(CSC_OPTIONS) meta-rpc.interface.import.scm -dynamic

clean:
	rm -f test/*.o *.o run unit-* *.c test/*.c *.so *.import.scm test/run src/*.c src/*.so
	rm -f meta-rpc.*.sh *.link
