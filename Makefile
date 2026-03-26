SCHEME ?= scheme
JERBOA ?= $(HOME)/mine/jerboa/lib
GHERKIN ?= $(HOME)/mine/gherkin/src
CHEZHTTPS ?= $(HOME)/mine/chez-https/src
CHEZSSL ?= $(HOME)/mine/chez-ssl/src
LIBDIRS = lib:$(JERBOA):$(GHERKIN):$(CHEZHTTPS):$(CHEZSSL)
COMPILE = $(SCHEME) -q --libdirs $(LIBDIRS) --compile-imported-libraries

.PHONY: all compile binary build run run-pssm clean help test pssm

all: compile

compile:
	@echo "=== Compiling .sls → .so ==="
	$(COMPILE) < build-all.ss

build: binary

binary: compile
	@echo "=== Building standalone binary ==="
	$(SCHEME) -q --libdirs $(LIBDIRS) --program build-binary.ss

run:
	$(SCHEME) --libdirs $(LIBDIRS) --program aws.ss $(ARGS)

run-pssm:
	$(SCHEME) --libdirs $(LIBDIRS) --program pssm.ss $(ARGS)

pssm: compile
	@echo "=== Building pssm alias ==="
	@echo '#!/bin/sh' > pssm
	@echo 'exec scheme --libdirs "$(LIBDIRS)" --program "$(CURDIR)/pssm.ss" "$$@"' >> pssm
	@chmod +x pssm
	@echo "Created ./pssm"

test:
	@echo "=== Running tests ==="
	$(SCHEME) --libdirs $(LIBDIRS) --script test/test-all.ss

clean:
	find lib -name '*.so' -o -name '*.wpo' | xargs rm -f 2>/dev/null || true
	rm -f jerboa-aws jerboa-aws-main.o
	rm -f jerboa_aws_program.h jerboa_aws_petite_boot.h
	rm -f jerboa_aws_scheme_boot.h jerboa_aws_app_boot.h
	rm -f jerboa-aws-all.so aws.so aws.wpo jerboa-aws.boot
	rm -f pssm

help:
	@echo "Targets:"
	@echo "  all       - Compile all modules"
	@echo "  build     - Build standalone binary (./jerboa-aws)"
	@echo "  pssm      - Build pssm wrapper script (./pssm)"
	@echo "  run       - Run interpreted (ARGS='ec2 describe-instances')"
	@echo "  run-pssm  - Run pssm interpreted (ARGS=\"'web-*' uptime\")"
	@echo "  test      - Run tests"
	@echo "  clean     - Remove build artifacts"
