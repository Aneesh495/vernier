# Makefile for Vernier Distributed Tracing & Telemetry Engine

.PHONY: all test test-unit test-integration test-property test-python test-c server demo bench clean

RACKET ?= racket
PYTHON ?= python3
CC ?= gcc
CFLAGS ?= -std=c99 -Wall -Wextra -pedantic -O2

all: test

test: test-unit test-integration test-property test-python test-c
	@echo "================================================================================"
	@echo "All Vernier test suites passed successfully!"
	@echo "================================================================================"

test-unit:
	@echo "--> Running Racket Unit Tests..."
	@for f in tests/unit/test_*.rkt; do \
		echo "    Executing $$f"; \
		$(RACKET) "$$f" || exit 1; \
	done

test-integration:
	@echo "--> Running End-to-End Pipeline Integration Tests..."
	@for f in tests/integration/test_*.rkt; do \
		echo "    Executing $$f"; \
		$(RACKET) "$$f" || exit 1; \
	done

test-property:
	@echo "--> Running Property-Based Invariant Tests..."
	@for f in tests/property/test_*.rkt; do \
		echo "    Executing $$f"; \
		$(RACKET) "$$f" || exit 1; \
	done

test-python:
	@echo "--> Running Python SDK Unit Tests..."
	@$(PYTHON) -m unittest tests/unit/test_python_sdk.py

test-c:
	@echo "--> Compiling and Running C99 SDK Unit Tests..."
	@$(CC) $(CFLAGS) vernier/sdk/c/vernier.c tests/unit/test_c_sdk.c -o /tmp/vernier_test_c_sdk
	@/tmp/vernier_test_c_sdk
	@rm -f /tmp/vernier_test_c_sdk

server:
	$(RACKET) vernier.rkt server --port 9876

demo:
	$(RACKET) vernier.rkt demo --port 9876 --count 25

bench:
	$(RACKET) vernier.rkt benchmark --count 10000

clean:
	@rm -rf /tmp/vernier_* /tmp/test_c_sdk compiled/ .racket/
	@find . -type d -name "compiled" -exec rm -rf {} +
	@find . -type d -name "__pycache__" -exec rm -rf {} +
