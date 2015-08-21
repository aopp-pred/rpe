# Makefile for all test suites.
#

# Phony target declarations.
.PHONY: unit_tests test

# Build the unit test suite.
unit_tests:
	make -C unit unittests.x

# A target to run all the test suites.
test: unit_tests
	./runtests.sh
