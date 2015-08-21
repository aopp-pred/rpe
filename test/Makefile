# Makefile for all test suites.
#

# Phony target declarations.
.PHONY: unit_tests test clean-suites distclean-suites

# Build the unit test suite.
unit_tests:
	make -C unit unittests.x

# A target to run all the test suites.
test: clean-suites unit_tests
	./runtests.sh

# A target to clean all test suites.
clean-suites:
	make -C unit clean

# A target to perform a distclean on all test suites.
distclean-suites:
	make -C unit distclean