# Makefile for all test suites.
#

# Copyright 2015 Andrew Dawson, Peter Dueben
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Phony target declarations.
.PHONY: unit_tests integration_tests test clean-suites distclean-suites

# Build the unit test suite.
unit_tests:
	$(MAKE) -C unit unittests.x

integration_tests:
	$(MAKE) -C integration build-tests

# A target to run all the test suites.
test: clean-suites unit_tests integration_tests
	./runtests.sh

# A target to clean all test suites.
clean-suites:
	$(MAKE) -C unit clean
	$(MAKE) -C integration clean

# A target to perform a distclean on all test suites.
distclean-suites:
	$(MAKE) -C unit distclean
