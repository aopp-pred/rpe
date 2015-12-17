# Makefile for the reduced-precision emulator integration test suite.
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

# A list of all test suites that should be built.
TEST_SUITES = lorenz63

# A phony target called "build-tests" that will be executed by the main
# Makefile, and phony targets for each suite and cleanup.
.PHONY: build-tests $(TEST_SUITES) clean

# Building the tests requires building each module.
build-tests: $(TEST_SUITES)

# Build a module by calling make inside its directory using the target
# 'build-tests'.
$(TEST_SUITES):
	$(MAKE) -C $@ build-tests

# Clean each suite recursively.
clean:
	@for dir in $(TEST_SUITES); do $(MAKE) -C $$dir clean; done
