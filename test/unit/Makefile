# Makefile for the reduced-precision emulator unit test driver.
#

# Include the base pFUnit makefile.
include $(PFUNIT)/include/base.mk

# A list of all test modules that should be built.
TEST_MODULES = core types assignment

# Phony targets for building external test modules (where the tests are
# actually implemented).
.PHONY: suite_common test_modules $(TEST_MODULES) clean clean-test-modules

# Target to build the common module for all test suites.
suite_common:
	@echo "### Building support modules"
	$(MAKE) -C common module
	@echo ""

# Target to request all built test modules as dependencies.
test_modules: suite_common $(TEST_MODULES)

# Target to build each individual test module, each module must have a
# target called test_cases.
$(TEST_MODULES):
	@echo "### Building test module: $@"
	$(MAKE) -C $@ test_cases
	@echo ""

# Target to build the test suite driver.
unittests.x: testSuites.inc test_modules
	@echo "### Building the unit test executable"
	$(F90) -o $@ -I$(PFUNIT)/mod -I$(PFUNIT)/include -I./ $(FFLAGS) \
		$(PFUNIT)/include/driver.F90 \
		$(TEST_MODULES:=/*$(OBJ_EXT)) \
		common/*$(OBJ_EXT) \
	    -L$(PFUNIT)/lib -lpfunit -L../../lib -lrpe $(LIBS)
	@echo ""


# Over-ride the inherited clean rule so that it also cleans test module
# directories.
clean: local-base0-clean clean-test-modules

# Clean all the test module directories, including the common suite module.
clean-test-modules:
	for dir in $(TEST_MODULES) common; do $(MAKE) -C $$dir clean; done
