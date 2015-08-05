# Makefile for the common suite module for unit testing.
#

# Include the base pFUnit makefile.
include $(PFUNIT)/include/base.mk

# Define the source file containing the module, and the names of the
# Fortran module file and object file that will be generated from it.
SRC = suite_common.f90
MOD = $(SRC:.f90=.mod)
OBJ = $(SRC:.f90=$(OBJ_EXT))

# Declare a phony target which builds the module, this target is called by
# the main unit tests make file.
.PHONY: module

# Declare the required target for building the module.
module: $(MOD) $(OBJ)

# Build the object file from the source file using the fortran compiler.
$(OBJ): $(SRC)
	$(F90) -I../../../modules -c $(FFLAGS) $(FPPFLAGS) $<

# The module file will be created when the object file is generated, so link
# them with a simple dependency.
$(MOD): $(OBJ)
