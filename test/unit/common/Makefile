# Makefile for the common suite module for unit testing.
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
