# Makefile for the L63 integration test suite.
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

# Make sure a fortran compiler is defined.
ifneq ($(origin F90), environment)
F90 = gfortran
endif

refsrc := l63_reference.f90
refobj := l63_reference.o
refexe := l63ref.exe

redsrc := l63_reduced_precision.f90
redobj := l63_reduced_precision.o
redexe := l63red.exe

clisrc := cliargs_mod.f90
cliobj := cliargs_mod.o
climod := cliargs_mod.mod

# Location of the required module and library.
moduledir := ../../../modules
libdir := ../../../lib

# Phony targets for building the suite, each executable, and cleanup.
.PHONY: build-tests l63_reference l63_reduced clean

build-tests: l63_reference l63_reduced
l63_reference: $(refexe)
l63_reduced: $(redexe)

# Build the command line support library:
$(cliobj): $(clisrc)
	$(F90) -c $^
$(climod): $(cliobj)

# Reference model executable:
$(refobj): $(refsrc)
	$(F90) -c $^

$(refexe): $(refobj)
	$(F90) -o $@ $<

# Reduced-precision executable:
$(redobj): $(redsrc) $(climod)
	$(F90) -c -I$(moduledir) -O0 $<

$(redexe): $(redobj) $(cliobj)
	$(F90) -o $@ $^ -L$(libdir) -lrpe

# Clean the suite.
clean:
	$(RM) *.mod *.o *.exe
