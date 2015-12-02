# Makefile for the reduced-precision emulator.
#
# Uses a two-stage build, first the full library source is generated
# manually using the C preprocessor, which yields a single source file
# which can be compiled into the emulator library and associated module.
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

# Source file and headers used to generate the library code:
gensrc = rawsrc/rp_emulator.F90
genincdir = rawsrc/include
geninc = $(wildcard $(genincdir)/*.i $(genincdir)/*.f90)

# The processed source file:
source = src/rp_emulator.f90

# The module and library resulting from compiling the processed source:
object = src/rp_emulator.o
moduledir = modules
module = $(moduledir)/rp_emulator.mod
lib = lib/librpe.a

# Set the variable F90 if it is not set in the environment.
ifneq ($(origin F90), environment)
F90 = gfortran
endif

# Set the required compiler flags for each supported compiler.
ifeq ($(F90), gfortran)
FFLAGS += -ffree-line-length-none -J$(moduledir)
endif
ifeq ($(F90), ifort)
FFLAGS += -module $(moduledir)
endif

# Convenience targets for the source code and compiled library:
.PHONY: all source library
all: source library
source: $(source)
library: $(lib) source

# Generate the full source listing using the C preprocessor:
$(source): $(gensrc) $(geninc)
	cpp -I$(genincdir) $(gensrc) | sed '/^#/d' > $(source)

# Compile the emulator source to generate a module and an object file:
$(object): $(source)
	$(F90) -c $(FFLAGS) $(source) -o $(object)
$(module): $(source) $(object)

# Create a library archive from the compiled code:
$(lib): $(object)
	ar curv $(lib) $(object)

# Cleanup tasks:
clean:
	rm -f $(lib) $(module) $(object)

fullclean: clean
	rm -f $(source)
