# Makefile for reduced-precision emulator tests.

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

MODULEDIR=../modules
LIBDIR=../lib
LIBS=-lrpe

# Set the variable F90 if it is not set in the environment.
ifneq ($(origin F90), environment)
F90 = gfortran
endif

# Set the required compiler flags for each supported compiler.
ifeq ($(F90), gfortran)
FFLAGS += -Jbasic
endif
ifeq ($(F90), ifort)
FFLAGS += -module basic
endif

.PHONY: all test_basic clean

all: test_basic

test_basic: basic/test.f90 $(LIBDIR)/librpe.a
	$(F90) -o basic/test.exe -I$(MODULEDIR) $(FFLAGS) basic/test.f90 -L$(LIBDIR) $(LIBS)

clean:
	$(RM) basic/*.o basic/*.mod basic/*.exe
