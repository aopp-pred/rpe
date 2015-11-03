# Makefile for reduced-precision emulator tests.
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

MODULEDIR=../modules
LIBDIR=../lib
LIBS=-lrpe

.PHONY: all test_simple test_bits

all: test_simple test_bits

test_simple: test_simple/test.f90 $(LIBDIR)/librpe.a
	gfortran -o test_simple/test.exe -I$(MODULEDIR) test_simple/test.f90 -L$(LIBDIR) $(LIBS)

test_bits: test_bits/test.f90 $(LIBDIR)/librpe.a
	gfortran -o test_bits/test.exe -I$(MODULEDIR) test_bits/test.f90 -L$(LIBDIR) $(LIBS)
