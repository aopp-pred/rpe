# Makefile for reduced-precision emulator tests.
#

MODULEDIR=../modules
LIBDIR=../lib
LIBS=-lrpe

.PHONY: all test_simple test_bits

all: test_simple test_bits

test_simple: test_simple/test.f90 $(LIBDIR)/librpe.a
	gfortran -o test_simple/test.exe -I$(MODULEDIR) test_simple/test.f90 -L$(LIBDIR) $(LIBS)

test_bits: test_bits/test.f90 $(LIBDIR)/librpe.a
	gfortran -o test_bits/test.exe -I$(MODULEDIR) test_bits/test.f90 -L$(LIBDIR) $(LIBS)
