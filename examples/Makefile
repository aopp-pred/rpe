# Makefile for reduced-precision emulator tests.
#

MODULEDIR=../modules
LIBDIR=../lib
LIBS=-lrpe

.PHONY: all test_basic

all: test_basic

test_basic: test_basic/test.f90 $(LIBDIR)/librpe.a
	gfortran -o test_basic/test.exe -I$(MODULEDIR) test_basic/test.f90 -L$(LIBDIR) $(LIBS)
