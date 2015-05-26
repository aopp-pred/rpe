# Makefile for reduced-precision emulator tests.
#

MODULEDIR=../modules
LIBDIR=../lib
LIBS=-lrpe

.PHONY: all test_simple

all: test_simple

test_simple: test_simple/test.f90
	gfortran -o test_simple/test.exe -I$(MODULEDIR) test_simple/test.f90 -L$(LIBDIR) $(LIBS)
