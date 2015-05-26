# Makefile for the reduced-precision emulator code generator.
#
# Copyright (c) 2015 Andrew Dawson
#

GDIR = generated

.PHONY: all operators intrinsics clean

all: operators intrinsics

operators:
	mkdir -p $(GDIR)
	python python/generate_operators.py \
			   -i $(GDIR)/interface_operators.h \
			   -d $(GDIR)/implementation_operators.h \
			   -f

intrinsics:
	mkdir -p $(GDIR)
	python python/generate_intrinsics.py \
			   -i $(GDIR)/interface_intrinsics.h \
			   -d $(GDIR)/implementation_intrinsics.h \
			   -f

clean:
	rm -f $(GDIR)/*.h
