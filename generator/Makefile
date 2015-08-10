# Makefile for the reduced-precision emulator code generator.
#
# Copyright (c) 2015 Andrew Dawson
#

GDIR = generated
CONFIGDIR = configs

.PHONY: all operators intrinsics clean

all: operators intrinsics

operators:
	mkdir -p $(GDIR)
	python python/generate_operators.py \
			   -i $(GDIR)/interface_operators.i \
			   -d $(GDIR)/implementation_operators.f90 \
			   -f $(CONFIGDIR)/operators.json

intrinsics:
	mkdir -p $(GDIR)
	python python/generate_intrinsics.py \
			   -i $(GDIR)/interface_intrinsics.i \
			   -d $(GDIR)/implementation_intrinsics.f90 \
			   -f $(CONFIGDIR)/intrinsics.json

clean:
	rm -f $(GDIR)/*.h
