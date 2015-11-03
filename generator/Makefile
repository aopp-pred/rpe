# Makefile for the reduced-precision emulator code generator.

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
