# Makefile for the reduced-precision emulator.
#
# Uses a two-stage build, first the full library source is generated
# manually using the C preprocessor, which yields a single source file
# which can be compiled into the emulator library and associated module.
#

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

# Convenience targets for the source code and compiled library:
.PHONY: all source library
all: source library
source: $(source)
library: $(lib)

# Generate the full source listing using the C preprocessor:
$(source): $(gensrc) $(geninc)
	cpp -I$(genincdir) $(gensrc) | sed '/^#/d' > $(source)

# Compile the emulator source to generate a module and an object file:
$(module): $(source)
	gfortran -c -ffree-line-length-none -J$(moduledir) $(source) -o $(object)
$(object): $(source) $(module)

# Create a library archive from the compiled code:
$(lib): $(object)
	ar curv $(lib) $(object)

# Cleanup tasks:
clean:
	rm -f $(lib) $(module) $(object)

fullclean: clean
	rm -f $(source)
