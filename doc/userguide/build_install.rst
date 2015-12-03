=====================
Building the Emulator
=====================


Build requirements
==================

Building the emulator requires:

* GCC (gfortran) >= 4.8 or Intel Fortran (ifort) >= 15.0.2
* GNU Make

The emulator has been tested with GCC 4.8.4 and GCC 4.9.2 and Intel Fortran 15.0.2 and is known to work correctly ith these compilers.
No testing of other compilers has been done by us, it might work with other compilers, it might not.


Building
========

The library is built using GNU Make and a fortran compiler. The default action
when invoking `make` is to build the static library `lib/librpe.a` and the
Fortran module `modules/rp_emulator.mod`.

The code is tested with and set-up for building with GNU gfortran or Intel
Fortran (ifort) compilers. Which compiler to use is determined by the value
of the `F90` environment variable. If this variable is not set the Makefile
will use `gfortran`. The default build is a simple invocation of `make`:

    make

You can optionally specify a compiler on the command line:

    F90=ifort make

If you want to use a compiler other than `gfortran` or `ifort` you will
need to specify both the appropriate `F90` variable and the correct `FFLAGS`
for your compiler, bearing in mind the source code contains some lines over
132 characters in length. Compiler flags are also used to ensure the module
`rp_emulator.mod` is placed in the `modules/` directory in the source tree.

Unified source
--------------

The source code for the emulator is split across several files, and makes use
of the C preprocessor to combine them during the build process. If you want to
generate a unified source file for ease of use you can use the `source` target
of the Makefile:

    make source

This will generate the file `src/rp_emulator.f90` (note the lower case
extension) which can be integrated into the source of other projects.

If you choose to work with the unified source program you may need to allow
for the long lines when compiling it (for gfortran you need the
`-ffree-line-length-none` compiler flag, other compilers may require other
flags).


Integration
===========

Assuming you did a full build, integration with your project is fairly straightforward, requiring two files to be present, one at compile time and one at link time.

You must make sure that the module file ``rp_emulator.mod`` is available to the compiler when compiling any source file that uses the module.
You can do this by pecifying an include flag at compile time: ``-I/path/to/rp_emulator.mod``.
Alternatively you could place the module file in the same directory as your source files, but it is recommended to store it separately and use an include flag.

At link time the ``librpe.a`` library will also need to be available to the linker.
You can use a combination of linker path and library options to make sure this is the case: ``-L/path/to/librpe.a -lrpe``.
Alternatively, you can directly specify the full path to ``librpe.a`` as an object to include in linking.

Unified source builds
---------------------

If you wish, you can just build the unified source code of the emulator directly in your project.
However, you need to use the correct options when compiling the emulator.
The emulator source code may contain some lines longer than the default line-length limit for some compilers (these lines are prodcued by the code generator).
To make sure the library compiles properly you might need to tell the compiler to ignore line-length restrictions.
For example, when using `gfortran` the compiler option ``-ffree-line-length-none`` is needed.
