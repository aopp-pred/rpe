=====================
Building the Emulator
=====================


Build requirements
==================

Building the emulator requires:

* GCC (gfortran) >= 4.8 or Intel Fortran (ifort) >= 15.0.2
* GNU Make

The emulator has been tested with GCC 4.8.4 and GCC 4.9.2 and Intel Fortran 15.0.2 and is known to work correctly with these compilers.
No testing of other compilers has been done by us, it might work with other compilers, it might not.


Building
========

The library is built using GNU Make and a Fortran compiler. The default action
when invoking `make` is to build the static library `lib/librpe.a` and the
Fortran module `modules/rp_emulator.mod`.

The code is tested with and set-up for building with GNU gfortran or Intel
Fortran (ifort) compilers. Which compiler to use is determined by the value
of the `F90` environment variable. If this variable is not set the Makefile
will use `gfortran`. The default build is a simple invocation of `make`::

    make

It is also possible to build a shared library `lib/librpe.so` using the
`shared` target of the Makefile::

   make shared

You can optionally specify a compiler on the command line::

    F90=ifort make

If you want to use a compiler other than `gfortran` or `ifort` you will
need to specify both the appropriate `F90` variable and the correct `FFLAGS`
for your compiler. Compiler flags are used in the build to ensure the module
`rp_emulator.mod` is placed in the `modules/` directory in the source tree.

Unified source
--------------

The source code for the emulator is split across several files, and makes use
of the C preprocessor to combine them during the build process. If you want to
generate a unified source file for ease of use you can use the `source` target
of the Makefile::

    make source

This will generate the file ``src/rp_emulator.f90`` (note the lower case
extension) which can be integrated into the source of other projects.


Integration
===========

Assuming you did a full build, integration with your project is fairly straightforward, requiring two files to be present, one at compile time and one at link time.

You must make sure that the module file ``rp_emulator.mod`` is available to the compiler when compiling any source file that uses the module.
You can do this by specifying an include flag at compile time.
Alternatively you could place the module file in the same directory as your source files, but normally you would store it separately and use an include flag.

At link time the ``librpe.a`` (or ``librpe.so``) library will also need to be available to the linker.
You can use a combination of linker path and library options to make sure this is the case.
Alternatively, you can directly specify the full path to ``librpe.a`` as an object to include in linking.

For example, let's say we have placed the module file at ``$HOME/rpe/modules/rp_emulator.mod`` and the library at ``$HOME/rpe/lib/librpe.a``, our compilation command must tell the compiler to look in the right place for the module file::

    gfortran -c -I$HOME/rpe/modules myprogram.f90

and our linker command must tell the linker which libraries to link and where to look for them::

    gfortran -o myprogram.exe myprogram.o -L$HOME/rpe/lib -lrpe

The arguments are the same whether linking the static or shared library.


Unified source builds
---------------------

If you wish, you can just build the unified source code of the emulator directly in your project.
