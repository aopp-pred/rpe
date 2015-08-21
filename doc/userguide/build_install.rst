=====================
Building the Emulator
=====================


Build requirements
==================

Building the emulator requires:

* GCC (gfortran) >= 4.8
* GNU Make

The emulator has been tested with GCC 4.8.4 and GCC 4.9.2 and is known to work correctly ith these compilers.
No testing of other compilers has been done by us, it might work with other compilers, it might not.


Building
========

The library is built in two stages.
Stage 1 constructs the full emulator source from various components.
Stage 2 compiles the full source code into a library archive.

Stage 1 build
-------------

Stage 1 uses the C preprocessor to construct a single Fortran source file from a base module and extra included header files.
The result of stage 1 is a self-contained source file that can be used directly in other projects if desired, without proceeding to build stage 2.
The output of stage 1 is the file ``src/rp_emulator.f90``.
You can initiate a stage 1 build with:

    make source

Stage 2 build
-------------

Stage 2 compiles the emulator source code into a library archive and a corresponding Fortran module file.
The resulting library is the archive ``lib/librpe.a`` and the module file is ``modules/rp_emulator.mod``.
You can initiate a stage 2 build with:

    make library

The Make target ``all`` will also build the stage 2 library, and is the default target.


Integration
===========

Assuming you did a full (stage 2) build, integration with your project is fairly straightforward, requiring two files to be present, one at compile time and one at link time.

You must make sure that the module file ``rp_emulator.mod`` is available to the compiler when compiling any source file that uses the module.
You can do this by pecifying an include flag at compile time: ``-I/path/to/rp_emulator.mod``.
Alternatively you could place the module file in the same directory as your source files, but it is recommended to store it separately and use an include flag.

At link time the ``librpe.a`` library will also need to be available to the linker.
You can use a combination of linker path and library options to make sure this is the case: ``-L/path/to/librpe.a -lrpe``.
Alternatively, you can directly specify the full path to ``librpe.a`` as an object to include in linking.

Stage 1 builds
--------------

If you wish, you can just do a stage 1 build and include the source code of the emulator directly in your project.
However, you need to use the correct options when compiling the emulator.
The emulator source code may contain some lines longer than the default line-length limit (these lines are prodcued by the code generator).
To make sure the library compiles properly you need to tell the compiler to ignore line-length restrictions.
For gfortran the option is ``-ffree-line-length-none``.
