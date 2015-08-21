==================
Software Structure
==================


The emulator library itself is a Fortran library that can be included in other programs to introduce reduced precision variables.
However, the code repository contains several distinct components which are used to generate the final Fortran library.


The core library
================

The core of the emulator is written in Fortran, and is found in the file ``rawsrc/rp_emulator.F90``.
The emulator library provides two core derived types used to represent reduced precision floating point numbers.
These types have accompanying overloaded operator definitions, allowing them to be used in the same way as the builtin real number type.
For maximum ease of use, many of the builtin Fortran intrinsic functions are also overloaded to accept these reduced precision types.

The ``rawsrc/rp_emulator.F90`` file contains the module definition, the definition of the derived types, and the core functions and subroutines that control the emulator's functionality.
However, it does not contain most of the overloaded operator and intrinsic function definitions.
Instead there are a handful of C preprocessor directives in the file that pull in these definitions from external files in ``rawsrc/include``.
The way in which these external file are generated is described below.


The code generator
==================

A lot of the code required to complete the full emulator is repetetive boiler-plate code; code which is largely the same for a whole group of functions/subroutines.
Because of the repetetive nature of the required Fortran implementations for all overloaded operators and intrinsic functions, it is more efficient to provide only a template of what the code should look like, and let the computer actually write the code.
The code generator for the emulator is written in Python and is included in the ``generator/`` subdirectory.

Using a generator is a big time saver, if you need to change 1 line of every intrinsic function implementation, you only need to modify that line in a few templates and let the code generator write all the actual Fortran code for you.
Not only does this approach save time, it also has positive implications for code correctness.
For example if 50 function implementations are produced by the generator, it is not possible for one of them to contain an error that the others don't, as would often be the case when hand-writing sucha a large number of essentially similar routines.
You write the implementation carefully once, and the boring stuff is done automatically.

Due to its relative complexity, the :doc:`code_generator/index` is documented separately.


Extra definitions
=================

As well as code that is produced by the code generator, there are two more files in ``rawsrc/include`` that can be edited manually: ``interface_extras.i`` and ``implementation_extras.f90``.
You can add arbitrary functions/subroutines to the ``implementation_extras.i`` file, with any required interface definitions in ``interface_extras.i``, and they will be included in the main library source file automatically.
Just make sure you make your interface public, as the default is private.


Tests
=====

The emulator is provided with a suite of basic tests, in the ``tests/`` subdirectory.
It is expected that any change you make will not cause any of the existing tests to fail, and ideally new features should be accompanied with new tests to make sure they work, and continue to work in the future.

The tests are split into two categories, units tests and integration tests.
Unit tests should be relatively self-contained tests of the functionality of a particular small element of the code (code unit).
Ideally these tests are for a single component in isolation from the rest of the system, although the nature of the library means this sometimes is not possible.
Integration tests are more like realistic tests of the software in use, and should tests multiple aspects of the library together (in integration).
