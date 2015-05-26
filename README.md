# Reduced Precision Emulator

An emulator for reduced-precision floating-point calculations in Fortran.


## Building the library

The library can be built in a two-stage system:

* Stage 1

    Stage 1 uses the C preprocessor to construct a single Fortran source file from a base file and extra included header files.
    The resulting file is self-contained and can be included in other projects if needed.
    The source file resulting from stage 1 is `src/rp_emulator.f90`.

* Stage 2

    Stage 2 compiles the emulator into a library archive and a fortran module, which can be included in other projects directly.
    The resulting library is `lib/librpe.a`, and the module is `modules/rp_emulator.mod`.

If you choose to only use stage 1 of the build process you must allow for the following when compiling:

* The library source contains long lines, so a ocmpiler option to remove line-length restrictions is required.
  For `gfortran` this is `-ffree-line-length-none`.
