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

* The library source contains long lines, so a compiler option to remove line-length restrictions is required.
  For `gfortran` this is `-ffree-line-length-none`.


## Using the library

The library contains two new types: `rpe_var` and `rpe_shadow`.
Both of these types are subclasses of the abstract type `rpe_type`.

The `rpe_var` type is a simple container for a double precision floating point value.

The `rpe_shadow` type acts as a memory-view onto an existing double precision floating point number defined outside the type itself.
Changing the value of the shadow also changes the value of the floating point number it is shadowing and vice-versa, since they both refer to the same block of memory.

Using an `rpe_var` instance is as simple as declaring it and using it just as you would a real number:

    ...
    type(rpe_var) :: myvar
    myvar = 12
    myvar = myvar * 1.287
    ...

An `rpe_shadow` instance is different, it must first be initialized to point at an existing real-valued variable before it can be used:

    ...
    real(kind=rpe_real_kind) :: a
    type(rpe_shadow) :: myvar
    call init_shadow(myvar, a)
    myvar = 12   ! both `myvar` and `a` hold the value 12.0
    myvar = myvar * 1.287
    ...

### Controlling the precision

The precision used for calculations can be controlled by two module-level variables.

The logical variable `RPE_ACTIVE` (defaults to `.TRUE.`) controls whether or not reduced precision is used.
It may be set to `.FALSE.` for portions of code that should operate in full precision.

The integer variable `RPE_BITS` (defaults to 23) controls the number of bits that are used in the significand/mantissa of reduced precision floating-point operations.
