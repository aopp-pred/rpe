# Change-log


## v3.1.x


### Release v3.1.1 (3 November 2015)

* Licensed under Apache 2.0 license.


### Release v3.1.0 (16 October 2015)

#### Features

* Support for IEEE half-precision emulation via the `RPE_IEEE_HALF` module variable.



## v3.0.x


### Release v3.0.1 (4 November 2015)

* Licensed under Apache 2.0 license.


### Release v3.0.0 (21 August 2015)

#### Features

* Support for different precision levels in different variables:
  - You can set the `%sbits` attribute of any `rpe_type` instance to define
    the number of significand bits used by that variable.

* A set of unit tests is included to help us ensure the emulator core is robust.

* HTML documentation is included with the source (requires Sphinx to build).

#### Incompatibilities

This version is incompatible with the v2.0.x series.

* The public API is now smaller, including only the required parts of the library.

* Module variables renamed: `RPE_BITS` -> `RPE_DEFAULT_SBITS`

* Reduction of precision subroutine renamed: `reduce_precision` -> `apply_truncation`

* Internal differences to support mixed precision may cause different
  results to previous versions.



## v2.0.x


### Release v2.0.1 (4 November 2015)

* License under Apache 2.0 license.


### Release v2.0.0 (29 July 2015)

#### Features

* Reduce precision on assignment:
  - The precision of the value held within an `rpe_type` instance is reduced
    whenever a value is assigned, meaning an `rpe_var` instance cannot ever
    store a full precision value, and an `rpe_shadow` type will always store
    a reduced precision value when it has been assigned to directly (but one
    can of course assign a full precision value to the variable it is
    shadowing and have that value retained).
  - Explicit calls to `reduce_precision` are no longer required in any
    overloaded operators or intrinsic routines, as the reduction of precision
    will be performed implicitly on assignment of the result.

#### Incompatibilities

* The change from explicit reduction of precision within overloaded operators
  and intrinsics will likely cause the emulator to return different results
  than the v1 series.



## v1.0.x


### Release v1.0.1 (4 November 2015)

* License under Apache 2.0 license.


### Release v1.0.0 (28 July 2015)

#### Features

* Initial version used operationally for experiments.
