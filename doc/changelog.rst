Changelog
=========


v4.1.x
------

:Release: v4.1.1
:Date:

Add deprecation notices to API documentation. There are no changes to the source
code and no need to upgrade from v4.1.0.

:Release: v4.1.0
:Date: 22 August 2016

Adds an IEEE 754 compliant rounding scheme and new functionality for explicit
handling of literal floating-point values.

* Features

  * A new (currently opt-in) IEEE 754 compliant rounding mode, activated by
    setting the module variable ``RPE_IEEE_ROUNDING = .true.``. This option is
    provided to help manage a transition to IEEE 754 compliant rounding in a
    future release. Eventually this option will be removed and IEEE 754
    compliant rounding will become the default and only rounding mode.

  * A new helper function ``rpe_literal`` is provided to help write correct
    reduced-precision code that contains numeric literals.

* Deprecations

  * The current rounding mode (round to nearest) is deprecated. Future releases
    will use the IEEE 754 compliant rounding mode. Users should set
    ``RPE_IEEE_ROUNDING = .true.`` to get the new rounding behaviour. We
    recommend the IEEE 754 rounding mode to ensure best results.


v4.0.x
------

:Release: v4.0.0
:Date: 5 January 2016

This release is a major overhaul of the code with the aim of making it more
portable and reliable.

* Features

  * Compatible with Intel Fortran compilers. The code may work with other
    compilers too, but only Intel and GNU are tested currently.

* Incompatibilities: This release is not compatible with version 3 or below.

  * Removed the ``rpe_shadow`` derived type.
  * Removed abstract base class ``rpe_type``, the only user type is now
    ``rpe_var``.
  * Removed getter and setter methods, the value of an ``rpe_var`` instance is
    now accessed directly using the ``%val`` attribute.


v3.1.x
------

:Release: v3.1.1
:Date: 3 November 2015

* License code under the Apache 2.0 license.

:Release: v3.1.0
:Date: 16 October 2015

* Support for IEEE half-precision emulation via the ``RPE_IEEE_HALF`` module
  variable.


v3.0.x
------

:Release: v3.0.1
:Date: 4 November 2015

* License code under the Apache 2.0 license.

:Release: v3.0.1
:Date: 21 August 2015

* Features

  * Support for different precision levels in different variables:

    * You can set the ``%sbits`` attribute of any ``rpe_type`` instance to
      define the number of significand bits used by that variable.

  * A set of unit tests is included to help us ensure the emulator core is
    robust.

  * HTML documentation is included with the source (requires Sphinx to build).

* Incompatibilities

  * This version is incompatible with the v2.0.x series.

  * The public API is now smaller, including only the required parts of the
    library.

  * Module variables renamed: ``RPE_BITS`` -> ``RPE_DEFAULT_SBITS``

  * Reduction of precision subroutine renamed:
    ``reduce_precision`` -> ``apply_truncation``

  * Internal differences to support mixed precision may cause different
    results to previous versions.


v2.0.x
------

:Release: 2.0.1
:Date: 3 November 2015

* License code under the Apache 2.0 license.

:Release: 2.0.0
:Date: 29 July 2015

* Features

  * Reduce precision on assignment:

    * The precision of the value held within an ``rpe_type`` instance is reduced
      whenever a value is assigned, meaning an ``rpe_var`` instance cannot ever
      store a full precision value, and an ``rpe_shadow`` type will always store
      a reduced precision value when it has been assigned to directly (but one
      can of course assign a full precision value to the variable it is
      shadowing and have that value retained).

    * Explicit calls to ``reduce_precision`` are no longer required in any
      overloaded operators or intrinsic routines, as the reduction of precision
      will be performed implicitly on assignment of the result.

* Incompatibilities

  * The change from explicit reduction of precision within overloaded operators
    and intrinsics will likely cause the emulator to return different results
    than the v1.0.x series.


v1.0.x
------

:Release: v1.0.1
:Date: 4 November 2015

* License code under the Apache 2.0 license.

:Release: 1.0.0
:Date: 28 July 2015

* Features

  * Initial version used operationally for experiments.
