=================================
RPE API: :f:mod:`rp_emulator.mod`
=================================


Derived types
=============

.. f:type:: rpe_var

   A type representing a reduced-precision floating-point number.
   This type stores the number it represents internally.

   :f INTEGER sbits: Number of bits in the significand of the floating-point number
   :f REAL val [KIND=RPE_REAL_KIND]: The real value stored within the instance.


User-callable procedures
========================


.. f:subroutine:: apply_truncation (rpe)
   :attrs: elemental

   Apply the required truncation to the value stored within an :f:type:`rpe_var` instance.
   Operates on the input in-place, modifying its value.
   The truncation is determined by the :f:var:`sbits` attribute of the :f:type:`rpe_var` instance, if this is not set then the value of :f:var:`RPE_DEFAULT_SBITS`.

   :param rpe_var rpe [INOUT]: The :f:type:`rpe_var` instance to alter the precision of.


.. f:function:: significand_bits (x)
   :attrs: elemental

   Determine the number of significand bits being used by the input types.
   For inputs that are :f:type:`rpe_var` instances this function returns the number of significand bits in use by the reduced-precision number.
   For real numbers it will return either 23 for single-precision inputs or 52 for double-precision inputs.
   For all other input types the result will be zero.

   :param x [IN]: Any Fortran type.


Variables
=========

.. f:variable:: RPE_ACTIVE
   :type: LOGICAL
   :attrs: default=.TRUE.

   Logical value determining whether emulation is on or off.
   If set to ``.FALSE.`` then calls to :f:subr:`apply_truncation` will have no effect and all operations will be carried out at full precision.


.. f:variable:: RPE_DEFAULT_SBITS
   :type: INTEGER
   :attrs: default=23

   The default number of bits used in the significand of an :f:type:`rpe_var` instance when not explicitly specified.
   This takes effect internally when determining precision levels, but does not bind an :f:type:`rpe_var` instance to a particular precision level (doesn't set :f:var:`rpe_var%sbits`).


.. f:variable:: RPE_IEEE_HALF
   :type: LOGICAL
   :attrs: default=.FALSE.

   Logical value determining if IEEE half-precision emulation is turned on.
   If set to ``.TRUE.`` and a 10-bit significand is being emulated the emulator will additionally impose range constraints when applying truncation:

   * Values that overflow IEEE half-precision will lead to real overflows with a corresponding floating-point overflow exception.
   * Values out of the lower range of IEEE half-precision will be denormalised.

   This option only affects the emulation when emulating a 10-bit significand.


.. f:variable:: RPE_FAST_MODE
   :type: LOGICAL
   :attrs: default=.FALSE.

   Logical value determining whether or not to use a faster algorithm to reduced precision.
   The faster algorithm uses floating-point operations to bit-shift the value to the required precision, and takes approximately 2/3 of the time as the standard algorithm.

   .. note::

      With ``RPE_FAST_MODE=.TRUE.`` the results will not be the same as with ``RPE_FAST_MODE=.FALSE.`` due to differences in rounding schemes.

   .. warning::

      It is possible that this scheme will generate overflow errors when working with extremely large numbers truncated to a small number of bits.
      If this affects you then you should not use this mode.


Parameters
==========

.. f:variable:: RPE_DOUBLE_KIND
   :type: INTEGER

   The kind number for double precision real types.

.. f:variable:: RPE_SINGLE_KIND
   :type: INTEGER

   The kind number for single precision real types.

.. f:variable:: RPE_REAL_KIND
   :type: INTEGER

   The kind number of the real-values held by reduced precision types.
   This is a reference to :f:var:`RPE_DOUBLE_KIND`, but could be changed (in source) to be :f:var:`RPE_SINGLE_KIND`.

.. f:variable:: RPE_ALTERNATE_KIND
   :type: INTEGER

   The kind number of an alternate type of real-value.
   This is a reference to :f:var:`RPE_SINGLE_KIND`, but can be changed (in source) if the value referenced by :f:var:`RPE_REAL_KIND` is changed.
