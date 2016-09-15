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


.. f:function:: rpe_literal (x, n)

   Construct an :f:type:`rpe_var` instance from a value.
   Optionally a number of significand bits used to store the value may be given.
   The value will be truncated before storage.
   A typical usage of this constructor is to support reduced precision literal values without having to declare extra variables:

   .. code-block:: fortran

      type(rpe_var) :: a, b, c, d, e

      RPE_DEFAULT_SBITS = 10

      ! variables a, b and c have a 10-bit significands:
      a = 5.00
      b = 7.23
      c = 21.12

      ! The literals 7.29e-5, 3.14 and 18.17 have full precision
      ! (a 23-bit significand):
      d = 7.29e-5 * a + 3.14 * b + 18.17 * c  ! d = 406.5

      ! The literals are replaced by rpe_var instances with 10-bit significands:
      e = rpe_literal(7.29e-5) * a + rpe_literal(3.14) * b + rpe_literal(18.17) * c  ! e = 406.75

   :param x [IN]: A real or integer value to store in the resulting :f:type:`rpe_var` instance.
   :param integer n [IN,OPTIONAL]: An optional number of significand bits used to represent the number, equivalent of setting the :f:var:`sbits` attribute of an :f:type:`rpe_var` instance. If not specified then the resulting :f:type:`rpe_var` will use the default precision specified by :f:var:`RPE_DEFAULT_SBITS`.

   .. note::

      If you use this to create an :f:type:`rpe_var` instance and then assign the result to another instance, only the value will be copied:

      .. code-block:: fortran

         ! An rpe_var instance with a 13 bit significand:
         type(rpe_var) :: a
         a%sbits = 13

         ! Construct an rpe_type instance with a 15-bit significand and assign to a,
         ! the value will be truncated to 13 bits and the variable a will continue
         ! to store only 13 significand bits.
         a = rpe_literal(1.23456789, 15)


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
   :attrs: default=52

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
