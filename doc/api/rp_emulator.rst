=================================
RPE API: :f:mod:`rp_emulator.mod`
=================================


Derived types
=============

.. f:type:: rpe_type
   :attrs: abstract

   An abstract type defining a reduced-precision floating-point number. You cannot construct an instance of this type, but you can construct an instance of any type that extends this type.

   :f INTEGER sbits: Number of bits in the significand of the floating-point number

   .. f:function:: rpe_type%get_value()

      Retrieve the real value stored within an :f:type:`rpe_type` instance.

      :r REAL value [KIND=RPE_REAL_KIND]: Reduced-precision value stored in a built-in real-typed variable.


.. f:type:: rpe_var
   :attrs: extends(rpe_type)

   A type extending :f:type:`rpe_type` representing a reduced-precision floating-point number.
   This type stores the number it represents internally.

.. f:type:: rpe_shadow
   :attrs: extends(rpe_type)

   A type extending :f:type:`rpe_type` representing a reduced-precision floating-point number.
   The :f:type:`rpe_shadow` provides a memory-view onto an existing double precision floating point number defined outside the type itself.
   Changing the value of the :f:type:`rpe_shadow` also changes the value of the floating point number it is shadowing and vice-versa, since they both refer to the same block of memory.
   However, when values are assigned to the :f:type:`rpe_shadow` their precision is reduced, which is not the case when assigning to the variable being shadowed.

User-callable procedures
========================

.. f:subroutine:: init_shadow (shadow, target)

   Initialize an :f:type:`rpe_shadow` instance by associating it with a
   real-valued variable.

   :param rpe_shadow shadow [INOUT]: The :f:type:`rpe_shadow` instance to initialize.
   :param REAL target [KIND=RPE_REAL_KIND,IN]: A floating-point variable to shadow. This must be a variable defined within the scope of the :f:func:`init_shadow` call otherwise invalid memory references will occur.

.. f:subroutine:: apply_truncation (rpe)
   :attrs: elemental

   Apply the required truncation to the value stored within an :f:type:`rpe_type` instance.
   Operates on the input in-place, modifying its value.
   The truncation is determined by the :f:var:`sbits` attribute of the :f:type:`rpe_type` instance, if this is not set then the value of :f:var:`RPE_DEFAULT_SBITS`.

   :param rpe_type rpe [INOUT]: The :f:type:`rpe_type` instance to alter the precision of.


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

   The default number of bits used in the significand of an :f:type:`rpe_type` instance when not explicitly specified.
   This takes effect internally when determining precision levels, but does not bind an :f:type:`rpe_type` instance to a particular precision level (doesn't set :f:var:`rpe_type%sbits`).


.. f:variable:: RPE_IEEE_HALF
   :type: LOGICAL
   :attrs: default=.FALSE.

   Logical value determining if IEEE half-precision emulation is turned on.
   If set to ``.TRUE.`` and a 10-bit significand is being emulated the emulator will additionally impose range constraints when applying truncation:

   * Values that overflow IEEE half-precision will lead to real overflows with a corresponding floating-point overflow exception.
   * Values out of the lower range of IEEE half-precision will be denormalised.

   This option only affects the emulation when emulating a 10-bit significand.


.. f:variable:: RPE_IEEE_ROUNDING
   :type: LOGICAL
   :attrs: default=.FALSE.

    Logical value determining if full IEEE 754 rounding rules should be used.
    If ``.TRUE.`` then a *"round to nearest, tie to even"* rounding scheme will be used, which proceeds as normal rounding to the nearest representable number, except in the special case where a number is halfway between two representations where it will be rounded so that the least significant bit of the results is a zero.
    If ``.FALSE.`` then then rounding scheme rounds numbers halfway between two representations to the representation with larger absolute value.

    .. note::

       It is recommended to set this option to ``.TRUE.``. Currently the default is ``.FALSE.`` for backwards compatibility reasons. In a future release the bahaviour of the ``.TRUE.`` setting will become the default (and possibly only) option.


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
