==============================================================
Differences between :f:type:`rpe_var` and :f:type:`rpe_shadow`
==============================================================

Although both the :f:type:`rpe_var` and :f:type:`rpe_shadow` types are designed to do the same thing (and are both extensions of the `rpe_type` type), there are some subtle differences in the way they are used.

Storage of full-precision values inside reduced precision types
===============================================================

The basic idea of the reduced-precision types is that they hold reduced-precision values.
This is always true for the :f:type:`rpe_var` (see :ref:`errors-changing-default-sbits` for caveats to this), but it is not necessarily the case for the :f:type:`rpe_shadow` type.
This is because the value stored in an :f:type:`rpe_shadow` type can be set both by assigning to the :f:type:`rpe_shadow` instance *and* by assigning to the real variable it shadows.
When assigning directly to the :f:type:`rpe_shadow` instance the precision of the stored value will be altered according to the :f:var:`sbits` attribute, but when assigning to the real variable it shadows the assignment does not reduce the precision of the stored value.
This is perhaps best illustrated with some examples.

Example 1
---------

Assigning directly to a shadow, the precision is reduced to 8 bits as expected:

.. code-block:: fortran

   PROGRAM example1

       USE rp_emulator
       IMPLICIT NONE

       REAL(KIND=RPE_REAL_KIND) :: target
       TYPE(rpe_shadow)         :: shadow

       ! Use 8 bits of precision in the shadow's significand.
       shadow%sbits = 8
       CALL init_shadow (shadow, target)

       ! Assign a value directly to the shadow.
       shadow = 3.2812147

       ! Write the value contained within the shadow variable.
       WRITE (*, *) shadow%get_value()

   END PROGRAM

Output:

    3.2812500000000000


Example 2
---------

Assigning to the variable being shadowed, the precision is not reduced:

.. code-block:: fortran

   PROGRAM example2

       USE rp_emulator
       IMPLICIT NONE

       REAL(KIND=RPE_REAL_KIND) :: target
       TYPE(rpe_shadow)         :: shadow

       ! Use 8 bits of precision in the shadow's significand.
       shadow%sbits = 8
       CALL init_shadow (shadow, target)

       ! Assign a value to the variable being shadowed.
       target = 3.2812147

       ! Write the value contained within the shadow variable.
       WRITE (*, *) shadow%get_value()

   END PROGRAM

Output:

    3.2812147140502930


Example 3
---------

Shadowing a variable that is already assigned, the precision is not reduced:

.. code-block:: fortran

   PROGRAM example3

       USE rp_emulator
       IMPLICIT NONE

       REAL(KIND=RPE_REAL_KIND) :: target
       TYPE(rpe_shadow)         :: shadow

       ! Assign a value to the variable that will be shadowed later.
       target = 3.2812147

       ! Use 8 bits of precision in the shadow's significand.
       shadow%sbits = 8
       CALL init_shadow (shadow, target)

       ! Write the value contained within the shadow variable.
       WRITE (*, *) shadow%get_value()

   END PROGRAM

Output:

    3.2812147140502930


Resolutions
===========

The illustrated behaviour is by design.
To ensure consistency between implementations using :f:type:`rpe_var` and :f:type:`rpe_shadow` you need to make some manual calls to :f:subr:`apply_truncation`.
A slightly modified version of example 3 is given below, with an extra :f:subr:`apply_truncation` call inserted to ensure the value stored within the shadow has reduced-precision.

A simple fix
------------

.. code-block:: fortran

   PROGRAM simplefix

       USE rp_emulator
       IMPLICIT NONE

       REAL(KIND=RPE_REAL_KIND) :: target
       TYPE(rpe_shadow)         :: shadow

       ! Assign a value to the variable that will be shadowed later.
       target = 3.2812147

       ! Use 8 bits of precision in the shadow's significand.
       shadow%sbits = 8
       CALL init_shadow (shadow, target)
       CALL apply_truncation (shadow)

       ! Write the value contained within the shadow variable.
       WRITE (*, *) shadow%get_value()

   END PROGRAM

Output:

    3.2812500000000000

Realistic examples
==================

The :f:type:`rpe_shadow` type is designed to make changing only parts of an existing code simpler.
A good example of this is introducing reduced-precision types into a routine that then makes calls to subroutines that operate in full precision.
Below are two equivalent programs, one using the :f:type:`rpe_var` type and one using the :f:type:`rpe_shadow` type.
The programs are very similar, but the way calls to full-precision subroutines are made is different.

Using :f:type:`rpe_var`
-----------------------

.. code-block:: fortran

   PROGRAM myprog_var
   ! Program with main variable replaced by a reduced precision variable.
   !

       USE rp_emulator
       IMPLICIT NONE

       ! A reduced precision variable.
       TYPE(rpe_var)            :: reduced_value
       ! A temporary variable needed later.
       REAL(KIND=RPE_REAL_KIND) :: tmp_real_value

       ! Use 8 bits of significand precision for the reduced precision
       ! variable
       reduced_value%sbits = 8

       ! Assign a value to the reduced precision variable. In a real program
       ! there could be an arbitrary mount of computation here.
       reduced_value = 3.2812147

       ! Call the subroutine modify_value, this expects a real number rather
       ! than a reduced precision type, so we have to make a copy of our
       ! value to feed in and then copy the resulting modified value back into
       ! our reduced precision variable.
       tmp_real_value = reduced_value
       CALL modify_value (tmp_real_value)
       reduced_value = tmp_real_value

       ! Write the value contained within the shadow variable.
       WRITE (*, *) reduced_value%get_value()

   CONTAINS

       SUBROUTINE modify_value (x)
       ! Performs some simple modification on a real number, in this case
       ! just squares it, but this could be any black-box operation performed
       ! in full precision.
       !
           REAL(KIND=RPE_REAL_KIND), INTENT(INOUT) :: x
           x = x * x
       END SUBROUTINE modify_value

   END PROGRAM

Output:

    10.781250000000000

Using :f:type:`rpe_shadow`
--------------------------

.. code-block:: fortran

   PROGRAM myprog_shadow
   ! Program with main variable replaced by a reduced precision variable.
   !

       USE rp_emulator
       IMPLICIT NONE

       ! A real variable (the main variable) and a reduced precision shadow
       ! variable.
       REAL(KIND=RPE_REAL_KIND) :: real_value
       TYPE(rpe_shadow)         :: reduced_value

       ! Use 8 bits of significand precision for the reduced precision
       ! variable
       reduced_value%sbits = 8
       CALL init_shadow (reduced_value, real_value)

       ! Assign a value to the reduced precision variable. In a real program
       ! there could be an arbitrary mount of computation here.
       reduced_value = 3.2812147

       ! Call the subroutine modify_value, this expects a real number rather
       ! than a reduced precision type. Since the variables 'real_value' and
       ! 'reduced_value' refer to the same block of memory we can just input
       ! the 'real_value' variable and modifications to it will also appear in
       ! 'reduced_value'. However, since the modification is done in double
       ! precision, we'll need to call apply_truncation to ensure the resulting
       ! value is stored at reduced precision.
       CALL modify_value (real_value)
       CALL apply_truncation (reduced_value)

       ! Write the value contained within the shadow variable.
       WRITE (*, *) reduced_value%get_value()

   CONTAINS

       SUBROUTINE modify_value (x)
       ! Performs some simple modification on a real number, in this case
       ! just squares it, but this could be any black-box operation performed
       ! in full precision.
       !
           REAL(KIND=RPE_REAL_KIND), INTENT(INOUT) :: x
           x = x * x
       END SUBROUTINE modify_value

   END PROGRAM

Output:

    10.781250000000000
