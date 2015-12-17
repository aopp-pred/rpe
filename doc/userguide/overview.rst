========
Overview
========

The library contains a derived type: :f:type:`rpe_var`.
This type can be used in place of real-valued variables to perform calculations with floating-point numbers represented with a reduced number of bits in the floating-point significand.


Basic use of the reduced-precision type
=======================================

The :f:type:`rpe_var` type is a simple container for a double precision floating point value.
Using an :f:type:`rpe_var` instance is as simple as declaring it and using it just as you would a real number:

.. code-block:: fortran

   TYPE(rpe_var) :: myvar

   myvar = 12
   myvar = myvar * 1.287   ! reduced-precision result is stored in `myvar`


Controlling the precision
=========================

The precision used by reduced precision types can be controlled at two different levels.
Each reduced precision variable has an :f:var:`sbits` attribute which controls the number of explicit bits in its significand.
This can be set independently for different variables, and comes into effect after it is explicitly set.

.. code-block:: fortran

   TYPE(rpe_var) :: myvar1
   TYPE(rpe_var) :: myvar2

   ! Use 16 explicit bits in the significand of myvar1, but only 12 in the
   ! significand of myvar2.
   myvar1%sbits = 16
   myvar2%sbits = 12

For variables whose :f:var:`sbits` attribute has not been explicitly set, there is a default global precision level, set by :f:var:`RPE_DEFAULT_SBITS`.
This will stand-in for the number of explicit significand bits in any variable where :f:var:`sbits` has not been set.
Setting :f:var:`RPE_DEFAULT_SBITS` once to define the global default precision, and setting the precision of variables that require other precision manually using the :f:var:`sbits` attribute is generally a good strategy.
However, if you change :f:var:`RPE_DEFAULT_SBITS` during execution, the document :ref:`errors-changing-default-sbits` lists some details you should be aware of.

The emulator can also be turned off completely by setting the module variable :f:var:`RPE_ACTIVE` to ``.FALSE.``.
