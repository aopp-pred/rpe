==========================
Potential Sources of Error
==========================

There are a few dark corners of both the library and Fortran itself which may
catch out unsuspecting users.

Initialization on assignment
============================

The emulator overloads the assignment operator allowing you to assign integers, real numbers, and other :f:type:`rpe_var` instances to any :f:type:`rpe_var` instance:

.. code-block:: fortran

   PROGRAM assign_other_types

       USE rp_emulator
       IMPLICIT NONE

       TYPE(rpe_var) :: x

       ! Assign an integer value to x.
       x = 4

       ! Assign a single-precision real value to x.
       x = 4.0

       ! Assign a double-precision real value to x.
       x = 4.0d0

   END PROGRAM

However, you cannot perform the same assignments at the time the :f:type:`rpe_var` is defined.
This is not allowed:

.. code-block:: fortran

   PROGRAM assign_other_types

       USE rp_emulator
       IMPLICIT NONE

       TYPE(rpe_var) :: x = 4

   END PROGRAM

Compiling this code with `gfortran` will give the following compilation error:

.. code-block:: fortran

       TYPE(rpe_var) :: x = 4
                           1
   Error: Can't convert INTEGER(4) to TYPE(rpe_var) at (1)

This is because this form of assignment does not actually call the overloaded assignment operator defined for :f:type:`rpe_var` instances, instead it calls the default constructor for the derived type, which only allows the right-hand-side of the assignment to be another :f:type:`rpe_var` instance.


.. _errors-changing-default-sbits:

Changing :f:var:`RPE_DEFAULT_SBITS` during execution
====================================================

You are able to change the value of the module variable :f:var:`RPE_DEFAULT_SBITS` to anything you like at any time you like.
However, you need to be aware of the potential inconsistencies you might introduce by doing so.


.. code-block:: fortran

   PROGRAM change_default_bits1

       USE rp_emulator
       IMPLICIT NONE

       TYPE(rpe_var) :: pi

       RPE_DEFAULT_SBITS = 16

       ! This value of Pi will be stored with only 16 bits in the mantissa.
       pi = 3.1415926535897932d0
       WRITE (*, '("RPE_DEFAULT_SBITS=16, pi=", F20.18)') pi%get_value()

       ! Doing this means that whilst any operations on Pi following will assume
       ! 4 bits of significand precision, the value currently stored still has 16
       ! bits of significand precision
       RPE_DEFAULT_SBITS = 4
       WRITE (*, '("RPE_DEFAULT_SBITS=4,  pi=", F20.18)') pi%get_value()

   END PROGRAM

Output:

.. code-block:: fortran

   RPE_DEFAULT_SBITS=16, pi=3.141601562500000000
   RPE_DEFAULT_SBITS=4,  pi=3.141601562500000000

To avoid any issues you may want to insert manual calls to :f:subr:`apply_truncation` to ensure every variable used within the scope of the changed default is represented at the required precision.

In other circumstances this may not be a problem at all, for example around encapsulated subroutine calls.
In the example below the procedure :f:subr:`some_routine` takes no reduced precision types as arguments, but does work with reduced precision types internally, and in this case setting the default number of bits around the subroutine call is a useful way to set the default precision of all reduced precision variables within the subroutine (and within any routines it calls):

.. code-block:: fortran

   RPE_DEFAULT_SBITS = 4
   CALL some_routine (a, b, c)
   RPE_DEFAULT_SBITS = 16

Whatever you choose to do, you need to make sure you have considered this potential issue before you change the value of :f:var:`RPE_DEFAULT_SBITS` at run-time.


Parallel and thread safety
==========================

The default number of bits for any reduced precision type is controlled by a module variable :f:var:`RPE_DEFAULT_SBITS`.
This is a mutable variable that can be changed dynamically during program execution if desired.
If the application using the emulator is parallelised then the behaviour of the default bits setting needs to be considered.

For MPI parallelism, each MPI task will get its own separate instance of the :f:var:`RPE_DEFAULT_SBITS` variable, and modifying it within a task will only affect the default precision within that task (unless programmed otherwise using message passing).

For threaded parallelism (e.g. OpenMP) the behaviour is less clear.
Depending on the threading type used, the variable may be shared by threads, or each may have its own copy.

For parallel applications, care must be taken when changing the value of :f:var:`RPE_DEFAULT_SBITS` at run time to make sure the implementation is safe.


Non-equivalence of single and compound operations
=================================================

One would normally expect the following operations to yield identical results:

.. code-block:: fortran

   a * a * a

and

.. code-block:: fortran

   a ** 3

However, due to the way the emulator works by doing individual computations in full precision and reducing the precision of the result, these two would not necessarily yield the same result if ``a`` were a reduced precision variable.
In the first example the compound multiplication would be done in two parts, the first part computes ``a * a`` and the precision of the temporary result is reduced, then the second part multiplies this reduced precision result by ``a`` and once again reduces the precision of the final result.
However, in the second example a single operation is used to express the computation, this computation will be performed in full precision and the result will have its precision reduced.
Whereas the first example uses reduced precision to store intermediate results, the second does not.

This is true for any operator that can be expressed as multiple invocations of other operators.
