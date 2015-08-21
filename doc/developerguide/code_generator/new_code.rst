===========================================
Adding New Operators or Intrinsic Functions
===========================================


Extending the emulator by adding new overloaded operators or intrinsic functions is fairly straightforward, and usually requires only editing configuration files.


.. _pygen-adding-intrinsic:

Adding a new intrinsic function
===============================

In this example we'll add the `GAMMA`_ gfortran intrinsic to the emulator, allowing us to evaluate the :math:`\Gamma` function with a reduced precision input.


Overview of existing definitions
--------------------------------

We'll need to add a new entry to the ``intrinsics.json`` file in ``generator/configs`` to define this new function, but first let's take a look at some of the existing function definitions:

.. code-block:: json

   {"intrinsics":
       [
           ...
           {"epsilon":
               {
                   "return_type": "rpe_var",
                   "interface_types": ["1argscalar"]
               }
           },
           ...
           {"floor":
               {
                   "return_type": "integer",
                   "interface_types": ["1argelemental"]
               }
           },
           ...
       ]
   }

Each function is defined as a JSON object, with a name that corresponds to the name of the intrinsic function, and two attributes that determine the return type of the function and the types of interface the function has.

In the above snippet you can see that the function ``epsilon`` has a return type of ``"rpe_var"``, which corresponds to an :f:type:`rpe_var` instance.
The function ``floor`` has a return type of ``"integer"``, which just corresponds to a normal Fortran integer type.
A complete list of types that can be used in configuartion files can be found in :ref:`pygen-reference-fortran-types`.

The interface type is a concept used within the code generator to work out what kind of code it should produce.
You can see that the function ``epsilon`` has interface type ``"1argscalar"`` which corresponds to a function that takes one scalar as an argument.
The function ``floor`` has interface type ``"1argelemental"`` which corresponds again to a function that takes one argument, but this time the function is elemental meaning it can take a scalar or an array as input, and operate element-wise on array inputs returning an array output.
A function can have more than one interface type if it has multiple interfaces (e.g. ``atan``).
See :ref:`pygen-reference-interface-types` for a list of all intrinsic interface types.


Writing the new definition
--------------------------

From the gfortran `GAMMA`_ documentation we can see that our new function should accept a single input of a reduced precision number, and return a single output which will also be a reduced precision number.
We can also see that the function should be elemental, meaning it can be applied element-wise to an array of input values.
Therefore our definition in ``intrinsics.json`` should look like this:

.. code-block:: json
   :emphasize-lines: 4-9

   {"intrinsics":
       [
           ...
           {"gamma":
               {
                   "return_type": "rpe_var"
                   "interface_types": ["1argelemental"]
               }
           }
       ]
   }

.. warning::

   Never use ``"rpe_shadow"`` as the return type of a function as it is not possible to properly initialize an :f:type:`rpe_shadow` instance within the limited scope of a function and have it work correctly in the external scope of the function's return value. If you want to return a reduced precision value from a function always use an :f:type:`rpe_var` type by specifying ``"rpe_var"``.


Generating the code
-------------------

Now that you have created the definition for `GAMMA`_ you need to use the generator to actually write the code.
The simplest way to do this is by using the Makefile in the ``generator/`` directory::

    cd generator/
    make

This command will generatre a new set of files in the ``generated/`` subdirectory, and you can inspect these to verify that correct code was written for a `GAMMA`_ imnplementation on reduced precision types.
First lets look at ``interface_intrinsics.i``, it now has these extra lines:

.. code-block:: fortran

   PUBLIC :: gamma
   INTERFACE gamma
       MODULE PROCEDURE gamma_rpe
   END INTERFACE gamma

These lines define a public interface for a function ``gamma``, with one member function called ``gamma_rpe``.
Now let's look in the newly generated ``implementation_intrinsics.f90`` to see the implementation of ``gamma_rpe``:

.. code-block:: fortran

   !-------------------------------------------------------------------
   ! Overloaded definitions for 'gamma':
   !

   ELEMENTAL FUNCTION gamma_rpe (a) RESULT (x)
       CLASS(rpe_type), INTENT(IN) :: a
       TYPE(rpe_var) :: x
       x%sbits = significand_bits(a)
       x = GAMMA(a%get_value())
   END FUNCTION gamma_rpe

The generated implementation consists of a single elemental function definition accepting any :f:type:`rpe_type` type or a type that extends it (either :f:type:`rpe_var` or :f:type:`rpe_shadow`) and returns an :f:type:`rpe_var` type.
The body of the function is simple, it simply sets the nmumber of bits in the significand of the return value to match the input, then calls the normal Fortran ``GAMMA`` intrinsic with the real value cointained by the reduced precision number as input and stores the result in the output variable ``x``. The precision of the return value ``x`` is reduced by the assignment operation.

To include this code in a build of the library simply follow the instructions in :ref:`pygen-usage-integration`.


Adding a new operator
=====================

The process of adding a new operator proceeds much like :ref:`pygen-adding-intrinsic`, except with a different configuration file and different JSON attributes.
In this example we'll pretend that we don't already have a ``**`` operator and implement one.

The JSON configuration for operators is the ``operators.json`` file in ``generator/configs``.
An operator definition looks like this:

.. code-block:: json

   {<name>:
       {
           "operator": <operator-symbol>,
           "return_type": <return-type>,
           "operator_categories": [<categories>]
       }
   }

In this example ``<name>`` is the name of the operator, in our case this will be ``"pow"``; ``<operator-symbol`` is the symbol used to represent the operator, which in our case will be ``"**"``; ``<return-type>`` is just the type that will be returned by the operator, in this case we want to return a reduced precision value so we will use ``"rpe_var"`` as the return type.
The value supplied for ``"operator_categories"`` is a list of the categories this operator falls into.
There are only 2 categories available, ``"unary"`` for unary operators and ``"binary"`` for binary operators.
The list of categories can contain one or both of these values if appropriate, but is our case exponentiation is a binary operator so we'll supply the one value ``["binary"]``.

.. warning::

   Never use ``"rpe_shadow"`` as the return type of an operator as it is not possible to properly initialize an :f:type:`rpe_shadow` instance within the limited scope of an operator and have it work correctly in the external scope of the operator's return value. If you want to return a reduced precision value from an operator always use an :f:type:`rpe_var` type by specifying ``"rpe_var"``.

Generating the code for the new operator just requires running the Makefile in ``generator/``::

    cd generator/
    make

Let's take a look at what was generated in the ``generated/`` subdirectory, firstly in the ``interface_operators.i`` file:

.. code-block:: fortran

   PUBLIC :: OPERATOR(**)
   INTERFACE OPERATOR(**)
       MODULE PROCEDURE pow_rpe_rpe
       MODULE PROCEDURE pow_rpe_integer
       MODULE PROCEDURE pow_rpe_long
       MODULE PROCEDURE pow_rpe_real
       MODULE PROCEDURE pow_rpe_realalt
       MODULE PROCEDURE pow_integer_rpe
       MODULE PROCEDURE pow_long_rpe
       MODULE PROCEDURE pow_real_rpe
       MODULE PROCEDURE pow_realalt_rpe
   END INTERFACE OPERATOR(**)

This defines a public interface for the ``**`` operator, which contains 9 member functions.
These functions deal with all possible input combinations for the operator.
Now let's look at how these operators are defined in the generated ``implementation_operators.f90`` file, we'll just show a few of the 9 definitions to get a feel for what is generated:

.. code-block:: fortran

   !-------------------------------------------------------------------
   ! Overloaded definitions for (**):
   !

   ELEMENTAL FUNCTION pow_rpe_rpe (x, y) RESULT (z)
       CLASS(rpe_type), INTENT(IN) :: x
       CLASS(rpe_type), INTENT(IN) :: y
       TYPE(rpe_var) :: z
       z%sbits = MAX(significand_bits(x), significand_bits(y))
       z = x%get_value() ** y%get_value()
   END FUNCTION pow_rpe_rpe

   ...

   ELEMENTAL FUNCTION pow_rpe_real (x, y) RESULT (z)
       CLASS(rpe_type), INTENT(IN) :: x
       REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: y
       TYPE(rpe_var) :: z
       z%sbits = MAX(significand_bits(x), significand_bits(y))
       z = x%get_value() ** y
   END FUNCTION pow_rpe_real

   ...

   ELEMENTAL FUNCTION pow_real_rpe (x, y) RESULT (z)
       REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: x
       CLASS(rpe_type), INTENT(IN) :: y
       TYPE(rpe_var) :: z
       z%sbits = MAX(significand_bits(x), significand_bits(y))
       z = x ** y%get_value()
   END FUNCTION pow_real_rpe

The first definition defines how the ``**`` operator can be applied to two :f:type:`rpe_type` instances.
It can operate on either :f:type:`rpe_var` or :f:type:`rpe_shadow` types for each argument and returns an :f:type:`rpe_var` instance.
The number of bits in the significand of the result is set to the larger of the number of bits in the significands of the inputs, the calculation is then done in full precision and reduced to the specified precision on assignment to the return value ``z``.

The other two functions do something very similar, except they operate on inputs of one reduced precision type and one real number type, the first raising a reduced precision number to the power of a real number, and the second raising a real number to the power of a reduced precision number.

Now that the code for the new operator has been generated and checked it can be included in a build of the library by following the instructions in :ref:`pygen-usage-integration`.


.. _GAMMA: https://gcc.gnu.org/onlinedocs/gcc-4.8.1/gfortran/GAMMA.html
