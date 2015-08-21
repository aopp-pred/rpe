========================
Using the Code Generator
========================


The basic workflow for using the code generator is detailed below, and can be summarised as:

#. Make some change to the code to be generated, either by modifying the JSON configuration files or by modifying templates or even the generator library itself.
#. Run the generator.
#. Integrate the generated source code into the main Fortran library.


Defining the code to be generated
=================================

What the code generator produces can be controlled either by configuration files, or modifying the generator itself.
Modifying configuration files is covered in :doc:`new_code`.


Running the generator
=====================

The generator can be run from the top-level ``generator/`` directory using the Makefile::

    make -C generator

This will produce 4 files in the directory ``generator/generated``:

* ``interface_operators.i``: interface blocks for overloaded operators.
* ``implementation_operators.f90``: overloaded operator implementations.
* ``interface_intrinsics.i``: interface blocks for overloaded intrinsic functions.
* ``implementation_intrinsics.f90``: overloaded intrinsic function implementations.

Despite the file extensions, all 4 files contain Fortran code, the extensions are simply part of a naming convention.


.. _pygen-usage-integration:

Integrating generated code into the Fortran library
===================================================

Once you have generated new files, you will want to integrate the generated code into the main Fortran library.

.. warning::

   Make sure you carefully check the generated code is correct before proceeding.
   It is worthwhile looking at both the generator output, and the difference between each output and the existing file in ``rawsrc/include``, for example::

       diff generated/implementation_operators.f90 rawsrc/include/implementation_operators.f90

   This way you can be sure that you have achieved the change you wanted without changing something else you didn't expect to change.

To integrate the generated code all you need to do is copy/move the generated files from ``generator/generated`` to ``rawsrc/include``::

    cp generator/generated/* rawsrc/include

Once you have done this you should rebuild the library::

    make fullclean
    make all

Now you can run the tests and verify that the new code works as expected::

    cd tests
    make test
