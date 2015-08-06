==================
Using the Emulator
==================


Using the emulator in your code
===============================

In any subroutine, module or program where you want to use the emulator, you need to import the emulator's module.
For example, to include the emulator in a particular subroutine:

.. code-block:: fortran

   SUBROUTINE some_routine (...)
       USE rp_emulator
       ...
   END SUBROUTINE some_routine

You can then use any of the emulator features within the subroutine.


Reduced precision types
=======================

Two types are provided to represent reduced precision floating point numbers.
The :f:type:`rpe_var` type is used to contain an independent reduced precision floating point number, whereas the :f:type:`rpe_shadow` contains a pointer to an actual real variable elsewhere in the program and ensures that precision is reduced when operating on the shadow.

When to use each type
---------------------

For new codes where you can write from the start by using reduced precision variables instead of real numbers, or for relatively small existing codes with few subroutine calls, it is suggested to stick to the :f:type:`rpe_var` type, as in general you are less likely to encounter problems when using the :f:type:`rpe_var` type.

The :f:type:`rpe_shadow` is particularly useful when modifying only parts of an existing code, especially if the code structure has many subroutine calls.
In some situations using an :f:type:`rpe_shadow` provides a simpler solution than using and :f:type:`rpe_var`, or it may be easy to automtically parse an existing code and introduce :f:type:`rpe_shadow` instances than to modify the code to use :f:type:`rpe_var` types.

The choice of which to use is up to you, we suggest using whichever seems easiest to implement in your given scenario.
The two types can be mixed freely within a single code, so you don't have to choose one and stick to it, you can choose which type to use on a per-variable basis if you want.
The next section of this guide contains a short brief on the differences between the two types: :doc:`var_vs_shadow`.
