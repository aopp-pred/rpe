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
