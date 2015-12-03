===============================
Structure of the Code Generator
===============================


The code generator consists of a small Python library for code generation, 2 configuation files for determining what code is generated, and two driver programs to build the code.
The code generator itself resides within the top-level ``generator/`` directory.
At the top level is a Makefile used to run the code generator, the directory containing generated code


Configuration files
===================

The code generator driver programs require input files that define which overloaded operators and intrinsic functions need to be constructed.
These files can be found in the ``generator/configs/`` directory.
The file ``operators.json`` defines overloaded operators and the file ``intrinsics.json`` defines the overloaded intrinsic functions; the files are in `JSON`_ format.


Python components
=================

The python components are located in the subdirectory ``generator/python``.
In this directory are two Python programs ``generate_intrinsics.py`` and ``generate_operators.py``.
These programs take as input a JSON configuration file and output Fortran code files.

Also in this directory is another directory named ``rpgen/``, this is the Python library for code generation.


.. _JSON: http://json.org/
