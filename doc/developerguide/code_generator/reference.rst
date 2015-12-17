=========
Reference
=========


.. _pygen-reference-fortran-types:

Type names and variables
========================

================  ==========================  =================================
Type name (JSON)  Generator Type variable     Fortran type
================  ==========================  =================================
``"logical"``     ``rpgen.types.LOGICAL``     ``LOGICAL``
``"integer"``     ``rpgen.types.INTEGER``     ``INTEGER``
``"long"``        ``rpgen.types.LONG``        ``INTEGER(KIND=8)``
``"real"``        ``rpgen.types.REAL``        ``REAL(KIND=RPE_REAL_KIND)``
``"realalt"``     ``rpgen.types.REALALT``     ``REAL(KIND=RPE_ALTERNATE_KIND)``
``"rpe_var"``     ``rpgen.types.RPE_VAR``     ``TYPE(rpe_var)``
================  ==========================  =================================


.. _pygen-reference-operator-categories:

Operator categories
===================

========================  =================================================
Operator category (JSON)  Definition
========================  =================================================
``"unary"``               A unary operator with one input and one output.
``"binary"``              A binary operator with two inputs and one output.
========================  =================================================


.. _pygen-reference-interface-types:

Intrinsic function interface types
==================================

=====================  ==============================================================
Interface name (JSON)  Definition
=====================  ==============================================================
``"1argscalar"``       A function with one scalar argument.
``"1argelemental"``    An elemental function with one scalar or array argument.
``"2argelemental"``    An elemental function with two scalar or array arguments.
``"1arrayarg"``        A function with one array argument and a scalalr return value.
``"multiarg"``         An elemental function with multiple arguments.
=====================  ==============================================================
