"""Reduced-precision emulator code generator."""
# Copyright (c) 2015 Andrew Dawson
from __future__ import (absolute_import, print_function)

from .templates import render_template


def generate_code(name, **kwargs):
    """Generate Fortran source code for a procedure.

    **Argument:**

    *procedure_type*
        Type of procedure to generate, one of:
            * assign
            * unary
            * binary
            * 1arg_scalar
            * 1arg_elemental
            * 2arg_elemental
            * arrayarg
            * arrayarg_option
            * multiarg_elemental

    **Keyword arguments:**

    *kwargs*
        Keywords required to fill the Fortran code templates, which are:
            * assign:
              - type1, type2
            * unary:
              - type1, operator
            * binary:
              - type1, type2, operator
            * 1arg_scalar:
              - type1, function
            * 1arg_elemental:
              - type1, function
            * 2arg_elemental:
              - type1, type2, function
            * arrayarg:
              - type1, function
            * arrayarg_option:
              - type1, type2, function
            * multiarg_elemental:
              - types, function

    **Returns:**

    *(name, procedure)*
        A 2-tuple consisting of the procedure name and the procedure
        source code.

    """
    return render_template(name, **kwargs)


def generate_interface(interface_name, procedure_names):
    """Generate the source code for a Fortran interface block.

    **Arguments:**

    *interface_name*
        The name of the interface (e.g., 'OPERATOR(+)' or 'sum').

    *procedure_names*
        An iterable of module procedure names that are part of the
        interface.

    **Returns:**

    *interface*
        The Fortran source code for the interface block.

    **Example:**

    Generate an interface for the addition operator and include 3
    module procedures::

        procedures = ['add_mytype_int',
                      'add_int_mytype',
                      'add_mytype_mytype']
        interface = generate_interface('OPERATOR(+)', procedures)

    """
    return render_template('interface',
                           interface_name=interface_name,
                           procedure_names=procedure_names)
