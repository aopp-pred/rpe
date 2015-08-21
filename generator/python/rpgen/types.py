"""Fortran data type definitions."""
from __future__ import (absolute_import, print_function)


class FortranType(object):
    """A Fortran data type."""

    def __init__(self, name, declaration, accessor=None, rpe_instance=False):
        """Create a Fortran data type.

        **Arguments:**

        *name*
            The name of the data type, used for procedure names.

        *declaration*
            The Fortran declaration needed to instantiated this data
            type.

        **Optional arguments:**
        
        *accessor*
            If given, represents the extra syntax necessary to access
            the value contained within a type. Defaults to None (no
            accessor syntax).

        *rpe_instance*
            If `True` the type is an instance of `rpe_type` or subclass.

        **Examples:**

        Create a data type representing an 8-byte integer (on most
        systems)::

            integer_type = FortranType('integer', 'INTEGER(KIND=8)')

        Create a data type representing a polymorphic derived type class
        with its value in the `data_value` member::

            my_type = FortranType('my_type', 'CLASS(my_type)',
                                  accessor='%data_value',
                                  polymorphic=True)

        """
        self.name = name
        self.declaration = declaration
        self.accessor = accessor or ''
        self.rpe_instance = rpe_instance


#: Fortran built-in LOGICAL data type.
LOGICAL = FortranType('logical', 'LOGICAL')

#: Fortran built-in LOGICAL data type.
INTEGER = FortranType('integer', 'INTEGER')

#: Fortran built-in LOGICAL data type.
LONG = FortranType('long', 'INTEGER(KIND=8)')

#: Fortran built-in LOGICAL data type.
REAL = FortranType('real', 'REAL(KIND=RPE_REAL_KIND)')
#: Fortran built-in LOGICAL data type.

REALALT = FortranType('realalt', 'REAL(KIND=RPE_ALTERNATE_KIND)')

#: Fortran rpe_type polymorphic class.
RPE_TYPE = FortranType('rpe', 'CLASS(rpe_type)', accessor=r'%get_value()',
                       rpe_instance=True)

#: Fortran rpe_shadow concrete type.
RPE_SHADOW = FortranType('shadow', 'TYPE(rpe_shadow)', accessor=r'%get_value',
                         rpe_instance=True)

#: Fortran rpe_temporary contrete type.
RPE_VAR = FortranType('var', 'TYPE(rpe_var)', accessor=r'%get_value',
                      rpe_instance=True)


def get_fortran_type(type_name):
    type_mapping = {'logical': LOGICAL, 'integer': INTEGER, 'long': LONG,
                    'real': REAL, 'realalt': REALALT, 'rpe_type': RPE_TYPE,
                    'rpe_var': RPE_VAR, 'rpe_shadow': RPE_SHADOW}
    return type_mapping[type_name.lower()]
