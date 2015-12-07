"""Fortran data type definitions."""
# Copyright 2015 Andrew Dawson, Peter Dueben
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
from __future__ import (absolute_import, print_function)


class FortranType(object):
    """A Fortran data type."""

    def __init__(self, name, declaration, accessor=None):
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

        """
        self.name = name
        self.declaration = declaration
        self.accessor = accessor or ''


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

#: Fortran rpe_temporary contrete type.
RPE_VAR = FortranType('rpe', 'TYPE(rpe_var)', accessor=r'%val')


def get_fortran_type(type_name):
    type_mapping = {'logical': LOGICAL, 'integer': INTEGER, 'long': LONG,
                    'real': REAL, 'realalt': REALALT, 'rpe_var': RPE_VAR}
    return type_mapping[type_name.lower()]
