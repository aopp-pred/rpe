"""Fortran operator definitions."""
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

from .registry import Registry
from . import types


#: Registry of operators:
REGISTRY = Registry()

OPTYPE_UNARY = 1
OPTYPE_BINARY= 2


class FortranOperator(object):
    """A Fortran operator."""

    def __init__(self, name, operator, return_type, kind, reduce_precision):
        """Create a Fortran operator.
        
        **Arguments:**

        *name*
            The name of the operator, used to for function names.

        *operator*
            The string representation of the operator in Fortran.

        *return_type*
            A `FortranType` instance representing the return type of the
            operator.

        *reduce_precision*
            If *True* the operator will induce a loss of precision in
            the return value, if *False* there is no loss of precision.

        """
        self.name = name
        self.operator = operator
        self.return_type = return_type
        self.reduce_precision = reduce_precision
        self.kind = kind

    def is_kind(self, kind):
        return self.kind & kind == kind


#: Addition operator.
RPE_OP_ADD = FortranOperator(
    'add',
    '+',
    types.RPE_VAR,
    OPTYPE_UNARY | OPTYPE_BINARY,
    reduce_precision=True)
REGISTRY.register(RPE_OP_ADD)

#: Subtraction operator.
RPE_OP_SUB = FortranOperator(
    'sub',
    '-',
    types.RPE_VAR,
    OPTYPE_UNARY | OPTYPE_BINARY,
    reduce_precision=True)
REGISTRY.register(RPE_OP_SUB)

#: Multiplication operator.
RPE_OP_MUL = FortranOperator(
    'mul',
    '*',
    types.RPE_VAR,
    OPTYPE_BINARY,
    reduce_precision=True)
REGISTRY.register(RPE_OP_MUL)

#: Division operator.
RPE_OP_DIV = FortranOperator(
    'div',
    '/',
    types.RPE_VAR,
    OPTYPE_BINARY,
    reduce_precision=True)
REGISTRY.register(RPE_OP_DIV)

#: Greater-than or equal-to operator.
RPE_OP_GE = FortranOperator(
    'ge',
    '.GE.',
    types.LOGICAL,
    OPTYPE_BINARY,
    reduce_precision=False)
REGISTRY.register(RPE_OP_GE)

#: Less-than or equal-to operator.
RPE_OP_LE = FortranOperator(
    'le',
    '.LE.',
    types.LOGICAL,
    OPTYPE_BINARY,
    reduce_precision=False)
REGISTRY.register(RPE_OP_LE)

#: Greater-than operator.
RPE_OP_GT = FortranOperator(
    'gt',
    '.GT.',
    types.LOGICAL,
    OPTYPE_BINARY,
    reduce_precision=False)
REGISTRY.register(RPE_OP_GT)

#: Less-than operator.
RPE_OP_LT = FortranOperator(
    'lt',
    '.LT.',
    types.LOGICAL,
    OPTYPE_BINARY,
    reduce_precision=False)
REGISTRY.register(RPE_OP_LT)

#: Equal-to operator.
RPE_OP_EQ = FortranOperator(
    'eq',
    '==',
    types.LOGICAL,
    OPTYPE_BINARY,
    reduce_precision=False)
REGISTRY.register(RPE_OP_EQ)

#: Not-equal-to operator.
RPE_OP_NE = FortranOperator(
    'ne',
    '/=',
    types.LOGICAL,
    OPTYPE_BINARY,
    reduce_precision=False)
REGISTRY.register(RPE_OP_NE)

#: Exponentiation operator.
RPE_OP_POW = FortranOperator(
    'pow',
    '**',
    types.RPE_VAR,
    OPTYPE_BINARY,
    reduce_precision=True)
REGISTRY.register(RPE_OP_POW)
