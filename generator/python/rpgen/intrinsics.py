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


REGISTRY = Registry()


class FortranIntrinsic(object):
    """A Fortran intrinsic function."""

    def __init__(self, name, return_type, kind, reduce_precision):
        """Create a Fortran intrinsic function.
        
        **Arguments:**

        *name*
            The name of the function, used to for function names.

        *return_type*
            A `FortranType` instance representing the return type of the
            function.

        *reduce_precision*
            If *True* the function will induce a loss of precision in
            the return value, if *False* there is no loss of precision.

        """
        self.name = name
        self.return_type = return_type
        self.reduce_precision = reduce_precision
        self.kind = kind

    def is_kind(self, kind):
        return self.kind & kind == kind


FUNCTYPE_1ARG_SCALAR = 1
FUNCTYPE_1ARG_ELEMENTAL = 2
FUNCTYPE_2ARG_ELEMENTAL = 4
FUNCTYPE_1ARRAYARG = 8
FUNCTYPE_1ARRAYARG_OPT = 16
FUNCTYPE_MULTIARG = 32


#====
RPE_FN_EPSILON = FortranIntrinsic(
    'epsilon',
    types.RPE_VAR,
    FUNCTYPE_1ARG_SCALAR,
    reduce_precision=False,)
REGISTRY.register(RPE_FN_EPSILON)

RPE_FN_HUGE = FortranIntrinsic(
    'huge',
    types.RPE_VAR,
    FUNCTYPE_1ARG_SCALAR,
    reduce_precision=False,)
REGISTRY.register(RPE_FN_HUGE)

RPE_FN_TINY = FortranIntrinsic(
    'tiny',
    types.RPE_VAR,
    FUNCTYPE_1ARG_SCALAR,
    reduce_precision=False,)
REGISTRY.register(RPE_FN_TINY)

RPE_FN_KIND = FortranIntrinsic(
    'kind',
    types.INTEGER,
    FUNCTYPE_1ARG_SCALAR,
    reduce_precision=False)
REGISTRY.register(RPE_FN_KIND)


#====
RPE_FN_ABS = FortranIntrinsic(
    'abs',
    types.RPE_VAR,
    FUNCTYPE_1ARG_ELEMENTAL,
    reduce_precision=False)
REGISTRY.register(RPE_FN_ABS)

RPE_FN_COS = FortranIntrinsic(
    'cos',
    types.RPE_VAR,
    FUNCTYPE_1ARG_ELEMENTAL,
    reduce_precision=True)
REGISTRY.register(RPE_FN_COS)

RPE_FN_SIN = FortranIntrinsic(
    'sin',
    types.RPE_VAR,
    FUNCTYPE_1ARG_ELEMENTAL,
    reduce_precision=True)
REGISTRY.register(RPE_FN_SIN)

RPE_FN_TAN = FortranIntrinsic(
    'tan',
    types.RPE_VAR,
    FUNCTYPE_1ARG_ELEMENTAL,
    reduce_precision=True)
REGISTRY.register(RPE_FN_TAN)

RPE_FN_ACOS = FortranIntrinsic(
    'acos',
    types.RPE_VAR,
    FUNCTYPE_1ARG_ELEMENTAL,
    reduce_precision=True)
REGISTRY.register(RPE_FN_ACOS)

RPE_FN_ASIN = FortranIntrinsic(
    'asin',
    types.RPE_VAR,
    FUNCTYPE_1ARG_ELEMENTAL,
    reduce_precision=True)
REGISTRY.register(RPE_FN_ASIN)

RPE_FN_ATAN = FortranIntrinsic(
    'atan',
    types.RPE_VAR,
    FUNCTYPE_1ARG_ELEMENTAL | FUNCTYPE_2ARG_ELEMENTAL,
    reduce_precision=True)
REGISTRY.register(RPE_FN_ATAN)

RPE_FN_COSH = FortranIntrinsic(
    'cosh',
    types.RPE_VAR,
    FUNCTYPE_1ARG_ELEMENTAL,
    reduce_precision=True)
REGISTRY.register(RPE_FN_COSH)

RPE_FN_SINH = FortranIntrinsic(
    'sinh',
    types.RPE_VAR,
    FUNCTYPE_1ARG_ELEMENTAL,
    reduce_precision=True)
REGISTRY.register(RPE_FN_SINH)

RPE_FN_TANH = FortranIntrinsic(
    'tanh',
    types.RPE_VAR,
    FUNCTYPE_1ARG_ELEMENTAL,
    reduce_precision=True)
REGISTRY.register(RPE_FN_TANH)

RPE_FN_EXP = FortranIntrinsic(
    'exp',
    types.RPE_VAR,
    FUNCTYPE_1ARG_ELEMENTAL,
    reduce_precision=True)
REGISTRY.register(RPE_FN_EXP)

RPE_FN_LOG = FortranIntrinsic(
    'log',
    types.RPE_VAR,
    FUNCTYPE_1ARG_ELEMENTAL,
    reduce_precision=True)
REGISTRY.register(RPE_FN_LOG)

RPE_FN_LOG10 = FortranIntrinsic(
    'log10',
    types.RPE_VAR,
    FUNCTYPE_1ARG_ELEMENTAL,
    reduce_precision=True)
REGISTRY.register(RPE_FN_LOG10)

RPE_FN_SQRT = FortranIntrinsic(
    'sqrt',
    types.RPE_VAR,
    FUNCTYPE_1ARG_ELEMENTAL,
    reduce_precision=True)
REGISTRY.register(RPE_FN_SQRT)

RPE_FN_SPACING = FortranIntrinsic(
    'spacing',
    types.RPE_VAR,
    FUNCTYPE_1ARG_ELEMENTAL,
    reduce_precision=False)
REGISTRY.register(RPE_FN_SPACING)

RPE_FN_FLOOR = FortranIntrinsic(
    'floor',
    types.INTEGER,
    FUNCTYPE_1ARG_ELEMENTAL,
    reduce_precision=False,)
REGISTRY.register(RPE_FN_FLOOR)

RPE_FN_INT = FortranIntrinsic(
    'int',
    types.INTEGER,
    FUNCTYPE_1ARG_ELEMENTAL,
    reduce_precision=False)
REGISTRY.register(RPE_FN_INT)

RPE_FN_NINT = FortranIntrinsic(
    'nint',
    types.INTEGER,
    FUNCTYPE_1ARG_ELEMENTAL,
    reduce_precision=False)
REGISTRY.register(RPE_FN_NINT)

RPE_FN_ATAN2 = FortranIntrinsic(
    'atan2',
    types.RPE_VAR,
    FUNCTYPE_2ARG_ELEMENTAL,
    reduce_precision=True)
REGISTRY.register(RPE_FN_ATAN2)

RPE_FN_DIM = FortranIntrinsic(
    'dim',
    types.RPE_VAR,
    FUNCTYPE_2ARG_ELEMENTAL,
    reduce_precision=True)
REGISTRY.register(RPE_FN_DIM)

RPE_FN_MOD = FortranIntrinsic(
    'mod',
    types.RPE_VAR,
    FUNCTYPE_2ARG_ELEMENTAL,
    reduce_precision=True)
REGISTRY.register(RPE_FN_MOD)

RPE_FN_NEAREST = FortranIntrinsic(
    'nearest',
    types.RPE_VAR,
    FUNCTYPE_2ARG_ELEMENTAL,
    reduce_precision=False)
REGISTRY.register(RPE_FN_NEAREST)

RPE_FN_SIGN = FortranIntrinsic(
    'sign',
    types.RPE_VAR,
    FUNCTYPE_2ARG_ELEMENTAL,
    reduce_precision=False)
REGISTRY.register(RPE_FN_SIGN)

RPE_FN_MIN = FortranIntrinsic(
    'min',
    types.RPE_VAR,
    FUNCTYPE_2ARG_ELEMENTAL | FUNCTYPE_MULTIARG,
    reduce_precision=False)
REGISTRY.register(RPE_FN_MIN)

RPE_FN_MAX = FortranIntrinsic(
    'max',
    types.RPE_VAR,
    FUNCTYPE_2ARG_ELEMENTAL | FUNCTYPE_MULTIARG, # 2 arg is special case isn't it? Yes but *is* special, most 2 args don't need multiarg!
    reduce_precision=False)
REGISTRY.register(RPE_FN_MAX)


#====
RPE_FN_MINVAL = FortranIntrinsic(
    'minval',
    types.RPE_VAR,
    FUNCTYPE_1ARRAYARG | FUNCTYPE_1ARRAYARG_OPT,
    reduce_precision=False)
REGISTRY.register(RPE_FN_MINVAL)

RPE_FN_MAXVAL = FortranIntrinsic(
    'maxval',
    types.RPE_VAR,
    FUNCTYPE_1ARRAYARG | FUNCTYPE_1ARRAYARG_OPT,
    reduce_precision=False)
REGISTRY.register(RPE_FN_MAXVAL)

RPE_FN_SUM = FortranIntrinsic(
    'sum',
    types.RPE_VAR,
    FUNCTYPE_1ARRAYARG | FUNCTYPE_1ARRAYARG_OPT,
    reduce_precision=True)
REGISTRY.register(RPE_FN_SUM)
