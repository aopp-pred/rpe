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

from collections import namedtuple

from ._io import from_json as _from_json
from .types import get_fortran_type


#: A Fortran operator.
FortranOperator = namedtuple('FortranOperator',
                             ('name', 'operator', 'operator_categories',
                              'return_type'))


def _is_valid_operator_type(type_name):
    if type_name.lower() not in ('unary', 'binary'):
        raise ValueError('Invalid operator type: "{}"'.format(type_name))
    return type_name.lower()


def _operator_from_json(json_object):
    (name, defn), = json_object.items()
    try:
        operator = defn['operator']
        operator_categories = \
            [t for t in map(lambda x: x.lower(), defn['operator_categories'])
             if _is_valid_operator_type(t)]
        return_type = get_fortran_type(defn['return_type'])
    except KeyError:
        raise ValueError('The JSON definition of the operator "{}" '
                         'is malformed'.format(name))
    return FortranOperator(name, operator, operator_categories, return_type)


def from_json(json_file):
    return _from_json(json_file, 'operators', _operator_from_json)
