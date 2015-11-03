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


#: A Fortran intrinsic function.
FortranIntrinsic = namedtuple('FortranIntrinsic',
                              ('name', 'interface_types', 'return_type'))


def _is_valid_routine_type(type_name):
    if type_name.lower() not in ('1argscalar', '1argelemental',
                                 '2argelemental', '1arrayarg', 'multiarg'):
        raise ValueError('Invalid routine type: "{}"'.format(type_name))
    return type_name.lower()


def _intrinsic_from_json(json_object):
    (name, defn), = json_object.items()
    try:
        interface_types = \
            [t for t in map(lambda x: x.lower(), defn['interface_types'])
             if _is_valid_routine_type(t)]
        return_type = get_fortran_type(defn['return_type'])
    except KeyError:
        raise ValueError('The JSON definition of the intrinsic "{}" '
                         'is malformed'.format(name))
    return FortranIntrinsic(name, interface_types, return_type)


def from_json(json_file):
    return _from_json(json_file, 'intrinsics', _intrinsic_from_json)
