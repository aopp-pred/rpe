"""Fortran operator definitions."""
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
