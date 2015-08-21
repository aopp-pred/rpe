"""Fortran operator definitions."""
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
