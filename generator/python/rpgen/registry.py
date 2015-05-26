"""A registry wrapping a list."""
from __future__ import (absolute_import, print_function)


class Registry(object):

    def __init__(self):
        self._register = []

    def register(self, operator):
        self._register.append(operator)

    def items(self, **kwargs):
        args = filter(lambda (k, v): v is not None, kwargs.items())
        if not args:
            return [x for x in self._register]
        elif len(args) != 1:
            raise ValueError('only one search item is allowed')
        name, value = args[0]
        if name == 'kind':
            comparator = lambda x, v: x.is_kind(v)
        else:
            comparator = lambda x, v: x == v
        return [x for x in self._register
                if comparator(getattr(x, name, None), value)]
