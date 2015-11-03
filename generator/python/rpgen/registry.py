"""A registry wrapping a list."""
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
