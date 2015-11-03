"""Fortran code templates."""
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

import string

from jinja2 import (Environment, PackageLoader, Template)
from jinja2.exceptions import TemplateNotFound


def _remove_blanks(code):
    new_code = []
    for line in code.split('\n'):
        if line.strip():
            new_code.append(line)
    return '\n'.join(new_code)


def render_template(name, **kwargs):
    fcode = '{}.f90'.format(name)
    fname = '{}.name'.format(name)
    code_template = TEMPLATE_ENVIRONMENT.get_template(fcode)
    code = _remove_blanks(code_template.render(**kwargs))
    try:
        name_template = TEMPLATE_ENVIRONMENT.get_template(fname)
        name = name_template.render(**kwargs)
        rv = name, code
    except TemplateNotFound:
        rv = code
    return rv


TEMPLATE_ENVIRONMENT = Environment(loader=PackageLoader('rpgen'),
                                   trim_blocks=True, lstrip_blocks=True)
