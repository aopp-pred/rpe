"""Fortran code templates."""
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
