"""
Generate intrinsic function overload code for the reduced-precision
emulator.

"""
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
from __future__ import print_function

from argparse import ArgumentParser
import os
import sys

from rpgen import generate_code, generate_interface
from rpgen.intrinsics import from_json as intrinsics_from_json
import rpgen.types as rptypes


DIMENSIONS = (1, 2, 3, 4, 5)
ARGNUMS = (3, 4, 5, 6, 7, 8, 9)

HEADER = """    !-------------------------------------------------------------------
    ! Overloaded definitions for '{}':
    !"""


class Error(Exception):
    """Generic exception class raised for all errors."""
    pass


def generate_function_suite(fn):
    blocks = []
    if '1argscalar' in fn.interface_types:
        blocks.append(generate_code('1arg_scalar',
                                    type1=rptypes.RPE_TYPE, function=fn))
    if '1argelemental' in fn.interface_types:
        blocks.append(generate_code('1arg_elemental',
                                    type1=rptypes.RPE_TYPE, function=fn))
    if '2argelemental' in fn.interface_types:
        blocks.append(generate_code('2arg_elemental',
                                    type1=rptypes.RPE_TYPE,
                                    type2=rptypes.RPE_TYPE, function=fn))
        blocks.append(generate_code('2arg_elemental',
                                    type1=rptypes.RPE_TYPE, type2=rptypes.REAL,
                                    function=fn))
        blocks.append(generate_code('2arg_elemental',
                                    type1=rptypes.REAL, type2=rptypes.RPE_TYPE,
                                    function=fn))
    if '1arrayarg' in fn.interface_types:
        blocks += [generate_code('arrayarg', type1=rptypes.RPE_TYPE,
                                 function=fn, ndim=n)
                   for n in DIMENSIONS]
    if 'multiarg' in fn.interface_types:
        blocks += [generate_code('multiarg', types=[rptypes.RPE_TYPE] * n,
                                 function=fn)
                   for n in ARGNUMS]
    names = [b[0] for b in blocks]
    code = [b[1] for b in blocks]
    interface = generate_interface(fn.name, names)
    return interface, code
                   

def main(argv=None):
    """Program entry point."""
    if argv is None:
        argv = sys.argv
    ap = ArgumentParser()
    ap.add_argument('-i', '--interface-file', type=str,
                    default='interface_intrinsics.h',
                    help='file to write intrinsics interface to')
    ap.add_argument('-d', '--definition-file', type=str,
                    default='implementation_intrinsics.h',
                    help='file to write intrinsics definitions to')
    ap.add_argument('-f', '--force', action='store_true', default=False,
                    help='write output even if it overwrites an existing file')
    ap.add_argument('defn',
                    help='intrinsics function definition file in JSON format')
    argns = ap.parse_args(argv[1:])
    try:
        if not argns.force:
            # Check if output files exist already:
            interface_exists = os.path.exists(argns.interface_file)
            definition_exists = os.path.exists(argns.definition_file)
            if interface_exists or definition_exists:
                msg = ("Cannot overwrite existing files: {}. "
                       "Use -f or --force to force overwriting")
                if interface_exists and definition_exists:
                    filelist = (argns.interface_file, argns.definition_file)
                elif interface_exists:
                    filelist = (argns.interface_file,)
                elif definition_exists:
                    filelist = (argns.definition_file,)
                raise Error(msg.format(filelist))
        # Open the output files:
        try:
            fi = open(argns.interface_file, 'w')
        except IOError as e:
            raise Error(e.message)
        try:
            fd = open(argns.definition_file, 'w')
        except IOError as e:
            fi.close()
            raise Error(e)
        # Generate code for each function:
        first = True
        for fn in intrinsics_from_json(argns.defn):
            interface, code = generate_function_suite(fn)
            if not first:
                fi.write('\n\n')
            fi.write(interface)
            if not first:
                fd.write('\n\n')
            fd.write(HEADER.format(fn.name))
            fd.write('\n\n')
            fd.write('\n\n'.join(code))
            first = False
        return 0
    except Error as e:
        print("error: {}".format(e.message), file=sys.stderr)
        print("  use -h or --help for help", file=sys.stderr)
        return 1
    

if __name__ == '__main__':
    sys.exit(main())
