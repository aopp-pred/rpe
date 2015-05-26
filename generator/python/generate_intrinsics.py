"""
Generate intrinsic function overload code for the reduced-precision
emulator.

"""
# Copyright (c) 2015 Andrew Dawson
from __future__ import print_function

from argparse import ArgumentParser
import os
import sys

from rpgen import generate_code, generate_interface
import rpgen.intrinsics as rpint
import rpgen.types as rptypes


FUNCTIONS = rpint.REGISTRY.items()

DIMENSIONS = (1, 2, 3, 4, 5)

ARGNUMS = (3, 4, 5, 6, 7, 8, 9, 10)

HEADER = """    !-------------------------------------------------------------------
    ! Overloaded definitions for '{}':
    !"""


class Error(Exception):
    """Generic exception class raised for all errors."""
    pass


def generate_function_suite(fn):
    blocks = []
    if fn.is_kind(rpint.FUNCTYPE_1ARG_SCALAR):
        blocks.append(generate_code('1arg_scalar',
                                    type1=rptypes.RPE_TYPE, function=fn))
    if fn.is_kind(rpint.FUNCTYPE_1ARG_ELEMENTAL):
        blocks.append(generate_code('1arg_elemental',
                                    type1=rptypes.RPE_TYPE, function=fn))
    if fn.is_kind(rpint.FUNCTYPE_2ARG_ELEMENTAL):
        blocks.append(generate_code('2arg_elemental',
                                    type1=rptypes.RPE_TYPE,
                                    type2=rptypes.RPE_TYPE, function=fn))
        blocks.append(generate_code('2arg_elemental',
                                    type1=rptypes.RPE_TYPE, type2=rptypes.REAL,
                                    function=fn))
        blocks.append(generate_code('2arg_elemental',
                                    type1=rptypes.REAL, type2=rptypes.RPE_TYPE,
                                    function=fn))
    if fn.is_kind(rpint.FUNCTYPE_1ARRAYARG):
        blocks += [generate_code('arrayarg', type1=rptypes.RPE_TYPE,
                                 function=fn, ndim=n)
                   for n in DIMENSIONS]
    if fn.is_kind(rpint.FUNCTYPE_MULTIARG):
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
        for fn in FUNCTIONS:
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
