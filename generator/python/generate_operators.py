"""
Generate operator overload code for the reduced-precision emulator.

"""
# Copyright (c) 2015 Andrew Dawson
from __future__ import print_function

from argparse import ArgumentParser
import os
import sys

from rpgen import generate_code, generate_interface
from rpgen.operators import from_json as operators_from_json
import rpgen.types as rptypes


#: Types of variables that can be included in a binary operation with an
#: rpe_type instance.
BINARY_TYPES = [rptypes.INTEGER, rptypes.LONG, rptypes.REAL, rptypes.REALALT]

#: Section header for the generated source code.
HEADER = """    !-------------------------------------------------------------------
    ! Overloaded definitions for ({}):
    !"""


class Error(Exception):
    """Generic exception class raised for all errors."""
    pass


def generate_operator_suite(op):
    """Generate code and an interface for an operator."""
    blocks = []
    if 'unary' in op.operator_categories:
        blocks.append(generate_code('unaryop', type1=rptypes.RPE_TYPE,
                                    operator=op))
    if 'binary' in op.operator_categories:
        blocks.append(generate_code('binaryop', type1=rptypes.RPE_TYPE,
                                    type2=rptypes.RPE_TYPE, operator=op))
        blocks += [generate_code('binaryop', type1=rptypes.RPE_TYPE,
                                 type2=datatype, operator=op)
                   for datatype in BINARY_TYPES]
        blocks += [generate_code('binaryop', type1=datatype,
                                 type2=rptypes.RPE_TYPE, operator=op)
                   for datatype in BINARY_TYPES]
    names = [b[0] for b in blocks]
    code = [b[1] for b in blocks]
    interface = generate_interface('OPERATOR({})'.format(op.operator), names)
    return interface, code


def main(argv=None):
    """Program entry point."""
    if argv is None:
        argv = sys.argv
    ap = ArgumentParser()
    ap.add_argument('-i', '--interface-file', type=str,
                    default='interface_operators.h',
                    help='file to write operator interface to')
    ap.add_argument('-d', '--definition-file', type=str,
                    default='implementation_operators.h',
                    help='file to write operator definitions to')
    ap.add_argument('-f', '--force', action='store_true', default=False,
                    help='write output even if it overwrites an existing file')
    ap.add_argument('defn', help='operator definition file in JSON format')
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
        # Generate code for each operator:
        first = True
        for op in operators_from_json(argns.defn):
            interface, code = generate_operator_suite(op)
            if not first:
                fi.write('\n\n')
            fi.write(interface)
            if not first:
                fd.write('\n\n')
            fd.write(HEADER.format(op.operator))
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
