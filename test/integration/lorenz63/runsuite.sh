#!/bin/bash
#
# Run the L63 integration test suite.
#

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

set -u

readonly RUNDIR=$(dirname $(readlink -f "$0"))
# Names of model executables:
readonly L63REF="$RUNDIR/l63ref.exe"
readonly L63RED="$RUNDIR/l63red.exe"

#-----------------------------------------------------------------------
# Functions for running tests.
#

# Run a single test.
#
# Globals: None
# Arguments:
#   name
#     A short but descriptive name for the test.
#   command
#     The command that runs the test. The command should return 0 if
#     the test is passed and non-zero otherwise.
#   fail_description
#     A description of the failure mode to be printed if a the test
#     fails.
# Returns:
#   0 if the test passes, 1 if it fails.
#
runtest () {
    local name="$1"
    local cmd=$2
    local fail_description="$3"
    local stdout
    stdout=$(eval ${cmd})
    if [ $? -ne 0 ]; then
        echo "${name}: FAILED (${fail_description})"
        if [[ -n "$stdout" ]]; then
            echo "    captured stdout:"
            echo "$stdout"
            echo ""
        fi
        local status=1
    else
        echo "${name}: PASSED"
        local status=0
    fi
    return $status
}

# Run a single test file a file dependency.
#
# Globals: None
# Arguments:
#   name
#     A short but descriptive name for the test.
#   command
#     The command that runs the test. The command should return 0 if
#     the test is passed and non-zero otherwise.
#   fail_description
#     A description of the failure mode to be printed if a the test
#     fails.
#   filepath
#     Path to a file required for the test.
# Returns:
#   0 if the test passes, 1 if it fails, and 2 if the required file is
#   not found.
#
runtest_with_file () {
    local name="$1"
    local cmd="$2"
    local fail_description="$3"
    local filepath="$4"
    if [[ ! -f "$filepath" ]]; then
        echo "${name}: ERROR (missing required file: ${filepath})"
        return 2
    fi
    runtest "$name" "$cmd" "$fail_description"
    return $?
}


#-----------------------------------------------------------------------
# Test definitions
#

# An array of test function names. If a test is not listed here it will
# not be run.
TESTS=("test_deactivated" \
       "test_deactivated_with_sbits" \
       "test_deactivated_with_ieee" \
       "test_10_bit" \
       "test_10_bit_ieee")

# Check that the emulator doesn't do anything when it is deactivated.
test_deactivated () {
    runtest "test_deactivated" \
            'diff <(${L63RED} -d) <(${L63REF}) 2>&1' \
            "running with emulator deactivated is not the same as running with no emulator"
    return $?
}

# Check that the emulator doesn't do anything when it is deactivated,
# even if the number of significand bits to use is set explicitly.
test_deactivated_with_sbits () {
    runtest "test_deactivated_with_sbits" \
            'diff <(${L63RED} -d -n 10) <(${L63REF}) 2>&1' \
            "running with emulator deactivated should not care about number of bits"
    return $?
}

# Check that the emulator doesn't do anything when it is deactivated,
# even if IEEE half-precision mode is set explicitly.
test_deactivated_with_ieee () {
    runtest "test_deactivated_with_ieee" \
            'diff <(${L63RED} -d -n 10 --ieee) <(${L63REF}) 2>&1' \
            "running with emulator deactivated should not care about IEEE half mode"
    return $?
}

# Check a 10-bit simulation against a known result.
test_10_bit () {
    local test_file="$RUNDIR/results/l63.10bit.txt"
    runtest_with_file "test_10_bit" \
                      "diff ${test_file} <(${L63RED} -n 10) 2>&1" \
                      "10 bit integration does not match expected result" \
                      "$test_file"
    return $?
}

# Check a 10-bit IEEE half-precision simulation against a known result.
test_10_bit_ieee () {
    local test_file="$RUNDIR/results/l63.10bit.ieee.txt"
    runtest_with_file "test_10_bit_ieee" \
                      "diff ${test_file} <(${L63RED} -n 10) 2>&1" \
                      "10 bit IEEE mode integration does not match expected result" \
                      "$test_file"
    return $?
}


#-----------------------------------------------------------------------
# Main test program
#

main () {
    local fails=0
    local errors=0
    local test_status
    for testcase in ${TESTS[@]}; do
        ${testcase}
        test_status=$?
        if [[ $test_status -eq 1 ]]; then
            fails=$(( $fails + 1 ))
        elif [[ $test_status -eq 2 ]]; then
            errors=$(( $errors + 1 ))
        fi
    done
    if [[ $fails -ne 0 ]] && [[ $errors -ne 0 ]]; then
        echo "There were ${fails} test failures and ${errors} errors."
    elif [[ $fails -ne 0 ]]; then
        echo "There were ${fails} test failures"
    elif [[ $errors -ne 0 ]]; then
        echo "There were ${errors} errors running tests"
    else
        echo "L63 suite: all tests passed"
    fi
    return $(( $fails + $errors ))
}

main $@
