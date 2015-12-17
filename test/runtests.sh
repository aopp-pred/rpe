#!/bin/bash
#
# Run the full test suite.
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

# List of all the test suites that should be run.
readonly RUN_SUITES="unit integration"

# Define a mapping between test suite names and executables.
readonly UNIT_EXE="unit/unittests.x"
readonly INTEGRATION_EXE="integration/inttests.sh"
declare -A SUITE_MAP
SUITE_MAP["unit"]="${UNIT_EXE}"
SUITE_MAP["integration"]="${INTEGRATION_EXE}"

# Function to append a line to a variable.
appendline () {
    if [ -z "$1" ]; then echo "$2"; else echo "${1}\n${2}"; fi
}

main () {
    local status=0
    local suite
    local suite_status
    local suite_info
    local messages=""
    echo "========================================================================"
    echo "Running master test suite"
    echo "========================================================================"
    for suite in ${RUN_SUITES}; do
        echo
        echo "------------------------------------------------------------------------"
        echo "Running suite: '$suite'"
        echo "------------------------------------------------------------------------"
        "${SUITE_MAP["${suite}"]}"
        suite_status=$?
        suite_info="  suite: '${suite}'"
        if [ ${suite_status} -ne 0 ]; then
            messages=$(appendline "${messages}" "${suite_info} FAILED")
            status=$(( status + 1 ))
        else
            messages=$(appendline "${messages}" "${suite_info} PASSED")
        fi
    done
    echo
    echo "========================================================================"
    echo "Summary:"
    echo "========================================================================"
    echo -e "${messages}"
    return $status
}

main $@
