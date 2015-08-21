#!/bin/bash
#
# Run the full test suite.
#

set -u

# List of all the test suites that should be run.
readonly RUN_SUITES="unit"

# Define a mapping between test suite names and executables.
readonly UNIT_EXE="unit/unittests.x"
readonly INTEGRATION_EXE="integration/inttests.x"
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
    for suite in ${RUN_SUITES}; do
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
    echo "Summary:"
    echo -e "${messages}"
    return $status
}

main $@
