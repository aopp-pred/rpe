#!/bin/bash
#
# Run all integration tests.
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

# List of all integration test suites.
readonly SUITES="lorenz63"

# Main program runs the driver "runsuite.sh" in each suite.
main () {
    local suite
    local status
    local fails=0
    for suite in ${SUITES}; do
        echo
        local suite_driver="./integration/${suite}/runsuite.sh"
        if [[ -f "$suite_driver" ]]; then
            $suite_driver
            status=$?
        else
            echo "${suite}: ERROR missing driver '$suite_driver'" 1>&2
            status=1
        fi
        if [[ $status -ne 0 ]]; then
            fails=$(( $fails + 1 ))
        fi
    done
    return $fails
}

main
exit $?
