! Define the sub-suites to include in the full test suite.
!
! If a suite does not have an entry in this file it will not be included
! in the test driver application.
!
! Copyright 2015 Andrew Dawson, Peter Dueben
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

! Test suites for the emulator core.
ADD_TEST_SUITE(test_rp_emulator_suite)
ADD_TEST_SUITE(test_significand_bits_suite)
ADD_TEST_SUITE(test_apply_truncation_suite)

! Test suites for the emulator types.
ADD_TEST_SUITE(test_rpe_var_suite)

! Test suites for the assignment operator.
ADD_TEST_SUITE(test_assignment_to_builtin_suite)
ADD_TEST_SUITE(test_assignment_from_builtin_suite)
ADD_TEST_SUITE(test_assignment_rpe_rpe_suite)
