! Define the sub-suites to include in the full test suite.
!
! If a suite does not have an entry in this file it will not be included
! in the test driver application.
!

! Test suites for the emulator core.
ADD_TEST_SUITE(test_rp_emulator_suite)
ADD_TEST_SUITE(test_significand_bits_suite)
ADD_TEST_SUITE(test_apply_truncation_suite)

! Test suites for the emulator types.
ADD_TEST_SUITE(test_rpe_shadow_suite)
ADD_TEST_SUITE(test_rpe_var_suite)

! Test suites for the assignment operator.
ADD_TEST_SUITE(test_assignment_to_builtin_suite)
ADD_TEST_SUITE(test_assignment_from_builtin_suite)
ADD_TEST_SUITE(test_assignment_rpe_rpe_suite)
