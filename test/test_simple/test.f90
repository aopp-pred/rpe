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

PROGRAM test_rpe
! Test the reduced-precision emulator.
!
! Performs a simple operation on an array of real numbers with full
! precision, and again with reduced precision with 4 and 8 bits in the
! floating-point mantissa. This is done using both the `rpe_var` and
! `rpe_shadow` reduced-precision types.
!
    USE rp_emulator
    IMPLICIT NONE

    REAL(KIND=RPE_REAL_KIND), DIMENSION(5) :: normal, result_normal, result_c, result_p
    TYPE(rpe_var), DIMENSION(5) :: reduced_c
    TYPE(rpe_shadow), DIMENSION(5) :: reduced_p
    REAL(KIND=RPE_REAL_KIND) :: factor
    INTEGER :: i

    RPE_BITS = 23


    ! Initialize the arrays:
    WRITE (*, '("Initialized with ", I0, "-bit mantissa:")') RPE_BITS
    normal = (/ 1.2231923141234123d0, &
                3.3423421431234123d0, &
                2.2221132579875620d0, &
                123.45678111112223d0, &
                -317.8819499991110d0 /)
    DO i = 1, 5
        WRITE (*, *) normal(i), "# normal"
    END DO
    reduced_c = normal
    DO i = 1, 5
        WRITE (*, *) reduced_c(i)%get_value(), "# reduced (concrete)"
    END DO
    CALL init_shadow (reduced_p, normal)
    DO i = 1, 5
        WRITE (*, *) reduced_p(i)%get_value(), "# reduced (pointer)"
    END DO
    
    ! Multiplication with a 4-bit mantissa:
    RPE_BITS = 4
    factor = -1.21238546d0
    result_normal = normal * factor
    result_c = reduced_c * factor
    result_p = reduced_p * factor
    WRITE (*, '("Emulated reduced precision of ", I0, " bits")') RPE_BITS
    DO i = 1, 5
        WRITE (*, *) result_normal(i), "# normal"
        WRITE (*, *) result_c(i), "# reduced (concrete)"
        WRITE (*, *) result_p(i), "# reduced (pointer)"
    END DO
    
    ! Multiplication with an 8-bit mantissa:
    RPE_BITS = 8
    factor = -1.21238546d0
    result_normal = normal * factor
    result_c = reduced_c * factor
    result_p = reduced_p * factor
    WRITE (*, '("Emulated reduced precision of ", I0, " bits")') RPE_BITS
    DO i = 1, 5
        WRITE (*, *) result_normal(i), "# normal"
        WRITE (*, *) result_c(i), "# reduced (concrete)"
        WRITE (*, *) result_p(i), "# reduced (pointer)"
    END DO

END PROGRAM
