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

MODULE private_io
    IMPLICIT NONE
CONTAINS
    SUBROUTINE write_summary (a, b, c, d, n)
        USE rp_emulator
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: a, b, c, d
        INTEGER,                  INTENT(IN) :: n
        CHARACTER(LEN=64), PARAMETER :: bit_labels = "s" // &
                                                     REPEAT("e", 11) // &
                                                     REPEAT("m", 52)
        CHARACTER(LEN=64) :: bit_markers
        bit_markers = REPEAT(" ", 12) // REPEAT("|", n)
        WRITE (*, '("               ", A64)') bit_labels
        WRITE (*, '("               ", A64)') bit_markers
        WRITE (*, '("normal:        ", B0.64, "  ", F54.52)') a, a
        WRITE (*, '("reduced:       ", B0.64, "  ", F54.52)') b, b
        WRITE (*, '("shadow:        ", B0.64, "  ", F54.52)') c, c
        WRITE (*, '("shadow target: ", B0.64, "  ", F54.52)') d, d
        WRITE (*, '("")')
    END SUBROUTINE write_summary
END MODULE private_io

PROGRAM test_rpe
! Basic test of the reduced-precision emulator.
!
    USE rp_emulator
    USE private_io
    IMPLICIT NONE

    REAL(KIND=RPE_REAL_KIND) :: initial_value, normal, normal_target
    TYPE(rpe_var) :: reduced_c
    TYPE(rpe_shadow) :: reduced_p
    REAL(KIND=RPE_REAL_KIND) :: squared_normal, squared_reduced_c, &
                                squared_reduced_p, squared_normal_target
    INTEGER :: n, nbits

    ! Set the default precision to 23 bits, if otherwise not set an rpe_type
    ! instance will have this many bits of precision in the significand.
    RPE_DEFAULT_SBITS = 23
    
    ! Initialize some variables and write their valus to stdout:
    initial_value = 1.2345678_RPE_REAL_KIND
    normal = initial_value
    normal_target = initial_value
    reduced_c = initial_value
    CALL init_shadow (reduced_p, normal_target)
    CALL apply_truncation (reduced_p) ! reduces precision of reduced_p and
                                      ! normal_target which it points to
    WRITE (*, '("Default number of bits (", I0, ")")') RPE_DEFAULT_SBITS
    WRITE (*, '("-------------------------------------------------")')
    WRITE (*, '("Stored values:")')
    CALL write_summary (normal, reduced_c%get_value(), reduced_p%get_value(), &
                        normal_target, RPE_DEFAULT_SBITS)
    
    ! Square with default number of bits:
    squared_normal = normal * normal
    squared_reduced_c = reduced_c * reduced_c
    squared_reduced_p = reduced_p * reduced_p
    squared_normal_target = normal_target * normal_target
    WRITE (*, '("Computed squares:")')
    CALL write_summary(squared_normal, squared_reduced_c, squared_reduced_p, &
                       squared_normal_target, RPE_DEFAULT_SBITS)
    
    ! Perform the same calculations using different numbers of bits in the
    ! floating-point significand.
    DO n = 1, 4
        ! Square with n bits, first assign the new precision and print the
        ! stored values, then print the result:
        nbits = 4 * n
        reduced_c%sbits = nbits
        reduced_p%sbits = nbits
        reduced_c = initial_value
        reduced_p = initial_value
        CALL apply_truncation (reduced_c)
        CALL apply_truncation (reduced_p)
        WRITE (*, '(I0, " bits")') nbits
        WRITE (*, '("-------------------------------------------------")')
        WRITE (*, '("Stored values:")')
        CALL write_summary (normal, reduced_c%get_value(), &
                            reduced_p%get_value(), normal_target, nbits)
        squared_normal = normal * normal
        squared_reduced_c = reduced_c * reduced_c
        squared_reduced_p = reduced_p * reduced_p
        squared_normal_target = normal_target * normal_target
        WRITE (*, '("Computed squares:")')
        CALL write_summary(squared_normal, squared_reduced_c, &
                           squared_reduced_p, squared_normal_target, nbits)
    END DO

END PROGRAM
