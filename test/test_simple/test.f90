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

    ! Initialize the arrays:
    normal = (/ 1.22319, 3.34234, 2.22211, 123.45678, -317.88194 /)
    reduced_c = normal
    CALL init_shadow (reduced_p, normal)
    factor = -1.21238546
    
    ! Multiplication with a 4-bit mantissa:
    RPE_BITS = 4
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
