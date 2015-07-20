PROGRAM test_rpe
! Test the reduced-precision emulator.
!
! Check bit-strings associated with reduced-precision numbers.
!
    USE rp_emulator
    IMPLICIT NONE

    REAL(KIND=RPE_REAL_KIND), DIMENSION(5) :: normal, result_normal, result_c, result_p
    TYPE(rpe_var), DIMENSION(5) :: reduced_c
    TYPE(rpe_shadow), DIMENSION(5) :: reduced_p
    REAL(KIND=RPE_REAL_KIND) :: factor
    INTEGER :: i
    CHARACTER(len=52) :: bitmarker

    ! Initialize the arrays:
    normal = (/ 1.22319, 3.34234, 2.22211, 123.45678, -317.88194 /)
    reduced_c = normal
    CALL init_shadow (reduced_p, normal)
    factor = -1.21238546

    ! Multiplication with a 4-bit mantissa:
    RPE_BITS = 4
    bitmarker = REPEAT("|", RPE_BITS)
    result_normal = normal * factor
    result_c = reduced_c * factor
    result_p = reduced_p * factor
    WRITE (*, '("Emulated reduced precision of ", I0, " bits")') RPE_BITS
    WRITE (*, '("SEEEEEEEEEEEMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM")')
    WRITE (*, '("            ", A52)') bitmarker
    DO i = 1, 5
        WRITE (*, '(B64, "    # normal")') result_normal(i)
        WRITE (*, '(B64, "    # reduced (concrete)")') result_c(i)
        WRITE (*, '(B64, "    # reduced (pointer)")') result_p(i)
    END DO

    ! Multiplication with an 8-bit mantissa:
    RPE_BITS = 8
    bitmarker = REPEAT("|", RPE_BITS)
    result_normal = normal * factor
    result_c = reduced_c * factor
    result_p = reduced_p * factor
    WRITE (*, '("Emulated reduced precision of ", I0, " bits")') RPE_BITS
    WRITE (*, '("SEEEEEEEEEEEMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM")')
    WRITE (*, '("            ", A52)') bitmarker
    DO i = 1, 5
        WRITE (*, '(B64, "    # normal")') result_normal(i)
        WRITE (*, '(B64, "    # reduced (concrete)")') result_c(i)
        WRITE (*, '(B64, "    # reduced (pointer)")') result_p(i)
    END DO

END PROGRAM
