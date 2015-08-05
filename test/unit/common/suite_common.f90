MODULE suite_common
! Common data for unit test suites.
!
    USE rp_emulator
    IMPLICIT NONE
    
    ! A double precision floating-point number used for testing reduction
    ! of precision. This particular choice includes an exponent incursion
    ! at 1 bit of precision. The number has the following representations:
    !
    !  hexadecimal: 0x408C86A761308B4C
    !  binary: 0100000010001100100001101010011101100001001100001000101101001100
    !  floating-point: 912.831728343249324097996577621
    !
    REAL(KIND=RPE_REAL_KIND), PARAMETER :: utest64 = z'408C86A761308B4C'
    !
    ! An array storing the same number at significand precisions from 1 to 23
    ! bits. These truncated representations are rounded ocrrectly.
    !
    REAL(KIND=RPE_REAL_KIND), PARAMETER, DIMENSION(23) :: utest64_t = (/ &
        REAL(z'4090000000000000', RPE_REAL_KIND), & ! 1 (rounds into exponent)
        REAL(z'408C000000000000', RPE_REAL_KIND), & ! 2
        REAL(z'408C000000000000', RPE_REAL_KIND), & ! 3
        REAL(z'408D000000000000', RPE_REAL_KIND), & ! 4
        REAL(z'408C800000000000', RPE_REAL_KIND), & ! 5
        REAL(z'408C800000000000', RPE_REAL_KIND), & ! 6
        REAL(z'408C800000000000', RPE_REAL_KIND), & ! 7
        REAL(z'408C800000000000', RPE_REAL_KIND), & ! 8
        REAL(z'408C880000000000', RPE_REAL_KIND), & ! 9
        REAL(z'408C880000000000', RPE_REAL_KIND), & ! 10
        REAL(z'408C860000000000', RPE_REAL_KIND), & ! 11
        REAL(z'408C870000000000', RPE_REAL_KIND), & ! 12
        REAL(z'408C868000000000', RPE_REAL_KIND), & ! 13
        REAL(z'408C86C000000000', RPE_REAL_KIND), & ! 14
        REAL(z'408C86A000000000', RPE_REAL_KIND), & ! 15
        REAL(z'408C86A000000000', RPE_REAL_KIND), & ! 16
        REAL(z'408C86A800000000', RPE_REAL_KIND), & ! 17
        REAL(z'408C86A800000000', RPE_REAL_KIND), & ! 18
        REAL(z'408C86A800000000', RPE_REAL_KIND), & ! 19
        REAL(z'408C86A700000000', RPE_REAL_KIND), & ! 20
        REAL(z'408C86A780000000', RPE_REAL_KIND), & ! 21
        REAL(z'408C86A780000000', RPE_REAL_KIND), & ! 22
        REAL(z'408C86A760000000', RPE_REAL_KIND)  & ! 23
    /)
    
    REAL(KIND=RPE_ALTERNATE_KIND), PARAMETER :: utest32 = z'404ccccd'
    REAL(KIND=RPE_REAL_KIND),      PARAMETER :: utest32_64 = z'40099999A0000000'
    REAL(KIND=RPE_ALTERNATE_KIND), PARAMETER, DIMENSION(22) :: utest32_t = (/ &
        REAL(z'40400000', RPE_ALTERNATE_KIND), & ! 1
        REAL(z'40400000', RPE_ALTERNATE_KIND), & ! 2
        REAL(z'40500000', RPE_ALTERNATE_KIND), & ! 3
        REAL(z'40500000', RPE_ALTERNATE_KIND), & ! 4
        REAL(z'404c0000', RPE_ALTERNATE_KIND), & ! 5
        REAL(z'404c0000', RPE_ALTERNATE_KIND), & ! 6
        REAL(z'404d0000', RPE_ALTERNATE_KIND), & ! 7
        REAL(z'404d0000', RPE_ALTERNATE_KIND), & ! 8
        REAL(z'404cc000', RPE_ALTERNATE_KIND), & ! 9
        REAL(z'404cc000', RPE_ALTERNATE_KIND), & ! 10
        REAL(z'404cd000', RPE_ALTERNATE_KIND), & ! 11
        REAL(z'404cd000', RPE_ALTERNATE_KIND), & ! 12
        REAL(z'404cc000', RPE_ALTERNATE_KIND), & ! 13
        REAL(z'404cc000', RPE_ALTERNATE_KIND), & ! 14
        REAL(z'404ccd00', RPE_ALTERNATE_KIND), & ! 15
        REAL(z'404ccd00', RPE_ALTERNATE_KIND), & ! 16
        REAL(z'404cccc0', RPE_ALTERNATE_KIND), & ! 17
        REAL(z'404cccc0', RPE_ALTERNATE_KIND), & ! 18
        REAL(z'404cccd0', RPE_ALTERNATE_KIND), & ! 19
        REAL(z'404cccd0', RPE_ALTERNATE_KIND), & ! 20
        REAL(z'404ccccc', RPE_ALTERNATE_KIND), & ! 21
        REAL(z'404cccce', RPE_ALTERNATE_KIND)  & ! 22
    /)

END MODULE suite_common
