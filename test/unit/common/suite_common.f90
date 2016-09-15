! Copyright 2015-2016 Andrew Dawson, Peter Dueben
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

MODULE suite_common
! Common data for unit test suites.
!
    USE rp_emulator
    IMPLICIT NONE

    ! A double precision floating-point number used for testing reduction
    ! of precision. This particular choice includes an exponent incursion
    ! at 1 bit of precision and a case where extra IEEE rounding rules will
    ! be invoked to avoid rounding bias. The number has the following
    ! representations:
    !
    !  hexadecimal: 408C86A761308B44
    !  binary: 0100000010001100100001101010011101100001001100001000101101000100
    !  floating-point: 912.8317283432484
    !
    REAL(KIND=RPE_REAL_KIND), PARAMETER :: utest64 = z'408c86a761308b44'
    !
    ! An array storing the same number at significand precisions from 1 to 52
    ! bits. These truncated representations are rounded correctly (including
    ! IEEE 'round to even' rule).
    !
    REAL(KIND=RPE_REAL_KIND), PARAMETER, DIMENSION(52) :: utest64_t = (/ &
        REAL(z'4090000000000000', RPE_REAL_KIND), & ! 1
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
        REAL(z'408C86A760000000', RPE_REAL_KIND), & ! 23
        REAL(z'408C86A760000000', RPE_REAL_KIND), & ! 24
        REAL(z'408C86A760000000', RPE_REAL_KIND), & ! 25
        REAL(z'408C86A760000000', RPE_REAL_KIND), & ! 26
        REAL(z'408C86A762000000', RPE_REAL_KIND), & ! 27
        REAL(z'408C86A761000000', RPE_REAL_KIND), & ! 28
        REAL(z'408C86A761000000', RPE_REAL_KIND), & ! 29
        REAL(z'408C86A761400000', RPE_REAL_KIND), & ! 30
        REAL(z'408C86A761400000', RPE_REAL_KIND), & ! 31
        REAL(z'408C86A761300000', RPE_REAL_KIND), & ! 32
        REAL(z'408C86A761300000', RPE_REAL_KIND), & ! 33
        REAL(z'408C86A761300000', RPE_REAL_KIND), & ! 34
        REAL(z'408C86A761300000', RPE_REAL_KIND), & ! 35
        REAL(z'408C86A761310000', RPE_REAL_KIND), & ! 36
        REAL(z'408C86A761308000', RPE_REAL_KIND), & ! 37
        REAL(z'408C86A761308000', RPE_REAL_KIND), & ! 38
        REAL(z'408C86A761308000', RPE_REAL_KIND), & ! 39
        REAL(z'408C86A761309000', RPE_REAL_KIND), & ! 40
        REAL(z'408C86A761308800', RPE_REAL_KIND), & ! 41
        REAL(z'408C86A761308C00', RPE_REAL_KIND), & ! 42
        REAL(z'408C86A761308C00', RPE_REAL_KIND), & ! 43
        REAL(z'408C86A761308B00', RPE_REAL_KIND), & ! 44
        REAL(z'408C86A761308B80', RPE_REAL_KIND), & ! 45
        REAL(z'408C86A761308B40', RPE_REAL_KIND), & ! 46
        REAL(z'408C86A761308B40', RPE_REAL_KIND), & ! 47
        REAL(z'408C86A761308B40', RPE_REAL_KIND), & ! 48
        REAL(z'408C86A761308B40', RPE_REAL_KIND), & ! 49
        REAL(z'408C86A761308B44', RPE_REAL_KIND), & ! 50
        REAL(z'408C86A761308B44', RPE_REAL_KIND), & ! 51
        REAL(z'408C86A761308B44', RPE_REAL_KIND)  & ! 52
    /)

    ! A single precision floating-point number used for testing reduction
    ! of precision. This particular choice includes an exponent incursion
    ! at 1 bit of precision and a case where extra IEEE rounding rules will
    ! be invoked to avoid rounding bias. The number has the following
    ! representations:
    !
    !  hexadecimal: 40666664
    !  binary: 01000000011001100110011001100100
    !  floating-point: 3.599999427795413.59999942779541
    !
    REAL(KIND=RPE_ALTERNATE_KIND), PARAMETER :: utest32 = z'40666664'
    !
    ! An array storing the same number at significand precisions from 1 to 23
    ! bits. These truncated representations are rounded correctly (including
    ! IEEE 'round to even' rule).
    !
    REAL(KIND=RPE_ALTERNATE_KIND), PARAMETER, DIMENSION(23) :: utest32_t = (/ &
        REAL(z'40800000', RPE_ALTERNATE_KIND), & ! 1
        REAL(z'40600000', RPE_ALTERNATE_KIND), & ! 2
        REAL(z'40600000', RPE_ALTERNATE_KIND), & ! 3
        REAL(z'40680000', RPE_ALTERNATE_KIND), & ! 4
        REAL(z'40680000', RPE_ALTERNATE_KIND), & ! 5
        REAL(z'40660000', RPE_ALTERNATE_KIND), & ! 6
        REAL(z'40660000', RPE_ALTERNATE_KIND), & ! 7
        REAL(z'40668000', RPE_ALTERNATE_KIND), & ! 8
        REAL(z'40668000', RPE_ALTERNATE_KIND), & ! 9
        REAL(z'40666000', RPE_ALTERNATE_KIND), & ! 10
        REAL(z'40666000', RPE_ALTERNATE_KIND), & ! 11
        REAL(z'40666800', RPE_ALTERNATE_KIND), & ! 12
        REAL(z'40666800', RPE_ALTERNATE_KIND), & ! 13
        REAL(z'40666600', RPE_ALTERNATE_KIND), & ! 14
        REAL(z'40666600', RPE_ALTERNATE_KIND), & ! 15
        REAL(z'40666680', RPE_ALTERNATE_KIND), & ! 16
        REAL(z'40666680', RPE_ALTERNATE_KIND), & ! 17
        REAL(z'40666660', RPE_ALTERNATE_KIND), & ! 18
        REAL(z'40666660', RPE_ALTERNATE_KIND), & ! 19
        REAL(z'40666660', RPE_ALTERNATE_KIND), & ! 20
        REAL(z'40666664', RPE_ALTERNATE_KIND), & ! 21
        REAL(z'40666664', RPE_ALTERNATE_KIND), & ! 22
        REAL(z'40666664', RPE_ALTERNATE_KIND)  & ! 23
    /)

    ! A number represented by a 64-bit double that is exact when the
    ! significand is truncated to 23 bits:
    REAL(KIND=RPE_REAL_KIND), PARAMETER :: utest32_64 = z'40099999A0000000'

CONTAINS

    FUNCTION min_significand_bits (x) RESULT (n)
    ! Return the minimum number of bits required to represent the
    ! significand of the input.
    !
    ! This is a helper function used to determine if a number is properly
    ! truncated. For example, to test if a number is truncated to 10 bits
    ! you would test:
    !
    !     repr_significand_bits(number) <= 10
    !
    ! Note that you need to test less-than-or-equal-to, not just equal-to.
    !
    ! Argument:
    !
    ! * x: real(kind=RPE_REAL_KIND)
    !
    ! Returns:
    !
    ! * n: integer
    !     The number of bits required to represent the significand.
    !
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: x
        INTEGER         :: n
        INTEGER(KIND=8) :: bits
        INTEGER         :: bit
        bits = TRANSFER(x, bits)
        n = 0
        DO bit = 51, 0, -1
            IF (BTEST(bits, bit)) THEN
                n = 52 - bit
            END IF
        END DO
    END FUNCTION min_significand_bits

END MODULE suite_common
