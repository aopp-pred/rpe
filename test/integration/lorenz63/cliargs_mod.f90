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

MODULE cliargs_mod
! Provide command line argument handling for the reduced-precision
! Lorenz '63 code.
!

    IMPLICIT NONE

CONTAINS

    SUBROUTINE cliargs (active, nbits, ieee)
    ! Parse command line arguments, exit the Fortran runtime if an error
    ! is detected in the command line arguments.
    !
    ! Arguments:
    !
    ! * active: logical [output]
    !     Will be set to `.TRUE.` if the command line arguments indicate
    !     the emulator should be turned on, and `.FALSE.` otherwise.
    ! * nbits: integer [output]
    !     Will be set to the number of significand bits reqested on the
    !     command line (no validation of range).
    ! * ieee: logical [output]
    !     Will be set to `.TRUE.` if IEEE half-precision mode is chosen
    !     on the command line.
    !
        USE, INTRINSIC :: iso_fortran_env, ONLY : error_unit

        LOGICAL, INTENT(OUT) :: active
        INTEGER, INTENT(OUT) :: nbits
        LOGICAL, INTENT(OUT) :: ieee

        INTEGER :: iarg, stat, arglen
        CHARACTER(LEN=:), ALLOCATABLE :: arg

        ! Default values:
        nbits = 23
        ieee = .FALSE.
        active = .TRUE.

        iarg = 0
        DO WHILE (iarg < command_argument_count())
            iarg = iarg + 1
            ! Get the length of the next argument and allocate space for
            ! it before storing it.
            CALL get_command_argument (iarg, length=arglen)
            ALLOCATE (CHARACTER(LEN=arglen) :: arg)
            CALL get_command_argument (iarg, value=arg)
            ! Take the appropriate action depending on the value of the
            ! argument.
            SELECT CASE (arg)
            CASE ("-n", "--significand-bits")
                ! These arguments take a value, so we need to make space
                ! for that value then retrieve it.
                iarg = iarg + 1
                DEALLOCATE (arg)
                CALL get_command_argument (iarg, length=arglen)
                ALLOCATE (CHARACTER(LEN=arglen) :: arg)
                CALL get_command_argument (iarg, value=arg)
                ! Convert the argument to an integer, exit with an error
                ! if this is not possible.
                READ (arg, *, iostat=stat) nbits
                IF (stat /= 0) THEN
                    WRITE (error_unit, '("error: invalid number of significand bits")')
                    CALL exit (3)
                END IF
            CASE ("-d", "--deactivate")
                active = .FALSE.
            CASE ("-i", "--ieee")
                ieee = .TRUE.
            CASE default
                ! Handle unrecognised arguments by writing an error
                ! message and exiting.
                WRITE (error_unit, &
                       '("unrecognized command line argument ", A)') arg
                CALL exit ()
            END SELECT
            DEALLOCATE (arg)
        END DO

    END SUBROUTINE cliargs

END MODULE cliargs_mod
