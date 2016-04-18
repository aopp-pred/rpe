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

PROGRAM lorenz63
! A Lorenz '63 model capable of using reduced floating-point precision.
!

    USE rp_emulator
    USE cliargs_mod, ONLY : cliargs

    IMPLICIT NONE

    ! Time-stepping parameters.
    REAL(KIND=RPE_REAL_KIND) :: t
    REAL(KIND=RPE_REAL_KIND) :: time_start = 0
    REAL(KIND=RPE_REAL_KIND) :: time_stop = 5
    REAL(KIND=RPE_REAL_KIND) :: dt = 0.01

    ! Lorenz 63 system parameters.
    TYPE(rpe_var) :: sigma
    TYPE(rpe_var) :: rho
    TYPE(rpe_var) :: beta

    ! Array to store the 3d coordinate of the current system state.
    TYPE(rpe_var), DIMENSION(3) :: x

    ! Runge-Kutta parameters.
    TYPE(rpe_var), DIMENSION(3) :: k1, k2, k3, k4
    TYPE(rpe_var), DIMENSION(3) :: x2, x3, x4, xd

    ! Handle command line arguments.
    CALL cliargs (RPE_ACTIVE, RPE_DEFAULT_SBITS, RPE_IEEE_HALF)

    ! Set the model parameters.
    sigma = 10
    rho = 28
    beta = 8.0_RPE_REAL_KIND / 3.0_RPE_REAL_KIND

    ! Set the initial state.
    t = time_start
    x = (/ 1.508870_RPE_REAL_KIND, -1.531271_RPE_REAL_KIND, 25.46091_RPE_REAL_KIND /)

    ! Loop over time evaluating the model state (x).
    WRITE (*, '(F19.15, "    ", F19.15, "    ", F19.15)') x%val
    DO WHILE (t <= time_stop)
        k1 = f(x, sigma, rho, beta)
        x2 = x + rpe_literal(0.5) * dt * k1
        k2 = f(x2, sigma, rho, beta)
        x3 = x + rpe_literal(0.5) * dt * k2
        k3 = f(x3, sigma, rho, beta)
        x4 = x + dt * k3
        k4 = f(x4, sigma, rho, beta)
        x = x + dt * (k1 + 2 * (k2 + k3) + k4) / rpe_literal(6.)
        t = t + dt
        WRITE (*, '(F19.15, "    ", F19.15, "    ", F19.15)') x%val
    END DO

CONTAINS

    FUNCTION f (x, sigma, rho, beta)
    ! Update the Runge-Kutta parameters.
    !
        IMPLICIT NONE
        TYPE(rpe_var), DIMENSION(3), INTENT(IN) :: x
        TYPE(rpe_var),               INTENT(IN) :: sigma
        TYPE(rpe_var),               INTENT(IN) :: rho
        TYPE(rpe_var),               INTENT(IN) :: beta
        TYPE(rpe_var), DIMENSION(3) :: f
        f(1) = sigma * (x(2) - x(1))
        f(2) = x(1) * (rho - x(3)) - x(2)
        f(3) = x(1) * x(2) - beta * x(3)
    END FUNCTION f

END PROGRAM
