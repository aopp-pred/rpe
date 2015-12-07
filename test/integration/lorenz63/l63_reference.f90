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

PROGRAM lorenz63
! A reference double-precision Lorenz '63 model.
!

    IMPLICIT NONE
    
    INTEGER, PARAMETER :: REAL_KIND = kind(1.0d0)

    ! Time-stepping parameters.
    REAL(KIND=REAL_KIND) :: t
    REAL(KIND=REAL_KIND) :: time_start = 0
    REAL(KIND=REAL_KIND) :: time_stop = 5
    REAL(KIND=REAL_KIND) :: dt = 0.01
    
    ! Lorenz 63 system parameters.
    REAL(KIND=REAL_KIND) :: sigma
    REAL(KIND=REAL_KIND) :: rho
    REAL(KIND=REAL_KIND) :: beta
    
    ! Array to store the 3d coordinate of the current system state.
    REAL(KIND=REAL_KIND), DIMENSION(3) :: x

    ! Runge-Kutta parameters.
    REAL(KIND=REAL_KIND), DIMENSION(3) :: k1, k2, k3, k4

    ! Set the model parameters.
    sigma = 10
    rho = 28
    beta = 8.0_REAL_KIND / 3.0_REAL_KIND

    ! Set the initial state.
    t = time_start
    x = (/ 1.508870_REAL_KIND, -1.531271_REAL_KIND, 25.46091_REAL_KIND /)
    
    ! Loop over time evaluating the model state (x).
    WRITE (*, '(F19.15, "    ", F19.15, "    ", F19.15)') x
    DO WHILE (t <= time_stop)
        k1 = f(x, sigma, rho, beta)
        k2 = f(x + 0.5_REAL_KIND * dt * k1, sigma, rho, beta)
        k3 = f(x + 0.5_REAL_KIND * dt * k2, sigma, rho, beta)
        k4 = f(x + dt * k3, sigma, rho, beta)
        x = x + dt * (k1 + 2 * (k2 + k3) + k4) / 6.0_REAL_KIND
        t = t + dt
        WRITE (*, '(F19.15, "    ", F19.15, "    ", F19.15)') x
    END DO

CONTAINS

    FUNCTION f (x, sigma, rho, beta)
    ! Update the Runge-Kutta parameters.
    !
        IMPLICIT NONE
        REAL(KIND=REAL_KIND), DIMENSION(3), INTENT(IN) :: x
        REAL(KIND=REAL_KIND),               INTENT(IN) :: sigma
        REAL(KIND=REAL_KIND),               INTENT(IN) :: rho
        REAL(KIND=REAL_KIND),               INTENT(IN) :: beta
        REAL(KIND=REAL_KIND), DIMENSION(3) :: f
        f = (/ sigma * (x(2) - x(1)), &
               x(1) * (rho - x(3)) - x(2), &
               x(1) * x(2) - beta * x(3) /)
    END FUNCTION f

END PROGRAM
