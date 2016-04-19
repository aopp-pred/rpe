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

MODULE rp_emulator
! A reduced-precision emulator.
!
! The reduced-precision emulator provides two new data types: `rpe_var`
! and `rpe_shadow`. Both will perform calculations with the specified
! reduced precision.
!
! The `rpe_var` type is a simple container for a double precision
! floating point value.

! The `rpe_shadow` type acts as a memory-view onto an existing double
! precision floating point number defined outside the type itself.
! Changing the value of the shadow also changes the value of the
! floating point number it is shadowing and vice-versa, since they both
! refer to the same block of memory.
!
! Copyright (c) 2015 Andrew Dawson, Peter Dueben
!
    IMPLICIT NONE

    ! All definitions are private by default.
    PRIVATE

!-----------------------------------------------------------------------
! Module parameters and variables:
!-----------------------------------------------------------------------

    !: The Fortran kind of a single-precision and double-precision
    !: floating-point number.
    INTEGER, PARAMETER, PUBLIC :: RPE_SINGLE_KIND = kind(1.0)
    INTEGER, PARAMETER, PUBLIC :: RPE_DOUBLE_KIND = kind(1.0d0)

    !: The Fortran kind of the real data type used by the emulator
    !: (usually 64-bit double-precision).
    INTEGER, PARAMETER, PUBLIC :: RPE_REAL_KIND = RPE_DOUBLE_KIND
    !: The Fortran kind of an alternate real data type (usually 32-bit
    !: single-precision), should be single-precision if RPE_REAL_KIND
    !: is double-precision or double-precision if RPE_REAL_KIND is
    !: single-precision.
    INTEGER, PARAMETER, PUBLIC :: RPE_ALTERNATE_KIND = RPE_SINGLE_KIND
    
    !: Logical flag for turning the emulator on/off.
    LOGICAL, PUBLIC :: RPE_ACTIVE = .TRUE.

    !: The default number of bits to use in the reduced-precision significand.
    INTEGER, PUBLIC :: RPE_DEFAULT_SBITS = 23

    !: Logical flag for determining if IEEE half-precision rules should
    !: be used when operating on values with 10 bits in the significand.
    LOGICAL, PUBLIC :: RPE_IEEE_HALF = .FALSE.

    !: Logical flag for determining if IEEE rounding rules should be used.
    LOGICAL, PUBLIC :: RPE_IEEE_ROUNDING = .FALSE.

    !: An internal value used to represent the case where a reduced-precision
    !: number has no specified precision yet.
    INTEGER, PARAMETER, PRIVATE :: RPE_SBITS_UNSPECIFIED = -1

!-----------------------------------------------------------------------
! Module derived-type definitions:
!-----------------------------------------------------------------------

    PUBLIC :: rpe_type
    TYPE, ABSTRACT :: rpe_type
    ! An abstract base class for reduced-precision types.
    !
    ! A reduced-precision type is expected to be a container for some
    ! kind of floating-point value. It must provide the methods
    ! `get_value` and `set_value` which return the contained value and
    ! set the contained value respectively.
    !
        !: The number of bits used in the significand.
        INTEGER :: sbits = RPE_SBITS_UNSPECIFIED
    CONTAINS
        PROCEDURE(get_value_interface), PUBLIC,  DEFERRED :: get_value
        PROCEDURE(set_value_interface), PRIVATE, DEFERRED :: set_value
    END TYPE rpe_type

    ABSTRACT INTERFACE
    ! Interface definitions for abstract methods defined on `rpe_type`.
    !
        PURE FUNCTION get_value_interface (this) result (x)
            IMPORT rpe_type
            IMPORT RPE_REAL_KIND
            CLASS(rpe_type), INTENT(IN) :: this
            REAL(KIND=RPE_REAL_KIND) :: x
        END FUNCTION get_value_interface

        PURE SUBROUTINE set_value_interface (this, x)
            IMPORT rpe_type
            IMPORT RPE_REAL_KIND
            CLASS(rpe_type), INTENT(INOUT) :: this
            CLASS(*),        INTENT(IN)    :: x
        END SUBROUTINE set_value_interface

    END INTERFACE

    PUBLIC :: rpe_var
    TYPE, EXTENDS(rpe_type) :: rpe_var
    ! A reduced-precision floating-point number.
    !
    ! This type is a container for a floating-point number which is
    ! operated on in reduced precision.
    !
        REAL(KIND=RPE_REAL_KIND), PRIVATE :: val
    CONTAINS
        PROCEDURE, PUBLIC  :: get_value => get_var_value
        PROCEDURE, PRIVATE :: set_value => set_var_value
    END TYPE

    PUBLIC :: rpe_shadow
    TYPE, EXTENDS(rpe_type) :: rpe_shadow
    ! A reduced-precision 'shadow' of a floating-point number.
    !
    ! This type contains a pointer to a floating-point variable which is
    ! defined outside of the type. This acts as a memory-view onto an
    ! existing floating-point variable, where changes to the value of the
    ! shadow are also seen in the value of the floating-point variable it
    ! is shadowing and vice-versa, since they both refer to the same
    ! block of memory.
    !
    REAL(KIND=RPE_REAL_KIND), PRIVATE, POINTER :: ptr => NULL()
    CONTAINS
        ! Required definitions of abstract methods on the base class:
        PROCEDURE, PUBLIC  :: get_value => get_shadow_value
        PROCEDURE, PRIVATE :: set_value => set_shadow_value
    END TYPE

    PUBLIC :: init_shadow
    INTERFACE init_shadow
    ! Interfaces for initializing `rpe_shadow` instances.
    !
        MODULE PROCEDURE init_shadow_scalar
        MODULE PROCEDURE init_shadow_v1d
        MODULE PROCEDURE init_shadow_v2d
        MODULE PROCEDURE init_shadow_v3d
        MODULE PROCEDURE init_shadow_v4d
    END INTERFACE

    ! Create a public interface for constructing literal reduced
    ! precision values (rpe_var instances).
    PUBLIC rpe_literal
    INTERFACE rpe_literal
        MODULE PROCEDURE rpe_literal_real
        MODULE PROCEDURE rpe_literal_alternate
        MODULE PROCEDURE rpe_literal_integer
        MODULE PROCEDURE rpe_literal_long
    END INTERFACE

    ! Make the core emulator routines importable.
    PUBLIC :: apply_truncation, significand_bits

    PUBLIC ASSIGNMENT(=)
    INTERFACE ASSIGNMENT(=)
    ! Interfaces for the assignment operator with `rpe_type` instances.
    !
        MODULE PROCEDURE assign_rpe_rpe
        MODULE PROCEDURE assign_rpe_real
        MODULE PROCEDURE assign_rpe_alternate
        MODULE PROCEDURE assign_rpe_integer
        MODULE PROCEDURE assign_rpe_long
        MODULE PROCEDURE assign_real_rpe
        MODULE PROCEDURE assign_alternate_rpe
        MODULE PROCEDURE assign_integer_rpe
        MODULE PROCEDURE assign_long_rpe
    END INTERFACE

!-----------------------------------------------------------------------
! Interfaces for overloaded operators (external):
!-----------------------------------------------------------------------

    #include "interface_operators.i"

!-----------------------------------------------------------------------
! Interfaces for overloaded intrinsic functions (external):
!-----------------------------------------------------------------------

    #include "interface_intrinsics.i"

!-----------------------------------------------------------------------
! Interfaces for other extensions (external):
!-----------------------------------------------------------------------

    #include "interface_extras.i"

CONTAINS

!-----------------------------------------------------------------------
! Type-bound procedure definitions for rpe_type subclasses:
!-----------------------------------------------------------------------

    PURE FUNCTION get_var_value (this) result (x)
    ! Getter method for the value contained in an rpe_var instance.
    !
    ! Arguments:
    !
    ! * this: type(rpe_var) [input]
    !       An `rpe_var` instance.
    !
    ! Returns:
    !
    ! * x: real(kind=rpe_real_kind) [output]
    !       The value contained within the `rpe_var` instance.
    !
        CLASS(rpe_var), INTENT(IN) :: this
        REAL(KIND=RPE_REAL_KIND) :: x
        x = this%val
    END FUNCTION get_var_value

    PURE SUBROUTINE set_var_value (this, x)
    ! Setter method for the value contained in an rpe_var instance.
    !
    ! Arguments:
    !
    ! * this: type(rpe_var) [input/output]
    !       An `rpe_var` instance to set the value of.
    !
    ! * x: polymorphic [input]
    !       A value to assign to `this`, can be a real or integer type
    !       or an `rpe_type` instance.
    !
        CLASS(rpe_var), INTENT(INOUT) :: this
        CLASS(*),       INTENT(IN)    :: x
        SELECT TYPE (x)
        CLASS IS (rpe_type)
            this%val = x%get_value()
        TYPE IS (REAL(KIND=RPE_REAL_KIND))
            this%val = x
        TYPE IS (REAL(KIND=RPE_ALTERNATE_KIND))
            this%val = x
        TYPE IS (INTEGER(KIND=4))
            this%val = x
        TYPE IS (INTEGER(KIND=8))
            this%val = x
        END SELECT
    END SUBROUTINE set_var_value

    PURE FUNCTION get_shadow_value (this) result (x)
    ! Getter method for the value contained in an rpe_shadow instance.
    !
    ! Arguments:
    !
    ! * this: type(rpe_shadow) [input]
    !       An `rpe_shadow` instance.
    !
    ! Returns:
    !
    ! * x: real(kind=rpe_real_kind) [output]
    !       The value contained within the `rpe_shadow` instance.
    !
        CLASS(rpe_shadow), INTENT(IN) :: this
        REAL(KIND=RPE_REAL_KIND) :: x
        x = this%ptr
    END FUNCTION get_shadow_value

    PURE SUBROUTINE set_shadow_value (this, x)
    ! Setter method for the value contained in an rpe_shadow instance.
    !
    ! Arguments:
    !
    ! * this: type(rpe_shadow) [input/output]
    !       An `rpe_shadow` instance to set the value of.
    !
    ! * x: polymorphic [input]
    !       A value to assign to `this`, can be a real or integer type
    !       or an `rpe_type` instance.
    !
        CLASS(rpe_shadow), INTENT(INOUT) :: this
        CLASS(*),          INTENT(IN)    :: x
        SELECT TYPE (x)
        CLASS IS (rpe_type)
            this%ptr = x%get_value()
        TYPE IS (REAL(KIND=RPE_REAL_KIND))
            this%ptr = x
        TYPE IS (REAL(KIND=RPE_ALTERNATE_KIND))
            this%ptr = x
        TYPE IS (INTEGER(KIND=4))
            this%ptr = x
        TYPE IS (INTEGER(KIND=8))
            this%ptr = x
        END SELECT
    END SUBROUTINE set_shadow_value

!-----------------------------------------------------------------------
! Interface members for rpe_shadow instance initialization:
!-----------------------------------------------------------------------

    SUBROUTINE init_shadow_scalar (this, x)
    ! Initialization for scalar `rpe_type` instances.
    !
    ! Arguments:
    !
    ! * this: type(rpe_shadow) [output]
    !       An `rpe_shadow` instance to initialize.
    !
    ! * x: real(kind=RPE_REAL_KIND) [input]
    !       A real-valued variable to shadow with `this`.
    !
        TYPE(rpe_shadow),                 INTENT(INOUT) :: this
        REAL(KIND=RPE_REAL_KIND), TARGET, INTENT(IN)    :: x
        this%ptr => x
    END SUBROUTINE init_shadow_scalar

    SUBROUTINE init_shadow_v1d (this, x)
    ! Initialization for 1d-vector `rpe_type` instances.
    !
    ! Arguments:
    !
    ! * this: type(rpe_shadow), dimension(:) [output]
    !       An `rpe_shadow` instance to initialize.
    !
    ! * x: real(kind=RPE_REAL_KIND), dimension(:) [input]
    !       A real-valued variable to shadow with `this`.
    !
        TYPE(rpe_shadow),         DIMENSION(:),         INTENT(INOUT) :: this
        REAL(KIND=RPE_REAL_KIND), DIMENSION(:), TARGET, INTENT(IN)    :: x
        INTEGER :: i
        DO i = 1, SIZE(x, 1)
            this(i)%ptr => x(i)
        END DO
    END SUBROUTINE init_shadow_v1d

    SUBROUTINE init_shadow_v2d (this, x)
    ! Initialization for 2d-vector `rpe_type` instances.
    !
    ! Arguments:
    !
    ! * this: type(rpe_shadow), dimension(:, :) [output]
    !       An `rpe_shadow` instance to initialize.
    !
    ! * x: real(kind=RPE_REAL_KIND), dimension(:, :) [input]
    !       A real-valued variable to shadow with `this`.
    !
        TYPE(rpe_shadow),         DIMENSION(:, :),         INTENT(INOUT) :: this
        REAL(KIND=RPE_REAL_KIND), DIMENSION(:, :), TARGET, INTENT(IN)    :: x
        INTEGER :: i, j
        DO i = 1, SIZE(x, 1)
            DO j = 1, SIZE(x, 2)
                this(i, j)%ptr => x(i, j)
            END DO
        END DO
    END SUBROUTINE init_shadow_v2d

    SUBROUTINE init_shadow_v3d (this, x)
    ! Initialization for 3d-vector `rpe_type` instances.
    !
    ! Arguments:
    !
    ! * this: type(rpe_shadow), dimension(:, :, :) [output]
    !       An `rpe_shadow` instance to initialize.
    !
    ! * x: real(kind=RPE_REAL_KIND), dimension(:, :, :) [input]
    !       A real-valued variable to shadow with `this`.
    !
        TYPE(rpe_shadow),         DIMENSION(:, :, :),         INTENT(INOUT) :: this
        REAL(KIND=RPE_REAL_KIND), DIMENSION(:, :, :), TARGET, INTENT(IN)    :: x
        INTEGER :: i, j, k
        DO i = 1, SIZE(x, 1)
            DO j = 1, SIZE(x, 2)
                DO k = 1, SIZE(x, 3)
                    this(i, j, k)%ptr => x(i, j, k)
                END DO
            END DO
        END DO
    END SUBROUTINE init_shadow_v3d

    SUBROUTINE init_shadow_v4d (this, x)
    ! Initialization for 4d-vector `rpe_type` instances.
    !
    ! Arguments:
    !
    ! * this: type(rpe_shadow), dimension(:, :, :, :) [output]
    !       An `rpe_shadow` instance to initialize.
    !
    ! * x: real(kind=RPE_REAL_KIND), dimension(:, :, :, :) [input]
    !       A real-valued variable to shadow with `this`.
    !
        TYPE(rpe_shadow),         DIMENSION(:, :, :, :),         INTENT(INOUT) :: this
        REAL(KIND=RPE_REAL_KIND), DIMENSION(:, :, :, :), TARGET, INTENT(IN)    :: x
        INTEGER :: i, j, k, l
        DO i = 1, SIZE(x, 1)
            DO j = 1, SIZE(x, 2)
                DO k = 1, SIZE(x, 3)
                    DO l = 1, SIZE(x, 4)
                        this(i, j, k, l)%ptr => x(i, j, k, l)
                    END DO
                END DO
            END DO
        END DO
    END SUBROUTINE init_shadow_v4d

!-----------------------------------------------------------------------
! Core emulator procedures:
!-----------------------------------------------------------------------

    ELEMENTAL SUBROUTINE apply_truncation (x)
    ! Reduce the precision of a `rpe_type` instance.
    !
    ! Truncates the given floating-point number significand to the
    ! number of bits defined by the `sbits` member of the number. If the
    ! `sbits` attribute is not set it will truncate to the number of
    ! bits specified by the current value of `RPE_DEFAULT_SBITS`.
    !
    ! If the module variable RPE_ACTIVE is false this subroutine returns
    ! the unaltered input value, it only performs the bit truncation if
    ! RPE_ACTIVE is true.
    !
    ! Argument:
    !
    ! * x: type(rpe_type) [input/output]
    !     The `rpe_type` instance to truncate.
    !
        CLASS(rpe_type), INTENT(INOUT) :: x
        REAL(KIND=RPE_DOUBLE_KIND) :: y
        INTEGER :: truncation
        IF (RPE_ACTIVE) THEN
            ! Cast the input to a double-precision value.
            y = REAL(x%get_value(), RPE_DOUBLE_KIND)
            IF (x%sbits == RPE_SBITS_UNSPECIFIED) THEN
                ! If the input does not have a specified precision then assume
                ! the default precision. This is does not fix the precision of
                ! the input variable, it will still use whatever is specified
                ! as the default, even if that changes later.
                truncation = RPE_DEFAULT_SBITS
            ELSE
                truncation = x%sbits
            END IF
            ! Set the contained value to the truncated value.
            CALL x%set_value(truncate_significand(y, truncation))
        END IF
    END SUBROUTINE apply_truncation

    ELEMENTAL FUNCTION truncate_significand (x, n) RESULT (t)
    ! Truncate the significand of a double precision floating point
    ! number to a specified number of bits.
    !
    ! Arguments:
    !
    ! * x: real(kind=RPE_DOUBLE_KIND) [input]
    !     The double precision number to truncate.
    !
    ! * n: integer [input]
    !     The number of bits to truncate the significand to.
    !
    ! Returns:
    !
    ! * t: real(kind=RPE_DOUBLE_KIND)
    !     A double precision number representing `x` truncated to `n`
    !     bits in the significand.
    !
        REAL(KIND=RPE_DOUBLE_KIND), INTENT(IN) :: x
        INTEGER,                    INTENT(IN) :: n
        REAL(KIND=RPE_DOUBLE_KIND) :: t
        INTEGER                    :: truncation_bit
        INTEGER(KIND=8), PARAMETER :: two = 2
        INTEGER(KIND=8), PARAMETER :: zero_bits = 0
        INTEGER(KIND=8)            :: bits
        ! The truncation bit is the last bit that will be truncated
        ! (counting from 0 at the right-most bit). Double precision values
        ! have 52 bits in their significand.
        truncation_bit = 52 - n - 1
        IF (truncation_bit .GE. 0) THEN
            ! Copy the double-precision bit representation of the input
            ! into an integer so it can be manipulated:
            bits = TRANSFER(x, bits)
            IF (do_ieee_rounding(bits, truncation_bit)) THEN
                ! Round before truncation if required by IEEE rules.
                bits = bits + two ** truncation_bit
            END IF
            ! Move rounding_bit + 1 bits from the number zero (all bits
            ! set to zero) into the target to truncate at the given
            ! number of bits.
            CALL MVBITS (zero_bits, 0, truncation_bit + 1, bits, 0)
            t = TRANSFER(bits, t)
            ! Special case for IEEE half-precision representation.
            IF (n == 10 .AND. RPE_IEEE_HALF) THEN
                t = adjust_ieee_half(t)
            END IF
        ELSE
            t = x
        END IF
    END FUNCTION truncate_significand

    ELEMENTAL FUNCTION do_ieee_rounding (bits, truncation_bit)
    ! Determine if rounding is required before truncation according to
    ! IEEE 754 rounding rules.
    !
    ! Generally rounding is a round-to-nearest scheme, with a special
    ! case for values halfway between two representations which in which
    ! case a round-to-even scheme is used to ensure the last bit of the
    ! truncated value is a '0'.
    !
    ! Arguments:
    !
    ! * bits: integer(kind=8) [input]
    !     The bit-representation of a floating-point number (64 bits)
    !     stored in an 8-byte integer.
    !
    ! * truncation_bit: integer [input]
    !     The index of the most significant bit to be lost by a
    !     truncation. Indices start at 0 for the least significant bit.
    !
    ! Returns:
    !
    ! * do_ieee_rounding: logical
    !     Either `.TRUE.`, indicating that the floating-point number
    !     represented by `bits` should be rounded before truncation,
    !     or `.FALSE.` indicating that no rounding is required.
    !
        INTEGER(KIND=8), INTENT(IN) :: bits
        INTEGER,         INTENT(IN) :: truncation_bit
        LOGICAL :: do_ieee_rounding
        LOGICAL :: is_halfway, candidate
        INTEGER :: bit
        candidate = BTEST(bits, truncation_bit)
        IF (candidate .AND. RPE_IEEE_ROUNDING) THEN
            ! Most significant bit to be truncated is set ('1'), check if
            ! the remaining bits to be truncated are all '0', if so this
            ! number is halfway between two representations.
            is_halfway = .TRUE.
            DO bit = 0, truncation_bit - 1
                IF (BTEST(bits, bit)) THEN
                    is_halfway = .FALSE.
                    EXIT
                END IF
            END DO
            ! If the number is halfway between two representations and the
            ! least significant bit of the result is not set ('0') then no
            ! rounding is required.
            IF (is_halfway .AND. (.NOT. BTEST(bits, truncation_bit + 1))) THEN
                do_ieee_rounding = .FALSE.
            ELSE
                do_ieee_rounding = .TRUE.
            END IF
        ELSE IF (candidate) THEN
            do_ieee_rounding = .TRUE.
        ELSE
            do_ieee_rounding = .FALSE.
        END IF
    END FUNCTION do_ieee_rounding

    ELEMENTAL FUNCTION adjust_ieee_half (x) RESULT (y)
    ! Adjust a floating-point number according to the IEEE half-precision
    ! specification by ensuring that numbers outside the range of
    ! half-precision are either overflowed or denormalized.
    !
    ! Arguments:
    !
    ! * x: real(kind=RPE_DOUBLE_KIND) [input]
    !     The floating-point number to adjust.
    !
    ! Returns:
    !
    ! * y: real(kind=RPE_DOUBLE_KIND) [output]
    !     The adjusted floating-point value.
    !
        REAL(KIND=RPE_DOUBLE_KIND), INTENT(IN)  :: x
        REAL(KIND=RPE_DOUBLE_KIND)            :: y
        REAL(KIND=RPE_DOUBLE_KIND), PARAMETER :: two = 2.0_RPE_DOUBLE_KIND
        REAL(KIND=RPE_DOUBLE_KIND)            :: sx, d1, d2
        IF (ABS(x) > two ** 15) THEN
        ! Handle half-precision overflows.
            sx = SIGN(1.0_RPE_DOUBLE_KIND, x)
            d1 = HUGE(d1) * sx
            d2 = HUGE(d2) * sx
            ! This will deliberately cause a floating-point overflow exception
            ! and yield an infinity value with the same sign as the input if
            ! the exception is not handled.
            y = d1 + d2
        ELSE IF (ABS(x) < two ** (-14)) THEN
        ! Handle half-precision subnormal values.
            d1 = two ** (-24)    ! step size for subnormal values
            d2 = MOD(ABS(x), d1)
            ! The new value is the old value rounded to the nearest subnormal
            ! step interval in the direction of zero.
            y = (ABS(x) - d2) * SIGN(1.0_RPE_DOUBLE_KIND, x)
            ! If the rounding should have gone away from zero then correct for
            ! it afterwards.
            IF (ABS(d2) > two ** (-25)) THEN
                y = y + d1 * SIGN(1.0_RPE_DOUBLE_KIND, y)
            END IF
        ELSE
        ! If the value is in range then just return it.
            y = x
        END IF
    END FUNCTION adjust_ieee_half

    ELEMENTAL FUNCTION significand_bits (x) RESULT (z)
    ! Retrieve the number of bits in a floating point significand.
    !
    ! This returns actual values for inputs of type `rpe_type` or `real`
    ! and 0 for anything else. This function is usually used to find the
    ! highest precision level involved in a floating-point calculation.
    !
    ! Arguments:
    !
    ! * x: class(*) [input]
    !     A scalar input of any type.
    !
    ! Returns:
    !
    ! * z: integer [output]
    !     The number of bits in the significand of the input floating-point
    !     value, or 0 if the input was not a floating-point value.
    !
        CLASS(*), INTENT(IN) :: x
        INTEGER :: z
        SELECT TYPE (x)
        CLASS IS (rpe_type)
            IF (x%sbits == RPE_SBITS_UNSPECIFIED) THEN
                z = RPE_DEFAULT_SBITS
            ELSE
                z = x%sbits
            END IF
        TYPE IS (REAL(KIND=RPE_DOUBLE_KIND))
            z = 52
        TYPE IS (REAL(KIND=RPE_SINGLE_KIND))
            z = 23
        CLASS DEFAULT
            z = 0
        END SELECT
    END FUNCTION significand_bits

    FUNCTION rpe_literal_real (x, n) RESULT (z)
    ! Create an `rpe_var` instance from a real literal.
    !
    ! Arguments:
    !
    ! * x: real(kind=RPE_REAL_KIND) [input]
    !     The literal to transform to a reduced precision `rpe_var` instance.
    !
    ! * n: integer [input, optional]
    !     The number of bits in the significand of the resulting reduced
    !     precision number. If not specified then the result will have the
    !     default precision.
    !
    ! Returns:
    !
    ! * z: rpe_var
    !     An `rpe_var` instance representing the input literal at the given
    !     precision.
    !
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: x
        INTEGER, OPTIONAL,        INTENT(IN) :: n
        TYPE(rpe_var) :: z
        IF (PRESENT(n)) THEN
            z%sbits = n
        END IF
        z = x
    END FUNCTION rpe_literal_real

    FUNCTION rpe_literal_alternate (x, n) RESULT (z)
    ! Create an `rpe_var` instance from a real literal.
    !
    ! Arguments:
    !
    ! * x: real(kind=RPE_ALTERNATE_KIND) [input]
    !     The literal to transform to a reduced precision `rpe_var` instance.
    !
    ! * n: integer [input, optional]
    !     The number of bits in the significand of the resulting reduced
    !     precision number. If not specified then the result will have the
    !     default precision.
    !
    ! Returns:
    !
    ! * z: rpe_var
    !     An `rpe_var` instance representing the input literal at the given
    !     precision.
    !
        REAL(KIND=RPE_ALTERNATE_KIND),           INTENT(IN) :: x
        INTEGER,                       OPTIONAL, INTENT(IN) :: n
        TYPE(rpe_var) :: z
        IF (PRESENT(n)) THEN
            z%sbits = n
        END IF
        z = x
    END FUNCTION rpe_literal_alternate

    FUNCTION rpe_literal_integer (x, n) RESULT (z)
    ! Create an `rpe_var` instance from an integer literal.
    !
    ! Arguments:
    !
    ! * x: integer [input]
    !     The literal to transform to a reduced precision `rpe_var` instance.
    !
    ! * n: integer [input, optional]
    !     The number of bits in the significand of the resulting reduced
    !     precision number. If not specified then the result will have the
    !     default precision.
    !
    ! Returns:
    !
    ! * z: rpe_var
    !     An `rpe_var` instance representing the input literal at the given
    !     precision.
    !
        INTEGER,           INTENT(IN) :: x
        INTEGER, OPTIONAL, INTENT(IN) :: n
        TYPE(rpe_var) :: z
        IF (PRESENT(n)) THEN
            z%sbits = n
        END IF
        z = x
    END FUNCTION rpe_literal_integer

    FUNCTION rpe_literal_long (x, n) RESULT (z)
    ! Create an `rpe_var` instance from a long integer literal.
    !
    ! Arguments:
    !
    ! * x: integer(KIND=8) [input]
    !     The literal to transform to a reduced precision `rpe_var` instance.
    !
    ! * n: integer [input, optional]
    !     The number of bits in the significand of the resulting reduced
    !     precision number. If not specified then the result will have the
    !     default precision.
    !
    ! Returns:
    !
    ! * z: rpe_var
    !     An `rpe_var` instance representing the input literal at the given
    !     precision.
    !
        INTEGER(KIND=8),           INTENT(IN) :: x
        INTEGER,         OPTIONAL, INTENT(IN) :: n
        TYPE(rpe_var) :: z
        IF (PRESENT(n)) THEN
            z%sbits = n
        END IF
        z = x
    END FUNCTION rpe_literal_long


!-----------------------------------------------------------------------
! Overloaded assignment definitions:
!-----------------------------------------------------------------------

    ELEMENTAL SUBROUTINE assign_rpe_rpe (r1, r2)
    ! Assign an `rpe_type` instance to another `rpe_type` instance.
    !
    ! Arguments:
    !
    ! * r1: class(rpe_type) [input/output]
    !       An `rpe_type` instance to assign to.
    !
    ! * r2: class(rpe_type) [input]
    !       An `rpe_type` instance whose value will be assigned to `r1`.
    !
        CLASS(rpe_type), INTENT(INOUT) :: r1
        CLASS(rpe_type), INTENT(IN)    :: r2
        CALL r1%set_value(r2%get_value())
        CALL apply_truncation (r1)
    END SUBROUTINE assign_rpe_rpe

    ELEMENTAL SUBROUTINE assign_rpe_real (rpe, x)
    ! Assign a real variable to an `rpe_type` instance.
    !
    ! Arguments:
    !
    ! * rpe: class(rpe_type) [input/output]
    !       An `rpe_type` instance to assign to.
    !
    ! * x: real(kind=RPE_REAL_KIND) [input]
    !       A real variable whose value will be assigned to `rpe`.
    !
        CLASS(rpe_type),          INTENT(INOUT) :: rpe
        REAL(KIND=RPE_REAL_KIND), INTENT(IN)    :: x
        CALL rpe%set_value(x)
        CALL apply_truncation (rpe)
    END SUBROUTINE assign_rpe_real

    ELEMENTAL SUBROUTINE assign_rpe_alternate (rpe, x)
    ! Assign a real variable to an `rpe_type` instance.
    !
    ! Arguments:
    !
    ! * rpe: class(rpe_type) [input/output]
    !       An `rpe_type` instance to assign to.
    !
    ! * x: real(kind=RPE_ALTERNATE_KIND) [input]
    !       A real variable whose value will be assigned to `rpe`.
    !
        CLASS(rpe_type),               INTENT(INOUT) :: rpe
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN)    :: x
        CALL rpe%set_value(x)
        CALL apply_truncation (rpe)
    END SUBROUTINE assign_rpe_alternate

    ELEMENTAL SUBROUTINE assign_rpe_integer (rpe, x)
    ! Assign an integer variable to an `rpe_type` instance.
    !
    ! Arguments:
    !
    ! * rpe: class(rpe_type) [input/output]
    !       An `rpe_type` instance to assign to.
    !
    ! * x: integer(kind=4) [input]
    !       An integer variable whose value will be assigned to `rpe`.
    !
        CLASS(rpe_type), INTENT(INOUT) :: rpe
        INTEGER(KIND=4), INTENT(IN)    :: x
        CALL rpe%set_value(x)
        CALL apply_truncation (rpe)
    END SUBROUTINE assign_rpe_integer

    ELEMENTAL SUBROUTINE assign_rpe_long (rpe, x)
    ! Assign a long integer variable to an `rpe_type` instance.
    !
    ! Arguments:
    !
    ! * rpe: class(rpe_type) [input/output]
    !       An `rpe_type` instance to assign to.
    !
    ! * x: integer(kind=8) [input]
    !       A long integer variable whose value will be assigned to `rpe`.
    !
        CLASS(rpe_type), INTENT(INOUT) :: rpe
        INTEGER(KIND=8), INTENT(IN)    :: x
        CALL rpe%set_value(x)
        CALL apply_truncation (rpe)
    END SUBROUTINE assign_rpe_long

    ELEMENTAL SUBROUTINE assign_real_rpe (x, rpe)
    ! Assign an `rpe_type` instance to a real variable.
    !
    ! Arguments:
    !
    ! * x: real(kind=RPE_REAL_KIND) [input/output]
    !       A real variable assign to.
    !
    ! * rpe: class(rpe_type) [input]
    !       An `rpe_type` instance whose value will be assigned to `x`.
    !
        REAL(KIND=RPE_REAL_KIND), INTENT(INOUT) :: x
        CLASS(rpe_type),          INTENT(IN)    :: rpe
        x = rpe%get_value()
    END SUBROUTINE assign_real_rpe

    ELEMENTAL SUBROUTINE assign_alternate_rpe (x, rpe)
    ! Assign an `rpe_type` instance to a real variable.
    !
    ! Arguments:
    !
    ! * x: real(kind=RPE_ALTERNATE_KIND) [input/output]
    !       A real variable assign to.
    !
    ! * rpe: class(rpe_type) [input]
    !       An `rpe_type` instance whose value will be assigned to `x`.
    !
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(INOUT) :: x
        CLASS(rpe_type),               INTENT(IN)    :: rpe
        x = rpe%get_value()
    END SUBROUTINE assign_alternate_rpe

    ELEMENTAL SUBROUTINE assign_integer_rpe (x, rpe)
    ! Assign an `rpe_type` instance to an integer variable.
    !
    ! Arguments:
    !
    ! * x: integer(kind=4) [input/output]
    !       An integer variable assign to.
    !
    ! * rpe: class(rpe_type) [input]
    !       An `rpe_type` instance whose value will be assigned to `x`.
    !
        INTEGER(KIND=4), INTENT(INOUT) :: x
        CLASS(rpe_type), INTENT(IN)    :: rpe
        x = rpe%get_value()
    END SUBROUTINE assign_integer_rpe

    ELEMENTAL SUBROUTINE assign_long_rpe (x, rpe)
    ! Assign an `rpe_type` instance to a long integer variable.
    !
    ! Arguments:
    !
    ! * x: integer(kind=8) [input/output]
    !       A long integer variable assign to.
    !
    ! * rpe: class(rpe_type) [input]
    !       An `rpe_type` instance whose value will be assigned to `x`.
    !
        INTEGER(KIND=8), INTENT(INOUT) :: x
        CLASS(rpe_type), INTENT(IN)    :: rpe
        x = rpe%get_value()
    END SUBROUTINE assign_long_rpe

!-----------------------------------------------------------------------
! Overloaded operator definitions (external):
!-----------------------------------------------------------------------

    #include "implementation_operators.f90"

!-----------------------------------------------------------------------
! Overloaded intrinsic function definitions (external):
!-----------------------------------------------------------------------

    #include "implementation_intrinsics.f90"

!-----------------------------------------------------------------------
! Other extensions (external):
!-----------------------------------------------------------------------

    #include "implementation_extras.f90"

END MODULE rp_emulator
