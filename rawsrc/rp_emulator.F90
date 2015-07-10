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

!-----------------------------------------------------------------------
! Module parameters and variables:
!-----------------------------------------------------------------------

    !: The Fortran kind of the real data type used by the emulator
    !: (usually 64-bit double-precision).
    INTEGER, PARAMETER :: RPE_REAL_KIND = kind(1.0d0)
    !: The Fortran kind of an alternate real data type (usually 32-bit
    !: single-precision), should be single-precision if RPE_REAL_KIND
    !: is double-precision or double-precision if RPE_REAL_KIND is
    !: single-precision.
    INTEGER, PARAMETER :: RPE_ALTERNATE_KIND = kind(1.0)
    !: The Fortran kind of a single-precision floating-point number.
    INTEGER, PARAMETER :: RPE_SINGLE_KIND = kind(1.0)
    
    !: Logical flag for turning the emulator on/off.
    LOGICAL :: RPE_ACTIVE = .TRUE.

    !: The number of bits to use in the reduced-precision mantissa. A
    !: maximum of 23 bits can be retained.
    INTEGER :: RPE_BITS = 23

!-----------------------------------------------------------------------
! Module derived-type definitions:
!-----------------------------------------------------------------------

    TYPE, ABSTRACT :: rpe_type
    ! An abstract base class for reduced-precision types.
    !
    ! A reduced-precision type is expected to be a container for some
    ! kind of floating-point value. It must provide the methods
    ! `get_value` and `set_value` which return the contained value and
    ! set the contained value respectively.
    !
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

    INTERFACE init_shadow
    ! Interfaces for initializing `rpe_shadow` instances.
    !
        MODULE PROCEDURE init_shadow_scalar
        MODULE PROCEDURE init_shadow_v1d
        MODULE PROCEDURE init_shadow_v2d
        MODULE PROCEDURE init_shadow_v3d
        MODULE PROCEDURE init_shadow_v4d
    END INTERFACE

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
        TYPE(rpe_shadow),                INTENT(OUT) :: this
        REAL(KIND=RPE_REAL_KIND), TARGET, INTENT(IN)  :: x
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
        TYPE(rpe_shadow),         DIMENSION(:),         INTENT(OUT) :: this
        REAL(KIND=RPE_REAL_KIND), DIMENSION(:), TARGET, INTENT(IN)  :: x
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
        TYPE(rpe_shadow),         DIMENSION(:, :),         INTENT(OUT) :: this
        REAL(KIND=RPE_REAL_KIND), DIMENSION(:, :), TARGET, INTENT(IN)  :: x
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
        TYPE(rpe_shadow),         DIMENSION(:, :, :),         INTENT(OUT) :: this
        REAL(KIND=RPE_REAL_KIND), DIMENSION(:, :, :), TARGET, INTENT(IN)  :: x
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
        TYPE(rpe_shadow),         DIMENSION(:, :, :, :),         INTENT(OUT) :: this
        REAL(KIND=RPE_REAL_KIND), DIMENSION(:, :, :, :), TARGET, INTENT(IN)  :: x
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

    ELEMENTAL SUBROUTINE reduce_precision (x)
    ! Reduce the precision of a `rpe_type` instance.
    !
    ! Truncates the given floating-point number significand to the
    ! number of bits defined by the module variable RPE_BITS. If the
    ! module variable RPE_ACTIVE is false this subroutine returns the
    ! unaltered input value, it only performs the bit truncation if
    ! RPE_ACTIVE is true.
    !
    ! Argument:
    !
    ! *x: type(rpe_type) [input/output]
    !     The `rpe_type` instance to truncate.
    !
        CLASS(rpe_type), INTENT(INOUT) :: x
        REAL(KIND=RPE_SINGLE_KIND) :: y
        INTEGER :: rounding_bit
        INTEGER :: zero_bits, bits
        IF (RPE_ACTIVE) THEN
            ! First truncate the double-precision input to a single-precision
            ! real number. The mantissa has a maximum of 23-bits.
            y = REAL(x%get_value(), RPE_SINGLE_KIND)
            ! The rounding bit is the last bit that will be truncated
            ! (counting from 0 at the right-most bit).
            rounding_bit = 23 - RPE_BITS - 1
            ! If the rounding bit is < 0 then we no need to do
            ! anything, the full 23-bits of the mantissa remain (this
            ! allows one to specify that the precision should be 23
            ! bits or more in the mantsissa.
            IF (rounding_bit .GE. 0) THEN
                ! Copy the single-precision bit representation of the input
                ! into an integer so it can be manipulated:
                bits = TRANSFER(y, bits)
                IF (BTEST(bits, rounding_bit)) THEN
                    ! If the bit at which truncation occurs is set then
                    ! make sure the truncation rounds the number up:
                    bits = bits + 2 ** (rounding_bit)
                END IF
                ! Move rounding_bit + 1 bits from the number zero (all bits
                ! set to zero) into the target to truncate at the given
                ! number of bits.
                zero_bits = 0
                CALL MVBITS (zero_bits, 0, rounding_bit + 1, bits, 0)
                y = TRANSFER(bits, y)
            END IF
            x = y
        END IF
    END SUBROUTINE reduce_precision

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
