    !-------------------------------------------------------------------
    ! Overloaded definitions for 'epsilon':
    !

    FUNCTION epsilon_rpe (a) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x = EPSILON(a%get_value())
    END FUNCTION epsilon_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'huge':
    !

    FUNCTION huge_rpe (a) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x = HUGE(a%get_value())
    END FUNCTION huge_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'tiny':
    !

    FUNCTION tiny_rpe (a) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x = TINY(a%get_value())
    END FUNCTION tiny_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'kind':
    !

    FUNCTION kind_rpe (a) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        INTEGER :: x
        x = KIND(a%get_value())
    END FUNCTION kind_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'abs':
    !

    ELEMENTAL FUNCTION abs_rpe (a) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x = ABS(a%get_value())
    END FUNCTION abs_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'cos':
    !

    ELEMENTAL FUNCTION cos_rpe (a) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x = COS(a%get_value())
        CALL reduce_precision (x)
    END FUNCTION cos_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'sin':
    !

    ELEMENTAL FUNCTION sin_rpe (a) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x = SIN(a%get_value())
        CALL reduce_precision (x)
    END FUNCTION sin_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'tan':
    !

    ELEMENTAL FUNCTION tan_rpe (a) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x = TAN(a%get_value())
        CALL reduce_precision (x)
    END FUNCTION tan_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'acos':
    !

    ELEMENTAL FUNCTION acos_rpe (a) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x = ACOS(a%get_value())
        CALL reduce_precision (x)
    END FUNCTION acos_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'asin':
    !

    ELEMENTAL FUNCTION asin_rpe (a) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x = ASIN(a%get_value())
        CALL reduce_precision (x)
    END FUNCTION asin_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'atan':
    !

    ELEMENTAL FUNCTION atan_rpe (a) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x = ATAN(a%get_value())
        CALL reduce_precision (x)
    END FUNCTION atan_rpe

    ELEMENTAL FUNCTION atan_rpe_rpe (a, b) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        CLASS(rpe_type), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = ATAN(a%get_value(), b%get_value())
        CALL reduce_precision (x)
    END FUNCTION atan_rpe_rpe

    ELEMENTAL FUNCTION atan_rpe_real (a, b) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = ATAN(a%get_value(), b)
        CALL reduce_precision (x)
    END FUNCTION atan_rpe_real

    ELEMENTAL FUNCTION atan_real_rpe (a, b) RESULT (x)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: a
        CLASS(rpe_type), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = ATAN(a, b%get_value())
        CALL reduce_precision (x)
    END FUNCTION atan_real_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'cosh':
    !

    ELEMENTAL FUNCTION cosh_rpe (a) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x = COSH(a%get_value())
        CALL reduce_precision (x)
    END FUNCTION cosh_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'sinh':
    !

    ELEMENTAL FUNCTION sinh_rpe (a) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x = SINH(a%get_value())
        CALL reduce_precision (x)
    END FUNCTION sinh_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'tanh':
    !

    ELEMENTAL FUNCTION tanh_rpe (a) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x = TANH(a%get_value())
        CALL reduce_precision (x)
    END FUNCTION tanh_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'exp':
    !

    ELEMENTAL FUNCTION exp_rpe (a) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x = EXP(a%get_value())
        CALL reduce_precision (x)
    END FUNCTION exp_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'log':
    !

    ELEMENTAL FUNCTION log_rpe (a) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x = LOG(a%get_value())
        CALL reduce_precision (x)
    END FUNCTION log_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'log10':
    !

    ELEMENTAL FUNCTION log10_rpe (a) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x = LOG10(a%get_value())
        CALL reduce_precision (x)
    END FUNCTION log10_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'sqrt':
    !

    ELEMENTAL FUNCTION sqrt_rpe (a) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x = SQRT(a%get_value())
        CALL reduce_precision (x)
    END FUNCTION sqrt_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'spacing':
    !

    ELEMENTAL FUNCTION spacing_rpe (a) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x = SPACING(a%get_value())
    END FUNCTION spacing_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'floor':
    !

    ELEMENTAL FUNCTION floor_rpe (a) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        INTEGER :: x
        x = FLOOR(a%get_value())
    END FUNCTION floor_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'int':
    !

    ELEMENTAL FUNCTION int_rpe (a) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        INTEGER :: x
        x = INT(a%get_value())
    END FUNCTION int_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'nint':
    !

    ELEMENTAL FUNCTION nint_rpe (a) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        INTEGER :: x
        x = NINT(a%get_value())
    END FUNCTION nint_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'atan2':
    !

    ELEMENTAL FUNCTION atan2_rpe_rpe (a, b) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        CLASS(rpe_type), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = ATAN2(a%get_value(), b%get_value())
        CALL reduce_precision (x)
    END FUNCTION atan2_rpe_rpe

    ELEMENTAL FUNCTION atan2_rpe_real (a, b) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = ATAN2(a%get_value(), b)
        CALL reduce_precision (x)
    END FUNCTION atan2_rpe_real

    ELEMENTAL FUNCTION atan2_real_rpe (a, b) RESULT (x)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: a
        CLASS(rpe_type), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = ATAN2(a, b%get_value())
        CALL reduce_precision (x)
    END FUNCTION atan2_real_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'dim':
    !

    ELEMENTAL FUNCTION dim_rpe_rpe (a, b) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        CLASS(rpe_type), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = DIM(a%get_value(), b%get_value())
        CALL reduce_precision (x)
    END FUNCTION dim_rpe_rpe

    ELEMENTAL FUNCTION dim_rpe_real (a, b) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = DIM(a%get_value(), b)
        CALL reduce_precision (x)
    END FUNCTION dim_rpe_real

    ELEMENTAL FUNCTION dim_real_rpe (a, b) RESULT (x)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: a
        CLASS(rpe_type), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = DIM(a, b%get_value())
        CALL reduce_precision (x)
    END FUNCTION dim_real_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'mod':
    !

    ELEMENTAL FUNCTION mod_rpe_rpe (a, b) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        CLASS(rpe_type), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = MOD(a%get_value(), b%get_value())
        CALL reduce_precision (x)
    END FUNCTION mod_rpe_rpe

    ELEMENTAL FUNCTION mod_rpe_real (a, b) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = MOD(a%get_value(), b)
        CALL reduce_precision (x)
    END FUNCTION mod_rpe_real

    ELEMENTAL FUNCTION mod_real_rpe (a, b) RESULT (x)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: a
        CLASS(rpe_type), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = MOD(a, b%get_value())
        CALL reduce_precision (x)
    END FUNCTION mod_real_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'nearest':
    !

    ELEMENTAL FUNCTION nearest_rpe_rpe (a, b) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        CLASS(rpe_type), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = NEAREST(a%get_value(), b%get_value())
    END FUNCTION nearest_rpe_rpe

    ELEMENTAL FUNCTION nearest_rpe_real (a, b) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = NEAREST(a%get_value(), b)
    END FUNCTION nearest_rpe_real

    ELEMENTAL FUNCTION nearest_real_rpe (a, b) RESULT (x)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: a
        CLASS(rpe_type), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = NEAREST(a, b%get_value())
    END FUNCTION nearest_real_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'sign':
    !

    ELEMENTAL FUNCTION sign_rpe_rpe (a, b) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        CLASS(rpe_type), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = SIGN(a%get_value(), b%get_value())
    END FUNCTION sign_rpe_rpe

    ELEMENTAL FUNCTION sign_rpe_real (a, b) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = SIGN(a%get_value(), b)
    END FUNCTION sign_rpe_real

    ELEMENTAL FUNCTION sign_real_rpe (a, b) RESULT (x)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: a
        CLASS(rpe_type), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = SIGN(a, b%get_value())
    END FUNCTION sign_real_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'min':
    !

    ELEMENTAL FUNCTION min_rpe_rpe (a, b) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        CLASS(rpe_type), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = MIN(a%get_value(), b%get_value())
    END FUNCTION min_rpe_rpe

    ELEMENTAL FUNCTION min_rpe_real (a, b) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = MIN(a%get_value(), b)
    END FUNCTION min_rpe_real

    ELEMENTAL FUNCTION min_real_rpe (a, b) RESULT (x)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: a
        CLASS(rpe_type), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = MIN(a, b%get_value())
    END FUNCTION min_real_rpe

    ELEMENTAL FUNCTION min_rpe_rpe_rpe_3arg (a0, a1, a2) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a0
        CLASS(rpe_type), INTENT(IN) :: a1
        CLASS(rpe_type), INTENT(IN) :: a2
        TYPE(rpe_var) :: x
        x = MIN(a0%get_value(), a1%get_value(), a2%get_value())
    END FUNCTION min_rpe_rpe_rpe_3arg

    ELEMENTAL FUNCTION min_rpe_rpe_rpe_rpe_4arg (a0, a1, a2, a3) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a0
        CLASS(rpe_type), INTENT(IN) :: a1
        CLASS(rpe_type), INTENT(IN) :: a2
        CLASS(rpe_type), INTENT(IN) :: a3
        TYPE(rpe_var) :: x
        x = MIN(a0%get_value(), a1%get_value(), a2%get_value(), a3%get_value())
    END FUNCTION min_rpe_rpe_rpe_rpe_4arg

    ELEMENTAL FUNCTION min_rpe_rpe_rpe_rpe_rpe_5arg (a0, a1, a2, a3, a4) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a0
        CLASS(rpe_type), INTENT(IN) :: a1
        CLASS(rpe_type), INTENT(IN) :: a2
        CLASS(rpe_type), INTENT(IN) :: a3
        CLASS(rpe_type), INTENT(IN) :: a4
        TYPE(rpe_var) :: x
        x = MIN(a0%get_value(), a1%get_value(), a2%get_value(), a3%get_value(), a4%get_value())
    END FUNCTION min_rpe_rpe_rpe_rpe_rpe_5arg

    ELEMENTAL FUNCTION min_rpe_rpe_rpe_rpe_rpe_rpe_6arg (a0, a1, a2, a3, a4, a5) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a0
        CLASS(rpe_type), INTENT(IN) :: a1
        CLASS(rpe_type), INTENT(IN) :: a2
        CLASS(rpe_type), INTENT(IN) :: a3
        CLASS(rpe_type), INTENT(IN) :: a4
        CLASS(rpe_type), INTENT(IN) :: a5
        TYPE(rpe_var) :: x
        x = MIN(a0%get_value(), a1%get_value(), a2%get_value(), a3%get_value(), a4%get_value(), a5%get_value())
    END FUNCTION min_rpe_rpe_rpe_rpe_rpe_rpe_6arg

    ELEMENTAL FUNCTION min_rpe_rpe_rpe_rpe_rpe_rpe_rpe_7arg (a0, a1, a2, a3, a4, a5, a6) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a0
        CLASS(rpe_type), INTENT(IN) :: a1
        CLASS(rpe_type), INTENT(IN) :: a2
        CLASS(rpe_type), INTENT(IN) :: a3
        CLASS(rpe_type), INTENT(IN) :: a4
        CLASS(rpe_type), INTENT(IN) :: a5
        CLASS(rpe_type), INTENT(IN) :: a6
        TYPE(rpe_var) :: x
        x = MIN(a0%get_value(), a1%get_value(), a2%get_value(), a3%get_value(), a4%get_value(), a5%get_value(), a6%get_value())
    END FUNCTION min_rpe_rpe_rpe_rpe_rpe_rpe_rpe_7arg

    ELEMENTAL FUNCTION min_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_8arg (a0, a1, a2, a3, a4, a5, a6, a7) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a0
        CLASS(rpe_type), INTENT(IN) :: a1
        CLASS(rpe_type), INTENT(IN) :: a2
        CLASS(rpe_type), INTENT(IN) :: a3
        CLASS(rpe_type), INTENT(IN) :: a4
        CLASS(rpe_type), INTENT(IN) :: a5
        CLASS(rpe_type), INTENT(IN) :: a6
        CLASS(rpe_type), INTENT(IN) :: a7
        TYPE(rpe_var) :: x
        x = MIN(a0%get_value(), a1%get_value(), a2%get_value(), a3%get_value(), a4%get_value(), a5%get_value(), a6%get_value(), a7%get_value())
    END FUNCTION min_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_8arg

    ELEMENTAL FUNCTION min_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_9arg (a0, a1, a2, a3, a4, a5, a6, a7, a8) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a0
        CLASS(rpe_type), INTENT(IN) :: a1
        CLASS(rpe_type), INTENT(IN) :: a2
        CLASS(rpe_type), INTENT(IN) :: a3
        CLASS(rpe_type), INTENT(IN) :: a4
        CLASS(rpe_type), INTENT(IN) :: a5
        CLASS(rpe_type), INTENT(IN) :: a6
        CLASS(rpe_type), INTENT(IN) :: a7
        CLASS(rpe_type), INTENT(IN) :: a8
        TYPE(rpe_var) :: x
        x = MIN(a0%get_value(), a1%get_value(), a2%get_value(), a3%get_value(), a4%get_value(), a5%get_value(), a6%get_value(), a7%get_value(), a8%get_value())
    END FUNCTION min_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_9arg

    ELEMENTAL FUNCTION min_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_10arg (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a0
        CLASS(rpe_type), INTENT(IN) :: a1
        CLASS(rpe_type), INTENT(IN) :: a2
        CLASS(rpe_type), INTENT(IN) :: a3
        CLASS(rpe_type), INTENT(IN) :: a4
        CLASS(rpe_type), INTENT(IN) :: a5
        CLASS(rpe_type), INTENT(IN) :: a6
        CLASS(rpe_type), INTENT(IN) :: a7
        CLASS(rpe_type), INTENT(IN) :: a8
        CLASS(rpe_type), INTENT(IN) :: a9
        TYPE(rpe_var) :: x
        x = MIN(a0%get_value(), a1%get_value(), a2%get_value(), a3%get_value(), a4%get_value(), a5%get_value(), a6%get_value(), a7%get_value(), a8%get_value(), a9%get_value())
    END FUNCTION min_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_10arg

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'max':
    !

    ELEMENTAL FUNCTION max_rpe_rpe (a, b) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        CLASS(rpe_type), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = MAX(a%get_value(), b%get_value())
    END FUNCTION max_rpe_rpe

    ELEMENTAL FUNCTION max_rpe_real (a, b) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = MAX(a%get_value(), b)
    END FUNCTION max_rpe_real

    ELEMENTAL FUNCTION max_real_rpe (a, b) RESULT (x)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: a
        CLASS(rpe_type), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x = MAX(a, b%get_value())
    END FUNCTION max_real_rpe

    ELEMENTAL FUNCTION max_rpe_rpe_rpe_3arg (a0, a1, a2) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a0
        CLASS(rpe_type), INTENT(IN) :: a1
        CLASS(rpe_type), INTENT(IN) :: a2
        TYPE(rpe_var) :: x
        x = MAX(a0%get_value(), a1%get_value(), a2%get_value())
    END FUNCTION max_rpe_rpe_rpe_3arg

    ELEMENTAL FUNCTION max_rpe_rpe_rpe_rpe_4arg (a0, a1, a2, a3) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a0
        CLASS(rpe_type), INTENT(IN) :: a1
        CLASS(rpe_type), INTENT(IN) :: a2
        CLASS(rpe_type), INTENT(IN) :: a3
        TYPE(rpe_var) :: x
        x = MAX(a0%get_value(), a1%get_value(), a2%get_value(), a3%get_value())
    END FUNCTION max_rpe_rpe_rpe_rpe_4arg

    ELEMENTAL FUNCTION max_rpe_rpe_rpe_rpe_rpe_5arg (a0, a1, a2, a3, a4) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a0
        CLASS(rpe_type), INTENT(IN) :: a1
        CLASS(rpe_type), INTENT(IN) :: a2
        CLASS(rpe_type), INTENT(IN) :: a3
        CLASS(rpe_type), INTENT(IN) :: a4
        TYPE(rpe_var) :: x
        x = MAX(a0%get_value(), a1%get_value(), a2%get_value(), a3%get_value(), a4%get_value())
    END FUNCTION max_rpe_rpe_rpe_rpe_rpe_5arg

    ELEMENTAL FUNCTION max_rpe_rpe_rpe_rpe_rpe_rpe_6arg (a0, a1, a2, a3, a4, a5) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a0
        CLASS(rpe_type), INTENT(IN) :: a1
        CLASS(rpe_type), INTENT(IN) :: a2
        CLASS(rpe_type), INTENT(IN) :: a3
        CLASS(rpe_type), INTENT(IN) :: a4
        CLASS(rpe_type), INTENT(IN) :: a5
        TYPE(rpe_var) :: x
        x = MAX(a0%get_value(), a1%get_value(), a2%get_value(), a3%get_value(), a4%get_value(), a5%get_value())
    END FUNCTION max_rpe_rpe_rpe_rpe_rpe_rpe_6arg

    ELEMENTAL FUNCTION max_rpe_rpe_rpe_rpe_rpe_rpe_rpe_7arg (a0, a1, a2, a3, a4, a5, a6) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a0
        CLASS(rpe_type), INTENT(IN) :: a1
        CLASS(rpe_type), INTENT(IN) :: a2
        CLASS(rpe_type), INTENT(IN) :: a3
        CLASS(rpe_type), INTENT(IN) :: a4
        CLASS(rpe_type), INTENT(IN) :: a5
        CLASS(rpe_type), INTENT(IN) :: a6
        TYPE(rpe_var) :: x
        x = MAX(a0%get_value(), a1%get_value(), a2%get_value(), a3%get_value(), a4%get_value(), a5%get_value(), a6%get_value())
    END FUNCTION max_rpe_rpe_rpe_rpe_rpe_rpe_rpe_7arg

    ELEMENTAL FUNCTION max_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_8arg (a0, a1, a2, a3, a4, a5, a6, a7) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a0
        CLASS(rpe_type), INTENT(IN) :: a1
        CLASS(rpe_type), INTENT(IN) :: a2
        CLASS(rpe_type), INTENT(IN) :: a3
        CLASS(rpe_type), INTENT(IN) :: a4
        CLASS(rpe_type), INTENT(IN) :: a5
        CLASS(rpe_type), INTENT(IN) :: a6
        CLASS(rpe_type), INTENT(IN) :: a7
        TYPE(rpe_var) :: x
        x = MAX(a0%get_value(), a1%get_value(), a2%get_value(), a3%get_value(), a4%get_value(), a5%get_value(), a6%get_value(), a7%get_value())
    END FUNCTION max_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_8arg

    ELEMENTAL FUNCTION max_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_9arg (a0, a1, a2, a3, a4, a5, a6, a7, a8) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a0
        CLASS(rpe_type), INTENT(IN) :: a1
        CLASS(rpe_type), INTENT(IN) :: a2
        CLASS(rpe_type), INTENT(IN) :: a3
        CLASS(rpe_type), INTENT(IN) :: a4
        CLASS(rpe_type), INTENT(IN) :: a5
        CLASS(rpe_type), INTENT(IN) :: a6
        CLASS(rpe_type), INTENT(IN) :: a7
        CLASS(rpe_type), INTENT(IN) :: a8
        TYPE(rpe_var) :: x
        x = MAX(a0%get_value(), a1%get_value(), a2%get_value(), a3%get_value(), a4%get_value(), a5%get_value(), a6%get_value(), a7%get_value(), a8%get_value())
    END FUNCTION max_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_9arg

    ELEMENTAL FUNCTION max_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_10arg (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) RESULT (x)
        CLASS(rpe_type), INTENT(IN) :: a0
        CLASS(rpe_type), INTENT(IN) :: a1
        CLASS(rpe_type), INTENT(IN) :: a2
        CLASS(rpe_type), INTENT(IN) :: a3
        CLASS(rpe_type), INTENT(IN) :: a4
        CLASS(rpe_type), INTENT(IN) :: a5
        CLASS(rpe_type), INTENT(IN) :: a6
        CLASS(rpe_type), INTENT(IN) :: a7
        CLASS(rpe_type), INTENT(IN) :: a8
        CLASS(rpe_type), INTENT(IN) :: a9
        TYPE(rpe_var) :: x
        x = MAX(a0%get_value(), a1%get_value(), a2%get_value(), a3%get_value(), a4%get_value(), a5%get_value(), a6%get_value(), a7%get_value(), a8%get_value(), a9%get_value())
    END FUNCTION max_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_10arg

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'minval':
    !

    FUNCTION minval_rpe_1d (a) RESULT (x)
        CLASS(rpe_type), DIMENSION(:), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(SIZE(a, 1)) :: t
        t = a
        x = MINVAL(t)
    END FUNCTION minval_rpe_1d

    FUNCTION minval_rpe_2d (a) RESULT (x)
        CLASS(rpe_type), DIMENSION(:, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(SIZE(a, 1), SIZE(a, 2)) :: t
        t = a
        x = MINVAL(t)
    END FUNCTION minval_rpe_2d

    FUNCTION minval_rpe_3d (a) RESULT (x)
        CLASS(rpe_type), DIMENSION(:, :, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(SIZE(a, 1), SIZE(a, 2), SIZE(a, 3)) :: t
        t = a
        x = MINVAL(t)
    END FUNCTION minval_rpe_3d

    FUNCTION minval_rpe_4d (a) RESULT (x)
        CLASS(rpe_type), DIMENSION(:, :, :, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(SIZE(a, 1), SIZE(a, 2), SIZE(a, 3), SIZE(a, 4)) :: t
        t = a
        x = MINVAL(t)
    END FUNCTION minval_rpe_4d

    FUNCTION minval_rpe_5d (a) RESULT (x)
        CLASS(rpe_type), DIMENSION(:, :, :, :, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(SIZE(a, 1), SIZE(a, 2), SIZE(a, 3), SIZE(a, 4), SIZE(a, 5)) :: t
        t = a
        x = MINVAL(t)
    END FUNCTION minval_rpe_5d

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'maxval':
    !

    FUNCTION maxval_rpe_1d (a) RESULT (x)
        CLASS(rpe_type), DIMENSION(:), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(SIZE(a, 1)) :: t
        t = a
        x = MAXVAL(t)
    END FUNCTION maxval_rpe_1d

    FUNCTION maxval_rpe_2d (a) RESULT (x)
        CLASS(rpe_type), DIMENSION(:, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(SIZE(a, 1), SIZE(a, 2)) :: t
        t = a
        x = MAXVAL(t)
    END FUNCTION maxval_rpe_2d

    FUNCTION maxval_rpe_3d (a) RESULT (x)
        CLASS(rpe_type), DIMENSION(:, :, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(SIZE(a, 1), SIZE(a, 2), SIZE(a, 3)) :: t
        t = a
        x = MAXVAL(t)
    END FUNCTION maxval_rpe_3d

    FUNCTION maxval_rpe_4d (a) RESULT (x)
        CLASS(rpe_type), DIMENSION(:, :, :, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(SIZE(a, 1), SIZE(a, 2), SIZE(a, 3), SIZE(a, 4)) :: t
        t = a
        x = MAXVAL(t)
    END FUNCTION maxval_rpe_4d

    FUNCTION maxval_rpe_5d (a) RESULT (x)
        CLASS(rpe_type), DIMENSION(:, :, :, :, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(SIZE(a, 1), SIZE(a, 2), SIZE(a, 3), SIZE(a, 4), SIZE(a, 5)) :: t
        t = a
        x = MAXVAL(t)
    END FUNCTION maxval_rpe_5d

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'sum':
    !

    FUNCTION sum_rpe_1d (a) RESULT (x)
        CLASS(rpe_type), DIMENSION(:), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(SIZE(a, 1)) :: t
        t = a
        x = SUM(t)
        CALL reduce_precision (x)
    END FUNCTION sum_rpe_1d

    FUNCTION sum_rpe_2d (a) RESULT (x)
        CLASS(rpe_type), DIMENSION(:, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(SIZE(a, 1), SIZE(a, 2)) :: t
        t = a
        x = SUM(t)
        CALL reduce_precision (x)
    END FUNCTION sum_rpe_2d

    FUNCTION sum_rpe_3d (a) RESULT (x)
        CLASS(rpe_type), DIMENSION(:, :, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(SIZE(a, 1), SIZE(a, 2), SIZE(a, 3)) :: t
        t = a
        x = SUM(t)
        CALL reduce_precision (x)
    END FUNCTION sum_rpe_3d

    FUNCTION sum_rpe_4d (a) RESULT (x)
        CLASS(rpe_type), DIMENSION(:, :, :, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(SIZE(a, 1), SIZE(a, 2), SIZE(a, 3), SIZE(a, 4)) :: t
        t = a
        x = SUM(t)
        CALL reduce_precision (x)
    END FUNCTION sum_rpe_4d

    FUNCTION sum_rpe_5d (a) RESULT (x)
        CLASS(rpe_type), DIMENSION(:, :, :, :, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(SIZE(a, 1), SIZE(a, 2), SIZE(a, 3), SIZE(a, 4), SIZE(a, 5)) :: t
        t = a
        x = SUM(t)
        CALL reduce_precision (x)
    END FUNCTION sum_rpe_5d