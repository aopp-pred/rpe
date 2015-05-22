    INTERFACE epsilon
        MODULE PROCEDURE epsilon_rpe
    END INTERFACE epsilon

    INTERFACE huge
        MODULE PROCEDURE huge_rpe
    END INTERFACE huge

    INTERFACE tiny
        MODULE PROCEDURE tiny_rpe
    END INTERFACE tiny

    INTERFACE kind
        MODULE PROCEDURE kind_rpe
    END INTERFACE kind

    INTERFACE abs
        MODULE PROCEDURE abs_rpe
    END INTERFACE abs

    INTERFACE cos
        MODULE PROCEDURE cos_rpe
    END INTERFACE cos

    INTERFACE sin
        MODULE PROCEDURE sin_rpe
    END INTERFACE sin

    INTERFACE tan
        MODULE PROCEDURE tan_rpe
    END INTERFACE tan

    INTERFACE acos
        MODULE PROCEDURE acos_rpe
    END INTERFACE acos

    INTERFACE asin
        MODULE PROCEDURE asin_rpe
    END INTERFACE asin

    INTERFACE atan
        MODULE PROCEDURE atan_rpe
        MODULE PROCEDURE atan_rpe_rpe
        MODULE PROCEDURE atan_rpe_real
        MODULE PROCEDURE atan_real_rpe
    END INTERFACE atan

    INTERFACE cosh
        MODULE PROCEDURE cosh_rpe
    END INTERFACE cosh

    INTERFACE sinh
        MODULE PROCEDURE sinh_rpe
    END INTERFACE sinh

    INTERFACE tanh
        MODULE PROCEDURE tanh_rpe
    END INTERFACE tanh

    INTERFACE exp
        MODULE PROCEDURE exp_rpe
    END INTERFACE exp

    INTERFACE log
        MODULE PROCEDURE log_rpe
    END INTERFACE log

    INTERFACE log10
        MODULE PROCEDURE log10_rpe
    END INTERFACE log10

    INTERFACE sqrt
        MODULE PROCEDURE sqrt_rpe
    END INTERFACE sqrt

    INTERFACE spacing
        MODULE PROCEDURE spacing_rpe
    END INTERFACE spacing

    INTERFACE floor
        MODULE PROCEDURE floor_rpe
    END INTERFACE floor

    INTERFACE int
        MODULE PROCEDURE int_rpe
    END INTERFACE int

    INTERFACE nint
        MODULE PROCEDURE nint_rpe
    END INTERFACE nint

    INTERFACE atan2
        MODULE PROCEDURE atan2_rpe_rpe
        MODULE PROCEDURE atan2_rpe_real
        MODULE PROCEDURE atan2_real_rpe
    END INTERFACE atan2

    INTERFACE dim
        MODULE PROCEDURE dim_rpe_rpe
        MODULE PROCEDURE dim_rpe_real
        MODULE PROCEDURE dim_real_rpe
    END INTERFACE dim

    INTERFACE mod
        MODULE PROCEDURE mod_rpe_rpe
        MODULE PROCEDURE mod_rpe_real
        MODULE PROCEDURE mod_real_rpe
    END INTERFACE mod

    INTERFACE nearest
        MODULE PROCEDURE nearest_rpe_rpe
        MODULE PROCEDURE nearest_rpe_real
        MODULE PROCEDURE nearest_real_rpe
    END INTERFACE nearest

    INTERFACE sign
        MODULE PROCEDURE sign_rpe_rpe
        MODULE PROCEDURE sign_rpe_real
        MODULE PROCEDURE sign_real_rpe
    END INTERFACE sign

    INTERFACE min
        MODULE PROCEDURE min_rpe_rpe
        MODULE PROCEDURE min_rpe_real
        MODULE PROCEDURE min_real_rpe
        MODULE PROCEDURE min_rpe_rpe_rpe_3arg
        MODULE PROCEDURE min_rpe_rpe_rpe_rpe_4arg
        MODULE PROCEDURE min_rpe_rpe_rpe_rpe_rpe_5arg
        MODULE PROCEDURE min_rpe_rpe_rpe_rpe_rpe_rpe_6arg
        MODULE PROCEDURE min_rpe_rpe_rpe_rpe_rpe_rpe_rpe_7arg
        MODULE PROCEDURE min_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_8arg
        MODULE PROCEDURE min_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_9arg
        MODULE PROCEDURE min_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_10arg
    END INTERFACE min

    INTERFACE max
        MODULE PROCEDURE max_rpe_rpe
        MODULE PROCEDURE max_rpe_real
        MODULE PROCEDURE max_real_rpe
        MODULE PROCEDURE max_rpe_rpe_rpe_3arg
        MODULE PROCEDURE max_rpe_rpe_rpe_rpe_4arg
        MODULE PROCEDURE max_rpe_rpe_rpe_rpe_rpe_5arg
        MODULE PROCEDURE max_rpe_rpe_rpe_rpe_rpe_rpe_6arg
        MODULE PROCEDURE max_rpe_rpe_rpe_rpe_rpe_rpe_rpe_7arg
        MODULE PROCEDURE max_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_8arg
        MODULE PROCEDURE max_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_9arg
        MODULE PROCEDURE max_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_10arg
    END INTERFACE max

    INTERFACE minval
        MODULE PROCEDURE minval_rpe_1d
        MODULE PROCEDURE minval_rpe_2d
        MODULE PROCEDURE minval_rpe_3d
        MODULE PROCEDURE minval_rpe_4d
        MODULE PROCEDURE minval_rpe_5d
    END INTERFACE minval

    INTERFACE maxval
        MODULE PROCEDURE maxval_rpe_1d
        MODULE PROCEDURE maxval_rpe_2d
        MODULE PROCEDURE maxval_rpe_3d
        MODULE PROCEDURE maxval_rpe_4d
        MODULE PROCEDURE maxval_rpe_5d
    END INTERFACE maxval

    INTERFACE sum
        MODULE PROCEDURE sum_rpe_1d
        MODULE PROCEDURE sum_rpe_2d
        MODULE PROCEDURE sum_rpe_3d
        MODULE PROCEDURE sum_rpe_4d
        MODULE PROCEDURE sum_rpe_5d
    END INTERFACE sum