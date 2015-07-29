# Change-log



## v2.0.x


### Release v2.0.0 (29 July 2015)

#### Features

* Reduce precision on assignment:
  - The precision of the value held within an `rpe_type` instance is reduced
    whenever a value is assigned, meaning an `rpe_var` instance cannot ever
    store a full precision value, and an `rpe_shadow` type will always store
    a reduced precision value when it has been assigned to directly (but one
    can of course assign a full precision value to the variable it is
    shadowing and have that value retained).
  - Explicit calls to `reduce_precision` are no longer required in any
    overloaded operators or intrinsic routines, as the reduction of precision
    will be performed implicitly on assignment of the result.

#### Incompatibilities

* The change from explicit reduction of precision within overloaded operators
  and intrinsics will likely cause the emulator to return different results
  than the v1 series.



## v1.0.x


### Release v1.0.0 (28 July 2015)

#### Features

* Initial version used operationally for experiments.
