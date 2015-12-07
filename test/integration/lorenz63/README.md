# Lorenz '63 integration tests

A simple Lorenz '63 model used to test basic properties of the
reduced-precision emulator.

Two binaries are built: a reference binary with no emulator code
included, and a version built with the emulator. The emulator version
can be run using command line arguments to specify if the emulator is
on or off and the number of significand bits to be used.

