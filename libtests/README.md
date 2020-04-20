# Tests of Futhark's library backends

These tests are written in an ad hoc fashion, as the usual `futhark
test` tool only handles executables.

Since executables are to a large extent built using the exact same
library code, we only need to test library-specific concerns.
Specifically, the handling of opaque types (which don't work in
executables anyway) is important.
