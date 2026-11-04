# JavaScript library tests

This directory contains tests of calling Futhark from JavaScript, with tests
that ought to work with all backends. Some backend-specific code may still
exist, but the point is to demonstrate the uniform interface.

The `Makefile` has a `FUTHARK_BACKEND` variable that can be set to a support
backend. Run with e.g.

```
$ make FUTHARK_BACKEND=wasm-multicore
```

The tests are run with `node`, rather than via browser.

Each program `X.fut` is tested by `test_X.js`. General facilities are in
`harness.js`.
