# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [0.4.0]

### Added

   * The constructor for generated PyOpenCL classes now accepts a
     `command_queue` parameter (#480).

   * Transposing small arrays is now much faster when using OpenCL
     backend (#478).

   * Infix operators can now be defined in prefix notation, e.g.:

         let (+) (x: i32) (y: i32) = x - y

     This permits them to have type- and shape parameters.

   * Comparison operators (<=, <, >, >=) are now valid for boolean
     operands.

   * Ordinary functions can be used as infix by enclosing them in
     backticks, as in Haskell.  They are left-associative and have
     lowest priority.

   * Numeric modules now have `largest`/`smallest` values.

   * Numeric modules now have `sum`, `product`, `maximum`, and
     `minimum` functions.

   * Added ``--Werror`` command line option to compilers.

   * Higher-order functions are now supported (#323).

   * Type inference is now supported, although with some limitations
     around records, in-place updates, and `unzip`. (#503)

   * Added a range of higher-order utility functions to the prelude:

         val (|>) 'a '^b: a ->  (a -> b) -> b

         let (<|) 'a '^b: (a -> b) -> a -> b

         let (|>>) 'a 'b '^c: (a -> b) -> (b -> c) -> a -> c

         let (<<|) 'a 'b '^c: (b -> c) -> (a -> b) a -> c

### Removed

### Changed

   * `FUTHARK_VERSIONED_CODE` is now `FUTHARK_INCREMENTAL_FLATTENING`.

   * The SOACs `map`, `reduce`, `filter`, `partition`, `scan`,
     `stream_red,` and `stream_map` have been replaced with library
     functions.

   * The futlib/mss and futlib/lss modules have been rewritten to use
     higher-order functions instead of modules.

### Fixed

   * Transpositions in generated OpenCL code no longer crashes on
     large but empty arrays (#483).

   * Booleans can now be compared with relational operators without
     crashing the compiler (#499).

## [0.3.1]

### Added

   * `futhark-bench` now tries to align benchmark results for better
     legibility.

### Fixed

   * `futhark-test`: now handles CRLF linebreaks correctly (#471).

   * A record field can be projected from an array index expression (#473).

   * Futhark will now never automatically pick Apple's CPU device for
     OpenCL, as it is rather broken.  You can still select it
     manually (#475).

   * Fixes to `set_bit` functions in the math module (#476).

## [0.3.0]

### Added

   * A comprehensible error message is now issued when attempting to
     run a Futhark program on an OpenCL that does not support the
     types used by the program.  A common case was trying to use
     double-precision floats on an Intel GPU.

   * Parallelism inside of a branch can now be exploited if the branch
     condition and the size of its results is invariant to all
     enclosing parallel loops.

   * A new OpenCL memory manager can in some cases dramatically
     improve performance for repeated invocations of the same entry
     point.

   * Experimental support for incremental flattening.  Set the
     environment variable `FUTHARK_VERSIONED_CODE` to any value to try
     it out.

   * `futhark-dataset`: Add `-t`/`-type` option.  Useful for
     inspecting data files.

   * Better error message when ranges written with two dots
     (`x..y`).

   * Type errors involving abstract types from modules now use
     qualified names (less "expected 't', got 't'", more "expected
     'foo.t', got 'bar.t'").

   * Shorter compile times for most programs.

   * `futhark-bench`: Add ``--skip-compilation`` flag.

   * `scatter` expressions nested in `map`s are now parallelised.

   * futlib: an `fft` module has been added, thanks to David
     P.H. Jørgensen and Kasper Abildtrup Hansen.

### Removed

   * `futhark-dataset`: Removed `--binary-no-header` and
     `--binary-only-header` options.

   * The `split` language construct has been removed.  There is a
     library function `split` that does approximately the same.

### Changed

  * futlib: the `complex` module now produces a non-abstract `complex`
    type.

  * futlib: the `random` module has been overhauled, with several new
    engines and adaptors changed, and some of the module types
    changed.  In particular, `rng_distribution` now contains a numeric
    module instead of an abstract type.

  * futlib: The `vec2` and `vec3` modules now represent vectors as
    records rather than tuples.

  * futlib: The `linalg` module now has distinct convenience functions
    for multiplying matrices with row and column vectors.

  * Only entry points defined directly in the file given to the
    compiler will be visible.

  * Range literals are now written without brackets: `x...y`.

  * The syntax `(-x)` can no longer be used for a partial application
    of subtraction.

  * `futhark-test` and `futhark-bench` will no longer append `.bin` to
    executables.

  * `futhark-test` and `futhark-bench` now replaces actual/expected
    files from previous runs, rather than increasing the litter.

### Fixed

  * Fusion would sometimes remove safety checks on e.g. `reshape`
    (#436).

  * Variables used as implicit fields in a record construction are now
    properly recognised as being used.

  * futlib: the `num_bits` field for the integer modules in `math` now
    have correct values.

## [0.2.0]

### Added

  * Run-time errors due to failed assertions now include a stack
    trace.

  * Generated OpenCL code now picks more sensible group size and count
    when running on a CPU.

  * `scatter` expressions nested in `map`s may now be parallelised
    ("segmented scatter").

  * Add `num_bits`/`get_bit`/`set_bit` functions to numeric module
    types, including a new `float` module type.

  * Size annotations may now refer to preceding parameters, e.g:

        let f (n: i32) (xs: [n]i32) = ...

  * `futhark-doc`: retain parameter names in generated docs.

  * `futhark-doc`: now takes `-v`/`--verbose` options.

  * `futhark-doc`: now generates valid HTML.

  * `futhark-doc`: now permits files to contain a leading documentation
    comment.

  * `futhark-py`/`futhark-pyopencl`: Better dynamic type checking in
    entry points.

  * Primitive functions (sqrt etc) can now be constant-folded.

  * Futlib: /futlib/vec2 added.

### Removed

  * The built-in `shape` function has been removed.  Use `length` or
    size parameters.

### Changed

  * The `from_i32`/`from_i64` functions of the `numeric` module type
    have been replaced with functions named `i32`/`i64`.  Similarly
    functions have been added for all the other primitive types
    (factored into a new `from_prim` module type).

  * The overloaded type conversion functions (`i32`, `f32`, `bool`,
    etc) have been removed.  Four functions have been introduced for
    the special cases of converting between `f32`/`f64` and `i32`:
    `r32`, `r64`, `t32`, `t64`.

  * Modules and variables now inhabit the same name space.  As a
    consequence, we now use `x.y` to access field `y` of record `x`.

  * Record expression syntax has been simplified.  Record
    concatenation and update is no longer directly supported.
    However, fields can now be implicitly defined: `{x,y}` now creates
    a record with field `x` and `y`, with values taken from the
    variables `x` and `y` in scope.

### Fixed

  * The `!=` operator now works properly on arrays (#426).

  * Allocations were sometimes hoisted incorrectly (#419).

  * `f32.e` is no longer pi.

  * Various other fixes.

## [0.1.0]

  (This is just a list of highlights of what was included in the first
   release.)

  * Code generators: Python and C, both with OpenCL.

  * Higher-order ML-style module system.

  * In-place updates.

  * Tooling: futhark-test, futhark-bench, futhark-dataset, futhark-doc.

  * Beginnings of a basis library, "futlib".
