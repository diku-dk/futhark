# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [0.26.0]

### Added

### Removed

### Changed

### Fixed

## [0.25.13]

### Added

* Incremental flattening of `map`-`scan` compositions with nested
  parallelism (similar to the logic for `map`-`reduce` compositions
  that we have had for years).

* `futhark script`, for running FutharkScript expressions from the
  command line.

* `futhark repl` now prints out a message when it ignores a breakpoint
  during initialisation. (#2098)

### Fixed

* Flattening of `scatter` with multi-dimensional elements (#2089).

* Some instances of not-actually-irregular allocations were mistakenly
  interpreted as irregular. Fixing this was a dividend of the memory
  representation simplifications of 0.25.12.

* Obscure issue related to expansion of shared memory allocations (#2092).

* A crash in alias checking under some rare circumstances (#2096).

* Mishandling of existential sizes for top level constants. (#2099)

* Compiler crash when generating code for copying nothing at all. (#2100)

## [0.25.12]

### Added

* `f16.copysign`, `f32.copysign`, `f64.copysign`.

* Trailing commas are now allowed for all syntactical elements that
  involve comma-separation. (#2068)

* The C API now allows destruction and construction of sum types (with
  some caveats). (#2074)

* An overall reduction in memory copies, through simplifying the
  internal representation.

### Fixed

* C API would define distinct entry point types for Futhark types that
  differed only in naming of sizes (#2080).

* `==` and `!=` on sum types with array payloads. Constructing them is
  now a bit slower, though. (#2081)

* Somewhat obscure simplification error caused by neglecting to update
  metadata when removing dead scatter outputs.

* Compiler crash due to the type checker forgetting to respect the
  explicitly ascribed non-consuming diet of loop parameters (#2067).

* Size inference did incomplete level/scope checking, which could
  result in circular sizes, which usually manifested as the type
  checker going into an infinite loop (#2073).

* The OpenCL backend now more gracefully handles lack of platform.

## [0.25.11]

### Added

* New prelude function: `manifest`.  For doing subtle things to memory.

* The GPU backends now handle up to 20 operators in a single fused
  reduction.

* CUDA/HIP terminology for GPU concepts (e.g. "thread block") is now
  used in all public interfaces. The OpenCL names are still supported
  for backwards compatibility.

* More fusion across array slicing.

### Fixed

* Compatibility with CUDA versions prior than 12.

## [0.25.10]

### Added

* Faster non-commutative reductions in the GPU backends. Work by
  Anders Holst and Christian Påbøl Jacobsen.

### Fixed

* Interpreter crash for certain complicated size expressions involving
  internal bindings (#2053).

* Incorrect type checking of `let` binding with explicit size
  quantification, where size appears in type of body (#2048).

* GPU code generation for non-commutative non-segmented reductions
  with array operands (#2051).

* Histogram with non-vectorised reduction operators (#2056). (But it
  is probably not a good idea to write such programs.)

* Futhark's LSP server should work better with Eglot.

* Incorrect copy removal inside histograms could cause compiler error
  (#2058).

* CUDA backend now correctly queries for available shared memory,
  which affects performance (hopefully positively).

* `futhark literate` now switches to the directory containing the
  `.fut` file before executing its contents. This fixes accessing
  files through relative paths.

## [0.25.9]

### Added

* The `cuda` and `hip` backends now generate faster code for `scan`s
  that have been fused with `map`s that internally produce arrays.
  Work by Anders Holst and Christian Påbøl Jacobsen.

* `f16.ldexp`, `f32.ldexp`, `f64.ldexp`, corresponding to the
  functions in the C math library.

### Fixed

* Incorrect data dependency information for `scatter` and `vjp` could
  cause invalid simplification.

* Barrier divergence in certain complicated kernels that contain both
  bounds checks and intragroup scans.

## [0.25.8]

### Added

* FutharkScript now has a `$loadbytes` builtin function for reading
  arbitrary bytes into Futhark programs.

* `futhark profile` can now process reports produced by the C API
  function `futhark_context_report()`.

* `futhark profile` now also produces a timeline of events.

### Fixed

* `futhark literate` no longer fails if the final line is a directive
  without a trailing newline.

* Parser now allows arbitrary patterns in function parameters and
  `let` bindings, although the type checker will reject any that are
  refutable (#2017).

* Avoid generating invalid code in cases where deduplicated sum types
  are exposed through entry points (#1960).

* A bug in data dependency analysis for histogram operations would
  mistakenly classify some loop parameters as redundant, leaving to
  code being removed.

## [0.25.7]

### Added

* `futhark autotune` now supports `hip` backend.

* Better parallelisation of `scatter` when the target is
  multidimensional (#2035).

### Fixed

* Very large `iota`s now work.

* Lambda lifting in `while` conditions (#2038).

* Size expressions in local function parameters had an interesting
  interaction with defunctionalisation (#2040).

* The `store` command in server executables did not properly
  synchronise when storing opaque values, which would lead to
  use-after-free errors.

## [0.25.6]

### Added

* The various C API functions that accept strings now perform a copy,
  meaning the caller does not have to keep the strings alive.

* Slightly better lexer error messages.

* Fusion across slicing is now possible in some cases.

* New tool: `futhark profile`.

### Fixed

* Inefficient locking for certain segmented histograms (#2024).

## [0.25.5]

### Added

* `futhark repl` now has a `:format` command.  Work by Dominic
  Kennedy.

### Fixed

* Textual floating-point numbers printed by executables now always
  print enough digits to not hide information.  Binary output is
  unchanged.

* Invalid CSE on constants could crash the compiler (#2021).

## [0.25.4]

### Fixed

* Invalid simplification (#2015).

* Rarely occurring deadlock for fused map-scan compositions in CUDA
  backend, when a bounds check failed in the map function.

* Compiler and interpreter crash for tricky interactions of abstract
  types and sizes (#2016).  Solved by banning such uses - in principle
  this could break code.

* Incomplete alias tracking could cause removal of necessary copies,
  leading to compiler crash (#2018).

## [0.25.3]

### Added

* pyopencl backend: compatibility with future versions of PyOpenCL.

* New backend: hip.

### Fixed

* Exotic problems related to intra-group reductions with array
  operands.  (Very rare in real code, although sometimes generated by
  AD.)

* Interpreter issue related to sizes in modules (#1992, #1993, #2002).

* Incorrect insertion of size arguments in in complex cases (#1998).

* Incorrect handling of `match` in lambda lifting (#2000).

* Regression in checking of consumption (#2007).

* Error in type checking of horisontally fused `scatter`s could crash
  the compiler (#2009).

* Size-polymorphic value bindings with existential sizes are now
  rejected by type checker (#1993).

* Single pass scans with complicated fused map functions were
  insufficiently memory-expanded (#2023).

* Invalid short circuiting (#2013).

## [0.25.2]

### Added

* Flattening/unflattening as the final operation in an entry point no
  longer forces a copy.

* The `opencl` backend no longer *always* fails on platforms that do
  not support 64-bit integer atomics, although it will still fail if
  the program needs them.

* Various performance improvements to the compiler itself;
  particularly the frontend.  It should be moderately faster.

### Fixed

* Code generation for `f16` literals in CUDA backend (#1979).

* Branches that return arrays differing in sign of their stride
  (#1984).

## [0.25.1]

### Added

* Arbitrary expressions of type `i64` are now allowed as sizes.  Work
  by Lubin Bailly.

* New prelude function `resize`.

### Removed

* The prelude functions `concat_to` and `flatten_to`.  They are often
  not necessary now, and otherwise `resize` is available.

### Changed

* The prelude functions `flatten` and `unflatten` (and their
  multidimensional variants), as well as `split`, now have more
  precise types.

* Local and anonymous (lambda) functions that *must* return unique
  results (because they are passed to a higher order function that
  requires this) must now have an explicit return type ascription that
  declares this, using `*`.  This is very rare (in practice
  unobserved) in real programs.

### Fixed

* `futhark doc` produced some invalid links.

* `flatten` did not properly check for claimed negative array sizes.

* Type checker crash on some ill-typed programs (#1926).

* Some soundness bugs in memory short circuiting (#1927, #1930).

* Another compiler crash in block tiling (#1933, #1940).

* Global arrays with size parameters no longer have aliases.

* `futhark eval` no longer crashes on ambiguously typed expressions (#1946).

* A code motion pass was ignorant of consumption constraints, leading
  to compiler crash (#1947).

* Type checker could get confused and think unknown sizes were
  available when they really weren't (#1950).

* Some index optimisations removed certificates (#1952).

* GPU backends can now transpose arrays whose size does not fit in a
  32-bit integer (#1953).

* Bug in alias checking for the core language type checker (#1949).
  Actually (finally) a proper fix of #803.

* Defunctionalisation duplicates less code (#1968).

## [0.24.3]

### Fixed

* Certain cases of noninlined functions in `multicore` backend.

* Defunctionalisation of `match` where the constructors carry
  functions (#1917).

* Shape coercions involving sum types (#1918).  This required
  tightening the rules a little bit, so some coercions involving
  anonymous sizes may now be rejected.  Add expected sizes as needed.

* Defunctionalisation somtimes forgot about sizes bound at top level
  (#1920).

## [0.24.2]

### Added

* `futhark literate` (and FutharkScript in general) is now able to do
  a bit of type-coercion of constants.

### Fixed

* Accumulators (produced by AD) had defective code generation for
  intra-group GPU kernel versions. (#1895)

* The header file generated by `--library` contained a prototype for
  an undefined function. (#1896)

* Crashing bug in LSP caused by stray `undefined` (#1907).

* Missing check for anonymous sizes in type abbreviations (#1903).

* Defunctionalisation crashed on projection of holes.

* Loop optimisation would sometimes remove holes.

* A potential barrier divergence for certain GPU kernels that fail
  bounds checking.

* A potential infinite loop when looking up aliases (#1915).

* `futhark literate`: less extraneous whitespace.

## [0.24.1]

### Added

* The manifest file now lists which tuning parameters are relevant for
  each entry point. (#1884)

* A command `tuning_params` has been added to the server protocol.

### Changed

* If part of a function parameter is marked as consuming ("unique"),
  the *entire* parameter is now marked as consuming.

### Fixed

* A somewhat obscure simplification rule could mess up use of memory.

* Corner case optimisation for mapping over `iota` (#1874).

* AD for certain combinations of `map` and indexing (#1878).

* Barrier divergence in generated code in some exotic cases (#1883).

* Handling of higher-order holes (#1885).

## [0.23.1]

### Added

* `f16.log1p`/`f32.log1p`/`f64.log1p` by nbos (#1820).

* Better syntax errors for invalid use of `!`.

* `futhark literate` now supports a `$loadaudio` builtin function for loading
  audio to Futhark programs (#1829).

* You can now mix consumption and higher-order terms in slightly more
  cases (#1836).

* `futhark pkg` now invokes Git directly rather than scraping
  GitHub/GitLab.  This means package paths can now refer to any Git
  repository, as long as `git clone` works.  In particular, you can
  use private and self-hosted repositories.

* Significant reduction in compilation time by doing internal sanity
  checks in separate thread.

* New command: `futhark eval`. Evaluates Futhark expressions
  provided as command line arguments, optionally allowing a file
  import (#1408).

* `script input` now allows the use of `$loaddata`.

* Datasets used in `futhark test` and `futhark bench` can now be named
  (#1859).

* New command `futhark benchcmp` by William Due.

### Changed

* The C API function `futhark_context_new_with_command_queue()` for
  the OpenCL backend has been replaced with a configuration setting
  `futhark_context_config_set_command_queue()`.

### Fixed

* Minor parser regression that mostly affects the REPL (#1822).

* Parser did not recognise custom infix operators that did not have a
  builtin operator as prefix (#1824).

* GPU backends: expansion of irregular nested allocations involving
  consumption (#1837, #1838).

* CLI executables now handle entry points with names that are not
  valid C identifiers (#1841).

* Various oversights in the type checking of uniqueness annotations
  for higher-order functions (#1842).

* Invalid short-circuiting could cause compiler crashes (#1843).

* Defunctionalisation could mess up sum types, leading to invalid code
  generation by internalisation, leading to a compiler crash (#1847).

* The `#[break]` attribute now provides the right environment to
  `futhark repl`, allowing local variables to be inspected.

* Simplification of concatenations (#1851).

* Array payloads in sum types no longer need parens (#1853).

* When a file is loaded with `futhark repl`, `local` declarations are
  now available.

* Missing alias propagation when pattern matching incompletely known
  sum types (#1855).

* `reduce_by_index` and `hist` were in some cases unable to handle
  input sizes that do not fit in a 32-bit integer.

* A fusion bug related to fusing across transpositions could result in
  a compiler crash (#1858).

## [0.22.7]

### Added

* `futhark literate` now supports an `:audio` directive for producing audio
  files from arrays of `i8` (#1810).

* `futhark multicore` now parallelises copies (#1799).

* `futhark multicore` now uses an allocator that better handles large
  allocations (#1768).

### Fixed

* Some record field names could cause generation of invalid C API names (#1806).

* Memory block merging was extremely and unnecessarily slow for
  programs with many entry points.

* Simplification mistake could lead to out-of-bounds reads (#1808).

* `futhark lsp` now handles some bookkeeping messages sent by Eglot.

* Parser can now handle arbitrarily complex chaining of indexing and
  projection.

* Detect and complain about source files without .fut extension (#1813).

* Overly conservative checking of whether a function parameter is
  allowed by be consumed - it was disallowed if it contained any
  scalars (#1816).

## [0.22.6]

### Added

* Slightly better type errors for sum types (#1792).

* Better tracing output in interpreter (#1795).

* Improved optimisation of code that uses zero-element arrays
  (sometimes used for type witnesses).

### Fixed

* Mishandling of bounds checks in parallel backends could cause
  compiler crashes (#1791).

* Mis-simplification of certain sequentialised scatters into
  single-element arrays (#1793).

* Invalid `scatter` fusion would cause an internal compiler error
  (#1794).

* The code generator flipped the order of `match` cases.

* Simpification of concatenations (#1796).

* Defunctionalisation error for fancy size programming (#1798).

* Code generation for load-unbalanced reductions in multicore backend
  (#1800).

* Futhark now works on CUDA 12 (#1801).

* `mul_hi` and `mad_hi` for signed integer types now actually do
  signed multiplication (previously it was always unsigned).

## [0.22.5]

### Added

* Memory short circuiting now also applied to the `multicore` backend.

* Fixes for AD of `scan` with nonscalar operators.  Work by Lotte
  Bruun and Ulrik Larsen.

* Generalised histograms now supported in AD.  Work by Lotte Bruun and
  Ulrik Larsen.

* OpenCL kernels now embed group size information, which can
  potentially be used for better register allocation by the OpenCL
  implementation.

### Fixed

* A hole in the type checker that allowed one to sneak functions out
  of conditionals (#1787).

* `futhark repl`: unsigned integers are now printed correctly.

* A bug in the type-checking of `match` (#1783).

* Missing sequencing in type-checking of in-place `let` expressions (#1786).

* Crash in defunctionaliser caused by duplicate parameter names
  (#1780).

* Infelicities in multicore codegen for scans (#1777).

## [0.22.4]

### Added

* Memory short circuiting, a major new optimisation by Philip
  Munksgaard that avoids copies by constructing values in-place.

### Fixed

* `assert` was sometimes optimised away by CSE.

* `futhark literate` now handles type abbreviations in entry points
  (#1750).

* Handling of non-inlined functions in GPU code.  Still very restricted.

* Uniqueness checking bug (#1751).

* Simplification bug (#1753).

* A bug related to optimisation of scalar code migrated to GPU.

* Memory optimisation bug for top-level constants (#1755).

* Handling of holes in defunctionalisation (again).

* A few cases where optimisation (safely but perhaps confusingly)
  removed bounds checks (#1758).

* Futhark now works on Windows again (#1734).  This support remains
  very flaky and not well tested.

* Type inference of field projection (#1762).

## [0.22.3]

### Fixed

* Non-server executables neglected to synchronise before printing results (#1731).

* Fixed handling of holes in defunctionalisation (#1738).

* Ascription of higher-order modules (#1741).

* Fixed compiler crash when attempting to tile irregular parallelism (#1739).

* Incorrect regularity checking in loop interchange (#1744).

* Pattern match exhaustiveness of bools (#1748).

* Improper consumption checking of nonlocal bindings (#1749).

## [0.22.2]

### Added

* `futhark repl` is now delightfully more colourful.

* `futhark repl` no longer prints scalar types with type suffixes (#1724).

* `futhark pyopencl` executables now accept ``--build-option``.

* New functions: `f16.nextafter`, `f32.nextafter`, `f64.nextafter`,
  matching the ones in the C math library.

* `futhark literate` now prints directives in the output exactly as
  they appear in the source.

### Fixed

* Diagnostics will no longer contain control codes when output is a
  file.

* CLI executables now fail when printing an opaque instead of
  producing garbage.  This improves handling of some incorrect uses of
  `auto output` (#1251).

## [0.22.1]

### Removed

* Removed prelude functions `reduce_stream`, `map_stream`,
  `reduce_stream_per`, and `reduce_map_per`.

### Fixed

* Various fixes to scalar migration (#1721).

## [0.21.15]

### Fixed

* Corrupted OOM error messages.

* Excessive internal fragmentation for some programs (#1715).

## [0.21.14]

### Fixed

* `replicate` was broken for sizes that didn't fit in `i32`.

* Transposition of empty arrays in interpreter (#1700).

* Exotic simplification error (#1309).

* Rare race condition could lead to leaking of error message memory in
  `multicore` and `ispc` backends (#1709).

* Overzealous aliasing for built-in overloaded types (#1711).

## [0.21.13]

### Added

* New fusion engine by Amar Topalovic and Walter Restelli-Nielsen.
  Fuses more, which is good for some programs and bad for others.
  Most programs see no change.  This is mainly a robust foundation for
  further improvements.

* New experimental backend: `ispc`.  By Louis Marott Normann,
  Kristoffer August Kortbæk, William Pema Norbu Holmes Malling, and
  Oliver Bak Kjersgaard Petersen.

* New prelude functions: `hist`, `spread`.  These function as
  non-consuming variants of `reduce_by_index` and `scatter`.

* Using `==` to compare arrays is now deprecated.

* New command: `futhark tokens`.  You probably don't care about this one.

* In the C API, opaque types that correspond to tuples or records can
  now have their elements inspected and be created from elements
  (#1568).

* New server protocol commands: `types`, `fields`, `entry_points`.

* Tuples and records can now be passed from FutharkScript to Futhark
  entry points (#1684).

### Fixed

* Sometimes missing cases in `match` expressions were not detected.

* A defective simplification rule could in very rare cases lead to
  infinite recursion in the compiler (#1685).

* Some broken links in `futhark doc` output (#1686).

* Incorrect checking of whether a function parameter is consumable
  based on its type (#1687).

* Missing aliases for functions that return multiple aliased values
  (#1690).

* `new`/`values` functions for GPU backends are now properly
  asynchronous (#1664).  This may uncover bugs in application code.

## [0.21.12]

### Added

* Somewhat simplified the handling of "uniqueness types" (which is a
  term we are moving away from).  You should never see `*` in
  non-function types, and they are better thought of as effect
  indicators.

* `futhark literate`: prints tracing output (and other logging
  messages that may occur) when run with `-v` (#1678).

* Entry points can now be any valid Futhark identifier.

### Fixed

* `futhark test -C` was broken.

* `futhark_context_free()` for the GPU backends neglected to free some
  memory used for internal bookkeeping, which could lead to memory
  leaks for processes that repeatedly create and destroy contexts
  (#1676).

* FutharkScript now allows `'` in names.

* `futhark lsp` now handles warnings in programs that also have errors.

## [0.21.11]

### Added

* The CUDA backend now supports compute capability 8.6 and 8.7.

* Philip Børgesen has implemented a new optimisation for GPU backends
  that migrates scalar work to the GPU, in order to reduce
  synchronisation.  This results in major speedup for some programs.

* String literals are now allowed in `input` blocks.

* Experimental and undocumented support for automatic differentiation,
  available on the secret menu.

* Assertions and attributes are now ignored when used as size
  expressions.  E.g. `iota (assert p n) 0` now has size `n`.

* `futhark test` only runs the interpreter if passed `-i`.

* `futhark literate` now shows progress bars when run with `-v`.

### Fixed

* `futhark lsp` is now better at handling multiple files (#1647).

* Incorrect handling of local quantification when determining type
  equivalence in during module type ascription (#1648).

* Incorrect checking of liftedness when instantiating polymorphic
  functions during module type ascription.

* Tightened some restrictions on the use of existential sizes that
  could otherwise lead to compiler crashes (#1650).  This restriction
  is perhaps a bit *too* might and it may be possible to loosen it in
  the future.

* Another defunctorisation bug (#1653).  Somehow we find these every
  time Martin Elsman writes a nontrivial Futhark program.

* `futhark bench`: convergence phase now does at least `--runs` runs.

* Errors and warnings no longer awkwardly mixed together in console output.

* Slightly better type errors for ambiguous sizes (#1661).

* Better type errors for module ascription involving nested modules
  (#1660).

* `futhark doc`: some formatting bugs.

* `futhark doc` didn't notice all `local` module types (#1666).

* Missing consumption check in optimisation could lead to ICE (#1669).

## [0.21.10]

### Added

* New math functions: `f16.erf`, `f32.erf`, `f64.erf`.

* New math functions: `f16.erfc`, `f32.erfc`, `f64.erfc`.

* New math functions: `f16.cbrt`, `f32.cbrt`, `f64.cbrt`.

### Fixed

* Variables being indexed now have correct source spans in AST.

* `futhark lsp`s hover information now contains proper range information.

* `futhark query` and `futhark lsp` incorrectly thought size
  parameters had type `i32`.

* `futhark doc` put documentation for prelude modules in the wrong
  location (which also led to messed-up style sheets).

## [0.21.9]

### Added

* Sun Haoran has implemented unnamed typed holes, with syntax `???`.

* Sun Haoran has implemented the beginnings of a language server:
  `futhark lsp`.  A VSCode language extension is available on the
  marketplace, but the language server should work with any editor.

* Crucial new command: `futhark thanks`.

* The GPU backends now support a caching mechanism for JIT-compiled
  code, significantly improving startup times.  Use the
  `futhark_context_config_set_cache_file()` in the C API, the
  `--cache-file` option on executables, or the `--cache-extension`
  option on `futhark test` and `futhark bench`.  These also work for
  the non-GPU backends, but currently have no effect.  (#1614)

* Aleksander Junge has improved `futhark bench` such that it
  intelligently chooses how many runs to perform (#1335).

### Fixed

* Incomplete simplification would cause some instances of nested
  parallelism to require irregular allocations (#1610).

* Missing alias checking for a simplification rule related to in-place
  updates (#1615, #1628).

* Incorrect code generation for certain copies of transposed arrays
  (#1627).

* Fusion would mistakenly try to treat some loops with irregular sizes
  (#1631).

* Memory annotation bug for non-inlined functions (#1634).

## [0.21.8]

### Added

* Slightly better parse errors (again).

* `futhark literate` now supports a `file:` option in `:img` and
  `:video` directives (#1491).

### Fixed

* Improved hoisting of size computations.  This could cause some
  regular nested parallel programs to run into compiler limitations,
  as if they were irregular.

* Rare code generation bug for histograms (#1609).

## [0.21.7]

### Added

* `futhark check-syntax`: check syntactic validity of a program
  file, without type-checking.

* Parsing multi-file programs is now parallelised, making it
  *slightly* faster.

* Reloading a large program in `futhark repl` is now faster, as long
  as not too many of its files have been modified (#1597).

### Fixed

* Mistake in occurs check could cause infinite loop in type checker
  for programs with type errors (#1599).

## [0.21.6]

### Added

* `futhark bench` now explicitly notes when a tuning file is not
  present.

* `futhark bench`, `futhark test` and friends are now better at
  handling fatally terminating programs (e.g. segmentation faults).

* Generated C code is now a lot smaller for large programs, as error
  recovery has been more centralised (#1584).

### Fixed

* Some bugs in checking for local memory capacity for particularly
  exotic generated code.

* Insufficient hoisting of allocation sizes led to problems with
  memory expansion in rare cases (#1579).

* Conversion of floating-point NaNs and infinities to integers now
  well defined (produces zero) (#1586).

* Better handling of OOM for certain short-lived CPU-side allocations (#1585).

## [0.21.5]

### Added

* API functions now return more precise error codes in some cases.

* Out-of-memory errors contain more information.

### Fixed

* Yet another in-place lowering issue (#1569).

* Removed unnecessary bounds checks in register tiling, giving about
  1.8x speedup on e.g. matrix multiplication on newer NVIDIA GPUs.

* A parser bug erroneously demanded whitespace in some type
  expressions (#1573).

* Some memory was not being freed correctly when shutting down OpenCL
  and CUDA contexts, which could lead to memory leaks in processes
  that created and freed many contexts.

* An incorrect copy-removal in some exotic cases (#1572).

* 'restore'-functions might perform undefined pointer arithmetic when
  passed garbage.

## [0.21.4]

### Fixed

* A size inference bug in type checking of `loop`s (#1565).

* Exotic flattening bug (#1563).

* Segmented `reduce_by_index` with fairly small histogram size would
  use vastly more memory than needed.

## [0.21.3]

### Added

* Parse errors now list possible expected tokens.

* Lexer errors now mention the file.

### Fixed

* Overloaded number literals cannot be sum types (#1557).

* Defective GPU code generation for vectorised non-commutative
  operatators (#1559).

* Excessive memory usage for some programs (#1325).

## [0.21.2]

### Added

* New functions: `reduce_by_index_2d`, `reduce_by_index_3d`.

* Manifests now contain compiler version information.

### Fixed

* Allocation insertion pass bug (#1546).

* An exotic bug involving TLS and dynamically loading code generated
  by the `multicore` backend.

* Unconstrained ambiguous types now default to `()` (#1552).  This
  should essentially never have any observable impact, except that
  more programs will type check.

* Double buffering compiler crash (#1553).

## [0.21.1]

### Added

* Top-level value definitions can (and should) now be declared with
  with `def`, although `let` still works.

* New tool: `futhark defs`, for printing locations of top-level
  definitions.

### Changed

* `def` is now a reserved word.

### Fixed

* Contrived intra-group code versions with no actual parallelism would
  be given a group size of zero (#1524).

## [0.20.8]

### Added

* `futhark repl` now allows Ctrl-c to interrupt execution.

### Fixed

* Alias tracking of sum types.

* Proper checking that a function declared to return a unique-typed
  value actually does so.

* Faulty uniqueness checking and inference for lambdas (#1535).

* Monomorphisation would duplicate functions under rare circumstances
  (#1537).

* Interpreter didn't check that the arguments passed to `unflatten`
  made sense (#1539).

* `futhark literate` now supports a `$loaddata` builtin function for
  passing datasets to Futhark programs.

## [0.20.7]

### Added

* Better exploitation of parallelism in fused nested segmented
  reductions.

* Prelude function `not` for negating booleans.

### Fixed

* Some incorrect removal of copies (#1505).

* Handling of parametric modules with top-level existentials (#1510).

* Module substitution fixes (#1512, #1518).

* Invalid in-place lowering (#1523).

* Incorrect code generation for some intra-group parallel code versions.

* Flattening crash in the presence of irregular parallelism (#1525).

* Incorrect substitution of type abbreviations with hidden sizes (#1531).

* Proper handling of NaN in `min`/`max` functions for
  `f16`/`f32`/`f64` in interpreter (#1528).

## [0.20.6]

### Added

* Much better code generation for segmented scans with vectorisable
  operators.

### Fixed

* Fixes to extremely exotic GPU scans involving array operators.

* Missing alias tracking led to invalid rewrites, causing a compiler
  crash (#1499).

* Top-level bindings with existential sizes were mishandled (#1500, #1501).

* A variety of memory leaks in the multicore backend, mostly (or
  perhaps exclusively) centered around context freeing or failing
  programs - this should not have affected many people.

* Various fixes to `f16` handling in the GPU backends.

## [0.20.5]

### Added

* Existential sizes can now be explicitly quantified in type
  expressions (#1308).

* Significantly expanded error index.

* Attributes can now be numeric.

* Patterns can now have attributes.  None have any effect at the
  moment.

* `futhark autotune` and `futhark bench` now take a `--spec-file`
  option for loading a test specification from another file.

### Fixed

* `auto output` reference datasets are now recreated when the program
  is newer than the data files.

* Exotic hoisting bug (#1490).

## [0.20.4]

### Added

* Tuning parameters now (officially) exposed in the C API.

* `futhark autotune` is now 2-3x faster on many programs, as it now
  keeps the process running.

* Negative numeric literals are now allowed in `case` patterns.

### Fixed

* `futhark_context_config_set_profiling` was missing for the `c` backend.

* Correct handling of nested entry points (#1478).

* Incorrect type information recorded when doing in-place lowering (#1481).

## [0.20.3]

### Added

* Executables produced by C backends now take a `--no-print-result` option.

* The C backends now generate a manifest when compiling with
  `--library`.  This can be used by FFI generators (#1465).

* The beginnings of a Rust-style error index.

* `scan` on newer CUDA devices is now much faster.

### Fixed

* Unique opaque types are named properly in entry points.

* The CUDA backend in library mode no longer `exit()`s the process if
  NVRTC initialisation fails.

## [0.20.2]

### Fixed

* Simplification bug (#1455).

* In-place-lowering bug (#1457).

* Another in-place-lowering bug (#1460).

* Don't try to tile inside loops with parameters with variant sizes (#1462).

* Don't consider it an ICE when the user passes invalid command line
  options (#1464).

## [0.20.1]

### Added

  * The `#[trace]` and `#[break]` attributes now replace the `trace`
    and `break` functions (although they are still present in
    slightly-reduced but compatible form).

  * The `#[opaque]` attribute replaces the `opaque` function, which is
    now deprecated.

  * Tracing now works in compiled code, albeit with several caveats
    (mainly, it does not work for code running on the GPU).

  * New `wasm` and `wasm-multicore` backends by Philip Lassen.  Still
    very experimental; do not expect API stability.

  * New intrinsic type `f16`, along with a prelude module `f16`.
    Implemented with hardware support where it is available, and with
    `f32`-based emulation where it is not.

  * Sometimes slightly more informative error message when input of
    the wrong type is passed to a test program.

### Changed

  * The `!` function in the integer modules is now called `not`.

  * `!` is now builtin syntax.  You can no longer define a function
    called `!`.  It is extremely unlikely this affects you.  This
    removes the last special-casing of prefix operators.

  * A prefix operator section (i.e. `(!)`) is no longer permitted
    (and it never was according to the grammar).

  * The offset parameter for the "raw" array creation functions in the
    C API is now `int64_t` instead of `int`.

### Fixed

  * `i64.abs` was wrong for arguments that did not fit in an `i32`.

  * Some `f32` operations (`**`, `abs`, `max`) would be done in double
    precision on the CUDA backend.

  * Yet another defunctorisation bug (#1397).

  * The `clz` function would sometimes exhibit undefined behaviour in
    CPU code (#1415).

  * Operator priority of prefix `-` was wrong - it is now the same as
    `!` (#1419).

  * `futhark hash` is now invariant to source location as well as
    stable across OS/compiler/library versions.

  * `futhark literate` is now much better at avoiding unnecessary
    recalculation.

  * Fixed a hole in size type checking that would usually lead to
    compiler crashes (#1435).

  * Underscores now allowed in numeric literals in test data (#1440).

  * The `cuda` backend did not use single-pass segmented scans as
    intended.  Now it does.

## [0.19.7]

### Added

  * A new memory reuse optimisation has been added.  This results in
    slightly lower footprint for many programs.

  * The `cuda` backend now uses a fast single-pass implementation for
    segmented `scan`s, due to Morten Tychsen Clausen (#1375).

  * `futhark bench` now prints interim results while it is running.

### Fixed

  * `futhark test` now provides better error message when asked to
    test an undefined entry point (#1367).

  * `futhark pkg` now detects some nonsensical package paths (#1364).

  * FutharkScript now parses `f x y` as applying `f` to `x` and `y`,
    rather than as `f (x y)`.

  * Some internal array utility functions would not be generated if
    entry points exposed both unit arrays and boolean arrays (#1374).

  * Nested reductions used (much) more memory for intermediate results
    than strictly needed.

  * Size propagation bug in defunctionalisation (#1384).

  * In the C FFI, array types used only internally to implement opaque
    types are no longer exposed (#1387).

  * `futhark bench` now copes with test programs that consume their
    input (#1386).  This required an extension of the server protocol
    as well.

## [0.19.6]

### Added

  * `f32.hypot` and `f64.hypot` are now much more numerically exact in
    the interpreter.

  * Generated code now contains a header with information about the
    version of Futhark used (and maybe more information in the
    future).

  * Testing/benchmarking with large input data (including randomly
    generated data) is much faster, as each file is now only read
    once.

  * Test programs may now use arbitrary FutharkScript expressions to
    produce test input, in particular expressions that produce opaque
    values.  This affects both testing, benchmarking, and autotuning.

  * Compilation is about 10% faster, especially for large programs.

### Fixed

  * `futhark repl` had trouble with declarations that produced unknown
    sizes (#1347).

  * Entry points can now have same name as (undocumented!) compiler intrinsics.

  * FutharkScript now detects too many arguments passed to functions.

  * Sequentialisation bug (#1350).

  * Missing causality check for index sections.

  * `futhark test` now reports mismatches using proper indexes (#1356).

  * Missing alias checking in fusion could lead to compiler crash (#1358).

  * The absolute value of NaN is no longer infinity in the interpreter (#1359).

  * Proper detection of zero strides in compiler (#1360).

  * Invalid memory accesses related to internal bookkeeping of bounds checking.

## [0.19.5]

### Added

  * Initial work on granting programmers more control over existential
    sizes, starting with making type abbreviations function as
    existential quantifiers (#1301).

  * FutharkScript now also supports arrays and scientific notation.

  * Added `f32.epsilon` and `f64.epsilon` for the difference between
    1.0 and the next larger representable number.

  * Added `f32.hypot` and `f64.hypot` for your hypothenuse needs (#1344).

  * Local size bindings in `let` expressions, e.g:

    ```
    let [n] (xs': [n]i32) = filter (>0) xs
    in ...
    ```

### Fixed

  * `futhark_context_report()` now internally calls
    `futhark_context_sync()` before collecting profiling information
    (if applicable).

  * `futhark literate`: Parse errors for expression directives now
    detected properly.

  * `futhark autotune` now works with the `cuda` backend (#1312).

  * Devious fusion bug (#1322) causing compiler crashes.

  * Memory expansion bug for certain complex GPU kernels (#1328).

  * Complex expressions in index sections (#1332).

  * Handling of sizes in abstract types in the interpreter (#1333).

  * Type checking of explicit size requirements in `loop` parameter (#1324).

  * Various alias checking bugs (#1300, #1340).

## [0.19.4]

### Fixed

  * Some uniqueness ignorance in fusion (#1291).

  * An invalid transformation could in rare cases cause race
    conditions (#1292).

  * Generated Python and C code should now be warning-free.

  * Missing check for uses of size-lifted types (#1294).

  * Error in simplification of concatenations could cause compiler
    crashes (#1296).

## [0.19.3]

### Added

  * Better `futhark test`/`futhark bench` errors when test data does
    not have the expected type.

### Fixed

  * Mismatch between how thresholds were printed and what the
    autotuner was looking for (#1269).

  * `zip` now produces unique arrays (#1271).

  * `futhark literate` no longer chokes on lines beginning with `--`
    without a following whitespace.

  * `futhark literate`: `:loadimg` was broken due to overzealous
    type checking (#1276).

  * `futhark literate`: `:loadimg` now handles relative paths properly.

  * `futhark hash` no longer considers the built-in prelude.

  * Server executables had broken store/restore commands for opaque types.

## [0.19.2]

### Added

  * New subcommand: `futhark hash`.

  * `futhark literate` is now smart about when to regenerate image and
    animation files.

  * `futhark literate` now produces better error messages passing
    expressions of the wrong type to directives.

### Fixed

  * Type-checking of higher-order functions that take consuming
    funtional arguments.

  * Missing cases in causality checking (#1263).

  * `f32.sgn` was mistakenly defined with double precision arithmetic.

  * Only include double-precision atomics if actually needed by
    program (this avoids problems on devices that only support single
    precision).

  * A lambda lifting bug due to not handling existential sizes
    produced by loops correctly (#1267).

  * Incorrect uniqueness attributes inserted by lambda lifting
    (#1268).

  * FutharkScript record expressions were a bit too sensitive to
    whitespace.

## [0.19.1]

### Added

  * `futhark literate` now supports a `$loadimg` builtin function for
    passing images to Futhark programs.

  * The `futhark literate` directive for generating videos is now
    `:video`.

  * Support for 64-bit atomics on CUDA and OpenCL for higher
    performance with `reduce_by_index` in particular.
    Double-precision float atomics are used on CUDA.

  * New functions: `f32.recip` and `f64.recip` for multiplicative inverses.

  * Executables produced with the `c` and `multicore` backends now
    also accept `--tuning` and `--size` options (although there are
    not yet any tunable sizes).

  * New functions: `scatter_2d` and `scatter_3d` for scattering to
    multi-dimensional arrays (#1258).

### Removed

  * The math modules no longer define the name `negate` (use `neg`
    instead).

### Fixed

  * Exotic core language alias tracking bug (#1239).

  * Issue with entry points returning constant arrays (#1240).

  * Overzealous CSE collided with uniqueness types (#1241).

  * Defunctionalisation issue (#1242).

  * Tiling inside multiply nested loops (#1243).

  * Substitution bug in interpreter (#1250).

  * `f32.sgn`/`f64.sgn` now correct for NaN arguments.

  * CPU backends (`c`/`multicore`) are now more careful about staying
    in single precision for `f32` functions (#1253).

  * `futhark test` and `futhark bench` now detect program
    initialisation errors in a saner way (#1246).

  * Partial application of operators with parameters used in a
    size-dependent way now works (#1256).

  * An issue regarding abstract size-lifted sum types (#1260).

## [0.18.6]

### Added

  * The C API now exposes serialisation functions for opaque values.

  * The C API now lets you pick which stream (if any) is used for
    logging prints (#1214).

  * New compilation mode: `--server`.  For now used to support faster
    benchmarking and testing tools, but can be used to build even
    fancier things in the future (#1179).

  * Significantly faster reading/writing of large values.  This mainly
    means that validation of test and benchmark results is much faster
    (close to an order of magnitude).

  * The experimental `futhark literate` command allows vaguely a
    notebook-like programming experience.

  * All compilers now accept an `--entry` option for treating more
    functions as entry points.

  * The `negate` function is now `neg`, but `negate` is kept around
    for a short while for backwards compatibility.

  * Generated header-files are now declared `extern "C"` when
    processed with a C++ compiler.

  * Parser errors in test blocks used by `futhark bench` and `futhark
    test` are now reported with much better error messages.

### Fixed

  * Interaction between slice simplification and in-place updates
    (#1222).

  * Problem with user-defined functions with the same name as intrinsics.

  * Names from transitive imports no longer leak into scope (#1231).

  * Pattern-matching unit values now works (#1232).

## [0.18.5]

### Fixed

  * Fix tiling crash (#1203).

  * `futhark run` now does slightly more type-checking of its inputs
    (#1208).

  * Sum type deduplication issue (#1209).

  * Missing parentheses when printing sum values in interpreter.

## [0.18.4]

### Added

  * When compiling to binaries in the C-based backends, the compiler
    now respects the ``CFLAGS`` and ``CC`` environment variables.

  * GPU backends: avoid some bounds-checks for parallel sections
    inside intra-kernel loops.

  * The `cuda` backend now uses a much faster single-pass `scan`
    implementation, although only for nonsegmented scans where the
    operator operates on scalars.

### Fixed

  * `futhark dataset` now correctly detects trailing commas in textual
    input (#1189).

  * Fixed local memory capacity check for intra-group-parallel GPU kernels.

  * Fixed compiler bug on segmented rotates where the rotation amount
    is variant to the nest (#1192).

  * `futhark repl` no longer crashes on type errors in given file (#1193).

  * Fixed a simplification error for certain arithmetic expressions
    (#1194).

  * Fixed a small uniqueness-related bug in the compilation of
    operator section.

  * Sizes of opaque entry point arguments are now properly checked
    (related to #1198).

## [0.18.3]

### Fixed

  * Python backend now disables spurious NumPy overflow warnings for
    both library and binary code (#1180).

  * Undid deadlocking over-synchronisation for freeing opaque objects.

  * `futhark datacmp` now handles bad input files better (#1181).

## [0.18.2]

### Added

  * The GPU loop tiler can now handle loops where only a subset of the
    input arrays are tiled.  Matrix-vector multiplication is one
    important program where this helps (#1145).

  * The number of threads used by the `multicore` backend is now
    configurable (`--num-threads` and
    `futhark_context_config_set_num_threads()`). (#1162)

### Fixed

  * PyOpenCL backend would mistakenly still streat entry point
    argument sizes as 32 bit.

  * Warnings are now reported even for programs with type errors.

  * Multicore backend now works properly for very large iteration
    spaces.

  * A few internal generated functions (`init_constants()`,
    `free_constants()`) were mistakenly declared non-static.

  * Process exit code is now nonzero when compiler bugs and
    limitations are encountered.

  * Multicore backend crashed on `reduce_by_index` with nonempty target
    and empty input.

  * Fixed a flattening issue for certain complex `map` nestings
    (#1168).

  * Made API function `futhark_context_clear_caches()` thread safe
    (#1169).

  * API functions for freeing opaque objects are now thread-safe
    (#1169).

  * Tools such as `futhark dataset` no longer crash with an internal
    error if writing to a broken pipe (but they will return a nonzero
    exit code).

  * Defunctionalisation had a name shadowing issue that would crop up
    for programs making very advanced use of functional
    representations (#1174).

  * Type checker erroneously permitted pattern-matching on string
    literals (this would fail later in the compiler).

  * New coverage checker for pattern matching, which is more correct.
    However, it may not provide quite as nice counter-examples
    (#1134).

  * Fix rare internalisation error (#1177).

## [0.18.1]

### Added

  * Experimental multi-threaded CPU backend, `multicore`.

### Changed

  * All sizes are now of type `i64`.  This has wide-ranging
    implications and most programs will need to be updated (#134).

## [0.17.3]

### Added

  * Improved parallelisation of `futhark bench` compilation.

### Fixed

  * Dataset generation for test programs now use the right `futhark`
    executable (#1133).

  * Really fix NaN comparisons in interpreter (#1070, again).

  * Fix entry points with a parameter that is a sum type where
    multiple constructors contain arrays of the same statically known
    size.

  * Fix in monomorphisation of types with constant sizes.

  * Fix in in-place lowering (#1142).

  * Fix tiling inside multiple nested loops (#1143).

## [0.17.2]

### Added

  * Obscure loop optimisation (#1110).

  * Faster matrix transposition in C backend.

  * Library code generated with CUDA backend can now be called from
    multiple threads.

  * Better optimisation of concatenations of array literals and
    replicates.

  * Array creation C API functions now accept `const` pointers.

  * Arrays can now be indexed (but not sliced) with any signed integer
    type (#1122).

  * Added --list-devices command to OpenCL binaries (#1131)

  * Added --help command to C, CUDA and OpenCL binaries (#1131)

### Removed

  * The integer modules no longer contain `iota` and `replicate`
    functions.  The top-level ones still exist.

  * The `size` module type has been removed from the prelude.

### Changed

  * Range literals may no longer be produced from unsigned integers.

### Fixed

  * Entry points with names that are not valid C (or Python)
    identifiers are now pointed out as problematic, rather than
    generating invalid C code.

  * Exotic tiling bug (#1112).

  * Missing synchronisation for in-place updates at group level.

  * Fixed (in a hacky way) an issue where `reduce_by_index` would use
    too much local memory on AMD GPUs when using the OpenCL backend.

## [0.16.4]

### Added

  * `#[unroll]` attribute.

  * Better error message when writing `a[i][j]` (#1095).

  * Better error message when missing "in" (#1091).

### Fixed

  * Fixed compiler crash on certain patterns of nested parallelism
    (#1068, #1069).

  * NaN comparisons are now done properly in interpreter (#1070).

  * Fix incorrect movement of array indexing into branches `if`s
    (#1073).

  * Fix defunctorisation bug (#1088).

  * Fix issue where loop tiling might generate out-of-bounds reads
    (#1094).

  * Scans of empty arrays no longer result in out-of-bounds memory
    reads.

  * Fix yet another defunctionalisation bug due to missing
    eta-expansion (#1100).

## [0.16.3]

### Added

  * `random` input blocks for `futhark test` and `futhark bench` now
    support floating-point literals, which must always have either an
    `f32` or `f64` suffix.

  * The `cuda` backend now supports the `-d` option for executables.

  * The integer modules now contain a `ctz` function for counting
    trailing zeroes.

### Fixed

  * The `pyopencl` backend now works with OpenCL devices that have
    multiple types (most importantly, oclgrind).

  * Fix barrier divergence when generating code for group-level
    colletive copies in GPU backend.

  * Intra-group flattening now looks properly inside of branches.

  * Intra-group flattened code versions are no longer used when the
    resulting workgroups would have less than 32 threads (with default
    thresholds anyway) (#1064).

## [0.16.2]

### Added

  * `futhark autotune`: added `--pass-option`.

### Fixed

  * `futhark bench`: progress bar now correct when number of runs is
    less than 10 (#1050).

  * Aliases of arguments passed for consuming parameters are now
    properly checked (#1053).

  * When using a GPU backend, errors are now properly cleared.
    Previously, once e.g. an out-of-bounds error had occurred, all
    future operations would fail with the same error.

  * Size-coercing a transposed array no longer leads to invalid code
    generation (#1054).

## [0.16.1]

### Added

  * Incremental flattening is now performed by default.  Use
    attributes to constrain and direct the flattening if you have
    exotic needs.  This will likely need further iteration and
    refinement.

  * Better code generation for `reverse` (and the equivalent explicit
    slice).

  * `futhark bench` now prints progress bars.

  * The `cuda` backend now supports similar profiling as the `opencl`
    option, although it is likely slightly less accurate in the
    presence of concurrent operations.

  * A preprocessor macro `FUTHARK_BACKEND_foo` is now defined in
    generated header files, where *foo* is the name of the backend
    used.

  * Non-inlined functions (via `#[noinline]`) are now supported in GPU
    code, but only for functions that *exclusively* operate on
    scalars.

  * `futhark repl` now accepts a command line argument to load a
    program initially.

  * Attributes are now also permitted on declarations and specs.

  * `futhark repl` now has a `:nanbreak` command (#839).

### Removed

  * The C# backend has been removed (#984).

  * The `unsafe` keyword has been removed.  Use `#[unsafe]` instead.

### Changed

  * Out-of-bounds literals are now an error rather than a warning.

  * Type ascriptions on entry points now always result in opaque types
    when the underlying concrete type is a tuple (#1048).

### Fixed

  * Fix bug in slice simplification (#992).

  * Fixed a typer checker bug for tracking the aliases of closures
    (#995).

  * Fixed handling of dumb terminals in futhark test (#1000).

  * Fixed exotic monomorphisation case involving lifted type
    parameters instantiated with functions that take named parameters
    (#1026).

  * Further tightening of the causality restriction (#1042).

  * Fixed alias tracking for right-operand operator sections (#1043).

## [0.15.8]

### Added


  * Warnings for overflowing literals, such as `1000 : u8`.

  * Futhark now supports an attribute system, whereby expressions can
    be tagged with attributes that provide hints or directions to the
    compiler.  This is an expert-level feature, but it is sometimes
    useful.

## [0.15.7]

### Added

  * Faster index calculations for very tight GPU kernels (such as the
    ones corresponding to 2D tiling).

  * `scan` with vectorised operators (e.g. `map2 (+)`) is now faster
    in some cases.

  * The C API has now been documented and stabilized, including
    obtaining profiling information (although this is still
    unstructured).

### Fixed

  * Fixed some cases of missing fusion (#953).

  * Context deinitialisation is now more complete, and should not leak
    memory (or at least not nearly as much, if any).  This makes it
    viable to repeatedly create and free Futhark contexts in the same
    process (although this can still be quite slow).

## [0.15.6]

### Added

  * Binary operators now act as left-to-right sequence points with
    respect to size types.

  * `futhark bench` now has more colourful and hopefully readable
    output.

  * The compiler is now about 30% faster for most nontrivial programs.
    This is due to parallelising the inlining stage, and tweaking the
    default configuration of the Haskell RTS.

  * `futhark dataset` is now about 8-10x faster.

### Fixed

  * Fixed some errors regarding constants (#941).

  * Fixed a few missing type checker cases for sum types (#938).

  * Fix OOB write in CUDA backend runtime code (#950).

## [0.15.5]

### Added

  * `reduce_by_index` with `f32`-addition is now approximately 2x
    faster in the CUDA backend.

### Fixed

  * Fixed kernel extractor bug in `if`-interchange (#921).

  * Fixed some cases of malformed kernel code generation (#922).

  * Fixed rare memory corruption bug involving branches returning
    arrays (#923).

  * Fixed spurious warning about entry points involving opaque return
    types, where the type annotations are put on a higher-order return
    type.

  * Fixed incorrect size type checking for sum types in negative
    position with unknown constructors (#927).

  * Fixed loop interchange for permuted sequential loops with more
    than one outer parallel loop (#928).

  * Fixed a type checking bug for branches returning incomplete sum
    types (#931).

## [0.15.4]

### Added

  * `futhark pkg` now shells out to `curl` for HTTP requests.

  * `futhark doc` now supports proper GitHub-flavored Markdown, as it
    uses the `cmark-gfm` library internally.

  * Top-level constants are now executed only once per program
    instance.  This matters when Futhark is used to generate library
    code.

  * `futhark autotune` is better at handling degrees of parallelism
    that assume multiple magnitudes during a single run.

  * `futhark pkg` now uses `curl` to retrieve packages.

  * Type errors are now printed in red for better legibility (thanks
    to @mxxo!).

### Fixed

  * Fixed incorrect handling of opaques in entry point return types.

  * `futhark pkg` now works properly with GitLab (#899).

## [0.15.3]

### Added

  * `scan` now supports operators whose operands are arrays.  They are
    significantly slower than primitive-typed scans, so avoid them if
    at all possible.

  * Precomputed constants are now handled much more efficiently.

  * Certain large programs that rely heavily on inlining now compile
    orders of magnitude faster.

### Fixed

  * Some fixes to complicated module expressions.

  * `futhark pkg` should no longer crash uncontrollably on network
    errors (#894).

  * Fixed local open in interpreter (#887).

  * Fix error regarding entry points that called other entry points
    which contained local functions (#895).

  * Fix loading OpenCL kernels from a binary.

## [0.15.2]

### Fixed

  * Fix a REPL regression that made it unable to handle overloaded
    types (such as numeric literals, oops).

  * The uniqueness of a record is now the minimum of the uniqueness of
    any of its elements (#870).

  * Bug in causality checking has been fixed (#872).

  * Invariant memory allocations in scan/reduce operators are now supported.

  * `futhark run` now performs more type checking on entry point input (#876).

  * Compiled Futhark programs now check for EOF after the last input
    argument has been read (#877).

  * Fixed a bug in `loop` type checking that prevented the result from
    ever aliasing the initial parameter values (#879).

## [0.15.1]

### Added

  * Futhark now type-checks size annotations using a size-dependent
    type system.

  * The parallel code generators can now handle bounds checking and
    other safety checks.

  * Integer division by zero is now properly safety-checked and
    produces an error message.

  * Integer exponentiation with negative exponent is now properly
    safety-checked and produces an error message.

  * Serious effort has been put into improving type errors.

  * `reduce_by_index` may be somewhat faster for complex operators on
    histograms that barely fit in local memory.

  * Improved handling of in-place updates of multidimensional arrays
    nested in `map`.  These are now properly parallelised.

  * Added `concat_to` and `flatten_to` functions to prelude.

  * Added `indices` function to the prelude.

  * `futhark check` and all compilers now take a `-w` option for
    disabling warnings.

  * `futhark bench` now accepts `--pass-compiler-option`.

  * The integer modules now have `mad_hi` and `mul_hi` functions for
    getting the upper part of multiplications.  Thanks to @porcuquine
    for the contribution!

  * The `f32` and `f64` modules now also define `sinh`, `cosh`,
    `tanh`, `asinh`, `acosh`, and `atanh` functions.

  * The `f32` and `f64` modules now also define `fma` and `mad`
    functions.

### Removed

  * Removed `update`, `split2`, `intersperse`, `intercalate`, `pick`,
    `steps`, and `range` from the prelude.

### Changed

  * `"futlib"` is now called `"prelude"`, and it is now an error to
    import it explicitly.

### Fixed

  * Corrected address calculations in `csharp` backend.

  * The C backends are now more careful about generating overflowing
    integer operations (since this is undefined behaviour in C, but
    defined in Futhark).

  * `futhark dataset` no longer crashes uncontrollably when used
    incorrectly (#849).

## [0.14.1]

### Added

  * The optimiser is now somewhat better at removing unnecessary
    copies of array slices.

  * `futhark bench` and `futhark test` now take a `--concurrency`
    option for limiting how many threads are used for housekeeping
    tasks.  Set this to a low value if you run out of memory.

  * `random` test blocks are now allowed to contain integer literals
    with type suffixes.

  * `:frame <n>` command for `futhark repl` for inspecting the stack.

  * `e :> t` notation, which means the same as `e : t` for now, but
    will have looser constraints in the future.

  * Size-lifted type abbreviations can be declared with `type~` and
    size-lifted type parameters with `'~`.  These currently have no
    significant difference from fully lifted types.

### Changed

  * Tuples are now 0-indexed (#821, which also includes a conversion
    script).

  * Invalid ranges like `1..<0` now produce a run-time error instead
    of an empty array.

  * Record updates (`r with f = e`) now require `r` to have a
    completely known type up to `f`.  This is a restriction that will
    hopefully be lifted in the future.

  * The backtrace format has changed to be innermost-first, like
    pretty much all other languages.

  * Value specs must now explicitly quantify all sizes of function
    parameters.  Instead of

        val sum: []t -> t

    you must write

        val sum [n]: [n]t -> t

  * `futhark test` now once again numbers un-named data sets from 0
    rather than from 1.  This fits a new general principle of always
    numbering from 0 in Futhark.

  * Type abbreviations declared with `type` may no longer contain
    functions or anonymous sizes in their definition.  Use `type^` for
    these cases.  Just a warning for now, but will be an error in the
    future.

### Fixed

  * Work around (probable) AMD OpenCL compiler bug for
    `reduce_by_index` operations with complex operators that require
    locking.

  * Properly handle another ICE on parse errors in test stanzas (#819).

  * `futhark_context_new_with_command_queue()` now actually works.  Oops.

  * Different scopes are now properly addressed during type inference
    (#838).  Realistically, there will still be some missing cases.

## [0.13.2]

### Added

  * New subcommand, `futhark query`, for looking up information about
    the name at some position in a file.  Intended for editor
    integration.

  * (Finally) automatic support for compute model 7.5 in the CUDA backend.

  * Somewhat better performance for very large target arrays for
    `reduce_by_index.`.

### Fixed

  * Fixed a slice-iota simplification bug (#813).

  * Fixed defunctionalisation crash involving intrinsics (#814).

## [0.13.1]

### Added

  * Stack traces are now multiline for better legibility.

### Changed

  * The `empty(t)` notation now specifies the type of the *entire
    value* (not just the element type), and requires dimension sizes
    when `t` is an array (e.g. `empty(i32)` is no longer allowed, you
    need for example `empty([0]i32)`).

  * All input files are now assumed to be in UTF-8.

### Fixed

  * Fixed exponential-time behaviour for certain kernels with large
    arithmetic expressions (#805).

  * `futhark test` and friends no longer crash when reporting some
    errors (#808).

  * Fix uniqueness of loop results (#810).

## [0.12.3]

### Added

  * Character literals can now be any integer type.

  * The integer modules now have `popc` and `clz` functions.

  * Tweaked inlining so that larger programs may now compile faster
    (observed about 20%).

  * Pattern-matching on large sum typed-values taken from arrays may
    be a bit faster.

### Fixed

  * Various small fixes to type errors.

  * All internal functions used in generated C code are now properly
    declared `static`.

  * Fixed bugs when handling dimensions and aliases in type ascriptions.

## [0.12.2]

### Added

  * New tool: `futhark autotune`, for tuning the threshold parameters
    used by incremental flattening.  Based on work by Svend Lund
    Breddam, Simon Rotendahl, and Carl Mathias Graae Larsen.

  * New tool: `futhark dataget`, for extracting test input data.  Most
    will probably never use this.

  * Programs compiled with the `cuda` backend now take options
    `--default-group-size`, `--default-num-groups`, and
    `--default-tile-size`.

  * Segmented `reduce_by_index` are now substantially fasted for small
    histograms.

  * New functions: `f32.lerp` and `f64.lerp`, for linear interpolation.

### Fixed

  * Fixes to aliasing of record updates.

  * Fixed unnecessary array duplicates after coalescing optimisations.

  * `reduce_by_index` nested in `map`s will no longer sometimes
    require huge amounts of memory.

  * Source location now correct for unknown infix operators.

  * Function parameters are no longer in scope of themselves (#798).

  * Fixed a nasty out-of-bounds error in handling of irregular allocations.

  * The `floor`/`ceil` functions in `f32`/`f64` now handle infinities
    correctly (and are also faster).

  * Using `%` on floats now computes fmod instead of crashing the compiler.

## [0.12.1]

### Added

  * The internal representation of parallel constructs has been
    overhauled and many optimisations rewritten.  The overall
    performance impact should be neutral on aggregate, but there may
    be changes for some programs (please report if so).

  * Futhark now supports structurally typed sum types and pattern
    matching!  This work was done by Robert Schenck.  There remain
    some problems with arrays of sum types that themselves contain
    arrays.

  * Significant reduction in compile time for some large programs.

  * Manually specified type parameters need no longer be exhaustive.

  * Mapped `rotate` is now simplified better.  This can be
    particularly helpful for stencils with wraparound.

### Removed

  * The `~` prefix operator has been removed.  `!` has been extended
    to perform bitwise negation when applied to integers.

### Changed

  * The `--futhark` option for `futhark bench` and `futhark test` now
    defaults to the binary being used for the subcommands themselves.

  * The legacy `futhark -t` option (which did the same as `futhark
    check`) has been removed.

  * Lambdas now bind less tightly than type ascription.

  * `stream_map` is now `map_stream` and `stream_red` is now
    `reduce_stream`.

### Fixed

  * `futhark test` now understands `--no-tuning` as it was always
    supposed to.

  * `futhark bench` and `futhark test` now interpret `--exclude` in
    the same way.

  * The Python and C# backends can now properly read binary boolean
    input.

## [0.11.2]

### Fixed

  * Entry points whose types are opaque due to module ascription, yet
    whose representation is simple (scalars or arrays of scalars) were
    mistakely made non-opaque when compiled with ``--library``.  This
    has been fixed.

  * The CUDA backend now supports default sizes in `.tuning` files.

  * Loop interchange across multiple dimensions was broken in some cases (#767).

  * The sequential C# backend now generates code that compiles (#772).

  * The sequential Python backend now generates code that runs (#765).

## [0.11.1]

### Added

  * Segmented scans are a good bit faster.

  * `reduce_by_index` has received a new implementation that uses
    local memory, and is now often a good bit faster when the target
    array is not too large.

  * The `f32` and `f64` modules now contain `gamma` and `lgamma`
    functions.  At present these do not work in the C# backend.

  * Some instances of `reduce` with vectorised operators (e.g. `map2
    (+)`) are orders of magnitude faster than before.

  * Memory usage is now lower on some programs (specifically the ones
    that have large `map`s with internal intermediate arrays).

### Removed

  * Size *parameters* (not *annotations*) are no longer permitted
    directly in `let` and `loop` bindings, nor in lambdas.  You are
    likely not affected (except for the `stream` constructs; see
    below).  Few people used this.

### Changed

  * The array creation functions exported by generated C code now take
    `int64_t` arguments for the shape, rather than `int`.  This is in
    line with what the shape functions return.

  * The types for `stream_map`, `stream_map_per`, `stream_red`, and
    `stream_red_per` have been changed, such that the chunk function
    now takes the chunk size as the first argument.

### Fixed

  * Fixes to reading values under Python 3.

  * The type of a variable can now be deduced from its use as a size
    annotation.

  * The code generated by the C-based backends is now also compilable
    as C++.

  * Fix memory corruption bug that would occur on very large segmented
    reductions (large segments, and many of them).

## [0.10.2]

### Added

  * `reduce_by_index` is now a good bit faster on operators whose
    arguments are two 32-bit values.

  * The type checker warns on size annotations for function parameters
    and return types that will not be visible from the outside,
    because they refer to names nested inside tuples or records.  For
    example, the function

        let f (n: i32, m: i32): [n][m]i32 = ...

    will cause such a warning.  It should instead be written

        let f (n: i32) (m: i32): [n][m]i32 = ...

  * A new library function
    `futhark_context_config_select_device_interactively()` has been
    added.

### Fixed

  * Fix reading and writing of binary files for C-compiled executables
    on Windows.

  * Fixed a couple of overly strict internal sanity checks related to
    in-place updates (#735, #736).

  * Fixed a couple of convoluted defunctorisation bugs (#739).

## [0.10.1]

### Added

  * Using definitions from the `intrinsic` module outside the prelude
    now results in a warning.

  * `reduce_by_index` with vectorised operators (e.g. `map2 (+)`) is
    orders of magnitude faster than before.

  * Executables generated with the `pyopencl` backend now support the
    options `--default-tile-size`, `--default-group-size`,
    `--default-num-groups`, `--default-threshold`, and `--size`.

  * Executables generated with `c` and `opencl` now print a help text
    if run with invalid options.  The `py` and `pyopencl` backends
    already did this.

  * Generated executables now support a `--tuning` flag for passing
    many tuned sizes in a file.

  * Executables generated with the `cuda` backend now take an
    `--nvrtc-option` option.

  * Executables generated with the `opencl` backend now take a
    `--build-option` option.

### Removed

  * The old `futhark-*` executables have been removed.

### Changed

  * If an array is passed for a function parameter of a polymorphic
    type, all arrays passed for parameters of that type must have the
    same shape.  For example, given a function

        let pair 't (x: t) (y: t) = (x, y)

    The application `pair [1] [2,3]` will now fail at run-time.

  * `futhark test` now numbers un-named data sets from 1 rather than
    0.  This only affects the text output and the generated JSON
    files, and fits the tuple element ordering in Futhark.

  * String literals are now of type `[]u8` and contain UTF-8 encoded
    bytes.

### Fixed

  * An significant problematic interaction between empty arrays and
    inner size declarations has been closed (#714).  This follows a
    range of lesser empty-array fixes from 0.9.1.

  * `futhark datacmp` now prints to stdout, not stderr.

  * Fixed a major potential out-of-bounds access when sequentialising
    `reduce_by_index` (in most cases the bug was hidden by subsequent
    C compiler optimisations).

  * The result of an anonymous function is now also forbidden from
    aliasing a global variable, just as with named functions.

  * Parallel scans now work correctly when using a CPU OpenCL
    implementation.

  * `reduce_by_index` was broken on newer NVIDIA GPUs when using fancy
    operators.  This has been fixed.

## [0.9.1]

### Added

  * `futhark cuda`: a new CUDA backend by Jakob Stokholm Bertelsen.

  * New command for comparing data files: `futhark datacmp`.

  * An `:mtype` command for `futhark repl` that shows the type of a
    module expression.

  * `futhark run` takes a `-w` option for disabling warnings.

### Changed

  * Major command reorganisation: all Futhark programs have been
    combined into a single all-powerful `futhark` program.  Instead of
    e.g. `futhark-foo`, use `futhark foo`.  Wrappers will be kept
    around under the old names for a little while.  `futharki` has
    been split into two commands: `futhark repl` and `futhark run`.
    Also, `py` has become `python` and `cs` has become `csharp`, but
    `pyopencl` and `csopencl` have remained as they were.

  * The result of a function is now forbidden from aliasing a global
    variable.  Surprisingly little code is affected by this.

  * A global definition may not be ascribed a unique type.  This never
    had any effect in the first place, but now the compiler will
    explicitly complain.

  * Source spans are now printed in a slightly different format, with
    ending the line number omitted when it is the same as the start
    line number.

### Fixed

  * `futharki` now reports source locations of `trace` expressions
    properly.

  * The type checker now properly complains if you try to define a
    type abbreviation that has unused size parameters.

## [0.8.1]

### Added

  * Now warns when `/futlib/...` files are redundantly imported.

  * `futharki` now prints warnings for files that are ":load"ed.

  * The compiler now warns when entry points are declared with types
    that will become unnamed and opaque, and thus impossible to
    provide from the outside.

  * Type variables invented by the type checker will now have a
    unicode subscript to distinguish them from type parameters
    originating in the source code.

  * `futhark-test` and `futhark-bench` now support generating random
    test data.

  * The library backends now generate proper names for arrays of
    opaque values.

  * The parser now permits empty programs.

  * Most transpositions are now a good bit faster, especially on
    NVIDIA GPUs.

### Removed

  * The `<-` symbol can no longer be used for in-place updates and
    record updates (deprecated in 0.7.3).

### Changed

  * Entry points that accept a single tuple-typed parameter are no
    longer silently rewritten to accept multiple parameters.

### Fixed

  * The `:type` command in `futharki` can now handle polymorphic
    expressions (#669).

  * Fixed serious bug related to chaining record updates.

  * Fixed type inference of record fields (#677).

  * `futharki` no longer goes in an infinite loop if a ``for`` loop
    contains a negative upper bound.

  * Overloaded number types can no longer carry aliases (#682).

## [0.7.4]

### Added

  * Support type parameters for operator specs defined with `val`.

### Fixed

  * Fixed nasty defunctionalisation bug (#661).

  * `cabal sdist` and `stack sdist` works now.

## [0.7.3]

### Added

  * Significant performance changes: there is now a constant extra
    compilation overhead (less than 200ms on most machines).  However,
    the rest of the compiler is 30-40% faster (or more in some cases).

  * A warning when ambiguously typed expressions are assigned a
    default (`i32` or `f64`).

  * In-place updates and record updates are now written with `=`
    instead of `<-`.  The latter is deprecated and will be removed in
    the next major version (#650).

### Fixed

  * Polymorphic value bindings now work properly with module type
    ascription.

  * The type checker no longer requires types used inside local
    functions to be unambiguous at the point where the local function
    is defined.  They must still be unambiguous by the time the
    top-level function ends.  This is similar to what other ML
    languages do.

  * `futhark-bench` now writes "μs" instead of "us".

  * Type inference for infix operators now works properly.

## [0.7.2]

### Added

  * `futhark-pkg` now supports GitLab.

  * `futhark-test`s `--notty` option now has a `--no-terminal` alias.
    `--notty` is deprecated, but still works.

  * `futhark-test` now supports multiple entry points per test block.

  * Functional record updates: `r with f <- x`.

### Fixed

  * Fix the `-C` option for `futhark-test`.

  * Fixed incorrect type of `reduce_by_index`.

  * Segmented `reduce_by_index` now uses much less memory.

## [0.7.1]

### Added

  * C# backend by Mikkel Storgaard Knudsen (`futhark-cs`/`futhark-csopencl`).

  * `futhark-test` and `futhark-bench` now take a `--runner` option.

  * `futharki` now uses a new interpreter that directly interprets the
    source language, rather than operating on the desugared core
    language.  In practice, this means that the interactive mode is
    better, but that interpretation is also much slower.

  * A `trace` function that is semantically `id`, but makes `futharki`
    print out the value.

  * A `break` function that is semantically `id`, but makes `futharki`
    stop and provide the opportunity to inspect variables in scope.

  * A new SOAC, `reduce_by_index`, for expressing generalised
    reductions (sometimes called histograms).  Designed and
    implemented by Sune Hellfritzsch.

### Removed

  * Most of futlib has been removed.  Use external packages instead:

    * `futlib/colour` => https://github.com/athas/matte

    * `futlib/complex` => https://github.com/diku-dk/complex

    * `futlib/date` => https://github.com/diku-dk/date

    * `futlib/fft` => https://github.com/diku-dk/fft

    * `futlib/linalg` => https://github.com/diku-dk/fft

    * `futlib/merge_sort`, `futlib/radix_sort` => https://github.com/diku-dk/sorts

    * `futlib/random` => https://github.com/diku-dk/cpprandom

    * `futlib/segmented` => https://github.com/diku-dk/segmented

    * `futlib/sobol` => https://github.com/diku-dk/sobol

    * `futlib/vector` => https://github.com/athas/vector

    No replacement: `futlib/mss`, `futlib/lss`.

  * `zip6`/`zip7`/`zip8` and their `unzip` variants have been removed.
    If you build gigantic tuples, you're on your own.

  * The `>>>` operator has been removed.  Use an unsigned integer type
    if you want zero-extended right shifts.

### Changed

  * The `largest`/`smallest` values for numeric modules have been
    renamed `highest`/`lowest`.

### Fixed

  * Many small things.

## [0.6.3]

### Added

  * Added a package manager: `futhark-pkg`.  See also [the
    documentation](http://futhark.readthedocs.io/en/latest/package-management.html).

  * Added `log2` and `log10` functions to `f32` and `f64`.

  * Module type refinement (`with`) now permits refining parametric
    types.

  * Better error message when invalid values are passed to generated
    Python entry points.

  * `futhark-doc` now ignores files whose doc comment is the word
    "ignore".

  * `copy` now works on values of any type, not just arrays.

  * Better type inference for array indexing.

### Fixed

  * Floating-point numbers are now correctly rounded to nearest even
    integer, even in exotic cases (#377).

  * Fixed a nasty bug in the type checking of calls to consuming
    functions (#596).

## [0.6.2]

### Added

  * Bounds checking errors now show the erroneous index and the size
    of the indexed array.  Some other size-related errors also show
    more information, but it will be a while before they are all
    converted (and say something useful - it's not entirely
    straightforward).

  * Opaque types now have significantly more readable names,
    especially if you add manual size annotations to the entry point
    definitions.

  * Backticked infix operators can now be used in operator sections.

### Fixed

  * `f64.e` is no longer pi.

  * Generated C library code will no longer `abort()` on application
    errors (#584).

  * Fix file imports on Windows.

  * `futhark-c` and `futhark-opencl` now generates thread-safe code (#586).

  * Significantly better behaviour in OOM situations.

  * Fixed an unsound interaction between in-place updates and
    parametric polymorphism (#589).

## [0.6.1]

### Added

  * The `real` module type now specifies `tan`.

  * `futharki` now supports entering declarations.

  * `futharki` now supports a `:type` command (or `:t` for short).

  * `futhark-test` and `futhark-benchmark` now support gzipped data
    files.  They must have a `.gz` extension.

  * Generated code now frees memory much earlier, which can help
    reduce the footprint.

  * Compilers now accept a `--safe` flag to make them ignore `unsafe`.

  * Module types may now define *lifted* abstract types, using the
    notation `type ^t`.  These may be instantiated with functional
    types.  A lifted abstract type has all the same restrictions as a
    lifted type parameter.

### Removed

  * The `rearrange` construct has been removed.  Use `transpose` instead.

  * `futhark-mode.el` has been moved to a [separate
    repository](https://github.com/diku-dk/futhark-mode).

  * Removed `|>>` and `<<|`.  Use `>->` and `<-<` instead.

  * The `empty` construct is no longer supported.  Just use empty
    array literals.

### Changed

  * Imports of the basis library must now use an absolute path
    (e.g. `/futlib/fft`, not simply `futlib/fft`).

  * `/futlib/vec2` and `/futlib/vec3` have been replaced by a new
    `/futlib/vector` file.

  * Entry points generated by the C code backend are now prefixed with
    `futhark_entry_` rather than just `futhark_`.

  * `zip` and `unzip` are no longer language constructs, but library
    functions, and work only on two arrays and pairs, respectively.
    Use functions `zipN/unzipN` (for `2<=n<=8`).

### Fixed

  * Better error message on EOF.

  * Fixed handling of `..` in `import` paths.

  * Type errors (and other compiler feedback) will no longer contain
    internal names.

  * `futhark-test` and friends can now cope with infinities and NaNs.
    Such values are printed and read as `f32.nan`, `f32.inf`,
    `-f32.inf`, and similarly for `f32`.  In `futhark-test`, NaNs
    compare equal.

## [0.5.2]

### Added

  * Array index section: `(.[i])` is shorthand for `(\x -> x[i])`.
    Full slice syntax supported. (#559)

  * New `assert` construct. (#464)

  * `futhark-mode.el` now contains a definition for flycheck.

### Fixed

  * The index produced by `futhark-doc` now contains correct links.

  * Windows linebreaks are now fully supported for test files (#558).

## [0.5.1]

### Added

  * Entry points need no longer be syntactically first-order.

  * Added overloaded numeric literals (#532).  This means type
    suffixes are rarely required.

  * Binary and unary operators may now be bound in patterns by
    enclosing them in parenthesis.

  * `futhark-doc` now produces much nicer documentation.  Markdown is
    now supported in documentation comments.

  * `/futlib/functional` now has operators `>->` and `<-<` for
    function composition.  `<<|` are `|>>` are deprecated.

  * `/futlib/segmented` now has a `segmented_reduce`.

  * Scans and reductions can now be horizontally fused.

  * `futhark-bench` now supports multiple entry points, just like
    `futhark-test`.

  * ".." is now supported in `include` paths.

### Removed

  * The `reshape` construct has been removed.  Use the
    `flatten`/`unflatten` functions instead.

  * `concat` and `rotate` no longer support the `@` notation.  Use
    `map` nests instead.

  * Removed `-I`/`--library`.  These never worked with
    `futhark-test`/`futhark-bench` anyway.

### Changed

  * When defining a module type, a module of the same name is no
    longer defined (#538).

  * The `default` keyword is no longer supported.

  * `/futlib/merge_sort` and `/futlib/radix_sort` now define
    functions instead of modules.

### Fixed

  * Better type inference for `rearrange` and `rotate`.

  * `import` path resolution is now much more robust.

## [0.4.1]

### Added

  * Unused-result elimination for reductions; particularly useful when
    computing with dual numbers for automatic differentiation.

  * Record field projection is now possible for variables of (then)
    unknown types.  A function parameter must still have an
    unambiguous (complete) type by the time it finishes checking.

### Fixed

  * Fixed interaction between type ascription and type inference (#529).

  * Fixed duplication when an entry point was also called as a function.

  * Futhark now compiles cleanly with GHC 8.4.1 (this is also the new default).

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

   * Added a range of higher-order utility functions to the prelude,
     including (among others):

         val (|>) '^a '^b: a ->  (a -> b) -> b

         val (<|) '^a '^b: (a -> b) -> a -> b

         val (|>>) '^a 'b '^c: (a -> b) -> (b -> c) -> a -> c

         val (<<|) '^a 'b '^c: (b -> c) -> (a -> b) a -> c

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
