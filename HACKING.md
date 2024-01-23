# Hacking on the Futhark Compiler

The Futhark compiler is a significant body of code with a not entirely
straightforward design. The main source of documentation is the
Haddock comments in the source code itself, including [this general
overview of the compiler
architecture](https://hackage.haskell.org/package/futhark-0.24.3/docs/Futhark.html).

To build the compiler, you need a recent version of
[GHC](http://ghc.haskell.org/), which can be installed via
[ghcup](https://www.haskell.org/ghcup/).  Alternatively, if you
[install Nix](https://nixos.org/download.html#download-nix) then you
can run `nix-shell` to get a shell environment in which all necessary
tools are installed.

After that, run `make docs` to generate internal compiler
documentation in HTML format.  The last few lines of output will tell
you the name of an `index.html` file which you should then open.  Go
to the documentation for the module named `Futhark`, which contains an
introduction to the compiler architecture.

For contributing code, see the [Haskell style guide](STYLE.md).

If you feel that the documentation is incomplete, or something lacks
an explanation, then feel free to [report it as an
issue](https://github.com/diku-dk/futhark/issues). Documentation bugs
are bugs too.

## Building

We include a `Makefile` with the following targets.

* `make build` (or just `make`) builds the compiler.

* `make install` builds the compiler and copies the resulting binaries
  to `$HOME/.local/bin`, or `$PREFIX/bin` if the `PREFIX` environment
  variable is set.

* `make docs` builds internal compiler documentation.  For the user
  documentation, see the `docs/` subdirectory.

* `make check` style-checks all code.  Requires [GNU
  Parallel](https://www.gnu.org/software/parallel/).

* `make check-commit` style-checks all code staged for a commit.
  Requires [GNU Parallel](https://www.gnu.org/software/parallel/).

You can also use `cabal` directly if you are familiar with it.  In
particular, `cabal run futhark -- args...` is useful for running the
Futhark compiler with the provided args.

### Enabling profiling

Asking GHC to generate profiling information is useful not just for
the obvious purpose of gathering profiling information, but also so
that stack traces become more informative.  Run

    make configure-profile

to turn on profiling.  This setting will be stored in the file
`cabal.project.local` and all future builds will be with profiling
information.  Note that the compiler runs significantly slower this
way.

To produce a profiling report when running the compiler, add `+RTS -p`
to the *end* command line.

See also [the chapter on
profiling](https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html)
in the GHC User's Guide.

Note that GHCs code generator is sometimes slightly buggy in its
handling of profiled code.  If you encounter a compiler crash with an
error message like "PAP object entered", then this is a GHC bug.

### Debugging compiler crashes

By default, Haskell does not produce very good stack traces.  If you
compile with `make configure-profile` as mentioned above, you can pass
`+RTS -xc` to the Futhark compiler in order to get better stack
traces.  You will see that you actually get *multiple* stack traces,
as the Haskell runtime system will print a stack trace for every
signal it receives, and several of these occur early, when the program
is read from disk.  Also, the *final* stack trace is often some
diagnostic artifact.  Usually the second-to-last stack trace is what
you are looking for.

## Testing

### Only internal compilation

This command tests compilation *without* compiling the generated C
code, which speeds up testing for internal compiler errors:

    futhark test -C tests --pass-compiler-option=--library

### Running only a single unit test

    cabal run unit -- -p '/reshape . fix . iota 3d/'

The argument to `-p` is the name of the test that fails, as reported
by `cabal test`.  You may have to scroll through the output a bit to
find it.

## Debugging Internal Type Errors

The Futhark compiler uses a typed core language, and the type checker
is run after every pass. If a given pass produces a program with
inconsistent typing, the compiler will report an error and
abort. While not every compiler bug will manifest itself as a core
language type error (unfortunately), many will. To write the erroneous
core program to `filename` in case of type error, pass `-vfilename` to
the compiler. This will also enable verbose output, so you can tell
which pass fails. The `-v` option is also useful when the compiler
itself crashes, as you can at least tell where in the pipeline it got
to.

## Checking Generated Code

Hacking on the compiler will often involve inspecting the quality of
the generated code. The recommended way to do this is to use
`futhark c` or `futhark opencl` to compile a Futhark program to an
executable. These backends insert various forms of instrumentation
that can be enabled by passing run-time options to the generated
executable.

-   As a first resort, use `-t` option to use the built-in runtime
    measurements. A nice trick is to pass `-t /dev/stderr`, while
    redirecting standard output to `/dev/null`. This will print the
    runtime on the screen, but not the execution result.
-   Optionally use `-r` to ask for several runs, e.g. `-r 10`. If
    combined with `-t`, this will cause several runtimes to be printed
    (one per line).
-   Pass `-D` to have the program print information on allocation and
    deallocation of memory.
-   (`futhark opencl` and `futhark cuda` only) Use the `-D` option to
    enable synchronous execution. `clFinish()` or the CUDA equivalent
    will be called after most OpenCL operations, and a running log of
    kernel invocations will be printed. At the end of execution, the
    program prints a table summarising all kernels and their total
    runtime and average runtime.

## Using `futhark dev`

For debugging specific compiler passes, the `futhark dev` subcommand
allows you to tailor your own compilation pipeline using command line
options. It is also useful for seeing what the AST looks like after
specific passes.

You may wish to set the environment variable
`FUTHARK_COMPILER_DEBUGGING=1`. This has the following effects:

- The frontend prints internal names. (This may affect code
  generation in some cases, so turn it off when actually
  generating code.)
- Tools that talk to server-mode executables will print the messages
  sent back and forth on the standard error stream.

## Running compiler pipelines

You can run the various compiler passes in whatever order you wish.
There are also various shorthands for running entire standard pipelines:

- `--gpu`: pipeline used for GPU backends (stopping just before adding
  memory information).
- `--gpu-mem`: pipeline used for GPU backends, with memory
  information. This will show the IR that is passed to ImpGen.
- `--seq`: pipeline used for sequential backends (stopping just before
  adding memory information).
- `--seq-mem`: pipeline used for sequential backends, with memory
  information. This will show the IR that is passed to ImpGen.
- `--mc`: pipeline used for multicore backends (stopping just before
  adding memory information).
- `--mc-mem`: pipeline used for multicore backends, with memory
  information. This will show the IR that is passed to ImpGen.

By default, `futhark dev` will print the resulting IR. You can switch to
a different *action* with one of the following options:

- `--compile-imp-seq`: generate sequential ImpCode and print it.
- `--compile-imp-gpu`: generate GPU ImpCode and print it.
- `--compile-imp-multicore`: generate multicore ImpCode and
  print it.

You must use the appropriate pipeline as well (e.g. `--gpu-mem` for
`--compile-imp-gpu`).

You can also use e.g. `--backend=c` to run the same code generation
and compilation as `futhark c`.  This is useful for experimenting with
other compiler pipelines, but still producing an executable or
library.

## When you are about to have a bad day

When using the `cuda` backend, you can use the `--dump-ptx` runtime
option to dump PTX, a kind of high-level assembly for NVIDIA GPUs,
corresponding to the GPU kernels. This can be used to investigate why
the generated code isn\'t running as fast as you expect (not fun), or
even whether NVIDIAs compiler is miscompiling something (extremely not
fun). With the OpenCL backend, `--dump-opencl-binary` does the same
thing.

On AMD platforms, `--dump-opencl-binary` tends to produce an actual
binary of some kind, and it is pretty tricky to obtain a debugger for it
(they are available and open source, but the documentation and
installation instructions are terrible). Instead, AMDs OpenCL kernel
compiler accepts a `-save-temps=foo` build option, which will make it
write certain intermediate files, prefixed with `foo`. In particular, it
will write an `.s` file that contains what appears to be HSA assembly
(at least when using ROCm). If you find yourself having to do do this,
then you are definitely going to have a bad day, and probably evening
and night as well.

## Minimising programs

Sometimes you have a program that produces the wrong results rather
than crashing the compiler.  These are some of the most difficult bugs
to handle.  If the result is at least deterministic and you have some
way of compiling the program that does work (either an older version
or a different backend), then the following procedure is useful for
reducing the program as much as possible.  Suppose that we are trying
to debug a miscompilation for the `opencl` backend where the `c`
backend works, the failing program is `prog.fut`, and the input data
is `prog.in`.  Write the following script `test.sh`:

```
set -x
set -e
futhark c prog.fut -o prog-c
futhark opencl prog.fut -o prog-opencl
cat prog.in | ./prog-c -b > output-c
cat prog.in | ./prog-opencl -b > output-opencl
futhark datacmp output-c output-opencl
```

This compares the results obtained from running the program with the
two compilers.  You can now (manually) start removing parts of
`prog.fut` while regularly rerunning `test.sh` to verify that it still
fails.  In particular, you can easily remove program return values,
which is not the case if you are comparing against a fixed expected
output. Eventually you will have a hopefully small program that
produces different results with the two compilers, and you can look in
detail at the IR to figure out what goes wrong.

## Graphs of internal data structures

Some passes can prettyprint internal representations in
[GraphViz](https://graphviz.org/) format.  For example, to see the
fusion graph (prior to fusion), do

    $ futhark dev -e --inline-aggr -e foo.fut  --fusion-graph > foo.dot

and then to render `foo.dot` as `foo.dot.pdf` with GraphViz:

    $ dot foo.dot -Tpdf -O

## Using Oclgrind

[Oclgrind](https://github.com/jrprice/oclgrind) is an OpenCL simulator
similar to Valgrind that can help find memory and synchronisation
errors.  It runs code somewhat slowly, but it allows testing of OpenCL
code on systems that are not otherwise capable of executing OpenCL.

It is very easy to run a program in Oclgrind:

    oclgrind ./foo

For use in `futhark test`, we have [a wrapper
script](tools/oclgrindgrunner.sh) that returns with a nonzero exit
code if Oclgrind detects a memory error.  You use it as follows:

    futhark test foo.fut --backend=opencl --runner=tools/oclgrindrunner.sh

Some versions of Oclgrind have an unfortunate habit of [generating
code they don't know how to
execute](https://github.com/jrprice/Oclgrind/issues/204).  To work
around this, disable optimisations in the OpenCL compiler:

    futhark test foo.fut --backend=opencl --runner=tools/oclgrindrunner.sh --pass-option=--build-option=-O0

## Using `futhark script`

The `futhark script` command is a handy way to run (server-mode)
executables with arbitrary input, while also seeing logging output in
real time. This is particularly useful for programs whose benchmarking
input are complicated FutharkScript expressions.

If you have a program `infinite.fut` containing

```Futhark
entry main n = iterate 1000000000 (map (+1)) (iota n)
```

then you can run

```
$ futhark script -D infinite.fut 'main 10i64'
```

to run it with debug prints. You can also use `-L` instead of `-D` to
just enable logging. The `main 10i64` can be an arbitrary FutharkScript
expression.

The above will compile `infinite.fut` using the `c` backend before
running it. Pass a `--backend` option to `futhark script` to use a
different backend, or pass an already compiled program instead of a
`.fut` file (e.g., `infinite`).

See the manpages for `futhark script` and `futhark literate` for more
information.
