Futhark Compiler Style Guide
============================

This document provides guidelines and advice on how to write and
format code will fit with the existing code base and ensure some
degree of consistency.  Some of these rules are enforced by the
automatic quality checker that is run after each push, but most are
not.  Not all of the current code follows these guidelines.  If you
find yourself working on such code, please reformat it while you are
there.  When something isn't covered by this guide you should stay
consistent with the code in the other modules.

There are two sets of rules in this document.  *Style Rules*, which
low-level details such as the amount of indentation and how to name
things, and *Design Rules*, which are less specific high-level
guidelines on some of the principles underlying the compiler design.

Style Rules
===========

Ormolu
------

Futhark uses [Ormolu](https://github.com/tweag/ormolu/) to enforce consistency
in formatting style. The style is checked by our CI, so you should make sure any
pull requests comply with Ormolus formatting before trying to merge.

### Installation

Installing Ormolu can be done through Cabal, Stack, Nix or the Arch Linux
package manager. For instance, to install through Cabal, run the following
command:

```
cabal install ormolu
```

If you're running Nix or NixOS, you can just use `nix-shell` to enter a
development environment with ormolu already installed.

### Basic Usage

The following command formats a single file:

```
ormolu -i FILE.hs
```

This command can be used to format all Haskell files in Futhark, while
checking that the formatting is idempotent:

```
./tools/run-formatter.sh src src-testing
```

The idempotence check is mostly done to make sure Ormolu (which is still a young
tool in active development) doesn't introduce any unnecessary changes. Any
idempotence errors should be reported upstream.

### Editor Integration

Emacs has [ormolu.el](https://github.com/vyorkin/ormolu.el) to help with
formatting. Once installed (as per the instructions on that page), it will
automatically format any open Haskell file on save.

The Ormolu README lists further integrations for VS Code and vim.

### Limitations

Ormolu doesn't handle all aspects of coding style. For instance, it will do no
significant rewrites of your code, like if-then-else to pattern matches or
enforcing the 80 character line limit, but it will ensure consistency in
alignment and basic formatting. Therefore, as a Futhark contributer, use Ormolu
to ensure basic style consistency, while still taking care to follow the more
general style rules listed below.

Formatting
----------

### Line Length

Maximum line length is *80 characters*.

Ormolu doesn't enfore the 80 character line limit, so it is up to the user to
introduce the necessary line breaks in the code. However, Ormolu will take a
hint. Imagine you've got the following line of code:

```haskell
onKernels :: (SegOp SegLevel KernelsMem -> ReuseAllocsM (SegOp SegLevel KernelsMem)) -> Stms KernelsMem -> ReuseAllocsM (Stms KernelsMem)
```

If the user introduces a newline before `ReuseAllocsM`, turning the above into
the following:

```haskell
onKernels :: (SegOp SegLevel KernelsMem -> ReuseAllocsM (SegOp SegLevel KernelsMem)) -> Stms KernelsMem ->
  ReuseAllocsM (Stms KernelsMem)
```

Ormolu will pick up the hint and reformat the entire declaration to this:

```haskell
onKernels ::
  (SegOp SegLevel KernelsMem -> ReuseAllocsM (SegOp SegLevel KernelsMem)) ->
  Stms KernelsMem ->
  ReuseAllocsM (Stms KernelsMem)
```


### Blank Lines

In large `do`-blocks, separate logically separate chunks of code with
a single blank line.


### Long Expressions

Long expressions should be split over multiple lines.

If splitting a definition using the `$` operator, Ormolu will incrementally add
more indentation, which may sometimes be undesirable.

For instance, the following expression:

```haskell
someAtrociouslyLongVariableName <- someFunction $ someOtherFunction withSomeVar $ someThirdFunction something $ map somethingElse
```

Will turn in to:

```haskell
someAtrociouslyLongVariableName <-
  someFunction $
    someOtherFunction withSomeVar $
      someThirdFunction something $
        map somethingElse
```

If you'd rather keep everything equally nested, consider using the `&` operator
instead, which is like a reverse `$`. The code above is semantically identical
to this:

```haskell
someAtrociouslyLongVariableName <-
  map somethingElse
    & someThirdFunction something
    & someOtherFunction withSomeVar
    & someFunction
```


### Export Lists

Format export lists as follows:

```haskell
module Data.Set
    ( -- * The @Set@ type
      empty,
      Set,
      singleton,

      -- * Querying
      member,
    ) where
```
### If-then-else clauses

Generally, guards and pattern matches should be preferred over if-then-else
clauses, where possible.  E.g, instead of this:

```haskell
foo b =
  if not b then a
  else c
```

Prefer;

```haskell
foo False = a
foo True  = c
```

Short cases should usually be put on a single line (when line length
allows it).


### Pattern matching

Prefer pattern-matching in function clauses to `case`.  Consider using
[view patterns](https://ghc.haskell.org/trac/ghc/wiki/ViewPatterns),
but be careful not to go overboard.


Imports
-------

Try to use explicit import lists or `qualified` imports for standard
and third party libraries.  This makes the code more robust against
changes in these libraries.  Exception: the Prelude.

Comments
--------

### Punctuation

Write proper sentences; start with a capital letter and use proper
punctuation.

### Top-Level Definitions

Comment every top level function (particularly exported functions),
and provide a type signature; use Haddock syntax in the comments.
Comment every exported data type.  Function example:

```haskell
-- | Send a message on a socket.  The socket must be in a connected
-- state.  Returns the number of bytes sent.  Applications are
-- responsible for ensuring that all data has been sent.
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int      -- ^ Bytes sent
```

For functions the documentation should give enough information to
apply the function without looking at the function's definition.

Record example:

```haskell
-- | Bla bla bla.
data Person = Person
  { age  :: !Int     -- ^ Age
  , name :: !String  -- ^ First name
  }
```

For fields that require longer comments format them like so:

```haskell
data Record = Record
  { -- | This is a very very very long comment that is split over
    -- multiple lines.
    field1 :: !Text

    -- | This is a second very very very long comment that is split
    -- over multiple lines.
  , field2 :: !Int
  }
```

Naming
------

Use camel case (e.g. `functionName`) when naming functions and upper
camel case (e.g. `DataType`) when naming data types.  Use underscores
to separate words in variables and parameters.  For compound names
consisting of just two words, it is acceptable to not separate them at
all, e.g. `flatarrs` instead of `flat_arrs`.  If a variable or
parameter is also a function, use your judgement as to whether it is
most like a function or most like a value.

For readability reasons, don't capitalize all letters when using an
abbreviation.  For example, write `HttpServer` instead of
`HTTPServer`.  Exception: Two letter abbreviations, e.g. `IO`.

Use concise and short names, but do not abbreviate aggresively,
especially in complex names.  E.g, use `filter_result_size`, not
`flt_res_sz`.


Misc
----

### Functions

Avoid partial functions like `head` and `!`.  They can usually (always
in the case of `head`) be replaced with a `case`-expression that
provides a meaningful error message in the "impossible" case.

Do not use `map`, `mapM`, `zipWithM` or similar with a nontrivial
anonymous function.  Either give the function a name or use `forM`.

### Literate Haskell

Never use Literate Haskell.

### Warnings

Code should be compilable with `-Wall -Werror`. There should be no
warnings.  `hlint` should not complain (except for a few rules that we
have disabled - see `tools/style-check.sh`).

### Braces and semicolons

Never use braces and semicolons - always use whitespace-based layout
instead (except for generated code).

### Prefer `pure` to `return`

When writing monadic code, use `pure` instead of `return`.

Design Rules
============

  * We try not to use too many crazy language extensions.  Haskell is
    merely the implementation language, so we try to keep it simple,
    and we are not trying to push GHC to its limits.  Syntactic
    language extensions are fine, as are most extensions that are
    isolated to the module and do not show up in the external module
    interface.  Do not go overboard with type system trickery.  The
    trickery we do have already causes plenty of pain.

  * Be aware of [boolean
    blindness](https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/)
    and try to avoid it.  This helps significantly with avoiding
    partial functions.  E.g, if a list must never be empty, represent
    it as a pair of an element and a list instead of just a list.

Notes
=====

For long comments, we (try to) use the Notes convention from GHC,
[explained here](https://www.stackbuilders.com/news/the-notes-of-ghc).
Essentially, instead of writing very long in-line comments that break
the flow of the code, we write

```haskell
-- See Note [Foo Bar]
```

and then somewhere else in the file (perhaps at the bottom), we put

```haskell
-- Note [Foo Bar]
--
-- Here is how you foo the bar...
```

There is no automation around this or a hard rule for what a "long
comment" is.  It's just a convention.

Credits
=======

Based on [this style
guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md).
