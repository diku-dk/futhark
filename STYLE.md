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

Formatting
----------

### Line Length

Maximum line length is *80 characters*.

### Indentation

Tabs are illegal. Use spaces for indenting.  Indent your code blocks
with *2 spaces*.  E.g, we write

```haskell
foo x = do
  case whatever x of
    Just bar ->
      ...
    Nothing ->
      ...
```

It is okay to use a different number of spaces for alignment with
other syntactic structures, e.g:

```haskell
case foo of Just x  -> do a
                          b
            Nothing -> do c
                          d
```

Tasteful alignment can made code easier to read, but do not go
overboard.  The functions `inputs` and `lambda` in
`Futhark.Analysis.HORepresentation.SOAC` are a good example of
pointless use of alignment, and are retained to serve as a reminder of
this.

If in doubt, don't align.  Spurious alignment makes the code feel weird
and off-key, and it can be remarkably ugly when not maintained.  Alignment
in expressions is usually a bad idea.

### Blank Lines

One blank line between top-level definitions.  No blank lines between
type signatures and function definitions, except in very large
functions, where each clause is also separated by whitespace.  Add one
blank line between functions in a type class instance declaration if
the functions bodies are large.  Use your judgement.

In large `do`-blocks, separate logically separate chunks of code with
a single blank line.

### Whitespace

Generally, surround binary operators with a single space on either
side.  Use your better judgement for the insertion of spaces around
arithmetic operators but always be consistent about whitespace on
either side of a binary operator.  Don't insert a space after a
lambda.  Trailing whitespace is not permitted.  Always have a space on
each side of '<-' and '='.  E.g, this is bad:

```haskell
x<- foo
let y =bar
```

But this is good:

```
x <- foo
let y = bar
```

### Data Declarations

Align the constructors in a data type definition.  Example:

```haskell
data Tree a = Branch !a !(Tree a) !(Tree a)
            | Leaf
```

For long type names the following formatting is also acceptable:

```haskell
data HttpException
    = InvalidStatusCode Int
    | MissingContentHeader
```

Format records as follows:

```haskell
data Person = Person
    { firstName :: !String  -- ^ First name
    , lastName  :: !String  -- ^ Last name
    , age       :: !Int     -- ^ Age
    } deriving (Eq, Show)
```

### List Declarations

Align the elements in the list.  Example:

```haskell
exceptions =
    [ InvalidStatusCode
    , MissingContentHeader
    , InternalServerError
    ]
```

Optionally, you can skip the first newline.  Use your judgement.

```haskell
directions = [ North
             , East
             , South
             , West
             ]
```

### Long Expressions

Long expressions should be split over multiple lines.  If splitting at
an operator use your judgement as to whether the operator goes at the
end or beginning of a line.  In the case of `$`, always put it at the
end of the line.  If splitting a function call, use indentation to
make more readable.

If splitting a definition, put a linebreak just after `=` or `<-`, and
indent the following expression with two spaces.  E.g,

```haskell
someAtrociouslyLongVariableName <- someFunction $ someOtherFunction withSomeVar $ someThirdFunction something $ map somethingElse
```

Should be:

```haskell
someAtrociouslyLongVariableName <-
  someFunction $
    someOtherFunction withSomeVar $
    someThirdFunction something $
    map somethingElse
```

This would also be acceptable:

```haskell
someAtrociouslyLongVariableName <-
  someFunction $ someOtherFunction withSomeVar $
                 someThirdFunction something $
                 map somethingElse
```

### Long Type Signatures

Formatting of long type signature are different from long expressions,
and should be done as

```haskell
analyseBindings :: Lore lore =>
                   [In.Binding lore]
                -> ([Out.Binding lore] -> RangeM a)
                -> RangeM a
```

### Hanging Lambdas

You may or may not indent the code following a "hanging" lambda.  Use
your judgement. Some examples:

```haskell
bar :: IO ()
bar = forM_ [1, 2, 3] $ \n -> do
        putStrLn "Here comes a number!"
        print n

foo :: IO ()
foo = alloca 10 $ \a ->
      alloca 20 $ \b ->
      cFunction a b
```

### Export Lists

Format export lists as follows:

```haskell
module Data.Set
    (
      -- * The @Set@ type
      Set
    , empty
    , singleton

      -- * Querying
    , member
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

When writing non-monadic code (i.e. when not using `do`) and guards
and pattern matches can't be used, you can align if-then-else clauses
like you would normal expressions:

```haskell
foo = if ...
      then ...
      else ...
```

Otherwise, you should be consistent with the 2-spaces indent rule, and
the `then` and the `else` keyword should be aligned.  Examples:

```haskell
foo = do
  someCode
  if condition
    then someMoreCode
    else someAlternativeCode
```

```haskell
foo = bar $ \qux -> if predicate qux
  then doSomethingSilly
  else someOtherCode
```

The same rule applies to nested do blocks:

```haskell
foo = do
  instruction <- decodeInstruction
  skip <- load Memory.skip
  if skip == 0x0000
    then do
      execute instruction
      addCycles $ instructionCycles instruction
    else do
      store Memory.skip 0x0000
      addCycles 1
```

### Pattern matching

Prefer pattern-matching in function clauses to `case`.  Consider using
[view patterns](https://ghc.haskell.org/trac/ghc/wiki/ViewPatterns),
but be careful not to go overboard.

### Case expressions

The alternatives in a case expression can be indented using either of
the two following styles:

```haskell
foobar = case something of
    Just j  -> foo
    Nothing -> bar
```

or as

```haskell
foobar = case something of
             Just j  -> foo
             Nothing -> bar
```

Align the `->` arrows when it helps readability.

Imports
-------

Imports should be grouped in the following order:

1. standard library imports
2. related third party imports
3. local application/library specific imports

Put a blank line between each group of imports.  The imports in each
group should be sorted alphabetically, by module name.

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
all, e.g. `flatarrs` instead of `flat_arrs.  If a variable or
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

Credits
=======

Based on [this style
guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md).
