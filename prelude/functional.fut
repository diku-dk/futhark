-- | Simple functional combinators.

-- | Left-to-right application.  Particularly useful for describing
-- computation pipelines:
--
-- ```
-- x |> f |> g |> h
-- ```
def (|>) '^a '^b (x: a) (f: a -> b): b = f x

-- | Right to left application.
--
-- Due to the causality restriction (see the language reference) this
-- is less useful than `|>`@term.  For example, the following is
-- a type error:
--
-- ```
-- length <| filter (>0) [-1,0,1]
-- ```
--
-- But this works:
--
-- ```
-- filter (>0) [-1,0,1] |> length
-- ```
def (<|) '^a '^b (f: a -> b) (x: a) = f x

-- | Function composition, with values flowing from left to right.
--
-- Note that functions with anonymous return sizes cannot be composed.
-- For example, the following is a type error:
--
-- ```
-- filter (>0) >-> length
-- ```
--
-- In such cases you can use the pipe operator `|>`@term instead.
def (>->) '^a '^b '^c (f: a -> b) (g: b -> c) (x: a): c = g (f x)

-- | Function composition, with values flowing from right to left.
-- This is the same as the `∘` operator known from mathematics.
--
-- Has the same restrictions with respect to anonymous sizes as
-- `>->`@term.
def (<-<) '^a '^b '^c (g: b -> c) (f: a -> b) (x: a): c = g (f x)

-- | Flip the arguments passed to a function.
--
-- ```
-- f x y == flip f y x
-- ```
def flip '^a '^b '^c (f: a -> b -> c) (b: b) (a: a): c =
  f a b

-- | Transform a function taking a pair into a function taking two
-- arguments.
def curry '^a '^b '^c (f: (a, b) -> c) (a: a) (b: b): c =
  f (a, b)

-- | Transform a function taking two arguments in a function taking a
-- pair.
def uncurry '^a '^b '^c (f: a -> b -> c) (a: a, b: b): c =
  f a b

-- | The constant function.
def const '^a '^b (x: a) (_: b): a = x

-- | The identity function.
def id '^a (x: a) = x

-- | Apply a function some number of times.
def iterate 'a (n: i32) (f: a -> a) (x: a) =
  loop x for _i < n do f x

-- | Keep applying `f` until `p` returns true for the input value.
-- May apply zero times.  *Note*: may not terminate.
def iterate_until 'a (p: a -> bool) (f: a -> a) (x: a) =
  loop x while !(p x) do f x

-- | Keep applying `f` while `p` returns true for the input value.
-- May apply zero times.  *Note*: may not terminate.
def iterate_while 'a (p: a -> bool) (f: a -> a) (x: a) =
  loop x while p x do f x
