-- | Simple functional combinators.

-- | Left-to-right application.  Particularly useful for describing
-- computation pipelines:
--
--     x |> f |> g |> h
let (|>) '^a '^b (x: a) (f: a -> b): b = f x

-- | Right to left application.
let (<|) '^a '^b (f: a -> b) (x: a) = f x

-- | Function composition, with values flowing from left to right.
let (>->) '^a '^b '^c (f: a -> b) (g: b -> c) (x: a): c = g (f x)

-- | Function composition, with values flowing from right to left.
-- This is the same as the `∘` operator known from mathematics.
let (<-<) '^a '^b '^c (g: b -> c) (f: a -> b) (x: a): c = g (f x)

-- | Deprecated alias for `>->`@term.
let (|>>) = (>->)

-- | Deprecated alias for `<-<`@term.
let (<<|) = (<-<)

-- | Flip the arguments passed to a function.
--
--     f x y == flip f y x
let flip '^a '^b '^c (f: a -> b -> c) (b: b) (a: a): c =
  f a b

-- | Transform a function taking a pair into a function taking two
-- arguments.
let curry '^a '^b '^c (f: (a, b) -> c) (a: a) (b: b): c =
  f (a, b)

-- | Transform a function taking two arguments in a function taking a
-- pair.
let uncurry '^a '^b '^c (f: a -> b -> c) (a: a, b: b): c =
  f a b

-- | The constant function.
let const '^a '^b (x: a) (_: b): a = x

-- | The identity function.
let id '^a (x: a) = x

-- | Apply a function some number of times.
let iterate 'a (n: i32) (f: a -> a) (x: a) =
  loop x for _i < n do f x

-- | Keep applying `f` until `p` returns true for the input value.
-- May apply zero times.  *Note*: may not terminate.
let iterate_until 'a (p: a -> bool) (f: a -> a) (x: a) =
  loop x while ! (p x) do f x

-- | Keep applying `f` while `p` returns true for the input value.
-- May apply zero times.  *Note*: may not terminate.
let iterate_while 'a (p: a -> bool) (f: a -> a) (x: a) =
  loop x while p x do f x
