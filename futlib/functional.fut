-- | Simple functional combinators.

let (|>) '^a '^b (x: a) (f: a -> b): b = f x

let (<|) '^a '^b (f: a -> b) (x: a) = f x

let (|>>) '^a '^b '^c (f: a -> b) (g: b -> c) (x: a): c = g (f x)

let (<<|) '^a '^b '^c (g: b -> c) (f: a -> b) (x: a): c = g (f x)

let flip '^a '^b '^c (f: a -> b -> c) (b: b) (a: a): c =
  f a b

let curry '^a '^b '^c (f: (a, b) -> c) (a: a) (b: b): c =
  f (a, b)

let uncurry '^a '^b '^c (f: a -> b -> c) (a: a, b: b): c =
  f a b

-- | The constant function.
let const '^a '^b (x: a) (_: b): a = x

-- | The identity function.
let id '^a (x: a) = x
