-- Note: this file is known to the compiler, and along with its
-- dependencies, it is embedded into the (compiler) binary.  Make sure
-- it does not depend on anything too big to be serialised
-- efficiently.

open import "/futlib/array"
open import "/futlib/math"

-- | Create single-precision float from integer.
let r32 (x: i32): f32 = f32.i32 x
-- | Create integer from single-precision float.
let t32 (x: f32): i32 = i32.f32 x

-- | Create double-precision float from integer.
let r64 (x: i32): f64 = f64.i32 x
-- | Create integer from double-precision float.
let t64 (x: f64): i32 = i32.f64 x

let (|>) 'a '^b (x: a) (f: a -> b): b = f x

let (<|) 'a '^b (f: a -> b) (x: a) = f x

let (|>>) 'a '^b '^c (f: a -> b) (g: b -> c) (x: a): c = g (f x)

let (<<|) 'a '^b '^c (g: b -> c) (f: a -> b) (x: a): c = g (f x)

let flip '^a '^b '^c (f: a -> b -> c) (b: b) (a: a): c =
  f a b

let curry '^a '^b '^c (f: (a, b) -> c) (a: a) (b: b): c =
  f (a, b)

let uncurry '^a '^b '^c (f: a -> b -> c) (a: a, b: b): c =
  f a b
