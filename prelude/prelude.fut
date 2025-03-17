-- | The default prelude that is implicitly available in all Futhark
-- files.

open import "soacs"
open import "array"
open import "math"
open import "functional"
open import "ad"
open import "properties"

-- | Create single-precision float from integer.
def r32 (x: i32): f32 = f32.i32 x
-- | Create integer from single-precision float.
def t32 (x: f32): i32 = i32.f32 x

-- | Create double-precision float from integer.
def r64 (x: i32): f64 = f64.i32 x
-- | Create integer from double-precision float.
def t64 (x: f64): i32 = i32.f64 x

-- | Negate a boolean.  `not x` is the same as `!x`.  This function is
-- mostly useful for passing to `map`.
def not (x: bool): bool = !x

-- | Semantically just identity, but serves as an optimisation
-- inhibitor.  The compiler will treat this function as a black box.
-- You can use this to work around optimisation deficiencies (or
-- bugs), although it should hopefully rarely be necessary.
-- Deprecated: use `#[opaque]` attribute instead.
def opaque 't (x: t): t =
  #[opaque] x

-- | Semantically just identity, but at runtime, the argument value
-- will be printed.  Deprecated: use `#[trace]` attribute instead.
def trace 't (x: t): t =
  #[trace(trace)] x

-- | Semantically just identity, but acts as a break point in
-- `futhark repl`.  Deprecated: use `#[break]` attribute instead.
def break 't (x: t): t =
  #[break] x
