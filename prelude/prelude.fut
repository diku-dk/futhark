-- | The default prelude that is implicitly available in all Futhark
-- files.

open import "soacs"
open import "array"
open import "math"
open import "functional"
open import "ad"

-- | Create single-precision float from integer.
let r32 (x: i32): f32 = f32.i32 x
-- | Create integer from single-precision float.
let t32 (x: f32): i32 = i32.f32 x

-- | Create double-precision float from integer.
let r64 (x: i32): f64 = f64.i32 x
-- | Create integer from double-precision float.
let t64 (x: f64): i32 = i32.f64 x

-- | Semantically just identity, but serves as an optimisation
-- inhibitor.  The compiler will treat this function as a black box.
-- You can use this to work around optimisation deficiencies (or
-- bugs), although it should hopefully rarely be necessary.
-- Deprecated: use `#[opaque]` attribute instead.
let opaque 't (x: t): t =
  #[opaque] x

-- | Semantically just identity, but at runtime, the argument value
-- will be printed.  Deprecated: use `#[trace]` attribute instead.
let trace 't (x: t): t =
  #[trace(trace)] x

-- | Semantically just identity, but acts as a break point in
-- `futhark repl`.  Deprecated: use `#[break]` attribute instead.
let break 't (x: t): t =
  #[break] x
