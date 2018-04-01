-- Note: this file is known to the compiler, and along with its
-- dependencies, it is embedded into the (compiler) binary.  Make sure
-- it does not depend on anything too big to be serialised
-- efficiently.

open import "/futlib/soacs"
open import "/futlib/array"
open import "/futlib/math"
open import "/futlib/functional"

-- | Create single-precision float from integer.
let r32 (x: i32): f32 = f32.i32 x
-- | Create integer from single-precision float.
let t32 (x: f32): i32 = i32.f32 x

-- | Create double-precision float from integer.
let r64 (x: i32): f64 = f64.i32 x
-- | Create integer from double-precision float.
let t64 (x: f64): i32 = i32.f64 x
