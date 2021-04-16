-- Ascription must respect uniqueness.
-- ==
-- error: \*\[d\]i32

module type mt = { val f : i32 -> ?[d].*[d]i32 }
module m = { let f (n: i32): []i32 = [n] } : mt
