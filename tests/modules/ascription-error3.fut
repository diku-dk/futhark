-- Ascription must respect uniqueness.
-- ==
-- error: \*\[\]i32

module type mt = { val f : i32 -> *[]i32 }
module m = { let f (n: i32): []i32 = [n] } : mt
