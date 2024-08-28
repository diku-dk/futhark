-- Ascription must respect uniqueness.
-- ==
-- error: \*\[d\]i32

module type mt = { val f [n] : [n]i32 -> ?[d].*[d]i32 }
module m = { def f (ns: []i32): []i32 = ns } : mt
