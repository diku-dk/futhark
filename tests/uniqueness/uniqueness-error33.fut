-- Type ascriptions must respect uniqueness.
-- ==
-- error: \*\[.*\]i32

let main (x: []i32) = x : *[]i32
