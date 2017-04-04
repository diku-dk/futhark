-- Using the same parameter name twice is forbidden.
--
-- ==
-- error: Duplicate.*x.*

let main (x: i32) (x: i32): i32 = x
