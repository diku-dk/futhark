-- Using the same parameter name twice is forbidden.
--
-- ==
-- error: also bound

let main (x: i32) (x: i32): i32 = x
