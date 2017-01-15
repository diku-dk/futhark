-- Using the same parameter name twice is forbidden.
--
-- ==
-- error: Duplicate.*x.*

fun main (x: i32) (x: i32): i32 = x
