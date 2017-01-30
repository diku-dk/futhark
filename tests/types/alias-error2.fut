-- No unique non-arrays
--
-- ==
-- error: .*non-array.*

type t = i32

fun main(x: *t): t = x
