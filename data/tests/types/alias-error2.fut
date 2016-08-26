-- No unique non-arrays
--
-- ==
-- error: .*non-array.*

type t = int

fun main(x: *t): t = x
