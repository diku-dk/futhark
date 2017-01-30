-- No circular types!
--
-- ==
-- error: Unknown type

type t = t

fun main(x: t): t = x
