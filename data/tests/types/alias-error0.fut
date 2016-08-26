-- No circular types!
--
-- ==
-- error: .*cycl.*

type t = t

fun main(x: t): t = x
