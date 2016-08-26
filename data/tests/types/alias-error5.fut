-- No tricky circular types!
--
-- ==
-- error: .*cycl.*

type t0 = []t1
type t1 = (int, float, t2)
type t2 = t0

fun main(x: t1): t1 = x
