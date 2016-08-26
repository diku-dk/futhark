-- No tricky circular types!
--
-- ==
-- error: .*cycl.*

type t0 = []t1
type t1 = []t0

fun main(x: t1): t1 = x
