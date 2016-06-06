-- No tricky circular types!
--
-- ==
-- error: .*cycl.*

type t0 = [t1]
type t1 = (int, float, t2)
type t2 = t0

fun t1 main(t1 x) = x
