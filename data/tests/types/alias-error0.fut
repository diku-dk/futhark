-- No circular types!
--
-- ==
-- error: .*cycl.*

type t = t

fun t main(t x) = x
