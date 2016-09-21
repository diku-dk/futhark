-- Constants cannot be used for return type shape declarations in
-- entry points.
--
-- ==
-- error:

val n: int = 3

fun main(): [n]int = replicate n 0
