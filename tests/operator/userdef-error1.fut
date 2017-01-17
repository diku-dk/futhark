-- You can't override ||.
-- ==
-- error: \|\|

fun (x: bool) || (y: bool) = x

fun main(x: bool) = x || x
