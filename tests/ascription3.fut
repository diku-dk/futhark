-- Array type ascription cannot change the rank of an array.
--
-- ==
-- error: must be one of

fun main(x: [n][m]i32) = x : []i32
