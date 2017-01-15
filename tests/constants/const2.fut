-- Can value declarations refer to each other?
--
-- ==
-- input { } output { 3 }

val x: i32 = 2
val y: i32 = x + 1

fun main(): i32 = y
