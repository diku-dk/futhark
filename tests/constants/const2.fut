-- Can value declarations refer to each other?
--
-- ==
-- input { } output { 3 }

val x: int = 2
val y: int = x + 1

fun main(): int = y
