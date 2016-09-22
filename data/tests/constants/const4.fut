-- You can use a constant as a shape declaration in another constant.
--
-- ==
-- input { } output { [0,0,0] }

val n: int = 3

val x: [n]int = replicate n 0

fun main(): []int = x
