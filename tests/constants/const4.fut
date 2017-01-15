-- You can use a constant as a shape declaration in another constant.
--
-- ==
-- input { } output { [0,0,0] }

val n: i32 = 3

val x: [n]i32 = replicate n 0

fun main(): []i32 = x
