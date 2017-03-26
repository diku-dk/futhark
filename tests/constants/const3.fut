--
-- ==
-- input { } output { [0,0,0] }

let n: i32 = 3

fun f(): [n]i32 = replicate n 0

fun main(): []i32 = f ()
