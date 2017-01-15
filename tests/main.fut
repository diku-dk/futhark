-- It is okay to call the main function explicitly.
-- ==
-- input { 3 } output { 6 }

fun main(n: i32): i32 = if n == 0 then 1 else n * main (n-1)
