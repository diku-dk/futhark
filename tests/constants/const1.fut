-- Can a constant be an array of tuples?
--
-- ==
-- input {} output { 3 }

val v: [](int,int) = [(1,2)]

fun main(): int = let (x,y) = v[0]
                  in x + y
