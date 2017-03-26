-- Can a constant be an array of tuples?
--
-- ==
-- input {} output { 3 }

let v: [](i32,i32) = [(1,2)]

fun main(): i32 = let (x,y) = v[0]
                  in x + y
