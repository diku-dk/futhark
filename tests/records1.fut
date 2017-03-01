-- Tuples can be used like records.
-- ==
-- input { 2 } output { 3 1 }

fun f(x: i32) = (x+1,x-1)

fun main(x: i32) =
  let r = f x
  in (#1 r, #2 r)
