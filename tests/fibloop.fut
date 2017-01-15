-- ==
-- input {  0 } output {  1 }
-- input { 10 } output { 89 }


fun fib(n: i32): i32 =
  loop ((x, y) = (1,1)) = for i < n do
                            (y, x+y)
  in x

fun main(n: i32): i32 = fib(n)
