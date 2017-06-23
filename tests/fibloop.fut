-- ==
-- input {  0 } output {  1 }
-- input { 10 } output { 89 }


let fib(n: i32): i32 =
  let (x,_) = loop ((x, y) = (1,1)) for i < n do (y, x+y)
  in x

let main(n: i32): i32 = fib(n)
