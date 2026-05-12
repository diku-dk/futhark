-- ==
-- input {  0 } output {  1 }
-- input { 10 } output { 89 }

def fib (n: i32) : i32 =
  let (x, _) = loop (x, y) = (1, 1) for _i < n do (y, x + y)
  in x

def main (n: i32) : i32 = fib (n)
