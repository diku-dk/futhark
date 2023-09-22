-- Also not actually invariant.
-- ==
-- input {  0 } output {  1 }
-- input { 10 } output { 89 }

entry main (n: i32) =
  let (x,_) = loop (x, y) = (1,1) for _i < n do (y, x+y)
  in x
