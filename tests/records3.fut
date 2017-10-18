-- Test tuple patterns.
-- ==
-- input { 2 } output { 3 1 }

let f(x: i32) = {a=x+1,b=x-1}

let main(x: i32) =
  let {a, b=c} = f x
  in (a,c)
