-- Test that we can distinguish function application with literal
-- array argument from array indexing.
-- ==
-- input { 1 } output { 3 }

let f(xs: []i32): i32 = xs[0]

let a: []i32 = [1,2,3]

let main(x: i32): i32 =
  f [x] + a[x]
