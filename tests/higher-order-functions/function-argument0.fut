-- Simple monomorphic higher-order function that takes a function as argument.
-- ==
-- input { 3 } output { 5 12 }
-- input { 16 } output {18 64 }

let twice (f : i32 -> i32) (x : i32) : i32 = f (f x)

let double (x : i32) : i32 = x+x
let add1 (x : i32) : i32 = x+1

let main (x : i32) : (i32, i32) = (twice add1 x, twice double x)
