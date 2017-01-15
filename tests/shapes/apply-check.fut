-- Check that shape annotations are checked when calling a function.
--
-- ==
-- input { true [1,2] [3,4] }
-- output { 4 }
-- input { true [1,2] [3] }
-- error:

-- Recursive to prevent inlining.
fun f(b: bool, as: [n]i32, bs: [n]i32): i32 =
  as[0] + bs[0] + if b
                  then 0
                  else f(b, as, bs)

-- Note that the as and bs arrays may have divergent sizes as far as
-- main is concerned.  It is only in the call to f that the checking
-- happens (so we can't realy on the input parser to do the check for
-- us).
fun main(b: bool, as: [n]i32, bs: [m]i32): i32 =
  f(b, as, bs)
