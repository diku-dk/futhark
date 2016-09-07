-- Check that shape annotations are checked when calling a function.
--
-- ==
-- input { True [1,2] [3,4] }
-- output { 4 }
-- input { True [1,2] [3] }
-- error:

-- Recursive to prevent inlining.
fun f(b: bool, as: [n]int, bs: [n]int): int =
  as[0] + bs[0] + if b
                  then 0
                  else f(b, as, bs)

-- Note that the as and bs arrays may have divergent sizes as far as
-- main is concerned.  It is only in the call to f that the checking
-- happens (so we can't realy on the input parser to do the check for
-- us).
fun main(b: bool, as: [n]int, bs: [m]int): int =
  f(b, as, bs)
