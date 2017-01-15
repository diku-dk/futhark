-- ==
-- input { [0]   [0] }
-- output { 0 }
-- input { [1,2,3] [4,5,6] }
-- output { 32 }

include futlib.linalg

module I32LinAlg = LinAlg(I32)

fun main(as: [n]i32, bs: [n]i32): i32 =
  I32LinAlg.dotprod as bs
