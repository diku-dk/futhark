-- ==
-- input { [0]   [0] }
-- output { 0 }
-- input { [1,2,3] [4,5,6] }
-- output { 32 }

include futlib.linalg

module I32LinAlg = LinAlg(I32)

fun main(as: [n]int, bs: [n]int): int =
  I32LinAlg.dotprod as bs
