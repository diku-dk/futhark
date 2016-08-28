-- Test that a map containing a (variant) replicate becomes a fully
-- parallel kernel, with no replicate.
--
-- ==
-- input { [1,2,3] 2 }
-- output { [[1,1], [2,2], [3,3]] }
-- structure distributed { Kernel 1 }

fun main(xs: [n]int, m: int): [n][m]int =
  map (fn (x: int): [m]int  =>
        replicate m x) xs
