-- Test that a map containing a (variant) replicate becomes a fully
-- parallel kernel, with no replicate.
--
-- ==
-- input { [1,2,3] 2 }
-- output { [[1,1], [2,2], [3,3]] }
-- structure distributed { MapKernel 1 Replicate 0 }

fun [n][m]int main([n]int xs, int m) =
  map(fn [m]int (int x) =>
        replicate(m, x),
      xs)
