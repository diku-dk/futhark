-- Test that a map containing a (variant) replicate becomes a fully
-- parallel kernel, with no replicate.
--
-- ==
-- input { [1,2,3] 2 }
-- output { [[1,1], [2,2], [3,3]] }
-- structure distributed { MapKernel 1 Replicate 0 }

fun [[int,m],n] main([int,n] xs, int m) =
  map(fn [int,m] (int x) =>
        replicate(m, x),
      xs)
