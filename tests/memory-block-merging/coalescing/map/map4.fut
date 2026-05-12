-- Even though there is an opportunity to short-circuit a memory allocation on
-- the GPU, we shouldn't because it will hurt coalesced access.
-- ==
-- input { [7i64, 8i64, 9i64] }
-- output { [[0i64, 8i64, 18i64],
--           [1i64, 9i64, 19i64],
--           [2i64, 10i64, 20i64]] }
-- compiled random input { [1024]i64 }
-- auto output
-- structure gpu-mem { Alloc 2 }
-- structure seq-mem { Alloc 1 }

def main [n] (xs: [n]i64) : [n][n]i64 =
  map (\j ->
         loop xs' = copy xs
         for i < n do
           let xs'[i] = xs'[i] * i + j
           in xs')
      (iota n)
