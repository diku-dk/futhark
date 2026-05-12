-- ==
-- entry: success
-- input { [0i64, 1i64, 2i64, 3i64, 4i64, 5i64, 6i64, 7i64, 8i64, 9i64] }
-- output { [0i64, 6i64, 7i64, 0i64, 4i64, 5i64, 6i64, 7i64, 8i64, 9i64] }
-- structure gpu-mem { Alloc 0 }
-- structure seq-mem { Alloc 0 }

entry success [n] (xs: *[n]i64) : *[n]i64 =
  let b = replicate 4 0
  let b[1:3] = xs[6:8]
  in xs with [:4] = b
