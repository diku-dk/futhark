-- ==
-- input { [[0i64, 1i64, 2i64], [3i64, 4i64, 5i64], [7i64, 8i64, 9i64]]
--         [42i64, 1337i64, 0i64]
--         1i64
--       }
-- output { 596444i64
--          [[0i64, 1i64, 2i64],
--           [588i64, 595856i64, 0i64],
--           [7i64, 8i64, 9i64]]
--        }
-- structure gpu-mem { Alloc 2 }
-- structure seq-mem { Alloc 1 }

def main [n] (xs: *[n][n]i64) (a0: [n]i64) (i: i64) : (i64, [n][n]i64) =
  let a = map (\e -> e * e / 3) a0
  let b = if i > 5 then a[i:n] else a[0:n - i]
  let s = reduce (+) 0i64 b
  let xs[i] = a
  in (s, xs)
