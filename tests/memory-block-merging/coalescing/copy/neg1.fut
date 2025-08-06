-- Memory block merging with a copy into a multidimensional array given as a
-- function parameter.
-- ==
-- input { [[0, 1, 2],
--          [3, 4, 5],
--          [6, 7, 8]]
--         1i64
--         [7, 1, 5]
--       }
-- output { [[0, 1, 2],
--           [14, 10, 13],
--           [6, 7, 8]]
--        }
-- structure seq-mem { Alloc 1 }
-- structure gpu-mem { Alloc 1 }

def main [n] (t1: *[n][n]i32) (i: i64) (ns: [n]i32) : [n][n]i32 =
  let t0 = map3 (\x y z -> x + y + z) t1[i] ns (rotate 1 t1[i])
  -- Will use the memory of t1[i].

  -- This is the basis array in which everything will be put.
  let t1[i] = t0
  in t1
