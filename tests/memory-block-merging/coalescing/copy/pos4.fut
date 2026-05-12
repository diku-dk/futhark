-- Memory block merging with a copy into a multidimensional array given as a
-- function parameter.
-- ==
-- input { [[0, 1, 2],
--          [0, 1, 2],
--          [0, 1, 2]]
--         1i64
--         [7, 0, 7]
--       }
-- output { [[0, 1, 2],
--           [7, 1, 9],
--           [0, 1, 2]]
--        }
-- structure seq-mem { Alloc 0 }
-- structure gpu-mem { Alloc 0 }

def main [n] (t1: *[n][n]i32) (i: i64) (ns: [n]i32) : [n][n]i32 =
  let t0 = map2 (+) t1[i] ns
  -- Will use the memory of t1[i].

  -- This is the basis array in which everything will be put.
  let t1[i] = t0
  in t1
