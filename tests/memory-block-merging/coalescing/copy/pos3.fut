-- Memory block merging with a copy into a multidimensional array.
-- ==
-- input { 1i64
--         [6, 0, 7]
--         [[-1, 0, 1],
--          [-1, 0, 1],
--          [-1, 0, 1]]
--       }
-- output { [[0, 1, 2],
--           [7, 1, 8],
--           [0, 1, 2]]
--        }
-- structure seq-mem { Alloc 1 }
-- structure gpu-mem { Alloc 2 }

def main [n] (i: i64) (ns: [n]i32) (mss: [n][n]i32) : [n][n]i32 =
  -- This is the basis array in which everything will be put.
  let t1 = map (\ms -> map (+ 1) ms) mss
  let k = t1[0, 1]
  let t0 = map (+ k) ns
  -- Will use the memory of t1[i].

  let t1[i] = t0
  in t1
