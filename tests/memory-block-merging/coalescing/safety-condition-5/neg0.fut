-- t0 cannot be coalesced into t1, since the index function includes i1, which
-- depends on the result of t0.
-- ==
-- input { [0, 1]
--         [[5, 5], [5, 5]]
--         0
--       }
-- output { [[5, 5], [1, 2]]
--        }
-- structure seq-mem { Alloc 1 }
-- structure gpu-mem { Alloc 1 }

def main [n] (ns: [n]i32) (t1: *[n][n]i32) (i0: i32) : [][]i32 =
  let t0 = map (+ 1) ns
  let i1 = t0[i0]
  let t1[i1] = t0
  in t1
