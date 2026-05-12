-- Memory block merging with a copy into a multidimensional array.  Very similar
-- to pos3.fut, but this should not coalesce in the CPU pipeline (see body
-- comment).
--
-- NOTE: Due to a regression in the fusion engine, the seq-mem code is not
-- actually being fused, which means that it can be short-circuited:
-- https://github.com/diku-dk/futhark/issues/1733
--
-- When that bug has been fixed, there should be two seq-mem allocations instead
-- of one.
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
-- structure gpu-mem { Alloc 1 }

def main [n] (i: i64) (ns: [n]i32) (mss: [n][n]i32) : [n][n]i32 =
  -- For the CPU pipeline, t1 and t0 can be fused into a single outer map.  This
  -- makes it impossible to coalesce, since mem_t1 is used after the creation of
  -- t0 through its use in the same map body as t0.
  --
  -- The fusion does not happen in the GPU pipeline, so in that case it is the
  -- same as pos3.fut, meaning it gets a coalescing.
  let t1 = map (map (+ 1)) mss
  let k = 1
  let t0 = map (+ k) ns
  let t1[i] = t0
  in t1
