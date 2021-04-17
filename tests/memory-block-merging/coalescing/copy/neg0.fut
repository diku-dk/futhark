-- Memory block merging with a copy into a multidimensional array.  Very similar
-- to pos3.fut, but this should not coalesce in the CPU pipeline (see body
-- comment).
-- ==
-- input { 1
--         [6, 0, 7]
--         [[-1, 0, 1],
--          [-1, 0, 1],
--          [-1, 0, 1]]
--       }
-- output { [[0, 1, 2],
--           [7, 1, 8],
--           [0, 1, 2]]
--        }
-- structure cpu { Alloc 2 }
-- structure gpu { Alloc 1 }

import "/futlib/array"

let main [n] (i: i32, ns: [n]i32, mss: [n][n]i32): [n][n]i32 =
  -- For the CPU pipeline, t1 and t0 can be fused into a single outer map.  This
  -- makes it impossible to coalesce, since mem_t1 is used after the creation of
  -- t0 through its use in the same map body as t0.
  --
  -- The fusion does not happen in the GPU pipeline, so in that case it is the
  -- same as pos3.fut, meaning it gets a coalescing.
  let t1 = map (\ms -> map (+ 1) ms) mss

  let k = 1
  let t0 = map (+ k) ns

  let t1[i] = t0
  in t1
