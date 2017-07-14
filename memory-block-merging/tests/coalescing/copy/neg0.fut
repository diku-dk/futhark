-- Memory block merging with a copy into a multidimensional array.  Very similar
-- to pos3.fut, but this should not coalesce (see body comment).
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

import "/futlib/array"

let main (i: i32, ns: [#n]i32, mss: [#n][#n]i32): [n][n]i32 =
  -- t1 and t0 can be fused into a single outer map.  This makes it impossible
  -- to coalesce, since mem_t1 is used after the creation of t0 through its use
  -- in the same map body as t0.
  let t1 = map (\ms -> map (+ 1) ms) mss

  let k = 1
  let t0 = map (+ k) ns

  let t1[i] = t0
  in t1
