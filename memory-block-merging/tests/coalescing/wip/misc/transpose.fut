-- We can choose to coalesce here, or we can choose not to.  Do we want to
-- coalesce memory with an index function of a rearrange?  We can save memory
-- allocation, but at the cost of bad memory access.
-- ==
-- input { [[1, 2],
--          [3, 4]]
--       }
-- output { [[2, 6],
--           [4, 8]]
--        }

-- structure cpu { Alloc ? }
-- structure gpu { Alloc ? }

import "/futlib/array"

let main (x: [#n][#n]i32): [n][n]i32 =
  let y = map (\t -> map (* 2) t) x
  let y' = transpose y
  let z = copy y'
  in z
