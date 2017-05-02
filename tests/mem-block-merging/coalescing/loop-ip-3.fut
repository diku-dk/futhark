-- Very Simple Example of Loop Coalescing.
-- ==
-- input {  [ [ [1,2], [3,4] ], [ [5,6], [7,8] ] ]
--          [ [1,3], [5, 7] ]
--       }
-- output {
--           [ [ [9i32, 2i32], [3i32, 4i32] ]
--           , [ [2i32, 4i32], [6i32, 8i32] ]
--           ]
--        }

import "/futlib/array"

-- Code below should result in 1 mem-block coalescing,
-- corresponding to 4 coalesced variables.
-- The statement `let a1[i] = x` should NOT result in colaescing
-- because `a1` is used in the computation of `x = map (+1) (a1[i])`,
-- hence `x` cannot share the memory block of `a1`.
-- This can potentially be done during register allocation stage.
let main(y: *[#n][#n][#n]i32, a : [#n][#n]i32): *[n][n][n]i32 =
  let y[0,0,0] = 9
  let a0 = copy(a)
  loop(a1 = a0) = for i < n do
    let x = map (+1) (a1[i])
    let a1[i] = x
    in  a1
 
  let y[n/2] = a1
  in  y
