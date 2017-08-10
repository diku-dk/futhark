-- Same as loop-existential.fut, except the inner map is manually transformed
-- into a loop.
-- ==
-- input { [3, 6]
--         2
--       }
-- output { [5, 8]
--        }

-- structure cpu { Alloc 1 }
-- structure gpu { Alloc 1 }

import "/futlib/array"

let main (a0: [#n]i32, n_iter: i32): []i32 =
  let a2 = loop a = a0 for _i < n_iter do
    let inner_loop_mem = replicate n 0
    let a' = loop mem = inner_loop_mem for j < n do
      -- This is a map in loop-existential.fut.
      let mem[j] = a[j] + 1
      in mem
    in a'
  let a3 = copy a2
  in a3
