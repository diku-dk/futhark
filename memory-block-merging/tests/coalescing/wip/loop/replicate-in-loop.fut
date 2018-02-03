-- A replicate in a loop, with a subsequent loop after the replicate.  The
-- coalescing transformation must make sure not to coalesce the loop into the
-- return value.
-- ==
-- input { [3, 6]
--         2
--       }
-- output { [5, 8]
--        }
-- structure cpu { Alloc 2 }
-- structure gpu { Alloc 2 }

import "/futlib/array"

let main [n] (a0: [n]i32, n_iter: i32): []i32 =
  let a2 = loop a = a0 for _i < n_iter do
    let b0 = replicate n 0
    let a' = loop b = b0 for j < n do
      let b[j] = a[j] + 1
      in b
    in a'
  let a3 = copy a2
  in a3
