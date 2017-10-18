-- Memory block merging with a copy and a reshape.
-- ==
-- input { [1, 2, 3, 4] }
-- output { [[2, 3, 4, 5]] }

-- structure cpu { Alloc 1 }
-- structure gpu { Alloc 1 }

import "/futlib/array"

let main [n] (ns: [n]i32): [1][n]i32 =
  -- Will both use the memory of t1.
  let t0 = map (+ 1) ns
  let t0a = reshape (1, n) t0

  let t1 = copy t0a
  in t1
