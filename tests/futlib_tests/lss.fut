-- ==
-- input { [1, -2, -2, 3, 4, -1, 5, -6, 1] }
-- output { 4 3 }
-- input { [5, 4, 3, 2, 1] }
-- output { 1 5 }
-- input { [1, 2, 3, 4, 5] }
-- output { 5 1 }

import "/futlib/lss"

let lss_ascending = lss 0 (const true) (<=)
let lss_descending = lss 0 (const true) (>=)

let main(xs: []i32): (i32,i32) =
  (lss_ascending xs,
   lss_descending xs)
