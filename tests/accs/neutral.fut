-- Test that we can remove updates that are just the neutral element.
-- ==
-- structure { WithAcc 0 }

import "intrinsics"

let f (acc: *acc ([]i32)) i =
  let acc = write acc i 1
  let acc = write acc (i+1) 1
  in acc

let main (xs: *[]i32) =
  reduce_by_index_stream xs (*) 1 f (iota 10)
