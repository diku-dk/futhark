-- If the accumulator isn't updated, the entire thing should go away.
-- This is not because of user code (nobody would write this), but
-- because the compiler may internally generate code like this
-- (possibly after other simplifications).
-- ==
-- structure { WithAcc 0 }

import "intrinsics"

let main (xs: *[]i32) =
  scatter_stream xs (\acc _ -> acc) (iota 10)
