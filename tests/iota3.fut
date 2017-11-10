-- Slicing a non-i32 iota.  We must still be able to get rid of the
-- slice.
--
-- ==
-- input { 255u8 250u8 } output { [250u8, 251u8, 252u8, 253u8, 254u8] }
-- structure { Index 0 }

import "/futlib/math"

let main(x: u8, i: u8) =
  let a = u8.iota x
  in a[i32.u8 i:]
