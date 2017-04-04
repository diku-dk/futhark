-- Test unary operators for u8.
--
-- ==
-- input { 0  0u8 } output {  0u8 }
-- input { 0  1u8 } output { 255u8 }
-- input { 0 255u8 } output {  1u8 }
-- input { 0  8u8 } output { 248u8 }
-- input { 0 248u8 } output {  8u8 }
--
-- input { 1  0u8 } output { 0u8 }
-- input { 1  1u8 } output { 1u8 }
-- input { 1 255u8 } output { 255u8 }
-- input { 1  8u8 } output { 8u8 }
-- input { 1 248u8 } output { 248u8 }
--
-- input { 2  0u8 } output { 0u8 }
-- input { 2  1u8 } output { 1u8 }
-- input { 2 255u8 } output { 1u8 }
-- input { 2  8u8 } output { 1u8 }
-- input { 2 248u8 } output { 1u8 }

import "futlib/math"

let main(f: i32, x: u8): u8 =
  if      f == 0 then -x
  else if f == 1 then u8.abs(x)
  else                u8.sgn(x)
