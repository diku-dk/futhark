-- Test unary operators for u16.
--
-- ==
-- input { 0  0u16 } output {  0u16 }
-- input { 0  1u16 } output { 65535u16 }
-- input { 0 65535u16 } output {  1u16 }
-- input { 0  8u16 } output { 65528u16 }
-- input { 0 65528u16 } output {  8u16 }
--
-- input { 1  0u16 } output { 0u16 }
-- input { 1  1u16 } output { 1u16 }
-- input { 1 65535u16 } output { 65535u16 }
-- input { 1  8u16 } output { 8u16 }
-- input { 1 65528u16 } output { 65528u16 }
--
-- input { 2  0u16 } output { 0u16 }
-- input { 2  1u16 } output { 1u16 }
-- input { 2 65535u16 } output { 1u16 }
-- input { 2  8u16 } output { 1u16 }
-- input { 2 65528u16 } output { 1u16 }

import "/futlib/math"

let main (f: i32) (x: u16): u16 =
  if      f == 0 then -x
  else if f == 1 then u16.abs(x)
  else                u16.sgn(x)
