-- Test unary operators for i16.
--
-- ==
-- input { 0  0i16 } output {  0i16 }
-- input { 0  1i16 } output { -1i16 }
-- input { 0 -1i16 } output {  1i16 }
-- input { 0  8i16 } output { -8i16 }
-- input { 0 -8i16 } output {  8i16 }
--
-- input { 1  0i16 } output { 0i16 }
-- input { 1  1i16 } output { 1i16 }
-- input { 1 -1i16 } output { 1i16 }
-- input { 1  8i16 } output { 8i16 }
-- input { 1 -8i16 } output { 8i16 }
--
-- input { 2  0i16 } output {  0i16 }
-- input { 2  1i16 } output {  1i16 }
-- input { 2 -1i16 } output { -1i16 }
-- input { 2  8i16 } output {  1i16 }
-- input { 2 -8i16 } output { -1i16 }

import "/futlib/math"

let main(f: i32, x: i16): i16 =
  if      f == 0 then -x
  else if f == 1 then i16.abs(x)
  else                i16.sgn(x)
