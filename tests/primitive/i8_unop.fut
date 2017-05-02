-- Test unary operators for i8.
--
-- ==
-- input { 0  0i8 } output {  0i8 }
-- input { 0  1i8 } output { -1i8 }
-- input { 0 -1i8 } output {  1i8 }
-- input { 0  8i8 } output { -8i8 }
-- input { 0 -8i8 } output {  8i8 }
--
-- input { 1  0i8 } output { 0i8 }
-- input { 1  1i8 } output { 1i8 }
-- input { 1 -1i8 } output { 1i8 }
-- input { 1  8i8 } output { 8i8 }
-- input { 1 -8i8 } output { 8i8 }
--
-- input { 2  0i8 } output {  0i8 }
-- input { 2  1i8 } output {  1i8 }
-- input { 2 -1i8 } output { -1i8 }
-- input { 2  8i8 } output {  1i8 }
-- input { 2 -8i8 } output { -1i8 }

import "/futlib/math"

let main(f: i32, x: i8): i8 =
  if      f == 0 then -x
  else if f == 1 then i8.abs(x)
  else                i8.sgn(x)
