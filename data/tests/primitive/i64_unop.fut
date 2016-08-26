-- Test unary operators for i64.
--
-- ==
-- input { 0  0i64 } output {  0i64 }
-- input { 0  1i64 } output { -1i64 }
-- input { 0 -1i64 } output {  1i64 }
-- input { 0  8i64 } output { -8i64 }
-- input { 0 -8i64 } output {  8i64 }
--
-- input { 1  0i64 } output { 0i64 }
-- input { 1  1i64 } output { 1i64 }
-- input { 1 -1i64 } output { 1i64 }
-- input { 1  8i64 } output { 8i64 }
-- input { 1 -8i64 } output { 8i64 }
--
-- input { 2  0i64 } output {  0i64 }
-- input { 2  1i64 } output {  1i64 }
-- input { 2 -1i64 } output { -1i64 }
-- input { 2  8i64 } output {  1i64 }
-- input { 2 -8i64 } output { -1i64 }

fun main(f: int, x: i64): i64 =
  if      f == 0 then -x
  else if f == 1 then abs(x)
  else                signum(x)
