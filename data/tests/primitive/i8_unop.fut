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

fun i8 main(int f, i8 x) =
  if      f == 0 then -x
  else if f == 1 then abs(x)
  else                signum(x)
