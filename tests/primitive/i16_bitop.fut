-- Bitwise operations on i16 values.
--
-- ==
-- input { 0  0i16  0i16 } output {  0i16 }
-- input { 0  0i16  1i16 } output {  0i16 }
-- input { 0  0i16 -1i16 } output {  0i16 }
-- input { 0  1i16  0i16 } output {  0i16 }
-- input { 0  1i16  1i16 } output {  1i16 }
-- input { 0  1i16 -1i16 } output {  1i16 }
-- input { 0 -1i16  0i16 } output {  0i16 }
-- input { 0 -1i16  1i16 } output {  1i16 }
-- input { 0 -1i16 -1i16 } output { -1i16 }
--
-- input { 1  0i16  0i16 } output {  0i16 }
-- input { 1  0i16  1i16 } output {  1i16 }
-- input { 1  0i16 -1i16 } output { -1i16 }
-- input { 1  1i16  0i16 } output {  1i16 }
-- input { 1  1i16  1i16 } output {  1i16 }
-- input { 1  1i16 -1i16 } output { -1i16 }
-- input { 1 -1i16  0i16 } output { -1i16 }
-- input { 1 -1i16  1i16 } output { -1i16 }
-- input { 1 -1i16 -1i16 } output { -1i16 }
-- input { 1 64i16 32i16 } output { 96i16 }
--
-- input { 2  0i16  0i16 } output {  0i16 }
-- input { 2  0i16  1i16 } output {  1i16 }
-- input { 2  0i16 -1i16 } output { -1i16 }
-- input { 2  1i16  0i16 } output {  1i16 }
-- input { 2  1i16  1i16 } output {  0i16 }
-- input { 2  1i16 -1i16 } output { -2i16 }
-- input { 2 -1i16  0i16 } output { -1i16 }
-- input { 2 -1i16  1i16 } output { -2i16 }
-- input { 2 -1i16 -1i16 } output {  0i16 }
-- input { 2 64i16 32i16 } output { 96i16 }
--
-- input { 3  0i16  0i16 } output {  0i16 }
-- input { 3  0i16  1i16 } output {  0i16 }
-- input { 3  1i16  0i16 } output {  1i16 }
-- input { 3  1i16  1i16 } output {  2i16 }
-- input { 3 -1i16  0i16 } output { -1i16 }
-- input { 3 -1i16  1i16 } output { -2i16 }
--
-- input { 4  0i16  0i16 } output {  0i16 }
-- input { 4  0i16  1i16 } output {  0i16 }
-- input { 4  1i16  0i16 } output {  1i16 }
-- input { 4  1i16  1i16 } output {  0i16 }
-- input { 4  2i16  1i16 } output {  1i16 }
-- input { 4 -1i16  0i16 } output { -1i16 }
-- input { 4 -1i16  1i16 } output { -1i16 }
--
-- input { 5  0i16  0i16 } output {  0i16 }
-- input { 5  0i16  1i16 } output {  0i16 }
-- input { 5  1i16  0i16 } output {  1i16 }
-- input { 5  1i16  1i16 } output {  0i16 }
-- input { 5  2i16  1i16 } output {  1i16 }
-- input { 5 -1i16  0i16 } output { -1i16 }
-- input { 5 -1i16  1i16 } output { 32767i16 }

fun main(f: i32, x: i16, y: i16): i16 =
  if      f == 0 then x & y
  else if f == 1 then x | y
  else if f == 2 then x ^ y
  else if f == 3 then x << y
  else if f == 4 then x >> y
  else                x >>> y
