-- Bitwise operations on i32 values.
--
-- ==
-- input { 0  0i32  0i32 } output {  0i32 }
-- input { 0  0i32  1i32 } output {  0i32 }
-- input { 0  0i32 -1i32 } output {  0i32 }
-- input { 0  1i32  0i32 } output {  0i32 }
-- input { 0  1i32  1i32 } output {  1i32 }
-- input { 0  1i32 -1i32 } output {  1i32 }
-- input { 0 -1i32  0i32 } output {  0i32 }
-- input { 0 -1i32  1i32 } output {  1i32 }
-- input { 0 -1i32 -1i32 } output { -1i32 }
--
-- input { 1  0i32  0i32 } output {  0i32 }
-- input { 1  0i32  1i32 } output {  1i32 }
-- input { 1  0i32 -1i32 } output { -1i32 }
-- input { 1  1i32  0i32 } output {  1i32 }
-- input { 1  1i32  1i32 } output {  1i32 }
-- input { 1  1i32 -1i32 } output { -1i32 }
-- input { 1 -1i32  0i32 } output { -1i32 }
-- input { 1 -1i32  1i32 } output { -1i32 }
-- input { 1 -1i32 -1i32 } output { -1i32 }
-- input { 1 64i32 32i32 } output { 96i32 }
--
-- input { 2  0i32  0i32 } output {  0i32 }
-- input { 2  0i32  1i32 } output {  1i32 }
-- input { 2  0i32 -1i32 } output { -1i32 }
-- input { 2  1i32  0i32 } output {  1i32 }
-- input { 2  1i32  1i32 } output {  0i32 }
-- input { 2  1i32 -1i32 } output { -2i32 }
-- input { 2 -1i32  0i32 } output { -1i32 }
-- input { 2 -1i32  1i32 } output { -2i32 }
-- input { 2 -1i32 -1i32 } output {  0i32 }
-- input { 2 64i32 32i32 } output { 96i32 }
--
-- input { 3  0i32  0i32 } output {  0i32 }
-- input { 3  0i32  1i32 } output {  0i32 }
-- input { 3  1i32  0i32 } output {  1i32 }
-- input { 3  1i32  1i32 } output {  2i32 }
-- input { 3 -1i32  0i32 } output { -1i32 }
-- input { 3 -1i32  1i32 } output { -2i32 }
--
-- input { 4  0i32  0i32 } output {  0i32 }
-- input { 4  0i32  1i32 } output {  0i32 }
-- input { 4  1i32  0i32 } output {  1i32 }
-- input { 4  1i32  1i32 } output {  0i32 }
-- input { 4  2i32  1i32 } output {  1i32 }
-- input { 4 -1i32  0i32 } output { -1i32 }
-- input { 4 -1i32  1i32 } output { -1i32 }
--
-- input { 5  0i32  0i32 } output {  0i32 }
-- input { 5  0i32  1i32 } output {  0i32 }
-- input { 5  1i32  0i32 } output {  1i32 }
-- input { 5  1i32  1i32 } output {  0i32 }
-- input { 5  2i32  1i32 } output {  1i32 }
-- input { 5 -1i32  0i32 } output { -1i32 }
-- input { 5 -1i32  1i32 } output { 2147483647i32 }

let main (f: i32) (x: i32) (y: i32): i32 =
  if      f == 0 then x & y
  else if f == 1 then x | y
  else if f == 2 then x ^ y
  else if f == 3 then x << y
  else if f == 4 then x >> y
  else                x >>> y
