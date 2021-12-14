-- Bitwise operations on i64 values.
--
-- ==
-- input { 0  0i64  0i64 } output {  0i64 }
-- input { 0  0i64  1i64 } output {  0i64 }
-- input { 0  0i64 -1i64 } output {  0i64 }
-- input { 0  1i64  0i64 } output {  0i64 }
-- input { 0  1i64  1i64 } output {  1i64 }
-- input { 0  1i64 -1i64 } output {  1i64 }
-- input { 0 -1i64  0i64 } output {  0i64 }
-- input { 0 -1i64  1i64 } output {  1i64 }
-- input { 0 -1i64 -1i64 } output { -1i64 }
--
-- input { 1  0i64  0i64 } output {  0i64 }
-- input { 1  0i64  1i64 } output {  1i64 }
-- input { 1  0i64 -1i64 } output { -1i64 }
-- input { 1  1i64  0i64 } output {  1i64 }
-- input { 1  1i64  1i64 } output {  1i64 }
-- input { 1  1i64 -1i64 } output { -1i64 }
-- input { 1 -1i64  0i64 } output { -1i64 }
-- input { 1 -1i64  1i64 } output { -1i64 }
-- input { 1 -1i64 -1i64 } output { -1i64 }
-- input { 1 64i64 32i64 } output { 96i64 }
--
-- input { 2  0i64  0i64 } output {  0i64 }
-- input { 2  0i64  1i64 } output {  1i64 }
-- input { 2  0i64 -1i64 } output { -1i64 }
-- input { 2  1i64  0i64 } output {  1i64 }
-- input { 2  1i64  1i64 } output {  0i64 }
-- input { 2  1i64 -1i64 } output { -2i64 }
-- input { 2 -1i64  0i64 } output { -1i64 }
-- input { 2 -1i64  1i64 } output { -2i64 }
-- input { 2 -1i64 -1i64 } output {  0i64 }
-- input { 2 64i64 32i64 } output { 96i64 }
--
-- input { 3  0i64  0i64 } output {  0i64 }
-- input { 3  0i64  1i64 } output {  0i64 }
-- input { 3  1i64  0i64 } output {  1i64 }
-- input { 3  1i64  1i64 } output {  2i64 }
-- input { 3 -1i64  0i64 } output { -1i64 }
-- input { 3 -1i64  1i64 } output { -2i64 }
--
-- input { 4  0i64  0i64 } output {  0i64 }
-- input { 4  0i64  1i64 } output {  0i64 }
-- input { 4  1i64  0i64 } output {  1i64 }
-- input { 4  1i64  1i64 } output {  0i64 }
-- input { 4  2i64  1i64 } output {  1i64 }
-- input { 4 -1i64  0i64 } output { -1i64 }
-- input { 4 -1i64  1i64 } output { -1i64 }

def main (f: i32) (x: i64) (y: i64): i64 =
  if      f == 0 then x & y
  else if f == 1 then x | y
  else if f == 2 then x ^ y
  else if f == 3 then x << y
  else if f == 4 then x >> y
  else                0
