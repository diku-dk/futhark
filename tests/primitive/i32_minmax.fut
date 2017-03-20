-- ==
-- input { 0i32 1i32 } output { 1i32 0i32 }
-- input { 1i32 1i32 } output { 1i32 1i32 }
-- input { -1i32 1i32 } output { 1i32 -1i32 }
-- input { 1i32 -1i32 } output { 1i32 -1i32 }

import "futlib/math"

fun main(x: i32) (y: i32): (i32,i32) =
  (i32.max x y,
   i32.min x y)
