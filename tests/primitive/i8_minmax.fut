-- ==
-- input { 0i8 1i8 } output { 1i8 0i8 }
-- input { 1i8 1i8 } output { 1i8 1i8 }
-- input { -1i8 1i8 } output { 1i8 -1i8 }
-- input { 1i8 -1i8 } output { 1i8 -1i8 }

import "futlib/math"

fun main(x: i8) (y: i8): (i8,i8) =
  (i8.max x y,
   i8.min x y)
