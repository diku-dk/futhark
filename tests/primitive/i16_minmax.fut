-- ==
-- input { 0i16 1i16 } output { 1i16 0i16 }
-- input { 1i16 1i16 } output { 1i16 1i16 }
-- input { -1i16 1i16 } output { 1i16 -1i16 }
-- input { 1i16 -1i16 } output { 1i16 -1i16 }

import "futlib/math"

let main(x: i16) (y: i16): (i16,i16) =
  (i16.max x y,
   i16.min x y)
