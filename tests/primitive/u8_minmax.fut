-- ==
-- input { 0u8 1u8 } output { 1u8 0u8 }
-- input { 1u8 1u8 } output { 1u8 1u8 }
-- input { 255u8 1u8 } output { 255u8 1u8 }
-- input { 1u8 255u8 } output { 255u8 1u8 }

import "/futlib/math"

let main(x: u8) (y: u8): (u8,u8) =
  (u8.max x y,
   u8.min x y)
