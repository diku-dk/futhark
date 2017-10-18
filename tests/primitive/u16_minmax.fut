-- ==
-- input { 0u16 1u16 } output { 1u16 0u16 }
-- input { 1u16 1u16 } output { 1u16 1u16 }
-- input { 65535u16 1u16 } output { 65535u16 1u16 }
-- input { 1u16 65535u16 } output { 65535u16 1u16 }

import "/futlib/math"

let main(x: u16) (y: u16): (u16,u16) =
  (u16.max x y,
   u16.min x y)
