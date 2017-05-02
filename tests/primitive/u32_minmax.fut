-- ==
-- input { 0u32 1u32 } output { 1u32 0u32 }
-- input { 1u32 1u32 } output { 1u32 1u32 }
-- input { 4294967295u32 1u32 } output { 4294967295u32 1u32 }
-- input { 1u32 4294967295u32 } output { 4294967295u32 1u32 }

import "/futlib/math"

let main(x: u32) (y: u32): (u32,u32) =
  (u32.max x y,
   u32.min x y)
