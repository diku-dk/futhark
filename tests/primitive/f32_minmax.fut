-- ==
-- input { 0f32 1f32 } output { 1f32 0f32 }
-- input { 1f32 1f32 } output { 1f32 1f32 }
-- input { -1f32 1f32 } output { 1f32 -1f32 }
-- input { 1f32 -1f32 } output { 1f32 -1f32 }


let main (x: f32) (y: f32): (f32,f32) =
  (f32.max x y,
   f32.min x y)
