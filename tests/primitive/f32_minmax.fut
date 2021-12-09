-- ==
-- input { 0f32 1f32 } output { 1f32 0f32 }
-- input { 1f32 1f32 } output { 1f32 1f32 }
-- input { -1f32 1f32 } output { 1f32 -1f32 }
-- input { 1f32 -1f32 } output { 1f32 -1f32 }
-- input { f32.nan -1f32 } output { -1f32 -1f32 }
-- input { -1f32 f32.nan } output { -1f32 -1f32 }
-- input { f32.nan f32.nan } output { f32.nan f32.nan }
-- input { -1f32 f32.inf } output { f32.inf -1f32 }
-- input { -1f32 -f32.inf } output { -1f32 -f32.inf }

def main (x: f32) (y: f32): (f32,f32) =
  (f32.max x y,
   f32.min x y)
