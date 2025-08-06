-- ==
-- entry: f
-- input { 1f32 } output { 1f32 }

-- ==
-- entry: f'
-- input { 1f32 } output { 2f32 }

entry f (x: f32) : f32 = x
entry f' (x: f32) : f32 = x + 1
