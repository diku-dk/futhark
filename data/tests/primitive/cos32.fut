-- Does the cos32 function work?
-- ==
-- input { 0f } output { 1f }
-- input { -1f } output { 0.5403023f }
-- input { 3.1415927f } output { -1f }
-- input { -3.1415927f } output { -1f }

fun f32 main(f32 x) = cos32(x)
