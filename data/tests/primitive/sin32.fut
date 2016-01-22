-- Does the sin32 function work?
-- ==
-- input { 0f } output { 0f }
-- input { -1f } output { -0.84147096f }
-- input { 3.1415927f } output { -8.742278e-8f }
-- input { -3.1415927f } output { 8.742278e-8f }

fun f32 main(f32 x) = sin32(x)
