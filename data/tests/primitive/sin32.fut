-- Does the sin32 function work?
-- ==
-- input { 0f32 } output { 0f32 }
-- input { -1f32 } output { -0.84147096f32 }
-- input { 3.1415927f32 } output { -8.742278e-8f32 }
-- input { -3.1415927f32 } output { 8.742278e-8f32 }

fun f32 main(f32 x) = sin32(x)
