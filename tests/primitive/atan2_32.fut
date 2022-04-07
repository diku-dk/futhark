-- Does the atan2_32 function work?
-- ==
-- input { [0f32, 1f32, 1f32, 0f32, -1f32, 1f32, -1f32] [0f32, 0f32, 1f32, 1f32, 1f32, -1f32, -1f32] }
-- output { [0f32, 1.570796f32, 0.785398f32, 0.000000f32, -0.785398f32, 2.356194f32, -2.356194f32] }

def main = map2 f32.atan2
