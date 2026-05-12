-- Rounding down floats to whole numbers.
-- ==
-- input { [1.0000001192092896f32, -0.9999999403953552f32, 0f32, -0f32, 0.49999999999999994f32, 0.5f32, 0.5000000000000001f32] }
-- output { [1f32, -1f32, 0f32, -0f32, 0f32, 0f32, 0f32] }
-- input { [1.18e-38f32, -f32.inf, f32.inf, f32.nan, -0f32] }
-- output { [0f32, -f32.inf, f32.inf, f32.nan, -0f32] }

def main = map f32.floor
