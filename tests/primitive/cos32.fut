-- Does the cos32 function work?
-- ==
-- input { [0f32, -1f32, 3.1415927f32, -3.1415927f32] }
-- output { [1f32, 0.5403023f32, -1f32, -1f32] }

def main = map f32.cos
