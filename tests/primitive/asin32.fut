-- Does the asin32 function work?
-- ==
-- input { [0f32, -0.84147096f32, -8.742278e-8f32, 8.742278e-8f32] }
-- output { [0f32, -1f32, -8.742278e-8f32, 8.742278e-8f32] }

def main = map f32.asin
