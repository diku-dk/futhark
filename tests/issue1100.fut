-- ==
-- input { [[1.0f32, 2.0f32], [3.0f32, 4.0f32]] }
-- output { [[1.0f32, 2.0f32], [3.0f32, 4.0f32]] }

def main xss =
  let scale (f: f32) = map (map (* f))
  in scale 1.0 xss
