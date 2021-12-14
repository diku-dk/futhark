-- Floats can contain underscores
-- ==
-- input { 123_.456f32 }
-- output { 100000.123456f32 }

def main (_: f32) =
  100_000.123_456f32
