-- Integers can contain underscores
-- ==
-- input { 123_456 }
-- output { 101000i32 }

def main (x: i32) =
  let x = 100_000i32
  in x + i32.i16 (1_000i16)
