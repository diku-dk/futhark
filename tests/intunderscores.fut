-- Integers can contain underscores
-- ==
-- input { 100_000i32 }
-- output { 101000i32 }

let main(x: i32) =
  x + i32.i16(1_000i16)
