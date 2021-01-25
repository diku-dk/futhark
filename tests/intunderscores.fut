-- Integers can contain underscores
-- ==
-- input {  }
-- output { 101000i32 }

let main =
  let x = 100_000i32
  in x + i32.i16(1_000i16)
