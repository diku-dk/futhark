-- Loops can use any signed integral type
def main (x: i8): i64 =
  let x = loop x = x for i < -128i8 do x + i
  let x = loop x = i16.i8 x for i < 128i16 do x + i
  let x = loop x = i32.i16 x for i < 128i32 do x + i
  let x = loop x = i64.i32 x for i < 128i64 do x + i
  in x