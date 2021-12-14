-- Convert booleans to different types.
--
-- ==
-- input { false } output { false 0i8 0i16 0i32 0i64 0f32 0f64 }
-- input {  true } output { true 1i8 1i16 1i32 1i64 1f32 1f64 }

def main(b: bool): (bool,i8,i16,i32,i64,f32,f64) =
  (bool.bool b, i8.bool b, i16.bool b, i32.bool b, i64.bool b, f32.bool b, f64.bool b)
