-- Convert booleans to different types.
--
-- ==
-- input { false } output { 0i8 0i16 0i32 0i64 0f32 0f64 }
-- input {  true } output { 1i8 1i16 1i32 1i64 1f32 1f64 }

fun main(b: bool): (i8,i16,i32,i64,f32,f64) =
  (i8 b, i16 b, i32 b, i64 b, f32 b, f64 b)
