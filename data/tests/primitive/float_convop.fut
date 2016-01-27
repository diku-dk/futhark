-- Convert back and forth between different float- and integer types.
--
-- ==
-- input { 0i32 0f32 0f64 } output { 0f32 0f64 0f32 0f64 0f32 0f64 }
-- input { 1i32 1f32 1f64 } output { 1f32 1f64 1f32 1f64 1f32 1f64 }
-- input { -1i32 -1f32 -1f64 } output { -1f32 -1f64 -1f32 -1f64 -1f32 -1f64 }
-- input { 2147483647i32 2147483647f32 2147483647f64 }
-- output { 2147483600.0f32 2147483647f64
--          2147483600.0f32 2147483647f64
--          2147483600.0f32 2147483647f64 }

fun {f32, f64, f32, f64, f32, f64} main(int x, f32 y, f64 z) =
  {f32(x), f64(x), f32(y), f64(y), f32(y), f64(y)}
