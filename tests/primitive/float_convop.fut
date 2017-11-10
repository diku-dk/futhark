-- Convert back and forth between different float- and integer types.
--
-- ==
-- input { 0i32 0u32 0f32 0f64 } output { false 0f32 0f64 0f32 0f64 0f32 0f64 0f32 0f64 }
-- input { 1i32 1u32 1f32 1f64 } output { true 1f32 1f64 1f32 1f64 1f32 1f64 1f32 1f64 }
-- input { -1i32 4294967288u32 -1f32 -1f64 }
-- output { true -1f32 -1f64 4294967300f32 4294967288f64 -1f32 -1f64 -1f32 -1f64 }
-- input { 2147483647i32 2147483647u32 2147483647f32 2147483647f64 }
-- output { true
--          2147483600.0f32 2147483647f64
--          2147483600.0f32 2147483647f64
--          2147483600.0f32 2147483648f64
--          2147483600.0f32 2147483647f64 }

let main(x: i32, y: u32, z: f32, v: f64): (bool, f32, f64, f32, f64, f32, f64, f32, f64) =
  (bool.i32(x), f32.i32(x), f64.i32(x),
   f32.u32(y), f64.u32(y),
   f32.f32(z), f64.f32(z),
   f32.f64(v), f64.f64(v))
