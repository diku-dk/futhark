-- Test type conversions involving WebGPU emulated types.
-- Tests conversions between i8/i16 (packed in i32), i64/u64 (vec2<u32>),
-- and f16 (native) to ensure proper handling.

-- ==
-- entry: i8_to_i32
-- input { [-128i8, -1i8, 0i8, 1i8, 127i8] }
-- output { [-128i32, -1i32, 0i32, 1i32, 127i32] }

-- ==
-- entry: i32_to_i8
-- input { [-128i32, -1i32, 0i32, 1i32, 127i32, 255i32, -129i32] }
-- output { [-128i8, -1i8, 0i8, 1i8, 127i8, -1i8, 127i8] }

-- ==
-- entry: i16_to_i32
-- input { [-32768i16, -1i16, 0i16, 1i16, 32767i16] }
-- output { [-32768i32, -1i32, 0i32, 1i32, 32767i32] }

-- ==
-- entry: i32_to_i16
-- input { [-32768i32, -1i32, 0i32, 1i32, 32767i32, 65535i32] }
-- output { [-32768i16, -1i16, 0i16, 1i16, 32767i16, -1i16] }

-- ==
-- entry: i32_to_i64
-- input { [-2147483648i32, -1i32, 0i32, 1i32, 2147483647i32] }
-- output { [-2147483648i64, -1i64, 0i64, 1i64, 2147483647i64] }

-- ==
-- entry: i64_to_i32
-- input { [-2147483648i64, -1i64, 0i64, 1i64, 2147483647i64, 4294967295i64] }
-- output { [-2147483648i32, -1i32, 0i32, 1i32, 2147483647i32, -1i32] }

-- ==
-- entry: f16_to_f32
-- input { [0f16, 1f16, -1f16, 65504f16, f16.inf, -f16.inf] }
-- output { [0f32, 1f32, -1f32, 65504f32, f32.inf, -f32.inf] }

-- ==
-- entry: f32_to_f16
-- input { [0f32, 1f32, -1f32, 65504f32, 1e10f32, f32.inf] }
-- output { [0f16, 1f16, -1f16, 65504f16, f16.inf, f16.inf] }

-- ==
-- entry: i8_to_f16
-- input { [-128i8, -1i8, 0i8, 1i8, 127i8] }
-- output { [-128f16, -1f16, 0f16, 1f16, 127f16] }

-- ==
-- entry: i64_to_f32
-- input { [0i64, 1i64, -1i64, 1000000i64, -9223372036854775808i64] }
-- output { [0f32, 1f32, -1f32, 1000000f32, -9223372036854775808f32] }

entry i8_to_i32 = map i32.i8
entry i32_to_i8 = map i8.i32
entry i16_to_i32 = map i32.i16
entry i32_to_i16 = map i16.i32
entry i32_to_i64 = map i64.i32
entry i64_to_i32 = map i32.i64
entry f16_to_f32 = map f32.f16
entry f32_to_f16 = map f16.f32
entry i8_to_f16 = map f16.i8
entry i64_to_f32 = map f32.i64
