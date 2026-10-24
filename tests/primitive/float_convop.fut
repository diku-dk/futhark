-- Converting back and forth between different float- and integer types.

entry f16_to_f32 = map f32.f16
entry f16_to_f64 = map f64.f16
entry f32_to_f16 = map f16.f32
entry f32_to_f16_bits = map (f16.f32 >-> f16.to_bits)
entry f32_to_f64 = map f64.f32
entry f64_to_f16 = map f16.f64
entry f64_to_f32 = map f32.f64
entry u16_to_f16 = map f16.from_bits
entry u32_to_f32 = map f32.from_bits
entry u64_to_f64 = map f64.from_bits
entry f32_to_i32 = map i32.f32
entry f32_to_u32 = map u32.f32
entry f64_to_i32 = map i32.f64
entry f64_to_u32 = map u32.f64
entry f16_to_i16 = map i16.f16
entry f16_to_u16 = map u16.f16
entry f64_to_i64 = map i64.f64
entry f16_to_bool = map bool.f16
entry f32_to_bool = map bool.f32
entry f64_to_bool = map bool.f64

-- ==
-- entry: f16_to_f32
-- input { [f16.inf, -f16.inf, f16.nan, -1f16, 1f16] }
-- output { [f32.inf, -f32.inf, f32.nan, -1.0f32, 1.0f32] }

-- ==
-- entry: f16_to_f64
-- input { [f16.inf, -f16.inf, f16.nan, -1f16, 1f16] }
-- output { [f64.inf, -f64.inf, f64.nan, -1.0f64, 1.0f64] }

-- ==
-- entry: f32_to_f16
-- input { [f32.inf, -f32.inf, -1f32, 1f32, 100000f32, -100000f32] }
-- output { [f16.inf, -f16.inf, -1.0f16, 1.0f16, f16.inf, -f16.inf] }

-- Rounding to nearest, ties to even, including into the subnormals
-- and up to infinity.
-- ==
-- entry: f32_to_f16_bits
-- input { [1.0007f32, -1.0007f32, 1.00048828125f32, 1.00146484375f32,
--          65519f32, 65520f32, 2.9802322e-8f32, 3e-8f32,
--          8.940697e-8f32] }
-- output { [0x3c01u16, 0xbc01u16, 0x3c00u16, 0x3c02u16,
--           0x7bffu16, 0x7c00u16, 0x0000u16, 0x0001u16,
--           0x0002u16] }

-- ==
-- entry: f32_to_f64
-- input { [f32.inf, -f32.inf, -1f32, 1f32, 100000f32, -100000f32] }
-- output { [f64.inf, -f64.inf, -1.0f64, 1.0f64, 100000.0f64, -100000.0f64] }

-- ==
-- entry: f64_to_f16
-- input { [f64.inf, -f64.inf, -1f64, 1f64, 3.5028234664e38f64, -3.5028234664e38f64] }
-- output { [f16.inf, -f16.inf, -1.0f16, 1.0f16, f16.inf, -f16.inf] }

-- ==
-- entry: f64_to_f32
-- input { [f64.inf, -f64.inf, -1f64, 1f64, 3.5028234664e38f64, -3.5028234664e38f64] }
-- output { [f32.inf, -f32.inf, -1.0f32, 1.0f32, f32.inf, -f32.inf] }

-- ==
-- entry: u16_to_f16
-- input { [31744u16, 65024u16, 48128u16, 15360u16] }
-- output { [f16.inf, f16.nan, -1.0f16, 1.0f16] }

-- ==
-- entry: u32_to_f32
-- input { [2139095040u32, 4290772992u32, 3212836864u32, 1065353216u32] }
-- output { [f32.inf, f32.nan, -1f32, 1f32] }

-- ==
-- entry: u64_to_f64
-- input { [9218868437227405312u64, 18444492273895866368u64, 13830554455654793216u64, 4607182418800017408u64] }
-- output { [f64.inf, f64.nan, -1.0f64, 1.0f64] }

-- ==
-- entry: f32_to_i32
-- input { [f32.nan, f32.inf, -f32.inf, -1f32, 1f32, 3.5f32, -3.5f32] }
-- output { [0, 0, 0, -1, 1, 3, -3] }

-- ==
-- entry: f32_to_u32
-- input { [f32.nan, f32.inf, -f32.inf, -1f32, 1f32, 3.5f32, -3.5f32] }
-- output { [0u32, 0u32, 0u32, 0u32, 1u32, 3u32, 0u32] }

-- ==
-- entry: f64_to_i32
-- input { [f64.nan, f64.inf, -f64.inf, -1f64, 1f64, 3.5f64, -3.5f64] }
-- output { [0, 0, 0, -1, 1, 3, -3] }

-- ==
-- entry: f64_to_u32
-- input { [f64.nan, f64.inf, -f64.inf, -1f64, 1f64, 3.5f64, -3.5f64] }
-- output { [0u32, 0u32, 0u32, 0u32, 1u32, 3u32, 0u32] }

-- ==
-- entry: f16_to_i16
-- input { [f16.nan, f16.inf, -f16.inf, 40000f16, -40000f16, -1f16, 3.5f16, -3.5f16] }
-- output { [0i16, 0i16, 0i16, 0i16, 0i16, -1i16, 3i16, -3i16] }

-- ==
-- entry: f16_to_u16
-- input { [f16.nan, f16.inf, -f16.inf, -1f16, 1f16, 3.5f16, 65504f16] }
-- output { [0u16, 0u16, 0u16, 0u16, 1u16, 3u16, 65504u16] }

-- ==
-- entry: f64_to_i64
-- input { [f64.nan, f64.inf, 9223372036854775808f64, -9223372036854775808f64, -3.5f64] }
-- output { [0i64, 0i64, 0i64, -9223372036854775808i64, -3i64] }

-- ==
-- entry: f16_to_bool
-- input { [f16.nan, f16.inf, -f16.inf, -1f16, 1f16, 0f16] }
-- output { [true, true, true, true, true, false] }

-- ==
-- entry: f32_to_bool
-- input { [f32.nan, f32.inf, -f32.inf, -1f32, 1f32, 0f32] }
-- output { [true, true, true, true, true, false] }

-- ==
-- entry: f64_to_bool
-- input { [f64.nan, f64.inf, -f64.inf, -1f64, 1f64, 0f64] }
-- output { [true, true, true, true, true, false] }
