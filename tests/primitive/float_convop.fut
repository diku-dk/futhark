-- Converting back and forth between different float- and integer types.

entry f16_to_f32 = map f32.f16
entry f16_to_f64 = map f64.f16
entry f32_to_f16 = map f16.f32
entry f32_to_f64 = map f64.f32
entry f64_to_f16 = map f16.f64
entry f64_to_f32 = map f32.f64
entry u16_to_f16 = map f16.from_bits
entry u32_to_f32 = map f32.from_bits
entry u64_to_f64 = map f64.from_bits

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
