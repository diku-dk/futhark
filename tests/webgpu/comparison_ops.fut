-- Test comparison operations for WebGPU emulated types.
-- Ensures correct ordering and comparison semantics.

-- ==
-- entry: i8_lt
-- input { [-128i8, -1i8, 0i8, 1i8, 127i8]
--         [-127i8, 0i8, 0i8, 0i8, 126i8] }
-- output { [true, true, false, false, false] }

-- ==
-- entry: i8_le
-- input { [-128i8, -1i8, 0i8, 1i8, 127i8]
--         [-128i8, 0i8, 0i8, 0i8, 127i8] }
-- output { [true, true, true, false, true] }

-- ==
-- entry: i64_lt
-- input { [-9223372036854775808i64, -1i64, 0i64, 4294967296i64]
--         [-9223372036854775807i64, 0i64, 0i64, 4294967295i64] }
-- output { [true, true, false, false] }

-- ==
-- entry: i64_le
-- input { [-9223372036854775808i64, -1i64, 0i64, 4294967296i64]
--         [-9223372036854775808i64, 0i64, 0i64, 4294967296i64] }
-- output { [true, true, true, true] }

-- ==
-- entry: u64_lt
-- input { [0u64, 1u64, 4294967295u64, 4294967296u64]
--         [1u64, 1u64, 4294967296u64, 4294967295u64] }
-- output { [true, false, true, false] }

-- ==
-- entry: f16_lt
-- input { [0f16, -1f16, f16.nan, f16.inf]
--         [1f16, 0f16, 1f16, f16.inf] }
-- output { [true, true, false, false] }

-- ==
-- entry: i8_eq
-- input { [-128i8, 0i8, 127i8]
--         [-128i8, 1i8, 127i8] }
-- output { [true, false, true] }

-- ==
-- entry: i64_eq
-- input { [0i64, 4294967296i64, -1i64]
--         [0i64, 4294967296i64, 4294967295i64] }
-- output { [true, true, false] }

entry i8_lt = map2 (i8.<)
entry i8_le = map2 (i8.<=)
entry i64_lt = map2 (i64.<)
entry i64_le = map2 (i64.<=)
entry u64_lt = map2 (u64.<)
entry f16_lt = map2 (f16.<)
entry i8_eq = map2 (i8.==)
entry i64_eq = map2 (i64.==)
