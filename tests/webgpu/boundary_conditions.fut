-- Test boundary conditions for WebGPU emulated types.
-- Ensures correct handling of min/max values and overflow wraparound.

-- ==
-- entry: i8_boundaries
-- input { }
-- output { -128i8 127i8 -128i8 127i8 }

-- ==
-- entry: i16_boundaries
-- input { }
-- output { -32768i16 32767i16 -32768i16 32767i16 }

-- ==
-- entry: i64_boundaries
-- input { }
-- output { -9223372036854775808i64 9223372036854775807i64 -9223372036854775808i64 9223372036854775807i64 }

-- ==
-- entry: u64_boundaries
-- input { }
-- output { 0u64 18446744073709551615u64 0u64 18446744073709551615u64 }

-- ==
-- entry: f16_boundaries
-- input { }
-- output { -65504f16 65504f16 }

-- ==
-- entry: i8_overflow_add
-- input { 127i8 1i8 }
-- output { -128i8 }

-- ==
-- entry: i8_overflow_sub
-- input { -128i8 1i8 }
-- output { 127i8 }

-- ==
-- entry: i64_overflow_add
-- input { 9223372036854775807i64 1i64 }
-- output { -9223372036854775808i64 }

-- ==
-- entry: u64_overflow_add
-- input { 18446744073709551615u64 1u64 }
-- output { 0u64 }

entry i8_boundaries : (i8, i8, i8, i8) = (i8.lowest, i8.highest, i8.lowest, i8.highest)
entry i16_boundaries : (i16, i16, i16, i16) = (i16.lowest, i16.highest, i16.lowest, i16.highest)
entry i64_boundaries : (i64, i64, i64, i64) = (i64.lowest, i64.highest, i64.lowest, i64.highest)
entry u64_boundaries : (u64, u64, u64, u64) = (u64.lowest, u64.highest, u64.lowest, u64.highest)
entry f16_boundaries : (f16, f16) = (f16.lowest, f16.highest)

entry i8_overflow_add (a: i8) (b: i8) : i8 = a + b
entry i8_overflow_sub (a: i8) (b: i8) : i8 = a - b
entry i64_overflow_add (a: i64) (b: i64) : i64 = a + b
entry u64_overflow_add (a: u64) (b: u64) : u64 = a + b
