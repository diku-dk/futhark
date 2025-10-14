-- Convert back and forth between different integer types.

-- ==
-- entry: u64tobool
-- input { [0u64, 64u64, 9223372036854775807u64, 18446744073709551615u64] }
-- output { [false, true, true, true] }

-- ==
-- entry: u64toi8
-- input { [0u64, 64u64, 9223372036854775807u64, 18446744073709551615u64] }
-- output { [0i8, 64i8, -1i8, -1i8] }

-- ==
-- entry: u64toi16
-- input { [0u64, 64u64, 9223372036854775807u64, 18446744073709551615u64] }
-- output { [0i16, 64i16, -1i16, -1i16] }

-- ==
-- entry: u64toi32
-- input { [0u64, 64u64, 9223372036854775807u64, 18446744073709551615u64] }
-- output { [0i32, 64i32, -1i32, -1i32] }

-- ==
-- entry: u64toi64
-- input { [0u64, 64u64, 9223372036854775807u64, 18446744073709551615u64] }
-- output { [0i64, 64i64, 9223372036854775807i64, -1i64] }

-- ==
-- entry: u64tou8
-- input { [0u64, 64u64, 9223372036854775807u64, 18446744073709551615u64] }
-- output { [0u8, 64u8, 255u8, 255u8] }

-- ==
-- entry: u64tou16
-- input { [0u64, 64u64, 9223372036854775807u64, 18446744073709551615u64] }
-- output { [0u16, 64u16, 65535u16, 65535u16] }

-- ==
-- entry: u64tou32
-- input { [0u64, 64u64, 9223372036854775807u64, 18446744073709551615u64] }
-- output { [0u32, 64u32, 4294967295u32, 4294967295u32] }

-- ==
-- entry: u64tou64
-- input { [0u64, 64u64, 9223372036854775807u64, 18446744073709551615u64] }
-- output { [0u64, 64u64, 9223372036854775807u64, 18446744073709551615u64] }

entry u64tobool = map (bool.u64)
entry u64toi8 = map (i8.u64)
entry u64toi16 = map (i16.u64)
entry u64toi32 = map (i32.u64)
entry u64toi64 = map (i64.u64)
entry u64tou8 = map (u8.u64)
entry u64tou16 = map (u16.u64)
entry u64tou32 = map (u32.u64)
entry u64tou64 = map (u64.u64)
