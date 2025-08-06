-- Convert back and forth between different integer types.

-- ==
-- entry: i64tobool
-- input { [0i64, 64i64, 9223372036854775807i64, -9223372036854775808i64] }
-- output { [false, true, true, true] }

-- ==
-- entry: i64toi8
-- input { [0i64, 64i64, 9223372036854775807i64, -9223372036854775808i64] }
-- output { [0i8, 64i8, -1i8, 0i8] }

-- ==
-- entry: i64toi16
-- input { [0i64, 64i64, 9223372036854775807i64, -9223372036854775808i64] }
-- output { [0i16, 64i16, -1i16, 0i16] }

-- ==
-- entry: i64toi32
-- input { [0i64, 64i64, 9223372036854775807i64, -9223372036854775808i64] }
-- output { [0i32, 64i32, -1i32, 0i32] }

-- ==
-- entry: i64toi64
-- input { [0i64, 64i64, 9223372036854775807i64, -9223372036854775808i64] }
-- output { [0i64, 64i64, 9223372036854775807i64, -9223372036854775808i64] }

-- ==
-- entry: i64tou8
-- input { [0i64, 64i64, 9223372036854775807i64, -9223372036854775808i64] }
-- output { [0u8, 64u8, 255u8, 0u8] }

-- ==
-- entry: i64tou16
-- input { [0i64, 64i64, 9223372036854775807i64, -9223372036854775808i64] }
-- output { [0u16, 64u16, 65535u16, 0u16] }

-- ==
-- entry: i64tou32
-- input { [0i64, 64i64, 9223372036854775807i64, -9223372036854775808i64] }
-- output { [0u32, 64u32, 4294967295u32, 0u32] }

-- ==
-- entry: i64tou64
-- input { [0i64, 64i64, 9223372036854775807i64, -9223372036854775808i64] }
-- output { [0u64, 64u64, 9223372036854775807u64, 9223372036854775808u64] }

entry i64tobool = map (bool.i64)
entry i64toi8 = map (i8.i64)
entry i64toi16 = map (i16.i64)
entry i64toi32 = map (i32.i64)
entry i64toi64 = map (i64.i64)
entry i64tou8 = map (u8.i64)
entry i64tou16 = map (u16.i64)
entry i64tou32 = map (u32.i64)
entry i64tou64 = map (u64.i64)
