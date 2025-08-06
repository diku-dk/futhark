-- Convert back and forth between different integer types.

-- ==
-- entry: u32tobool
-- input { [0u32, 64u32, 2147483647u32, 4294967295u32] }
-- output { [false, true, true, true] }

-- ==
-- entry: u32toi8
-- input { [0u32, 64u32, 2147483647u32, 4294967295u32] }
-- output { [0i8, 64i8, -1i8, -1i8] }

-- ==
-- entry: u32toi16
-- input { [0u32, 64u32, 2147483647u32, 4294967295u32] }
-- output { [0i16, 64i16, -1i16, -1i16] }

-- ==
-- entry: u32toi32
-- input { [0u32, 64u32, 2147483647u32, 4294967295u32] }
-- output { [0i32, 64i32, 2147483647i32, -1i32] }

-- ==
-- entry: u32toi64
-- input { [0u32, 64u32, 2147483647u32, 4294967295u32] }
-- output { [0i64, 64i64, 2147483647i64, 4294967295i64] }

-- ==
-- entry: u32tou8
-- input { [0u32, 64u32, 2147483647u32, 4294967295u32] }
-- output { [0u8, 64u8, 255u8, 255u8] }

-- ==
-- entry: u32tou16
-- input { [0u32, 64u32, 2147483647u32, 4294967295u32] }
-- output { [0u16, 64u16, 65535u16, 65535u16] }

-- ==
-- entry: u32tou32
-- input { [0u32, 64u32, 2147483647u32, 4294967295u32] }
-- output { [0u32, 64u32, 2147483647u32, 4294967295u32] }

-- ==
-- entry: u32tou64
-- input { [0u32, 64u32, 2147483647u32, 4294967295u32] }
-- output { [0u64, 64u64, 2147483647u64, 4294967295u64] }

entry u32tobool = map (bool.u32)
entry u32toi8 = map (i8.u32)
entry u32toi16 = map (i16.u32)
entry u32toi32 = map (i32.u32)
entry u32toi64 = map (i64.u32)
entry u32tou8 = map (u8.u32)
entry u32tou16 = map (u16.u32)
entry u32tou32 = map (u32.u32)
entry u32tou64 = map (u64.u32)
