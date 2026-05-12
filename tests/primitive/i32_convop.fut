-- Convert back and forth between different integer types.

-- ==
-- entry: i32tobool
-- input { [0i32, 64i32, 2147483647i32, -2147483648i32] }
-- output { [false, true, true, true] }

-- ==
-- entry: i32toi8
-- input { [0i32, 64i32, 2147483647i32, -2147483648i32] }
-- output { [0i8, 64i8, -1i8, -0i8] }

-- ==
-- entry: i32toi16
-- input { [0i32, 64i32, 2147483647i32, -2147483648i32] }
-- output { [0i16, 64i16, -1i16, 0i16] }

-- ==
-- entry: i32toi32
-- input { [0i32, 64i32, 2147483647i32, -2147483648i32] }
-- output { [0i32, 64i32, 2147483647i32, -2147483648i32] }

-- ==
-- entry: i32toi64
-- input { [0i32, 64i32, 2147483647i32, -2147483648i32] }
-- output { [0i64, 64i64, 2147483647i64, -2147483648i64] }

-- ==
-- entry: i32tou8
-- input { [0i32, 64i32, 2147483647i32, -2147483648i32] }
-- output { [0u8, 64u8, 255u8, 0u8] }

-- ==
-- entry: i32tou16
-- input { [0i32, 64i32, 2147483647i32, -2147483648i32] }
-- output { [0u16, 64u16, 65535u16, 0u16] }

-- ==
-- entry: i32tou32
-- input { [0i32, 64i32, 2147483647i32, -2147483648i32] }
-- output { [0u32, 64u32, 2147483647u32, 2147483648u32] }

-- ==
-- entry: i32tou64
-- input { [0i32, 64i32, 2147483647i32, -2147483648i32] }
-- output { [0u64, 64u64, 2147483647u64, 2147483648u64] }

entry i32tobool = map (bool.i32)
entry i32toi8 = map (i8.i32)
entry i32toi16 = map (i16.i32)
entry i32toi32 = map (i32.i32)
entry i32toi64 = map (i64.i32)
entry i32tou8 = map (u8.i32)
entry i32tou16 = map (u16.i32)
entry i32tou32 = map (u32.i32)
entry i32tou64 = map (u64.i32)
