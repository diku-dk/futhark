-- Convert back and forth between different integer types.

-- ==
-- entry: u16tobool
-- input { [0u16, 64u16, 32767u16, 65535u16] }
-- output { [false, true, true, true] }

-- ==
-- entry: u16toi8
-- input { [0u16, 64u16, 32767u16, 65535u16] }
-- output { [0i8, 64i8, 32767i8, -1i8] }

-- ==
-- entry: u16toi16
-- input { [0u16, 64u16, 32767u16, 65535u16] }
-- output { [0i16, 64i16, 32767i16, -1i16] }

-- ==
-- entry: u16toi32
-- input { [0u16, 64u16, 32767u16, 65535u16] }
-- output { [0i32, 64i32, 32767i32, 65535i32] }

-- ==
-- entry: u16toi64
-- input { [0u16, 64u16, 32767u16, 65535u16] }
-- output { [0i64, 64i64, 32767i64, 65535i64] }

-- ==
-- entry: u16tou8
-- input { [0u16, 64u16, 32767u16, 65535u16] }
-- output { [0u8, 64u8, 32767u8, 255u8] }

-- ==
-- entry: u16tou16
-- input { [0u16, 64u16, 32767u16, 65535u16] }
-- output { [0u16, 64u16, 32767u16, 65535u16] }

-- ==
-- entry: u16tou32
-- input { [0u16, 64u16, 32767u16, 65535u16] }
-- output { [0u32, 64u32, 32767u32, 65535u32] }

-- ==
-- entry: u16tou64
-- input { [0u16, 64u16, 32767u16, 65535u16] }
-- output { [0u64, 64u64, 32767u64, 65535u64] }

entry u16tobool = map (bool.u16)
entry u16toi8 = map (i8.u16)
entry u16toi16 = map (i16.u16)
entry u16toi32 = map (i32.u16)
entry u16toi64 = map (i64.u16)
entry u16tou8 = map (u8.u16)
entry u16tou16 = map (u16.u16)
entry u16tou32 = map (u32.u16)
entry u16tou64 = map (u64.u16)
