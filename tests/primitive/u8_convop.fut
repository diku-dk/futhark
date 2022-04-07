-- Convert back and forth between different integer types.

-- ==
-- entry: u8tobool
-- input { [0u8, 64u8, 127u8, 255u8] }
-- output { [false, true, true, true] }

-- ==
-- entry: u8toi8
-- input { [0u8, 64u8, 127u8, 255u8] }
-- output { [0i8, 64i8, 127i8, -1i8] }

-- ==
-- entry: u8toi16
-- input { [0u8, 64u8, 127u8, 255u8] }
-- output { [0i16, 64i16, 127i16, 255i16] }

-- ==
-- entry: u8toi32
-- input { [0u8, 64u8, 127u8, 255u8] }
-- output { [0i32, 64i32, 127i32, 255i32] }

-- ==
-- entry: u8toi64
-- input { [0u8, 64u8, 127u8, 255u8] }
-- output { [0i64, 64i64, 127i64, 255i64] }

-- ==
-- entry: u8tou8
-- input { [0u8, 64u8, 127u8, 255u8] }
-- output { [0u8, 64u8, 127u8, 255u8] }

-- ==
-- entry: u8tou16
-- input { [0u8, 64u8, 127u8, 255u8] }
-- output { [0u16, 64u16, 127u16, 255u16] }

-- ==
-- entry: u8tou32
-- input { [0u8, 64u8, 127u8, 255u8] }
-- output { [0u32, 64u32, 127u32, 255u32] }

-- ==
-- entry: u8tou64
-- input { [0u8, 64u8, 127u8, 255u8] }
-- output { [0u64, 64u64, 127u64, 255u64] }

entry u8tobool = map (bool.u8)
entry u8toi8 = map (i8.u8)
entry u8toi16 = map (i16.u8)
entry u8toi32 = map (i32.u8)
entry u8toi64 = map (i64.u8)
entry u8tou8 = map (u8.u8)
entry u8tou16 = map (u16.u8)
entry u8tou32 = map (u32.u8)
entry u8tou64 = map (u64.u8)
