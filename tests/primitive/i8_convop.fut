-- Convert back and forth between different integer types.

-- ==
-- entry: i8tobool
-- input { [0i8, 64i8, 127i8, -128i8] }
-- output { [false, true, true, true] }

-- ==
-- entry: i8toi8
-- input { [0i8, 64i8, 127i8, -128i8] }
-- output { [0i8, 64i8, 127i8, -128i8] }

-- ==
-- entry: i8toi16
-- input { [0i8, 64i8, 127i8, -128i8] }
-- output { [0i16, 64i16, 127i16, -128i16] }

-- ==
-- entry: i8toi32
-- input { [0i8, 64i8, 127i8, -128i8] }
-- output { [0i32, 64i32, 127i32, -128i32] }

-- ==
-- entry: i8toi64
-- input { [0i8, 64i8, 127i8, -128i8] }
-- output { [0i64, 64i64, 127i64, -128i64] }

-- ==
-- entry: i8tou8
-- input { [0i8, 64i8, 127i8, -128i8] }
-- output { [0u8, 64u8, 127u8, 128u8] }

-- ==
-- entry: i8tou16
-- input { [0i8, 64i8, 127i8, -128i8] }
-- output { [0u16, 64u16, 127u16, 128u16] }

-- ==
-- entry: i8tou32
-- input { [0i8, 64i8, 127i8, -128i8] }
-- output { [0u32, 64u32, 127u32, 128u32] }

-- ==
-- entry: i8tou64
-- input { [0i8, 64i8, 127i8, -128i8] }
-- output { [0u64, 64u64, 127u64, 128u64] }

entry i8tobool = map (bool.i8)
entry i8toi8 = map (i8.i8)
entry i8toi16 = map (i16.i8)
entry i8toi32 = map (i32.i8)
entry i8toi64 = map (i64.i8)
entry i8tou8 = map (u8.i8)
entry i8tou16 = map (u16.i8)
entry i8tou32 = map (u32.i8)
entry i8tou64 = map (u64.i8)
