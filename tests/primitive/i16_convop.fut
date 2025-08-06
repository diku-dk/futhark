-- Convert back and forth between different integer types.
--
-- ==
-- entry: i16tobool
-- input { [0i16, 64i16, 32767i16, -32768i16] }
-- output { [false, true, true, true] }

-- ==
-- entry: i16toi8
-- input { [0i16, 64i16, 32767i16, -32768i16] }
-- output { [0i8, 64i8, -1i8, 0i8] }

-- ==
-- entry: i16toi16
-- input { [0i16, 64i16, 32767i16, -32768i16] }
-- output { [0i16, 64i16, 32767i16, -32768i16] }

-- ==
-- entry: i16toi32
-- input { [0i16, 64i16, 32767i16, -32768i16] }
-- output { [0i32, 64i32, 32767i32, -32768i32] }

-- ==
-- entry: i16toi64
-- input { [0i16, 64i16, 32767i16, -32768i16] }
-- output { [0i64, 64i64, 32767i64, -32768i64] }

-- ==
-- entry: i16tou8
-- input { [0i16, 64i16, 32767i16, -32768i16] }
-- output { [0u8, 64u8, 255u8, 0u8] }

-- ==
-- entry: i16tou16
-- input { [0i16, 64i16, 32767i16, -32768i16] }
-- output { [0u16, 64u16, 32767u16, 32768u16] }

-- ==
-- entry: i16tou32
-- input { [0i16, 64i16, 32767i16, -32768i16] }
-- output { [0u32, 64u32, 32767u32, 32768u32] }

-- ==
-- entry: i16tou64
-- input { [0i16, 64i16, 32767i16, -32768i16] }
-- output { [0u64, 64u64, 32767u64, 32768u64] }

entry i16tobool = map (bool.i16)
entry i16toi8 = map (i8.i16)
entry i16toi16 = map (i16.i16)
entry i16toi32 = map (i32.i16)
entry i16toi64 = map (i64.i16)
entry i16tou8 = map (u8.i16)
entry i16tou16 = map (u16.i16)
entry i16tou32 = map (u32.i16)
entry i16tou64 = map (u64.i16)
