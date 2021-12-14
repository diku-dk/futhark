-- Convert back and forth between different integer types.
--
-- ==
-- input { 0i16 } output { false 0i8 0i16 0i32 0i64 0u8 0u16 0u32 0u64 }
-- input { 64i16 } output { true 64i8 64i16 64i32 64i64 64u8 64u16 64u32 64u64 }
-- input { 32767i16 } output { true -1i8 32767i16 32767i32 32767i64 255u8 32767u16 32767u32 32767u64 }
-- input { -32768i16 } output { true 0i8 -32768i16 -32768i32 -32768i64
--                              0u8 32768u16 32768u32 32768u64}

def main(x: i16): (bool,i8,i16,i32,i64,u8,u16,u32,u64) =
  (bool.i16(x),
   i8.i16(x), i16.i16(x), i32.i16(x), i64.i16(x),
   u8.i16(x), u16.i16(x), u32.i16(x), u64.i16(x))
