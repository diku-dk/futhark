-- Convert back and forth between different integer types.
--
-- ==
-- input { 0u8 } output { false 0i8 0i16 0i32 0i64 0u8 0u16 0u32 0u64 }
-- input { 64u8 } output { true 64i8 64i16 64i32 64i64 64u8 64u16 64u32 64u64 }
-- input { 127u8 } output { true 127i8 127i16 127i32 127i64 127u8 127u16 127u32 127u64 }
-- input { 255u8 } output { true -1i8 255i16 255i32 255i64
--                          255u8 255u16 255u32 255u64 }

def main(x: u8): (bool,i8,i16,i32,i64, u8, u16, u32, u64) =
  (bool.u8(x),
   i8.u8(x), i16.u8(x), i32.u8(x), i64.u8(x),
   u8.u8(x), u16.u8(x), u32.u8(x), u64.u8(x))
