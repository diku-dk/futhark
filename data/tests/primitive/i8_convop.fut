-- Convert back and forth between different integer types.
--
-- ==
-- input { 0i8 } output { 0i8 0i16 0i32 0i64 0u8 0u16 0u32 0u64 }
-- input { 64i8 } output { 64i8 64i16 64i32 64i64 64u8 64u16 64u32 64u64 }
-- input { 127i8 } output { 127i8 127i16 127i32 127i64 127u8 127u16 127u32 127u64 }
-- input { -128i8 } output { -128i8 -128i16 -128i32 -128i64
--                           128u8 128u16 128u32 128u64 }

fun main(x: i8): (i8,i16,i32,i64, u8, u16, u32, u64) =
  (i8(x), i16(x), i32(x), i64(x),
   u8(x), u16(x), u32(x), u64(x))
