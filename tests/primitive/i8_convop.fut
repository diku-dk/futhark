-- Convert back and forth between different integer types.
--
-- ==
-- input { 0i8 } output { false 0i8 0i16 0i32 0i64 0u8 0u16 0u32 0u64 }
-- input { 64i8 } output { true 64i8 64i16 64i32 64i64 64u8 64u16 64u32 64u64 }
-- input { 127i8 } output { true 127i8 127i16 127i32 127i64 127u8 127u16 127u32 127u64 }
-- input { -128i8 } output { true -128i8 -128i16 -128i32 -128i64
--                           128u8 128u16 128u32 128u64 }

let main(x: i8): (bool,i8,i16,i32,i64, u8, u16, u32, u64) =
  (bool.i8(x),
   i8.i8(x), i16.i8(x), i32.i8(x), i64.i8(x),
   u8.i8(x), u16.i8(x), u32.i8(x), u64.i8(x))
