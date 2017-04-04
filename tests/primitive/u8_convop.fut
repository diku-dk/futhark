-- Convert back and forth between different integer types.
--
-- ==
-- input { 0u8 } output { 0i8 0i16 0i32 0i64 0u8 0u16 0u32 0u64 }
-- input { 64u8 } output { 64i8 64i16 64i32 64i64 64u8 64u16 64u32 64u64 }
-- input { 127u8 } output { 127i8 127i16 127i32 127i64 127u8 127u16 127u32 127u64 }
-- input { 255u8 } output { -1i8 255i16 255i32 255i64
--                           255u8 255u16 255u32 255u64 }

let main(x: u8): (i8,i16,i32,i64, u8, u16, u32, u64) =
  (i8(x), i16(x), i32(x), i64(x),
   u8(x), u16(x), u32(x), u64(x))
