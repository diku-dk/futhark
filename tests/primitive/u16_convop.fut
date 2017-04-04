-- Convert back and forth between different integer types.
--
-- ==
-- input { 0u16 } output { 0i8 0i16 0i32 0i64 0u8 0u16 0u32 0u64 }
-- input { 64u16 } output { 64i8 64i16 64i32 64i64 64u8 64u16 64u32 64u64 }
-- input { 32767u16 } output { 32767i8 32767i16 32767i32 32767i64 32767u8 32767u16 32767u32 32767u64 }
-- input { 65535u16 } output { -1i8 -1i16 65535i32 65535i64
--                           255u8 65535u16 65535u32 65535u64 }

let main(x: u16): (i8,i16,i32,i64, u8, u16, u32, u64) =
  (i8(x), i16(x), i32(x), i64(x),
   u8(x), u16(x), u32(x), u64(x))
