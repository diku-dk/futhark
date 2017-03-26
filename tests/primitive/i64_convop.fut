-- Convert back and forth between different integer types.
--
-- ==
-- input { 0i64 } output { 0i8 0i16 0i32 0i64 0u8 0u16 0u32 0u64 }
-- input { 64i64 } output { 64i8 64i16 64i32 64i64 64u8 64u16 64u32 64u64 }
-- input { 9223372036854775807i64 }
-- output { -1i8 -1i16 -1i32 9223372036854775807i64
--          255u8 65535u16 4294967295u32 9223372036854775807u64 }
-- input { -9223372036854775808i64 }
-- output { 0i8 0i16 0i32 -9223372036854775808i64
--          0u8 0u16 0u32 9223372036854775808u64 }

let main(x: i64): (i8,i16,i32,i64,u8,u16,u32,u64) =
  (i8(x), i16(x), i32(x), i64(x),
   u8(x), u16(x), u32(x), u64(x))
