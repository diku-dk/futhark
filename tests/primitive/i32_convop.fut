-- Convert back and forth between different integer types.
--
-- ==
-- input { 0i32 } output { false 0i8 0i16 0i32 0i64 0u8 0u16 0u32 0u64 }
-- input { 64i32 } output { true 64i8 64i16 64i32 64i64 64u8 64u16 64u32 64u64 }
-- input { 2147483647i32 }
-- output { true -1i8 -1i16 2147483647i32 2147483647i64
--          255u8 65535u16 2147483647u32 2147483647u64 }
-- input { -2147483648i32 }
-- output { true 0i8 0i16 -2147483648i32 -2147483648i64
--          0u8 0u16 2147483648u32 2147483648u64 }

let main(x: i32): (bool,i8,i16,i32,i64,u8,u16,u32,u64) =
  (bool.i32(x),
   i8.i32(x), i16.i32(x), i32.i32(x), i64.i32(x),
   u8.i32(x), u16.i32(x), u32.i32(x), u64.i32(x))
