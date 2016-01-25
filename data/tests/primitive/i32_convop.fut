-- Convert back and forth between different integer types.
--
-- ==
-- input { 0i32 } output { 0i8 0i16 0i32 0i64 }
-- input { 64i32 } output { 64i8 64i16 64i32 64i64 }
-- input { 2147483647i32 } output { 127i8 32767i16 2147483647i32 2147483647i64 }
-- input { -2147483648i32 } output { -128i8 -32768i16 -2147483648i32 -2147483648i64 }

fun {i8,i16,i32,i64} main(i32 x) =
  {i8(x), i16(x), i32(x), i64(x)}
