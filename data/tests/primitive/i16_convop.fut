-- Convert back and forth between different integer types.
--
-- ==
-- input { 0i16 } output { 0i8 0i16 0i32 0i64 }
-- input { 64i16 } output { 64i8 64i16 64i32 64i64 }
-- input { 32767i16 } output { 127i8 32767i16 32767i32 32767i64 }
-- input { -32768i16 } output { -128i8 -32768i16 -32768i32 -32768i64 }

fun {i8,i16,i32,i64} main(i16 x) =
  {i8(x), i16(x), i32(x), i64(x)}
