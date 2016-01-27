-- Convert back and forth between different integer types.
--
-- ==
-- input { 0i8 } output { 0i8 0i16 0i32 0i64 }
-- input { 64i8 } output { 64i8 64i16 64i32 64i64 }
-- input { 127i8 } output { 127i8 127i16 127i32 127i64 }
-- input { -128i8 } output { -128i8 -128i16 -128i32 -128i64 }

fun {i8,i16,i32,i64} main(i8 x) =
  {i8(x), i16(x), i32(x), i64(x)}
