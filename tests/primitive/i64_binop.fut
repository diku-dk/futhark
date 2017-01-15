-- i64 test.
--
-- ==
-- input { 0 0i64 0i64 } output { 0i64 }
-- input { 0 2i64 2i64 } output { 4i64 }
-- input { 0 9223372036854775807i64 9223372036854775807i64 } output { -2i64 }
-- input { 0 9223372036854775807i64 -2i64 } output { 9223372036854775805i64 }
--
-- input { 1 2i64 2i64 } output { 0i64 }
-- input { 1 0i64 9223372036854775807i64 } output { -9223372036854775807i64 }
-- input { 1 9223372036854775807i64 -2i64 } output { -9223372036854775807i64 }
--
-- input { 2 2i64 3i64 } output { 6i64 }
-- input { 2 2i64 -3i64 } output { -6i64 }
-- input { 2 -2i64 3i64 } output { -6i64 }
-- input { 2 -2i64 -3i64 } output { 6i64 }
-- input { 2 6442450941i64 2147483647i64 } output { -4611686031312289789i64 }
--
-- input { 3 2i64 3i64 } output { 8i64 }
-- input { 3 4021i64 0i64 } output { 1i64 }
-- input { 3 4021i64 1i64 } output { 4021i64 }
-- input { 3 4021i64 2i64 } output { 16168441i64 }
-- input { 3 4021i64 3i64 } output { 65013301261i64 }
-- input { 3 4021i64 4i64 } output { 261418484370481i64 }
-- input { 3 4021i64 5i64 } output { 1051163725653704101i64 }
-- input { 3 4021i64 6i64 } output { 2424947974056870057i64 }
-- input { 3 4021i64 7i64 } output { -7611811309678305667i64 }

fun main(f: i32, x: i64, y: i64): i64 =
  if      f == 0 then x + y
  else if f == 1 then x - y
  else if f == 2 then x * y
  else           x ** y
