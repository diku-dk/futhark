-- i16 test.
--
-- ==
-- input { 0 0i16 0i16 } output { 0i16 }
-- input { 0 2i16 2i16 } output { 4i16 }
-- input { 0 32767i16 32767i16 } output { -2i16 }
-- input { 0 32767i16 -2i16 } output { 32765i16 }
--
-- input { 1 2i16 2i16 } output { 0i16 }
-- input { 1 0i16 32767i16 } output { -32767i16 }
-- input { 1 32767i16 -2i16 } output { -32767i16 }
--
-- input { 2 2i16 3i16 } output { 6i16 }
-- input { 2 2i16 -3i16 } output { -6i16 }
-- input { 2 -2i16 3i16 } output { -6i16 }
-- input { 2 -2i16 -3i16 } output { 6i16 }
-- input { 2 128i16 512i16 } output { 0i16 }
--
-- input { 3 2i16 3i16 } output { 8i16 }
-- input { 3 2i16 0i16 } output { 1i16 }
-- input { 3 11i16 1i16 } output { 11i16 }
-- input { 3 11i16 2i16 } output { 121i16 }
-- input { 3 11i16 3i16 } output { 1331i16 }
-- input { 3 11i16 4i16 } output { 14641i16 }
-- input { 3 11i16 5i16 } output { 29979i16 }
-- input { 3 11i16 6i16 } output { 2089i16 }
-- input { 3 11i16 7i16 } output { 22979i16 }

fun main(f: int, x: i16, y: i16): i16 =
  if      f == 0 then x + y
  else if f == 1 then x - y
  else if f == 2 then x * y
  else           x ** y
