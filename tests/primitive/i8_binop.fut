-- i8 test.
--
-- ==
-- input { 0 0i8 0i8 } output { 0i8 }
-- input { 0 2i8 2i8 } output { 4i8 }
-- input { 0 127i8 127i8 } output { -2i8 }
-- input { 0 127i8 -2i8 } output { 125i8 }
--
-- input { 1 2i8 2i8 } output { 0i8 }
-- input { 1 0i8 127i8 } output { -127i8 }
-- input { 1 127i8 -2i8 } output { -127i8 }
--
-- input { 2 2i8 3i8 } output { 6i8 }
-- input { 2 2i8 -3i8 } output { -6i8 }
-- input { 2 -2i8 3i8 } output { -6i8 }
-- input { 2 -2i8 -3i8 } output { 6i8 }
-- input { 2 4i8 64i8 } output { 0i8 }
--
-- input { 3 2i8 3i8 } output { 8i8 }
-- input { 3 2i8 0i8 } output { 1i8 }
-- input { 3 3i8 1i8 } output { 3i8 }
-- input { 3 3i8 2i8 } output { 9i8 }
-- input { 3 3i8 3i8 } output { 27i8 }
-- input { 3 3i8 4i8 } output { 81i8 }
-- input { 3 3i8 5i8 } output { -13i8 }
-- input { 3 3i8 6i8 } output { -39i8 }
-- input { 3 3i8 7i8 } output { -117i8 }

let main (f: i32) (x: i8) (y: i8): i8 =
  if      f == 0 then x + y
  else if f == 1 then x - y
  else if f == 2 then x * y
  else           x ** y
