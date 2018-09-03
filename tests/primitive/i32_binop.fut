-- i32 test.
--
-- ==
-- input { 0 0i32 0i32 } output { 0i32 }
-- input { 0 2i32 2i32 } output { 4i32 }
-- input { 0 2147483647i32 2147483647i32 } output { -2i32 }
-- input { 0 2147483647i32 -2i32 } output { 2147483645i32 }
--
-- input { 1 2i32 2i32 } output { 0i32 }
-- input { 1 0i32 2147483647i32 } output { -2147483647i32 }
-- input { 1 2147483647i32 -2i32 } output { -2147483647i32 }
--
-- input { 2 2i32 3i32 } output { 6i32 }
-- input { 2 2i32 -3i32 } output { -6i32 }
-- input { 2 -2i32 3i32 } output { -6i32 }
-- input { 2 -2i32 -3i32 } output { 6i32 }
-- input { 2 262144i32 262144i32 } output { 0i32 }
--
-- input { 3 2i32 3i32 } output { 8i32 }
-- input { 3 47i32 0i32 } output { 1i32 }
-- input { 3 47i32 1i32 } output { 47i32 }
-- input { 3 47i32 2i32 } output { 2209i32 }
-- input { 3 47i32 3i32 } output { 103823i32 }
-- input { 3 47i32 4i32 } output { 4879681i32 }
-- input { 3 47i32 5i32 } output { 229345007i32 }
-- input { 3 47i32 6i32 } output { -2105686559i32 }
-- input { 3 47i32 7i32 } output { -183020465i32 }

let main (f: i32) (x: i32) (y: i32): i32 =
  if      f == 0 then x + y
  else if f == 1 then x - y
  else if f == 2 then x * y
  else           x ** y
