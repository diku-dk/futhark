-- f32 test.  Does not test for infinity/NaN as we have no way of writing
-- that in Futhark yet.  Does test for overflow.
--
-- ==
-- input { 0 0.0f 0.0f }
-- output { 0.0f }
-- input { 0 1.0f 0.0f }
-- output { 1.0f }
-- input { 0 1.0f 0.0f }
-- output { 1.0f }
-- input { 0 -1.0f 0.0f }
-- output { -1.0f }
-- input { 0 3.402823e38f 10f }
-- output { 340282306073709652508363335590014353408.000000f }
--
-- input { 1 0.0f 0.0f }
-- output { 0.0f }
-- input { 1 0.0f 1.0f }
-- output { -1.0f }
-- input { 1 0.0f -1.0f }
-- output { 1.0f }
-- input { 1 -3.402823e38f 10f }
-- output { -340282306073709652508363335590014353408.000000f }
--
-- input { 2 0.0f 0.0f }
-- output { 0.0f }
-- input { 2 0.0f 1.0f }
-- output { 0.0f }
-- input { 2 0.0f -1.0f }
-- output { 0.0f }
-- input { 2 1.0f -1.0f }
-- output { -1.0f }
-- input { 2 2.0f 1.5f }
-- output { 3.0f }
--
-- input { 3 0.0f 1.0f }
-- output { 0.0f }
-- input { 3 0.0f -1.0f }
-- output { 0.0f }
-- input { 3 1.0f -1.0f }
-- output { -1.0f }
-- input { 3 2.0f 1.5f }
-- output { 1.3333333333333f }
--
-- input { 4 0.0f 1.0f }
-- output { 0.0f }
-- input { 4 1.0f -1.0f }
-- output { 1.0f }
-- input { 4 2.0f 1.5f }
-- output { 2.8284271247461903f }
-- input { 4 2.0f 0f }
-- output { 1f }

fun f32 main(int f, f32 x, f32 y) =
  if      f == 0 then x + y
  else if f == 1 then x - y
  else if f == 2 then x * y
  else if f == 3 then x / y
  else           x ** y
