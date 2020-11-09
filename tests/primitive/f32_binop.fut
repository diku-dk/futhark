-- f32 test.  Does not test for infinity/NaN as we have no way of writing
-- that in Futhark yet.  Does test for overflow.
--
-- ==
-- input { 0 0.0f32 0.0f32 }
-- output { 0.0f32 }
-- input { 0 1.0f32 0.0f32 }
-- output { 1.0f32 }
-- input { 0 1.0f32 0.0f32 }
-- output { 1.0f32 }
-- input { 0 -1.0f32 0.0f32 }
-- output { -1.0f32 }
-- input { 0 3.402823e38f32 10f32 }
-- output { 340282306073709652508363335590014353408.000000f32 }
-- input { 0 0f32 f32.nan }
-- output { f32.nan }
-- input { 0 0f32 f32.inf }
-- output { f32.inf }
--
-- input { 1 0.0f32 0.0f32 }
-- output { 0.0f32 }
-- input { 1 0.0f32 1.0f32 }
-- output { -1.0f32 }
-- input { 1 0.0f32 -1.0f32 }
-- output { 1.0f32 }
-- input { 1 -3.402823e38f32 10f32 }
-- output { -340282306073709652508363335590014353408.000000f32 }
--
-- input { 2 0.0f32 0.0f32 }
-- output { 0.0f32 }
-- input { 2 0.0f32 1.0f32 }
-- output { 0.0f32 }
-- input { 2 0.0f32 -1.0f32 }
-- output { 0.0f32 }
-- input { 2 1.0f32 -1.0f32 }
-- output { -1.0f32 }
-- input { 2 2.0f32 1.5f32 }
-- output { 3.0f32 }
--
-- input { 3 0.0f32 1.0f32 }
-- output { 0.0f32 }
-- input { 3 0.0f32 -1.0f32 }
-- output { 0.0f32 }
-- input { 3 1.0f32 -1.0f32 }
-- output { -1.0f32 }
-- input { 3 2.0f32 1.5f32 }
-- output { 1.3333333333333f32 }
--
-- input { 4 0.0f32 1.0f32 }
-- output { 0.0f32 }
-- input { 4 0.0f32 -1.0f32 }
-- output { -0.0f32 }
-- input { 4 1.0f32 -1.0f32 }
-- output { -0.0f32 }
-- input { 4 2.0f32 1.5f32 }
-- output { 0.5f32 }
--
-- input { 5 0.0f32 1.0f32 }
-- output { 0.0f32 }
-- input { 5 1.0f32 -1.0f32 }
-- output { 1.0f32 }
-- input { 5 2.0f32 1.5f32 }
-- output { 2.8284271247461903f32 }
-- input { 5 2.0f32 0f32 }
-- output { 1f32 }

let main (f: i32) (x: f32) (y: f32): f32 =
  if      f == 0 then x + y
  else if f == 1 then x - y
  else if f == 2 then x * y
  else if f == 3 then x / y
  else if f == 4 then x % y
  else                x ** y
