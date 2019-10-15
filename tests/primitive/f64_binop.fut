-- f64 test.  Does not test for infinity/NaN as we have no way of writing
-- that in Futhark yet.  Does test for overflow.
--
-- ==
-- input { 0 0.0 0.0 }
-- output { 0.0 }
-- input { 0 1.0 0.0 }
-- output { 1.0 }
-- input { 0 1.0 0.0 }
-- output { 1.0 }
-- input { 0 -1.0 0.0 }
-- output { -1.0 }
-- input { 0 1.79769e308 10.0 }
-- output { 1.79769e308 }
-- input { 0 0f64 f64.nan }
-- output { f64.nan }
-- input { 0 0f64 f64.inf }
-- output { f64.inf }
--
-- input { 1 0.0 0.0 }
-- output { 0.0 }
-- input { 1 0.0 1.0 }
-- output { -1.0 }
-- input { 1 0.0 -1.0 }
-- output { 1.0 }
-- input { 1 -1.79769e308 10.0 }
-- output { -1.79769e308 }
--
-- input { 2 0.0 0.0 }
-- output { 0.0 }
-- input { 2 0.0 1.0 }
-- output { 0.0 }
-- input { 2 0.0 -1.0 }
-- output { 0.0 }
-- input { 2 1.0 -1.0 }
-- output { -1.0 }
-- input { 2 2.0 1.5 }
-- output { 3.0 }
--
-- input { 3 0.0 1.0 }
-- output { 0.0 }
-- input { 3 0.0 -1.0 }
-- output { 0.0 }
-- input { 3 1.0 -1.0 }
-- output { -1.0 }
-- input { 3 2.0 1.5 }
-- output { 1.3333333333333 }
--
-- input { 4 0.0f64 1.0f64 }
-- output { 0.0f64 }
-- input { 4 0.0f64 -1.0f64 }
-- output { -0.0f64 }
-- input { 4 1.0f64 -1.0f64 }
-- output { -0.0f64 }
-- input { 4 2.0f64 1.5f64 }
-- output { 0.5f64 }
--
-- input { 5 0.0 1.0 }
-- output { 0.0 }
-- input { 5 1.0 -1.0 }
-- output { 1.0 }
-- input { 5 2.0 1.5 }
-- output { 2.8284271247461903 }
-- input { 5 2.00 0.0 }
-- output { 1.0 }

let main (f: i32) (x: f64) (y: f64): f64 =
  if      f == 0 then x + y
  else if f == 1 then x - y
  else if f == 2 then x * y
  else if f == 3 then x / y
  else if f == 4 then x % y
  else           x ** y
