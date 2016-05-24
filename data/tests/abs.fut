-- Test the absolute value prefix operator.
--
-- ==
-- input { 2 -3 [1,-2,3] }
-- output { 2 3 [1,2,3] }

fun (int,int,[int]) main(int x, int y, [int] a) =
  (abs x,
   abs y,
   map(abs,a))
