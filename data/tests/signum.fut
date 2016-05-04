-- Test the sign function prefix operator.
--
-- ==
-- input { 2 0 -3 [1,-2,3,0] }
-- output { 1 0 -1 [1,-1,1,0] }

fun (int,int,int,[int]) main(int x, int y, int z, [int] a) =
  (signum x,
   signum y,
   signum z,
   map(signum,a))
