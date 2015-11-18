-- Test that remainder and quotient on integers works properly.
--
-- ==
-- input {  7  3 } output {  2  1 }
-- input { -7  3 } output { -2 -1 }
-- input { 7  -3 } output { -2  1 }
-- input { -7 -3 } output {  2 -1 }

fun {int,int} main(int x, int y) = {x // y, x %% y}
