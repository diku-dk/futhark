-- This might look like a transposition to the naive, but it is not!
-- ==
-- input { [[1, 2], [3, 4]] } output { [[1, 2], [3, 4]] }

entry main [n] [m] (A: [n][m]i32) : [m][n]i32 = A[0:m, 0:n]
