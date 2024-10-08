-- Test of division-like operators for i64 values.
-- ==
-- entry: divide
-- input { [7i64, -7i64, 7i64, -7i64]
--         [3i64, 3i64, -3i64, -3i64] }
-- output { [2i64, -3i64, -3i64, 2i64] }
-- ==
-- entry: mod
-- input { [7i64, -7i64, 7i64, -7i64]
--         [3i64, 3i64, -3i64, -3i64] }
-- output { [1i64, 2i64, -2i64, -1i64] }
-- ==
-- entry: quot
-- input { [7i64, -7i64, 7i64, -7i64]
--         [3i64, 3i64, -3i64, -3i64] }
-- output { [2i64, -2i64, -2i64, 2i64] }
-- ==
-- entry: rem
-- input { [7i64, -7i64, 7i64, -7i64]
--         [3i64, 3i64, -3i64, -3i64] }
-- output { [1i64, -1i64, 1i64, -1i64] }
entry divide = map2 i64.(/)

entry mod = map2 i64.(%)

entry quot = map2 i64.(//)

entry rem = map2 i64.(%%)