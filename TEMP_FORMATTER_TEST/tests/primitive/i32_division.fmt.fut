-- Test of division-like operators for i32 values.
-- ==
-- entry: divide
-- input { [7i32, -7i32, 7i32, -7i32]
--         [3i32, 3i32, -3i32, -3i32] }
-- output { [2i32, -3i32, -3i32, 2i32] }
-- ==
-- entry: mod
-- input { [7i32, -7i32, 7i32, -7i32]
--         [3i32, 3i32, -3i32, -3i32] }
-- output { [1i32, 2i32, -2i32, -1i32] }
-- ==
-- entry: quot
-- input { [7i32, -7i32, 7i32, -7i32]
--         [3i32, 3i32, -3i32, -3i32] }
-- output { [2i32, -2i32, -2i32, 2i32] }
-- ==
-- entry: rem
-- input { [7i32, -7i32, 7i32, -7i32]
--         [3i32, 3i32, -3i32, -3i32] }
-- output { [1i32, -1i32, 1i32, -1i32] }
entry divide = map2 i32.(/)

entry mod = map2 i32.(%)

entry quot = map2 i32.(//)

entry rem = map2 i32.(%%)