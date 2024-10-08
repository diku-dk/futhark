-- Test of division-like operators for i16 values.

-- ==
-- entry: divide
-- input { [7i16, -7i16, 7i16, -7i16]
--         [3i16, 3i16, -3i16, -3i16] }
-- output { [2i16, -3i16, -3i16, 2i16] }

-- ==
-- entry: mod
-- input { [7i16, -7i16, 7i16, -7i16]
--         [3i16, 3i16, -3i16, -3i16] }
-- output { [1i16, 2i16, -2i16, -1i16] }

-- ==
-- entry: quot
-- input { [7i16, -7i16, 7i16, -7i16]
--         [3i16, 3i16, -3i16, -3i16] }
-- output { [2i16, -2i16, -2i16, 2i16] }

-- ==
-- entry: rem
-- input { [7i16, -7i16, 7i16, -7i16]
--         [3i16, 3i16, -3i16, -3i16] }
-- output { [1i16, -1i16, 1i16, -1i16] }

entry divide = map2 (i16./)
entry mod = map2 (i16.%)
entry quot = map2 (i16.//)
entry rem = map2 (i16.%%)