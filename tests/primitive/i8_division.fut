-- Test of division-like operators for i8 values.  Dividing the
-- most negative value by -1 wraps around.

-- ==
-- entry: divide
-- input { [7i8, -7i8, 7i8, -7i8, -128i8, -128i8, 127i8]
--         [3i8, 3i8, -3i8, -3i8, -1i8, 1i8, -1i8] }
-- output { [2i8, -3i8, -3i8, 2i8, -128i8, -128i8, -127i8] }

-- ==
-- entry: mod
-- input { [7i8, -7i8, 7i8, -7i8, -128i8, -128i8, 127i8]
--         [3i8, 3i8, -3i8, -3i8, -1i8, 1i8, -1i8] }
-- output { [1i8, 2i8, -2i8, -1i8, 0i8, 0i8, 0i8] }

-- ==
-- entry: quot
-- input { [7i8, -7i8, 7i8, -7i8, -128i8, -128i8, 127i8]
--         [3i8, 3i8, -3i8, -3i8, -1i8, 1i8, -1i8] }
-- output { [2i8, -2i8, -2i8, 2i8, -128i8, -128i8, -127i8] }

-- ==
-- entry: rem
-- input { [7i8, -7i8, 7i8, -7i8, -128i8, -128i8, 127i8]
--         [3i8, 3i8, -3i8, -3i8, -1i8, 1i8, -1i8] }
-- output { [1i8, -1i8, 1i8, -1i8, 0i8, 0i8, 0i8] }

entry divide = map2 (i8./)
entry mod = map2 (i8.%)
entry quot = map2 (i8.//)
entry rem = map2 (i8.%%)
