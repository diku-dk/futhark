-- Test of division-like operators for i16 values.  Dividing the
-- most negative value by -1 wraps around.

-- ==
-- entry: divide
-- input { [7i16, -7i16, 7i16, -7i16, -32768i16, -32768i16, 32767i16]
--         [3i16, 3i16, -3i16, -3i16, -1i16, 1i16, -1i16] }
-- output { [2i16, -3i16, -3i16, 2i16, -32768i16, -32768i16, -32767i16] }

-- ==
-- entry: mod
-- input { [7i16, -7i16, 7i16, -7i16, -32768i16, -32768i16, 32767i16]
--         [3i16, 3i16, -3i16, -3i16, -1i16, 1i16, -1i16] }
-- output { [1i16, 2i16, -2i16, -1i16, 0i16, 0i16, 0i16] }

-- ==
-- entry: quot
-- input { [7i16, -7i16, 7i16, -7i16, -32768i16, -32768i16, 32767i16]
--         [3i16, 3i16, -3i16, -3i16, -1i16, 1i16, -1i16] }
-- output { [2i16, -2i16, -2i16, 2i16, -32768i16, -32768i16, -32767i16] }

-- ==
-- entry: rem
-- input { [7i16, -7i16, 7i16, -7i16, -32768i16, -32768i16, 32767i16]
--         [3i16, 3i16, -3i16, -3i16, -1i16, 1i16, -1i16] }
-- output { [1i16, -1i16, 1i16, -1i16, 0i16, 0i16, 0i16] }

entry divide = map2 (i16./)
entry mod = map2 (i16.%)
entry quot = map2 (i16.//)
entry rem = map2 (i16.%%)
