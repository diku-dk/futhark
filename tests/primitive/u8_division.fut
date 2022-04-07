-- Test of division-like operators for u8 values.

-- ==
-- entry: divide
-- input { [7u8, 128u8]
--         [3u8, 9u8] }
-- output { [2u8, 14u8] }

-- ==
-- entry: mod
-- input { [7u8, 128u8]
--         [3u8, 9u8] }
-- output { [1u8, 2u8] }

-- ==
-- entry: quot
-- input { [7u8, 128u8]
--         [3u8, 9u8] }
-- output { [2u8, 14u8] }

-- ==
-- entry: rem
-- input { [7u8, 128u8]
--         [3u8, 9u8] }
-- output { [1u8, 2u8] }

entry divide = map2 (u8./)
entry mod = map2 (u8.%)
entry quot = map2 (u8.//)
entry rem = map2 (u8.%%)
