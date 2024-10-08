-- Test of division-like operators for u16 values.
-- ==
-- entry: divide
-- input { [7u16, 32768u16]
--         [3u16, 9u16] }
-- output { [2u16, 3640u16] }
-- ==
-- entry: mod
-- input { [7u16, 32768u16]
--         [3u16, 9u16] }
-- output { [1u16, 8u16] }
-- ==
-- entry: quot
-- input { [7u16, 32768u16]
--         [3u16, 9u16] }
-- output { [2u16, 3640u16] }
-- ==
-- entry: rem
-- input { [7u16, 32768u16]
--         [3u16, 9u16] }
-- output { [1u16, 8u16] }
entry divide = map2 u16.(/)

entry mod = map2 u16.(%)

entry quot = map2 u16.(//)

entry rem = map2 u16.(%%)