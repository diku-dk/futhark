-- Test of division-like operators for u64 values.

-- ==
-- entry: divide
-- input { [7u64, 9223372036854775808u64]
--         [3u64, 9u64] }
-- output { [2u64, 1024819115206086200u64] }

-- ==
-- entry: mod
-- input { [7u64, 9223372036854775808u64]
--         [3u64, 9u64] }
-- output { [1u64, 8u64] }

-- ==
-- entry: quot
-- input { [7u64, 9223372036854775808u64]
--         [3u64, 9u64] }
-- output { [2u64, 1024819115206086200u64] }

-- ==
-- entry: rem
-- input { [7u64, 9223372036854775808u64]
--         [3u64, 9u64] }
-- output { [1u64, 8u64] }

entry divide = map2 (u64./)
entry mod = map2 (u64.%)
entry quot = map2 (u64.//)
entry rem = map2 (u64.%%)


