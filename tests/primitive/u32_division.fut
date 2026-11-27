-- Test of division-like operators for u32 values.  Dividing by zero
-- is an error.

-- ==
-- entry: divide
-- input { [7u32, 2147483648u32]
--         [3u32, 9u32] }
-- output { [2u32, 238609294u32] }
-- compiled no_webgpu input { [1u32] [0u32] } error: division by zero

-- ==
-- entry: mod
-- input { [7u32, 2147483648u32]
--         [3u32, 9u32] }
-- output { [1u32, 2u32] }
-- compiled no_webgpu input { [1u32] [0u32] } error: division by zero

-- ==
-- entry: quot
-- input { [7u32, 2147483648u32]
--         [3u32, 9u32] }
-- output { [2u32, 238609294u32] }
-- compiled no_webgpu input { [1u32] [0u32] } error: division by zero

-- ==
-- entry: rem
-- input { [7u32, 2147483648u32]
--         [3u32, 9u32] }
-- output { [1u32, 2u32] }
-- compiled no_webgpu input { [1u32] [0u32] } error: division by zero

entry divide = map2 (u32./)
entry mod = map2 (u32.%)
entry quot = map2 (u32.//)
entry rem = map2 (u32.%%)
