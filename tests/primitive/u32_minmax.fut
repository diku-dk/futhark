-- ==
-- entry: testMax
-- input { [0u32, 1u32, 4294967295u32, 1u32]
--         [1u32, 1u32, 1u32, 4294967295u32]}
-- output { [1u32, 1u32, 4294967295u32, 4294967295u32] }

-- ==
-- entry: testMin
-- input { [0u32, 1u32, 4294967295u32, 1u32]
--         [1u32, 1u32, 1u32, 4294967295u32]}
-- output { [0u32, 1u32, 1u32, 1u32] }

entry testMax = map2 u32.max
entry testMin = map2 u32.min
