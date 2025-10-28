-- ==
-- entry: testMax
-- input { [0u16, 1u16, 65535u16, 1u16]
--         [1u16, 1u16, 1u16, 65535u16]}
-- output { [1u16, 1u16, 65535u16, 65535u16] }

-- ==
-- entry: testMin
-- input { [0u16, 1u16, 65535u16, 1u16]
--         [1u16, 1u16, 1u16, 65535u16]}
-- output { [0u16, 1u16, 1u16, 1u16] }

entry testMax = map2 u16.max
entry testMin = map2 u16.min
