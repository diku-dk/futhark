-- ==
-- entry: testMax
-- input { [0u8, 1u8, 255u8, 1u8]
--         [1u8, 1u8, 1u8, 255u8]}
-- output { [1u8, 1u8, 255u8, 255u8] }

-- ==
-- entry: testMin
-- input { [0u8, 1u8, 255u8, 1u8]
--         [1u8, 1u8, 1u8, 255u8]}
-- output { [0u8, 1u8, 1u8, 1u8] }

entry testMax = map2 u8.max
entry testMin = map2 u8.min
