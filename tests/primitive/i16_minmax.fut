-- ==
-- entry: testMax
-- input { [0i16, 1i16, -1i16, 1i16]
--         [1i16, 1i16, 1i16, -1i16]}
-- output { [1i16, 1i16, 1i16, 1i16] }

-- ==
-- entry: testMin
-- input { [0i16, 1i16, -1i16, 1i16]
--         [1i16, 1i16, 1i16, -1i16]}
-- output { [0i16, 1i16, -1i16, -1i16] }
entry testMax = map2 i16.max
entry testMin = map2 i16.min