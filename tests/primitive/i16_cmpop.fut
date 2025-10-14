-- Test comparison of i16 values.
--
-- ==
-- entry: lt
-- input { [0i16, 1i16, -1i16, 1i16, -2i16 ]
--         [0i16, 2i16, 1i16, -1i16, -1i16] }
-- output { [false, true, true, false, true] }

-- ==
-- entry: eq
-- input { [0i16, 1i16, -1i16, 1i16, -2i16 ]
--         [0i16, 2i16, 1i16, -1i16, -1i16] }
-- output { [true, false, false, false, false] }

-- ==
-- entry: lte
-- input { [0i16, 1i16, -1i16, 1i16, -2i16 ]
--         [0i16, 2i16, 1i16, -1i16, -1i16] }
-- output { [true, true, true, false, true] }

entry lt (x: []i16) (y: []i16) = map2 (<) x y
entry eq (x: []i16) (y: []i16) = map2 (==) x y
entry lte (x: []i16) (y: []i16) = map2 (<=) x y
