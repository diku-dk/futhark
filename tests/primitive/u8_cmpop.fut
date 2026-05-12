-- Test comparison of u8 values.

-- ==
-- entry: lt
-- input { [0u8, 1u8, 255u8, 1u8]
--         [0u8, 2u8, 1u8, 255u8] }
-- output { [false, true, false, true] }

-- ==
-- entry: eq
-- input { [0u8, 1u8, 255u8, 1u8]
--         [0u8, 2u8, 1u8, 255u8] }
-- output { [true, false, false, false] }

-- ==
-- entry: lte
-- input { [0u8, 1u8, 255u8, 1u8]
--         [0u8, 2u8, 1u8, 255u8] }
-- output { [true, true, false, true] }

entry lt (x: []u8) (y: []u8) = map2 (<) x y
entry eq (x: []u8) (y: []u8) = map2 (==) x y
entry lte (x: []u8) (y: []u8) = map2 (<=) x y
