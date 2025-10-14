-- Test comparison of u16 values.

-- ==
-- entry: lt
-- input { [0u16, 1u16, 65535u16, 1u16]
--         [0u16, 2u16, 1u16, 65535u16] }
-- output { [false, true, false, true] }

-- ==
-- entry: eq
-- input { [0u16, 1u16, 65535u16, 1u16]
--         [0u16, 2u16, 1u16, 65535u16] }
-- output { [true, false, false, false] }

-- ==
-- entry: lte
-- input { [0u16, 1u16, 65535u16, 1u16]
--         [0u16, 2u16, 1u16, 65535u16] }
-- output { [true, true, false, true] }

entry lt (x: []u16) (y: []u16) = map2 (<) x y
entry eq (x: []u16) (y: []u16) = map2 (==) x y
entry lte (x: []u16) (y: []u16) = map2 (<=) x y
