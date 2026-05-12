-- Test comparison of u32 values.

-- ==
-- entry: lt
-- input { [0u32, 1u32, 4294967295u32, 1u32]
--         [0u32, 2u32, 1u32, 4294967295u32] }
-- output { [false, true, false, true] }

-- ==
-- entry: eq
-- input { [0u32, 1u32, 4294967295u32, 1u32]
--         [0u32, 2u32, 1u32, 4294967295u32] }
-- output { [true, false, false, false] }

-- ==
-- entry: lte
-- input { [0u32, 1u32, 4294967295u32, 1u32]
--         [0u32, 2u32, 1u32, 4294967295u32] }
-- output { [true, true, false, true] }

entry lt (x: []u32) (y: []u32) = map2 (<) x y
entry eq (x: []u32) (y: []u32) = map2 (==) x y
entry lte (x: []u32) (y: []u32) = map2 (<=) x y
