-- Test comparison of i32 values.
--
-- ==
-- entry: lt
-- input { [0i32, 1i32, -1i32, 1i32, -2i32 ]
--         [0i32, 2i32, 1i32, -1i32, -1i32] }
-- output { [false, true, true, false, true] }

-- ==
-- entry: eq
-- input { [0i32, 1i32, -1i32, 1i32, -2i32 ]
--         [0i32, 2i32, 1i32, -1i32, -1i32] }
-- output { [true, false, false, false, false] }

-- ==
-- entry: lte
-- input { [0i32, 1i32, -1i32, 1i32, -2i32 ]
--         [0i32, 2i32, 1i32, -1i32, -1i32] }
-- output { [true, true, true, false, true] }

entry lt (x: []i32) (y: []i32) = map2 (<) x y
entry eq (x: []i32) (y: []i32) = map2 (==) x y
entry lte (x: []i32) (y: []i32) = map2 (<=) x y
