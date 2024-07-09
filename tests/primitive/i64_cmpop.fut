-- Test comparison of i64 values.
--
-- ==
-- entry: lt
-- input { [0i64, 1i64, -1i64, 1i64, -2i64 ]
--         [0i64, 2i64, 1i64, -1i64, -1i64] }
-- output { [false, true, true, false, true] }


-- ==
-- entry: eq
-- input { [0i64, 1i64, -1i64, 1i64, -2i64 ]
--         [0i64, 2i64, 1i64, -1i64, -1i64] }
-- output { [true, false, false, false, false] }

-- ==
-- entry: lte
-- input { [0i64, 1i64, -1i64, 1i64, -2i64 ]
--         [0i64, 2i64, 1i64, -1i64, -1i64] }
-- output { [true, true, true, false, true] }


entry lt (x:[]i64) (y:[]i64)= map2 (<) x y
entry eq (x:[]i64) (y:[]i64)= map2 (==) x y
entry lte (x:[]i64) (y:[]i64)= map2 (<=) x y