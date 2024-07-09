-- Test comparison of i8 values.
--
-- ==
-- entry: lt
-- input { [0i8, 1i8, -1i8, 1i8, -2i8 ]
--         [0i8, 2i8, 1i8, -1i8, -1i8] }
-- output { [false, true, true, false, true] }


-- ==
-- entry: eq
-- input { [0i8, 1i8, -1i8, 1i8, -2i8 ]
--         [0i8, 2i8, 1i8, -1i8, -1i8] }
-- output { [true, false, false, false, false] }

-- ==
-- entry: lte
-- input { [0i8, 1i8, -1i8, 1i8, -2i8 ]
--         [0i8, 2i8, 1i8, -1i8, -1i8] }
-- output { [true, true, true, false, true] }


entry lt (x:[]i8) (y:[]i8)= map2 (<) x y
entry eq (x:[]i8) (y:[]i8)= map2 (==) x y
entry lte (x:[]i8) (y:[]i8)= map2 (<=) x y