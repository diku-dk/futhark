-- Simple test for flattening an update with a constant value
-- ==
-- input { [1i64,2i64,3i64] } 
-- output { [[10i64, 1i64, 2i64], [10i64, 2i64, 3i64], [10i64, 3i64, 4i64]] }
entry main (xs: []i64) =
    map (\x -> [10, x, x+1]) xs