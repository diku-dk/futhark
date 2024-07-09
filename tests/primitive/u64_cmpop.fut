-- Test comparison of u64 values.

-- ==
-- entry: lt
-- input { [0u64, 1u64, 18446744073709551615u64, 1u64]
--         [0u64, 2u64, 1u64, 18446744073709551615u64] }
-- output { [false, true, false, true] }


-- ==
-- entry: eq
-- input { [0u64, 1u64, 18446744073709551615u64, 1u64]
--         [0u64, 2u64, 1u64, 18446744073709551615u64] }
-- output { [true, false, false, false] }

-- ==
-- entry: lte
-- input { [0u64, 1u64, 18446744073709551615u64, 1u64]
--         [0u64, 2u64, 1u64, 18446744073709551615u64] }
-- output { [true, true, false, true] }


entry lt (x:[]u64) (y:[]u64)= map2 (<) x y
entry eq (x:[]u64) (y:[]u64)= map2 (==) x y
entry lte (x:[]u64) (y:[]u64)= map2 (<=) x y
