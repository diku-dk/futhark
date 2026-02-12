-- Test parallel patterns with WebGPU emulated types.
-- Verifies that map, reduce, scan work correctly with packed types.

-- ==
-- entry: map_i8
-- input { [1i8, 2i8, 3i8, 4i8, 5i8] }
-- output { [2i8, 4i8, 6i8, 8i8, 10i8] }

-- ==
-- entry: map_i64
-- input { [1i64, 4294967296i64, 9223372036854775807i64] }
-- output { [2i64, 8589934592i64, -2i64] }

-- ==
-- entry: reduce_i8
-- input { [1i8, 2i8, 3i8, 4i8, 5i8] }
-- output { 15i8 }

-- ==
-- entry: reduce_i64
-- input { [1i64, 2i64, 4294967296i64] }
-- output { 4294967299i64 }

-- ==
-- entry: scan_i8
-- input { [1i8, 2i8, 3i8, 4i8, 5i8] }
-- output { [1i8, 3i8, 6i8, 10i8, 15i8] }

-- ==
-- entry: scan_i64
-- input { [1i64, 2i64, 4294967296i64] }
-- output { [1i64, 3i64, 4294967299i64] }

-- ==
-- entry: filter_i8
-- input { [1i8, -2i8, 3i8, -4i8, 5i8] }
-- output { [1i8, 3i8, 5i8] }

-- ==
-- entry: map2_i8_i16
-- input { [1i8, 2i8, 3i8]
--         [10i16, 20i16, 30i16] }
-- output { [11i32, 22i32, 33i32] }

-- ==
-- entry: reduce_f16
-- input { [1f16, 2f16, 3f16, 4f16, 5f16] }
-- output { 15f16 }

-- ==
-- entry: segmented_reduce_i8
-- input { [[1i8, 2i8, 3i8], [4i8, 5i8, 6i8], [7i8, 8i8, 9i8]] }
-- output { [6i8, 15i8, 24i8] }

entry map_i8 = map (\x -> x * 2i8)
entry map_i64 = map (\x -> x * 2i64)

entry reduce_i8 = reduce (+) 0i8
entry reduce_i64 = reduce (+) 0i64
entry reduce_f16 = reduce (+) 0f16

entry scan_i8 = scan (+) 0i8
entry scan_i64 = scan (+) 0i64

entry filter_i8 = filter (\x -> x > 0i8)

entry map2_i8_i16 (a: []i8) (b: []i16) = map2 (\x y -> i32.i8 x + i32.i16 y) a b

entry segmented_reduce_i8 = map (reduce (+) 0i8)
