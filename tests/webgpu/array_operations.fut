-- Test array operations with emulated types in WebGPU.
-- Ensures correct memory layout and indexing for packed types.

-- ==
-- entry: i8_index
-- input { [1i8, 2i8, 3i8, 4i8, 5i8] [0i64, 2i64, 4i64] }
-- output { [1i8, 3i8, 5i8] }

-- ==
-- entry: i16_index
-- input { [100i16, 200i16, 300i16, 400i16] [1i64, 3i64] }
-- output { [200i16, 400i16] }

-- ==
-- entry: i64_index
-- input { [4294967296i64, 4294967297i64, 4294967298i64] [0i64, 2i64] }
-- output { [4294967296i64, 4294967298i64] }

-- ==
-- entry: i8_update
-- input { [1i8, 2i8, 3i8, 4i8, 5i8] [1i64, 3i64] [10i8, 20i8] }
-- output { [1i8, 10i8, 3i8, 20i8, 5i8] }

-- ==
-- entry: i64_update
-- input { [1i64, 2i64, 3i64] [0i64, 2i64] [4294967296i64, 8589934592i64] }
-- output { [4294967296i64, 2i64, 8589934592i64] }

-- ==
-- entry: i8_scatter
-- input { [0i8, 0i8, 0i8, 0i8, 0i8]
--         [0i64, 2i64, 4i64]
--         [1i8, 2i8, 3i8] }
-- output { [1i8, 0i8, 2i8, 0i8, 3i8] }

-- ==
-- entry: i64_scatter
-- input { [0i64, 0i64, 0i64]
--         [0i64, 2i64]
--         [4294967296i64, 8589934592i64] }
-- output { [4294967296i64, 0i64, 8589934592i64] }

-- ==
-- entry: i8_replicate
-- input { 5i64 42i8 }
-- output { [42i8, 42i8, 42i8, 42i8, 42i8] }

-- ==
-- entry: i64_replicate
-- input { 3i64 4294967296i64 }
-- output { [4294967296i64, 4294967296i64, 4294967296i64] }

-- ==
-- entry: i8_iota
-- input { 5i64 }
-- output { [0i8, 1i8, 2i8, 3i8, 4i8] }

-- ==
-- entry: f16_iota
-- input { 5i64 }
-- output { [0f16, 1f16, 2f16, 3f16, 4f16] }

entry i8_index (arr: []i8) (is: []i64) = map (\i -> arr[i]) is
entry i16_index (arr: []i16) (is: []i64) = map (\i -> arr[i]) is
entry i64_index (arr: []i64) (is: []i64) = map (\i -> arr[i]) is

entry i8_update (arr: *[]i8) (is: []i64) (vs: []i8) = scatter arr is vs
entry i64_update (arr: *[]i64) (is: []i64) (vs: []i64) = scatter arr is vs

entry i8_scatter (arr: *[]i8) (is: []i64) (vs: []i8) = scatter arr is vs
entry i64_scatter (arr: *[]i64) (is: []i64) (vs: []i64) = scatter arr is vs

entry i8_replicate (n: i64) (v: i8) = replicate n v
entry i64_replicate (n: i64) (v: i64) = replicate n v

entry i8_iota (n: i64) = map i8.i64 (iota n)
entry f16_iota (n: i64) = map f16.i64 (iota n)
