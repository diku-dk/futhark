-- Test mixed precision computations in WebGPU.
-- Exercises combinations of emulated and native types in a single kernel.

-- ==
-- entry: i8_reduction
-- input { [[1i8, 2i8, 3i8, 4i8], [5i8, 6i8, 7i8, 8i8]] }
-- output { [10i8, 26i8] }

-- ==
-- entry: i16_reduction
-- input { [[100i16, 200i16, 300i16, 400i16], [500i16, 600i16, 700i16, 800i16]] }
-- output { [1000i16, 2600i16] }

-- ==
-- entry: i64_reduction
-- input { [[1i64, 2i64, 4294967296i64], [4294967297i64, 4294967298i64, 4294967299i64]] }
-- output { [4294967299i64, 12884901894i64] }

-- ==
-- entry: f16_reduction
-- input { [[1f16, 2f16, 3f16, 4f16], [0.5f16, 0.25f16, 0.125f16, 0.125f16]] }
-- output { [10f16, 1f16] }

-- ==
-- entry: dot_product_i8
-- input { [1i8, 2i8, 3i8, 4i8]
--         [4i8, 3i8, 2i8, 1i8] }
-- output { 20i32 }

-- ==
-- entry: dot_product_i16
-- input { [100i16, 200i16, 300i16]
--         [3i16, 2i16, 1i16] }
-- output { 1000i32 }

-- ==
-- entry: matmul_i8
-- input { [[1i8, 2i8], [3i8, 4i8]]
--         [[5i8, 6i8], [7i8, 8i8]] }
-- output { [[19i32, 22i32], [43i32, 50i32]] }

entry i8_reduction = map i8.sum
entry i16_reduction = map i16.sum
entry i64_reduction = map i64.sum
entry f16_reduction = map f16.sum

entry dot_product_i8 (a: []i8) (b: []i8) : i32 =
  reduce (+) 0 (map2 (\x y -> i32.i8 x * i32.i8 y) a b)

entry dot_product_i16 (a: []i16) (b: []i16) : i32 =
  reduce (+) 0 (map2 (\x y -> i32.i16 x * i32.i16 y) a b)

entry matmul_i8 [n][m][k] (a: [n][m]i8) (b: [m][k]i8) : [n][k]i32 =
  map (\a_row ->
    map (\b_col ->
      reduce (+) 0 (map2 (\x y -> i32.i8 x * i32.i8 y) a_row b_col)
    ) (transpose b)
  ) a
