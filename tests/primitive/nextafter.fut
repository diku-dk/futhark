-- ==
-- tags { no_webgpu }
-- entry: test_f16
-- input { [0f16, 0f16, -0f16, f16.nan] [1f16, -1f16, 0f16, f16.inf] }
-- output { [0.0f16, -0.0f16, 0.0f16, f16.nan] }

entry test_f16 = map2 f16.nextafter

-- ==
-- entry: test_f32
-- input { [0f32, 0f32, -0f32, f32.nan] [1f32, -1f32, 0f32, f32.inf] }
-- output { [1.0e-45f32, -1.0e-45f32, 0.0f32, f32.nan] }

entry test_f32 = map2 f32.nextafter

-- ==
-- entry: test_f64
-- input { [0f64, 0f64, -0f64, f64.nan, 5e-324f64, 1e-320f64] [1f64, -1f64, 0f64, f64.inf, 1f64, 1f64] }
-- output { [5.0e-324f64, -5.0e-324f64, 0.0f64, f64.nan, 1e-323f64, 1.0005e-320f64] }

entry test_f64 = map2 f64.nextafter

-- The result must be adjacent to the input, which is checked exactly
-- by comparing bit patterns.
-- ==
-- entry: test_f64_bits
-- input { [1f64, 3f64, 1e300f64, -1e300f64] [2f64, 0f64, 2e300f64, -2e300f64] }
-- output { [1i64, -1i64, 1i64, 1i64] }

entry test_f64_bits (xs: []f64) (ys: []f64) =
  map2 (\x y -> i64.u64 (f64.to_bits (f64.nextafter x y) - f64.to_bits x)) xs ys
