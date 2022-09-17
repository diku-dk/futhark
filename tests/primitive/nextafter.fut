-- ==
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
-- input { [0f64, 0f64, -0f64, f64.nan] [1f64, -1f64, 0f64, f64.inf] }
-- output { [5.0e-324f64, -5.0e-324f64, 0.0f64, f64.nan] }

entry test_f64 = map2 f64.nextafter
