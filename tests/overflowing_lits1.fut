-- Warn on overflowing literals – edge cases that should produce warnings
--
-- ==
-- warning: (out of bounds.*){6}

entry main : (i8, i8, u8, u8, f32, f64)
  = (-129, 128, -4, 256, -1e40, 1.8e308)
