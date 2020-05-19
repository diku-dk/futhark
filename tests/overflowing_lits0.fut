-- Warn on overflowing literals
--
-- ==
-- warning: (out of bounds.*){5}

entry main : (i16, i32, u16, u32, f32)
  = (-10000000, 1000000000000, 100000, -4, 9.7e42)
