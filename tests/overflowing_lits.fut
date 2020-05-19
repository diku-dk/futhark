-- Warn on overflowing literals
--
-- ==
-- warning: (out of bounds.*){3}

entry main : (i8, u8, f32) = (128, -4, -1e40)
