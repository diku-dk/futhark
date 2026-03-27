-- Some edge cases of literals that don't overflow, but are close
--
-- ==

-- tags { no_webgpu }
entry main : (i8, i8, u16, f64) = (-128, 127, 65535, 1.79e308)
