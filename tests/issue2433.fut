-- ==
-- input { 2i64 3i64 [1f32, 2f32, 3f32, 4f32, 5f32] }
-- output { [1f32, 2f32, 3f32, 4f32, 5f32] }

entry main (n: i64) (m: i64) (xs: [n + m]f32) = xs
