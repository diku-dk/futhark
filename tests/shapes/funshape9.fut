-- f's own return is a bare closure (not wrapped in a record).
-- ==
-- input { 2i64 3i64 } output { [0.0f32, 0.0f32, 0.0f32, 0.0f32, 0.0f32, 0.0f32, 0.0f32, 0.0f32, 0.0f32] }
def f (n: i64) (m: i64) = \(g: f32 -> [m ** n]f32) -> g 0

entry main n m = f n m (\x -> replicate (m ** n) x)
