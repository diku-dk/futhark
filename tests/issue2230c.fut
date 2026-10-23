-- ==
-- input { 1i64 2i64 } output { [0.0f32,0.0f32] }

def f (n: i64) = {h = \(m: i64) (g: f32 -> [m ** n]f32) -> g 0}

entry main a b = (f a).h b (\x -> replicate (b ** a) x)
