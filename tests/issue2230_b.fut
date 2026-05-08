-- ==
-- input { 1i64 2i64 } output { [0.0,0.0] }

def f (n: i64) (m: i64) (g: f64 -> [m ** n]f64) = g 0

entry main a b =
  let h a' b' c = f a' b' c
  in h a b (\x -> replicate (b ** a) x)
