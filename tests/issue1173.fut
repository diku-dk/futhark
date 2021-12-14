-- ==
-- input { 10i64 }
-- output { 130f32 }

def main m =
  let f [n] m' v: ([m']f32, (os: [n]f32) -> f32) =
    (replicate m' (v+f32.i64 n),
     f32.sum)
  let (x, g) = f m 3
  in g x
