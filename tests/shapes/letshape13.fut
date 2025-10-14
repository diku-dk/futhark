-- ==
-- input { 3 } output { 1i64 2i64 }

-- The module is just to make sure we get the right type for the function.
module m
  : {
      val contrived : i32 -> ?[n][m].([n]i32, [m]i32, [n + m]i32)
    } = {
  def contrived x =
    (replicate 1 x, replicate 2 x, replicate (1 + 2) x)
}

def main x =
  let [a][b] (_: ([a]i32, [b]i32, [a + b]i32)) = m.contrived x
  in (a, b)
