-- ==
-- input { [2i64,5i64] [3i64,4i64] [0,1] [[5,4], [4,5]] }
-- output { [[5, 4], [4, 5]] }

def main =
  map4 \n m (i: i32) (x: [2]i32) ->
    let A = opaque (replicate n x)
    let B = opaque (replicate m A)
    in #[unsafe] B[i, 0]
