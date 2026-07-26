-- ==
-- input { [1i64,2i64] [0, 1] [4,5] }
-- output { [4,5] }
-- structure gpu { SegScan 2 /Opaque 1 }

def main =
  map3 (\n (i: i32) (x: i32) ->
          let A = opaque (replicate n x)
          in #[unsafe] A[i])
