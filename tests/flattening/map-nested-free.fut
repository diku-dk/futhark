-- ==
-- input { [5i64,7i64] [5i64,7i64] }
-- output { [5i64, 7i64] }

def main =
  map2 (\n x ->
          #[unsafe]
          let A = #[opaque] replicate n x
          let B = #[opaque] map (\i -> A[i % x]) (iota n)
          in B[0])
