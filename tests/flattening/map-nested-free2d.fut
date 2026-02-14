-- ==
-- input { [5i64,7i64] [5i64,7i64] [3i64,2i64] }
-- output { [3i64, 2i64] }

def main = map3 (\n m x ->
                   #[unsafe]
                   let A = #[opaque] replicate n (replicate m x)
                   let B = #[opaque] map (\i -> A[i%x,i%x]) (iota n)
                   in B[0])
