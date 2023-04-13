-- ==
-- input { [5i64,7i64] [5i64,7i64] }
-- output { [23i64, 28i64] }

def main = map2 (\n x ->
                   let A = #[opaque] map (+2) (iota n)
                   in i64.sum (map (\i -> A[i%x]) (iota n)))
