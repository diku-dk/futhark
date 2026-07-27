-- An irregular array that is free in the map part of an inner
-- redomap, and hence must be replicated. Its size intentionally
-- differs from the width of the redomap.
-- ==
-- input { [1i64,4i64,0i64,3i64] }
-- auto output

def main (ns: []i64) =
  #[incremental_flattening(only_inner)]
  map (\n ->
         let ys = opaque (iota (n + 1))
         in i64.sum (map (\i -> ys[i] + ys[i + 1]) (iota n)))
      ns
