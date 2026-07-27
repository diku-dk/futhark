-- A two-dimensional irregular array that is free in the map part of
-- an inner redomap, and hence must be replicated.
-- ==
-- input { [1i64,4i64,0i64,3i64] }
-- auto output

def main (ns: []i64) =
  map (\n ->
         let yss = opaque (map (\i -> map (+ i) (iota n)) (iota n))
         in i64.sum (map (\i -> yss[i, i]) (iota n)))
      ns
