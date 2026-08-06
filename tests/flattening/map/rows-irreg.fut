-- Inner map over the rows of a 2D irregular array (Dense rep):
-- exercises the per-row segment splitting in onMapIrregularInputArr.
-- ==
-- input { [1i64,4i64,0i64,3i64] }
-- auto output
-- structure gpu { /SegScan 3 /Apply/repiota 3 /Apply/segiota 2 }
def main (ns: []i64) =
  #[incremental_flattening(only_inner)]
  map (\n ->
         let yss = opaque (map (\i -> map (+ i) (iota n)) (iota (n + 1)))
         in i64.sum (map i64.sum yss))
      ns
