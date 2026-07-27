-- Inner map over the rows of a 2D irregular array that is free in the
-- middle map (Replicated rep): exercises the Replicated case of the
-- per-row segment splitting in onMapIrregularInputArr.
-- ==
-- input { [1i64,4i64,0i64,3i64] }
-- auto output
-- structure gpu { /SegScan 3 /Apply/repiota 7 /Apply/segiota 2 }
def main (ns: []i64) =
  #[incremental_flattening(only_inner)]
  map (\n ->
         let yss = opaque (map (\i -> map (+ i) (iota n)) (iota (n + 1)))
         in i64.sum (#[incremental_flattening(only_inner)]
                     map (\m -> i64.sum (map (\row -> i64.sum row + m) yss))
                         (iota n)))
      ns
