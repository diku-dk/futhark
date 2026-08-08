-- An irregular array that is free in a middle map and again in the
-- redomap nested inside it, so the replicated view constructed for
-- the middle map is itself turned into a view for the inner redomap
-- (a view of a view).
-- ==
-- input { [1i64,4i64,0i64,3i64] }
-- auto output
-- structure gpu { /SegScan 2 }

def main (ns: []i64) =
  #[flattening(only_inner)]
  map (\n ->
         let ys = opaque (map (2 *) (iota n))
         in i64.sum (#[flattening(only_inner)]
                     map (\m -> i64.sum (map (\i -> ys[i] + m) (iota n))) (iota n)))
      ns
