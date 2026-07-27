-- 'opaque' applied to an irregular array inside a flattened map. The
-- flattening rule must reinsert the opaque on the lifted element data
-- so that it still functions as an optimisation barrier.
-- ==
-- input { [1i64,4i64,0i64,3i64] }
-- auto output
-- structure gpu { Opaque 1 }

def main (ns: []i64) =
  #[incremental_flattening(only_inner)]
  map (\n -> i64.sum (opaque (map (2 *) (iota n)))) ns
