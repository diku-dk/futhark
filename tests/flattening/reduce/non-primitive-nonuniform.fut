-- An inner reduce with a non-primitive operator whose result is
-- irregular. This cannot be expressed as a segmented reduction, so it
-- must be sequentialised, and the resulting loop flattened.
--
-- This reflects a weakness in flattening - this could indeed be parallelised
-- better, but it requires rewriting the reduce to be a tree-reduction in terms
-- of loop and map. Likely not faster in practice.
-- ==
-- input { [1i64,4i64,0i64,3i64] }
-- auto output
-- structure gpu { /Loop 1 /SegScan 1 /Apply/segiota 1 }

def main (ns: []i64) =
  #[incremental_flattening(only_inner)]
  map (\n ->
         i64.sum (reduce (map2 (+))
                         (replicate n 0i64)
                         (map (\x -> replicate n x) (iota 4))))
      ns
