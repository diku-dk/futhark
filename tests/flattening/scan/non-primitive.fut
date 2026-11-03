-- An inner scan with a non-primitive operator over an irregular
-- width. This cannot be expressed as a segmented scan, so it must be
-- sequentialised, and the resulting loop flattened.
--
-- This reflects a weakness in flattening - this could indeed be parallelised
-- better, but it requires rewriting the scan in terms of loop and map. Likely
-- not faster in practice.
-- ==
-- input { [1i64,4i64,0i64,3i64] }
-- auto output
-- structure gpu { /Loop 1 }

def main (ns: []i64) =
  #[flattening(only_inner)]
  map (\n ->
         i64.sum (map (\r -> i64.sum r)
                      (scan (map2 (+))
                            (replicate 3 0i64)
                            (map (\x -> [x, x + 1, x + 2]) (iota n)))))
      ns
