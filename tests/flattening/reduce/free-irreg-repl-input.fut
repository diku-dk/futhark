-- Two irregular arrays that are free in an inner map and used as the
-- inputs of a redomap nested inside a variant branch (which keeps the
-- redomap from being hoisted). Both become replicated views, and
-- since the redomap has no variant free variables, the reduction runs
-- once on the shared per-segment data with no densification copies -
-- the SegScan count asserts this. The arrays have different
-- provenance (one is a slice of a larger array) to stress the
-- assumption that same-width replicated views are aligned.
-- ==
-- input { [1i64,4i64,0i64,3i64] }
-- auto output
-- structure gpu { /SegScan 4 /Apply/segiota 4 /Apply/repiota 4 /Apply/partition 1 }

def main (ns: []i64) =
  #[flattening(only_inner)]
  map (\n ->
         let zs_base = opaque (map (3 *) (iota (n + 1)))
         let ys = opaque (map (2 *) (iota n))
         let zs = zs_base[1:] :> [n]i64
         in i64.sum (#[flattening(only_inner)]
                     map (\m -> if m % 2 == 0 then i64.sum (map2 (*) ys zs) else m)
                         (iota (n + 1))))
      ns
