-- Uniform redomap with variant neutral element; empty outer array too.
-- ==
-- input { [3i64,7i64,1i64] 4i64 }
-- auto output
-- input { empty([0]i64) 4i64 }
-- auto output
-- structure gpu { /SegRed 1 /Apply 0 }

def main (ns: []i64) (k: i64) =
  #[flattening(only_inner)]
  map (\n ->
         let ne = opaque (if n < 0 then 1i64 else 0)
         in reduce (+) ne (map (+ n) (iota k)))
      ns
