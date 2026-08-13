-- Nonuniform (irregular-width) scanomap with variant neutral element.
-- ==
-- input { [3i64,7i64,0i64,1i64] }
-- auto output
-- input { empty([0]i64) }
-- auto output
-- structure gpu { /Apply/repiota 1 /Apply/segiota 1 }

def main (ns: []i64) =
  #[flattening(only_inner)]
  map (\n ->
         let ne = opaque (if n < 0 then 1i64 else 0)
         in i64.sum (scan (+) ne (map (2 *) (iota n))))
      ns
