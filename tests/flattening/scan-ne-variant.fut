-- Uniform maposcanomap whose neutral element is a variant expression
-- (but of course still neutral in value). Includes an empty outer array.
-- ==
-- input { [3i64,7i64,1i64] 4i64 }
-- auto output
-- input { empty([0]i64) 4i64 }
-- auto output
def main (ns: []i64) (k: i64) =
  #[incremental_flattening(only_inner)]
  map (\n ->
         let ne = opaque (if n < 0 then 1i64 else 0)
         in scan (+) ne (map (+ n) (iota k)))
      ns
