-- Based on #2218.
-- ==
-- structure gpu { /SegMap 1 /SegMap/SegMap 1 }

def main (is: []i64) (vs: [][]f64) =
  map (\xsss ->
         #[incremental_flattening(only_intra)]
         map (\xss ->
                scatter (copy xss) is vs)
             xsss)
