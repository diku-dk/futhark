-- ==
-- structure { Replicate 1 Screma 0 }

def main [n] (is: [n]i64) (vs: [n]f64) =
  scatter (#[blank] copy (map (+ 1) vs)) is vs
