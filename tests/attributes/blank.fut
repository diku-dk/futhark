-- ==
-- structure { Replicate 1 Screma 1 }

def main [n] (is: [n]i64) (vs: [n]f32) =
  scatter (#[blank] copy (map (+ 1) vs)) is vs
