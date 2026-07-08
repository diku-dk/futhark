-- ==
-- structure { Replicate 0 Scratch 1 }

entry main [n] (is: [n]i64) (vs: [n]f64) =
  scatter (#[scratch] copy vs) is vs
