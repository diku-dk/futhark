-- ==
-- structure { Replicate 0 Scratch 1 }

entry main [n] (is: [n]i64) (vs: [n]f64) =
  scatter (#[scratch] copy vs) is vs

-- The following test is inspired by #2417 and ensure we don't get rid of
-- assertions.

entry permute [n] (is: [n]i64) (vs: [n]i64) =
  let filled = scatter (replicate n false) is (rep true) |> and
  in assert filled (scatter (#[scratch] copy vs) is vs)

-- ==
-- entry: permute
-- input {[0i64,1i64,2i64,3i64] [1i64,1i64,1i64,1i64]}
-- input {[0i64,1i64,2i64,0i64] [1i64,1i64,1i64,1i64]}
-- error: Assertion is false
