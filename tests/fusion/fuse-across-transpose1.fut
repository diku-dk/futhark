-- ==
-- structure { Screma 2 }

let main [n] (a: [][n]i32): [][]i32 =
  let b = map (\x1: [n]i32  -> map (+1) x1) a
  let c = map (\z1: [n]i32  -> map (*3) z1) (rearrange (1,0) b) in
  c
