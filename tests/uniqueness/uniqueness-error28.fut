-- Catch consumption even within curried expressions.
-- ==
-- error: QUUX.*consumed

let main () : []i32 =
  let QUUX = replicate 1 0
  let y = scatter QUUX [0] [2]
  let xs = map (+ QUUX[0]) [1]
  in xs
