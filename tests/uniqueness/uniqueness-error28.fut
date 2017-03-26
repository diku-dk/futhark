-- Catch consumption even within curried expressions.
-- ==
-- error: QUUX.*consumed

let main () : []i32 =
  let QUUX = replicate 1 0
  let y = write [0] [2] QUUX
  let xs = map (+ QUUX[0]) [1]
  in xs
