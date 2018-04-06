-- The defunctionaliser once messed up local closures.

let main (n: i32) =
  let scale (x: i32) (y: i32) = (x+y) / n
  in map (scale 1) (iota n)
