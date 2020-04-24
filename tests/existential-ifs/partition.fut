-- ==
-- input  { [1, 1, 1, 1, 1] }
-- output { [0, 1, 2, 3, 4] empty([0]i32)  }
let main [n] (cost: *[n]i32) =
  if opaque(true)
  then partition (\_ -> (opaque true)) (iota n)
  else ([], [])
