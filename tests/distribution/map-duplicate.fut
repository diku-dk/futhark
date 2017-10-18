-- A map with duplicate outputs should work.
-- ==
-- structure distributed { Kernel 1 }

let main (n: i32) (m: i32) =
  map (\i -> (replicate m i, replicate m i)) (iota n)
