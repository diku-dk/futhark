
let main [n] (xs:[n]i32): [n](i32) =
  scan (+) 0 xs
