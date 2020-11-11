-- ==
-- entry: main
--
-- compiled random input { [268435456]i32 } auto output

let main [n] (xs:[n]i32): [n]i32 =
  scan (+) 0 xs
