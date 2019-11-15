-- Using unsafe we can also avoid assertions due to shape checks.
--
-- ==
-- structure { Assert 0 }

let main [n][m] (a: [n]i32, b: [m]i32): ([n]i32,[n]i32) =
  unzip(unsafe zip a (b :> [n]i32))
