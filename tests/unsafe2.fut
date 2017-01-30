-- Using unsafe we can also avoid assertions due to shape checks.
--
-- ==
-- structure { Assert 0 }

fun main(a: [n]i32, b: [m]i32): ([n]i32,[n]i32) =
  unzip(unsafe zip a b)
