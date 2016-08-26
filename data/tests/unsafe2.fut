-- Using unsafe we can also avoid assertions due to shape checks.
--
-- ==
-- structure { Assert 0 }

fun main(a: [n]int, b: [m]int): ([n]int,[n]int) =
  unzip(unsafe zip(a, b))
