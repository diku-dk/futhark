-- Some ranges have known sizes.

let main (n: i64) : ([n]i64, [n]i64) =
  (0..<n, 1..2...n)
