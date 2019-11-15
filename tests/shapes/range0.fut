-- Some ranges have known sizes.

let main (n: i32) : ([n]i32, [n]i32) =
  (0..<n, 1..2...n)
