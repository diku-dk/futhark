-- Some ranges have known sizes.

def main (n: i64) : ([n]i64, [n]i64, [n]i64, [n + 1 - 1]i64) =
  (0..<n, 0..1..<n, 1..2...n, 1..<n + 1)
