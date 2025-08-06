-- Test that a unique array is properly considered non-unique inside a
-- lambda body (mostly so that the type annotation is correct.

def main [n] [m] (x: i32, a: *[n][m]i32) =
  let b = transpose a
  in map1 (\x -> b[m - x - 1]) (iota m)
