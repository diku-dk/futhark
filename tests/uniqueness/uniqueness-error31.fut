-- In-place updates with 'with' can also have errors.
-- ==
-- error: in-place

def main [n] (a: *[][n]i32, i: i32) : [][]i32 =
  a with [i] = a[0]
