-- Do not hide aliases with flatten.
-- ==
-- error: Cannot apply

def main [n] (xss: [n][n]i32) : *[]i32 =
  let xs = flatten xss
  in scatter xs (iota n) (iota n)
