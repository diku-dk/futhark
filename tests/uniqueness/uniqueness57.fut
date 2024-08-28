-- ==
-- error: "f".*consumed

def main [n] (xs: *[n]i32) =
  let f i = xs[i]
  in loop ys = xs for i < n do ys with [i] = f i
