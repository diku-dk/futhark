-- A contrived program that once made in-place lowering fail.

def main [n] (xs: *[n][n]i32, ys0: [n]i32, i: i32) : ([n][n]i32, i32) =
  let ys = map (+ 1) ys0
  let zs = map (+ 1) xs[i]
  let xs[i] = ys
  in (xs, zs[i])
