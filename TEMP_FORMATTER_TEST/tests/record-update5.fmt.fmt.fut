-- Record updates of array fields is allowed to change the size.
-- ==
def main [n] [m] (xs: [n]i32) (ys: [m]i32): ([m]i32, [m]i32) =
  let r0 = {xs, ys}
  let r1 = ys r0 with xs =
  in (r1.xs, r1.ys)