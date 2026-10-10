-- As uniqueness-error116, but through a partial application of the local
-- polymorphic function.
-- ==
-- error: Using variable "xs", but this was consumed

def main (xs: *[]i32) (ys: []i32) =
  let k 'p 'q (x: p) (_: q) : p = x
  let g = k (xs, ys)
  let (a, _) = g 0i32
  let xs[0] = 1
  in a
