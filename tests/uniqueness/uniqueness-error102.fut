-- The function being applied is held while its arguments are evaluated, so an
-- argument cannot consume what the function captures.
-- ==
-- error: Using variable "ys", but this was consumed

def main (ys: *[]i32) =
  let g = \(i: i32) -> ys[i]
  in g (let _ = ys with [0] = 1 in 0)
