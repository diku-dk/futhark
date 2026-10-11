-- The result of a local polymorphic function instantiated at a tuple aliases
-- what the tuple was built from.
-- ==
-- error: Using variable "xs", but this was consumed

def main (xs: *[]i32) (ys: []i32) =
  let pid 'q (x: q) : q = x
  let (a, _) = pid (xs, ys)
  let xs[0] = 1
  in a
