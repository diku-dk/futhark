-- A loop may permute its observed parameters, so each component of its result
-- may be any observed part of the initial value.
-- ==
-- error: Using variable "ys", but this was consumed

def main (xs: []i32) (ys: *[]i32) =
  let (a, _) = loop (a, b) = (xs, ys) for _i < 1 do (b, a)
  let ys[0] = 42
  in a
