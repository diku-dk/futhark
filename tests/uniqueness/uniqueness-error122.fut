-- A closure that refers to a local function only through an operator section
-- still captures that function's free variables.
-- ==
-- error: Using variable "xs", but this was consumed

def main (xs: *[]i32) =
  let op (a: i32) (b: i32) = a + b + xs[0]
  let g = \(y: i32) -> (`op` y) 1
  let xs[0] = 1
  in (g 2, xs)
