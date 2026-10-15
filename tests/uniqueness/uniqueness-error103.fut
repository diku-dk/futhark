-- The arguments of an application are evaluated before the function, so an
-- argument cannot consume what evaluating the function uses.
-- ==
-- error: Using variable "ys", but this was consumed

def main (ys: *[]i32) =
  (let k = ys[0] in \(x: i32) -> x + k) (let _ = ys with [0] = 1 in 2)
