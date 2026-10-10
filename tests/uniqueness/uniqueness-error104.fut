-- A loop evaluates its initial value before its form, so the initial value
-- cannot consume what the form uses.
-- ==
-- error: Using variable "ys", but this was consumed

def main (ys: *[]i32) =
  loop acc = (let _ = ys with [0] = 1 in 0) for x in map (+ 1) ys do acc + x
