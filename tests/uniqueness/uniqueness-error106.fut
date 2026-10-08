-- An update evaluates its value before its source, and then uses the value,
-- so the source cannot consume what the value aliases.
-- ==
-- error: Using variable "ys", but this was consumed

def main (ys: *[]i32) (p: {a: []i32, b: i32}) =
  (let _ = ys with [0] = 1 in p) with a = ys
